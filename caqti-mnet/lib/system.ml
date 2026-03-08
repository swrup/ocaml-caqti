(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

module type FLOW = Caqti_platform.System_sig.SOCKET_OPS with type 'a fiber = 'a

module TCP = Mnet.TCP

type ocaml = |

and system = |

type 'a impl =
  | OCaml : (module FLOW with type t = 'a) * 'a -> ocaml impl
  | System : Buffer.t * TCP.flow -> system impl

type socket = Socket : 'a impl -> socket [@@unboxed]

external reraise : exn -> 'a = "%reraise"

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

module System_core = struct
  include Caqti_miou.System_core

  type stdenv =
    { stack : Mnet.stack
    ; state : TCP.state
    ; dns : Mnet_dns.t
    }
end

include System_core

module Alarm = struct
  type t = Miou.Condition.t * Miou.Mutex.t

  let schedule ~sw ~stdenv:_ t fn =
    let t_now = Mtime.of_uint64_ns (Int64.of_int (Mkernel.clock_monotonic ())) in
    let dt_ns =
      if Mtime.is_later t ~than:t_now then 0 else
      Int64.to_int (Mtime.Span.to_uint64_ns (Mtime.span t t_now))
    in
    let mutex = Miou.Mutex.create ()
    and condition = Miou.Condition.create () in
    Logs.debug (fun m -> m "schedule an alarm");
    let _ =
      async ~sw @@ fun () ->
      Logs.debug (fun m -> m "really schedule an alarm");
      let sleeper =
        Miou.async @@ fun () ->
        Logs.debug (fun m -> m "Sleep %dns" dt_ns);
        Mkernel.sleep dt_ns;
        Logs.debug (fun m -> m "Ring the alarm");
        `Continue
      in
      let canceller =
        Miou.async @@ fun () ->
        Miou.Condition.wait condition mutex;
        `Cancel
      in
      match Miou.await_first [ sleeper; canceller ] with
      | Ok `Continue -> fn ()
      | Ok `Cancel -> ()
      | Error _exn -> ()
    in
    (condition, mutex)

  let unschedule (condition, mutex) =
    Miou.Mutex.protect mutex @@ fun () -> Miou.Condition.signal condition
end

module Stream = Caqti_miou.Stream
module Pool = Caqti_platform.Pool.Make (System_core) (Alarm)

module Net = struct
  module Sockaddr = struct
    type t =
      [ `Tcp of Ipaddr.t * int
      | `Unix of string
      ]

    let unix s = `Unix s

    let tcp (host, port) = `Tcp (host, port)
  end

  let getaddrinfo_ipv4 dns host port =
    let extract (_, ips) =
      Ipaddr.V4.Set.elements ips
      |> List.map (fun ip -> `Tcp (Ipaddr.V4 ip, port))
    in
    Mnet_dns.getaddrinfo dns Dns.Rr_map.A host |> Result.map extract

  let getaddrinfo_ipv6 dns host port =
    let extract (_, ips) =
      Ipaddr.V6.Set.elements ips
      |> List.map (fun ip -> `Tcp (Ipaddr.V6 ip, port))
    in
    Mnet_dns.getaddrinfo dns Dns.Rr_map.Aaaa host |> Result.map extract

  let getaddrinfo ~stdenv:{ stack; state = _; dns } host port =
    let laddrs = Mnet.addresses stack in
    match
      ( List.exists Ipaddr.(function V4 _ -> true | V6 _ -> false) laddrs
      , List.exists Ipaddr.(function V4 _ -> false | V6 _ -> true) laddrs )
    with
    | true, true -> (
      let r4 = getaddrinfo_ipv4 dns host port in
      let r6 = getaddrinfo_ipv6 dns host port in
      match (r4, r6) with
      | Ok addrs4, Ok addrs6 -> Ok (addrs4 @ addrs6)
      | Ok addrs, Error _ | Error _, Ok addrs -> Ok addrs
      | Error (`Msg msg4), Error (`Msg msg6) ->
        if String.equal msg4 msg6 then Error (`Msg msg4)
        else Error (`Msg ("IPv4: " ^ msg4 ^ " IPv6: " ^ msg6)) )
    | true, false -> getaddrinfo_ipv4 dns host port
    | false, true -> getaddrinfo_ipv6 dns host port
    | false, false -> Error (`Msg "No IP address assigned to host.")

  let convert_io_exception = function
    | Failure msg -> Some (Caqti_error.Msg msg) (* Channel.S.error *)
    | _ -> None

  type tcp_flow = TCP.flow

  type tls_flow = ocaml impl

  module Socket = struct
    type t = socket

    let output_char (Socket impl) chr =
      match impl with
      | System (buf, _) -> Buffer.add_char buf chr
      | OCaml ((module Flow), fd) -> Flow.output_char fd chr

    let output_string (Socket impl) str =
      match impl with
      | System (buf, _) -> Buffer.add_string buf str
      | OCaml ((module Flow), fd) -> Flow.output_string fd str

    let flush (Socket impl) =
      match impl with
      | System (buf, fd) ->
        let str = Buffer.contents buf in
        Buffer.clear buf;
        if String.length str > 0 then TCP.write_without_interruption fd str
      | OCaml ((module Flow), fd) -> Flow.flush fd

    let input_char (Socket impl) =
      match impl with
      | System (_, fd) ->
        let buf = Bytes.make 1 '\000' in
        let len = TCP.read fd buf in
        if len = 0 then raise End_of_file else Bytes.get buf 0
      | OCaml ((module Flow), fd) -> Flow.input_char fd

    let really_input (Socket impl) buf off len =
      match impl with
      | System (_, fd) ->
        let rec go off len =
          if len > 0 then
            let len' = TCP.read fd buf ~off ~len in
            go (off + len') (len - len')
        in
        go off len
      | OCaml ((module Flow), fd) -> Flow.really_input fd buf off len

    let close = function
      | Socket (System (_, fd)) -> TCP.close fd
      | Socket (OCaml ((module Flow), fd)) -> Flow.close fd
  end

  let connect_tcp ~sw:_ ~stdenv sockaddr =
    match sockaddr with
    | `Unix _ ->
      Error (Caqti_error.Msg "Unix sockets are not available under MirageOS.")
    | `Tcp (ipaddr, port) ->
      let flow = TCP.connect stdenv.state (ipaddr, port) in
      Ok (Socket (System (Buffer.create 0x7ff, flow)))

  let tcp_flow_of_socket (Socket impl) =
    match impl with System (_, fd) -> Some fd | OCaml _ -> None

  let socket_of_tls_flow : sw:_ -> tls_flow -> Socket.t =
   fun ~sw:_ -> function OCaml _ as impl -> Socket impl

  module type TLS_PROVIDER =
    Caqti_platform.System_sig.TLS_PROVIDER
      with type 'a fiber := 'a
       and type tcp_flow := tcp_flow
       and type tls_flow := tls_flow

  let tls_providers_r : (module TLS_PROVIDER) list ref = ref []

  let register_tls_provider p = tls_providers_r := p :: !tls_providers_r

  let tls_providers config =
    if Caqti_connect_config.mem_name "tls" config then begin
      match Caqti_platform.Connector.load_library "caqti-tls-miou" with
      | Ok () -> ()
      | Error msg ->
        Logs.warn (fun m ->
            m "TLS configured, but missing caqti-tls-miou: %s" msg )
    end;
    !tls_providers_r
end
