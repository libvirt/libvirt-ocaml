(* Simple demo program showing how to list out secrets.
   Usage: list_secrets [URI]
   (C) Copyright 2018 Pino Toscano, Red Hat Inc.
   https://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module S = Libvirt.Secret

let string_of_secret_usage_type = function
  | S.NoType -> "none"
  | S.Volume -> "volume"
  | S.Ceph -> "ceph"
  | S.ISCSI -> "iscsi"
  | S.TLS -> "tls"

let () =
  try
    let name =
      if Array.length Sys.argv >= 2 then
	Some (Sys.argv.(1))
      else
	None in
    let conn = C.connect_auth_readonly ?name (C.get_auth_default ()) in

    (* List all the secrets. *)
    let secrets = C.list_secrets conn (C.num_of_secrets conn) in
    let secrets = Array.to_list secrets in
    let secrets = List.map (S.lookup_by_uuid_string conn) secrets in
    List.iter (
      fun secret ->
	let uuid = S.get_uuid_string secret in
	let usageType = string_of_secret_usage_type (S.get_usage_type secret) in
	let usageId = S.get_usage_id secret in
	printf "%*s  %-7s  %s\n%!"
	  (Libvirt.uuid_string_length) uuid usageType usageId
    ) secrets
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  (* Run the garbage collector which is a good way to check for
   * memory corruption errors and reference counting issues in libvirt.
   *)
  Gc.compact ()
