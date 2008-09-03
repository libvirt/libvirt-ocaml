(* Simple demo program showing how to list out domains.
   Usage: list_domains [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

let string_of_state = function
  | D.InfoNoState -> "no state"
  | D.InfoRunning -> "running"
  | D.InfoBlocked -> "blocked"
  | D.InfoPaused -> "paused"
  | D.InfoShutdown -> "shutdown"
  | D.InfoShutoff -> "shutoff"
  | D.InfoCrashed -> "crashed"

let () =
  try
    let name =
      if Array.length Sys.argv >= 2 then
	Some (Sys.argv.(1))
      else
	None in
    let conn = C.connect_readonly ?name () in

    (* List all domains (running and inactive). *)
    let domains = D.get_domains_and_infos conn [D.ListAll] in
    List.iter (
      fun (dom, info) ->
	if info.D.state <> D.InfoShutoff then
	  printf "%8d %-20s %s\n%!"
	    (D.get_id dom) (D.get_name dom) (string_of_state info.D.state)
	else
	  printf "%8s %-20s shutoff\n%!" "" (D.get_name dom)
    ) domains
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  (* Run the garbage collector which is a good way to check for
   * memory corruption errors and reference counting issues in libvirt.
   *)
  Gc.compact ()
