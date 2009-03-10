(* virsh-like command line tool.
   (C) Copyright 2007-2008 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*)

open Printf
open Mlvirsh_gettext.Gettext

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

(* Program name. *)
let program_name = Filename.basename Sys.executable_name

(* Parse arguments. *)
let name = ref ""
let readonly = ref false

let argspec = Arg.align [
  "-c", Arg.Set_string name, "URI " ^ s_"Hypervisor connection URI";
  "-r", Arg.Set readonly,    " " ^    s_"Read-only connection";
]

let usage_msg =
  sprintf (f_"Synopsis:
  %s [options] [command]

List of all commands:
  %s help

Full description of a single command:
  %s help command

Options:")
    program_name program_name program_name

let add_extra_arg, get_extra_args =
  let extra_args = ref [] in
  let add_extra_arg s = extra_args := s :: !extra_args in
  let get_extra_args () = List.rev !extra_args in
  add_extra_arg, get_extra_args

let () = Arg.parse argspec add_extra_arg usage_msg

let name = match !name with "" -> None | name -> Some name
let readonly = !readonly
let extra_args = get_extra_args ()

(* Read a whole file into memory and return it (as a string). *)
let rec input_file filename =
  let chan = open_in_bin filename in
  let data = input_all chan in
  close_in chan;
  data
and input_all chan =
  let buf = Buffer.create 16384 in
  let tmpsize = 16384 in
  let tmp = String.create tmpsize in
  let n = ref 0 in
  while n := input chan tmp 0 tmpsize; !n > 0 do
    Buffer.add_substring buf tmp 0 !n;
  done;
  Buffer.contents buf

(* Split a string at a separator.
 * Functions copied from extlib Copyright (C) 2003 Nicolas Cannasse et al.
 * to avoid the explicit dependency on extlib.
 *)
let str_find str sub =
  let sublen = String.length sub in
  if sublen = 0 then
    0
  else
    let found = ref 0 in
    let len = String.length str in
    try
      for i = 0 to len - sublen do
        let j = ref 0 in
        while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
          incr j;
          if !j = sublen then begin found := i; raise Exit; end;
        done;
      done;
      raise Not_found
    with
      Exit -> !found

let str_split str sep =
  let p = str_find str sep in
  let len = String.length sep in
  let slen = String.length str in
  String.sub str 0 p, String.sub str (p + len) (slen - p - len)

let str_nsplit str sep =
  if str = "" then []
  else (
    let rec nsplit str sep =
      try
	let s1 , s2 = str_split str sep in
	s1 :: nsplit s2 sep
      with
	Not_found -> [str]
    in
    nsplit str sep
  )

(* Hypervisor connection. *)
type conn_t = No_connection | RO of Libvirt.ro C.t | RW of Libvirt.rw C.t
let conn = ref No_connection

let close_connection () =
  match !conn with
  | No_connection -> ()
  | RO c ->
      C.close c;
      conn := No_connection
  | RW c ->
      C.close c;
      conn := No_connection

let do_command =
  (* Command helper functions.
   *
   * Each cmd<n> is a function that constructs a command.
   *    string string string  ...  <--- user types on the command line
   *      |      |      |
   *     arg1   arg2   arg3   ...  <--- conversion functions
   *      |      |      |
   *      V      V      V
   *         function f            <--- work function
   *             |
   *             V
   *        print result           <--- printing function
   *
   * (Note that cmd<n> function constructs and returns the above
   * function, it isn't the function itself.)
   *
   * Example: If the function takes one parameter (an int) and
   * returns a string to be printed, you would use:
   *
   *   cmd1 print_endline f int_of_string
   *)
  let cmd0 print fn = function		(* Command with no args. *)
    | [] -> print (fn ())
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmd1 print fn arg1 = function	(* Command with one arg. *)
    | [str1] -> print (fn (arg1 str1))
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmd2 print fn arg1 arg2 = function (* Command with 2 args. *)
    | [str1; str2] -> print (fn (arg1 str1) (arg2 str2))
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmd3 print fn arg1 arg2 arg3 = function (* Command with 3 args. *)
    | [str1; str2; str3] -> print (fn (arg1 str1) (arg2 str2) (arg3 str3))
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmd4 print fn arg1 arg2 arg3 arg4 = function (* Command with 4 args. *)
    | [str1; str2; str3; str4] ->
	print (fn (arg1 str1) (arg2 str2) (arg3 str3) (arg4 str4))
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmd01 print fn arg1 = function	(* Command with 0 or 1 arg. *)
    | [] -> print (fn None)
    | [str1] -> print (fn (Some (arg1 str1)))
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmd12 print fn arg1 arg2 = function (* Command with 1 or 2 args. *)
    | [str1] -> print (fn (arg1 str1) None)
    | [str1; str2] -> print (fn (arg1 str1) (Some (arg2 str2)))
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmd012 print fn arg1 arg2 = function (* Command with 0, 1 or 2 args. *)
    | [] -> print (fn None None)
    | [str1] -> print (fn (Some (arg1 str1)) None)
    | [str1; str2] -> print (fn (Some (arg1 str1)) (Some (arg2 str2)))
    | _ -> failwith (s_"incorrect number of arguments for function")
  in
  let cmdN print fn =		(* Command with any number of args. *)
    fun args -> print (fn args)
  in

  (* Get the connection or fail if we don't have one. *)
  let rec get_full_connection () =
    match !conn with
    | No_connection -> failwith (s_"not connected to the hypervisor")
    | RO _ -> failwith (s_"tried to do read-write operation on read-only hypervisor connection")
    | RW conn -> conn
  and get_readonly_connection () =
    match !conn with
    | No_connection -> failwith (s_"not connected to the hypervisor")
    | RO conn -> conn
    | RW conn -> C.const conn
(*
  and with_full_connection fn =
    fun () -> fn (get_full_connection ())
*)
  and with_readonly_connection fn =
    fun () -> fn (get_readonly_connection ())
  and arg_full_connection fn =
    fun str -> fn (get_full_connection ()) str
  and arg_readonly_connection fn =
    fun str -> fn (get_readonly_connection ()) str
  in

  (* Parsing of command arguments. *)
  let string_of_readonly = function
    | "readonly" | "read-only" | "ro" -> true
    | _ -> failwith (sprintf (f_"flag should be '%s'") "readonly")
  in
  let string_of_string (str : string) = str in
  let boolean_of_string = function
    | "enable" | "enabled" | "on" | "1" | "true" -> true
    | "disable" | "disabled" | "off" | "0" | "false" -> false
    | _ -> failwith (sprintf (f_"setting should be '%s' or '%s'") "on" "off")
  in
  let domain_of_string conn str =
    try
      (try
	 let id = int_of_string str in
	 D.lookup_by_id conn id
       with
	 Failure "int_of_string" ->
	   if String.length str = Libvirt.uuid_string_length then
	     D.lookup_by_uuid_string conn str
	   else
	     D.lookup_by_name conn str
      )
    with
      Libvirt.Virterror err ->
	failwith (sprintf (f_"domain %s: not found.  Additional info: %s")
		    str (Libvirt.Virterror.to_string err));
  in
  let network_of_string conn str =
    try
      if String.length str = Libvirt.uuid_string_length then
	N.lookup_by_uuid_string conn str
      else
	N.lookup_by_name conn str
    with
      Libvirt.Virterror err ->
	failwith (sprintf (f_"network %s: not found.  Additional info: %s")
		    str (Libvirt.Virterror.to_string err));
  in
  let rec parse_sched_params = function
    | [] -> []
    | [_] -> failwith (s_"expected field value pairs, but got an odd number of arguments")
    | field :: value :: rest ->
	(* XXX We only support the UINT type at the moment. *)
	(field, D.SchedFieldUInt32 (Int32.of_string value))
	  :: parse_sched_params rest
  in
  let cpumap_of_string str =
    let c = get_readonly_connection () in
    let info = C.get_node_info c in
    let cpumap =
      String.make (C.cpumaplen (C.maxcpus_of_node_info info)) '\000' in
    List.iter (C.use_cpu cpumap)
      (List.map int_of_string (str_nsplit str ","));
    cpumap
  in

  (* Printing of command results. *)
  let no_return _ = () in
  let print_int i = print_endline (string_of_int i) in
  let print_int64 i = print_endline (Int64.to_string i) in
  let print_int64_array a = Array.iter print_int64 a in
  let print_bool b = print_endline (string_of_bool b) in
  let print_version v =
    let major = v / 1000000 in
    let minor = (v - major * 1000000) / 1000 in
    let release = (v - major * 1000000 - minor * 1000) in
    printf "%d.%d.%d\n" major minor release
  in
  let string_of_domain_state = function
    | D.InfoNoState -> s_"unknown"
    | D.InfoRunning -> s_"running"
    | D.InfoBlocked -> s_"blocked"
    | D.InfoPaused -> s_"paused"
    | D.InfoShutdown -> s_"shutdown"
    | D.InfoShutoff -> s_"shutoff"
    | D.InfoCrashed -> s_"crashed"
  in
  let string_of_vcpu_state = function
    | D.VcpuOffline -> s_"offline"
    | D.VcpuRunning -> s_"running"
    | D.VcpuBlocked -> s_"blocked"
  in
  let print_domain_list doms =
    List.iter (
      fun (dom, info) ->
	let id =
	  try sprintf "%d" (D.get_id dom)
	  with Libvirt.Virterror _ -> "" in
	let name =
	  try sprintf "%s" (D.get_name dom)
	  with Libvirt.Virterror _ -> "" in
	let state =
	  try
	    let { D.state = state } = info in
	    string_of_domain_state state
	  with Libvirt.Virterror _ -> "" in
	printf "%5s %-30s %s\n" id name state
    ) doms
  in
  let print_network_array nets =
    Array.iter (
      fun net ->
	printf "%s\n" (N.get_name net)
    ) nets
  in
  let print_node_info info =
    let () = printf (f_"model: %s\n") info.C.model in
    let () = printf (f_"memory: %Ld K\n") info.C.memory in
    let () = printf (f_"cpus: %d\n") info.C.cpus in
    let () = printf (f_"mhz: %d\n") info.C.mhz in
    let () = printf (f_"nodes: %d\n") info.C.nodes in
    let () = printf (f_"sockets: %d\n") info.C.sockets in
    let () = printf (f_"cores: %d\n") info.C.cores in
    let () = printf (f_"threads: %d\n") info.C.threads in
    ()
  in
  let print_domain_state { D.state = state } =
    print_endline (string_of_domain_state state)
  in
  let print_domain_info info =
    let () = printf (f_"state: %s\n") (string_of_domain_state info.D.state) in
    let () = printf (f_"max_mem: %Ld K\n") info.D.max_mem in
    let () = printf (f_"memory: %Ld K\n") info.D.memory in
    let () = printf (f_"nr_virt_cpu: %d\n") info.D.nr_virt_cpu in
    let () = printf (f_"cpu_time: %Ld ns\n") info.D.cpu_time in
    ()
  in
  let print_sched_param_array params =
    Array.iter (
      fun (name, value) ->
	printf "%-20s" name;
	match value with
	| D.SchedFieldInt32 i -> printf " %ld\n" i
	| D.SchedFieldUInt32 i -> printf " %lu\n" i
	| D.SchedFieldInt64 i -> printf " %Ld\n" i
	| D.SchedFieldUInt64 i -> printf " %Lu\n" i
	| D.SchedFieldFloat f -> printf " %g\n" f
	| D.SchedFieldBool b -> printf " %b\n" b
    ) params
  in
  let print_vcpu_info (ncpus, vcpu_infos, cpumaps, maplen, maxcpus) =
    for n = 0 to ncpus-1 do
      let () = printf (f_"virtual CPU: %d\n") n in
      let () = printf (f_"\ton physical CPU: %d\n") vcpu_infos.(n).D.cpu in
      let () = printf (f_"\tcurrent state: %s\n")
	(string_of_vcpu_state vcpu_infos.(n).D.vcpu_state) in
      let () = printf (f_"\tCPU time: %Ld ns\n") vcpu_infos.(n).D.vcpu_time in
      print_string ("\t" ^ s_"CPU affinity" ^ ": ");
      for m = 0 to maxcpus-1 do
	print_char (if C.cpu_usable cpumaps maplen n m then 'y' else '-')
      done;
      print_endline "";
    done
  in
  let print_block_stats { D.rd_req = rd_req; rd_bytes = rd_bytes;
			  wr_req = wr_req; wr_bytes = wr_bytes;
			  errs = errs } =
    if rd_req >= 0L then   printf (f_"read requests: %Ld\n") rd_req;
    if rd_bytes >= 0L then printf (f_"read bytes: %Ld\n") rd_bytes;
    if wr_req >= 0L then   printf (f_"write requests: %Ld\n") wr_req;
    if wr_bytes >= 0L then printf (f_"write bytes: %Ld\n") wr_bytes;
    if errs >= 0L then     printf (f_"errors: %Ld\n") errs;
  and print_interface_stats { D.rx_bytes = rx_bytes; rx_packets = rx_packets;
			      rx_errs = rx_errs; rx_drop = rx_drop;
			      tx_bytes = tx_bytes; tx_packets = tx_packets;
			      tx_errs = tx_errs; tx_drop = tx_drop } =
    if rx_bytes >= 0L then   printf (f_"rx bytes: %Ld\n") rx_bytes;
    if rx_packets >= 0L then printf (f_"rx packets: %Ld\n") rx_packets;
    if rx_errs >= 0L then    printf (f_"rx errs: %Ld\n") rx_errs;
    if rx_drop >= 0L then    printf (f_"rx dropped: %Ld\n") rx_drop;
    if tx_bytes >= 0L then   printf (f_"tx bytes: %Ld\n") tx_bytes;
    if tx_packets >= 0L then printf (f_"tx packets: %Ld\n") tx_packets;
    if tx_errs >= 0L then    printf (f_"tx errs: %Ld\n") tx_errs;
    if tx_drop >= 0L then    printf (f_"tx dropped: %Ld\n") tx_drop;
  in

  (* Help for domain, network parameters. *)
  let dom_help = s_"dom", s_"domain ID or name" in
  let net_help = s_"net", s_"network ID or name" in
  let readonly_help = s_"readonly|ro", s_"if given, connect read-only" in

  (* List of commands. *)
  let commands = [
    "attach-device",
      cmd2 no_return D.attach_device
	(arg_full_connection domain_of_string) input_file,
      s_"Attach device to domain.",
      [dom_help; s_"file",s_"XML file describing device"];
    "autostart",
      cmd2 no_return D.set_autostart
	(arg_full_connection domain_of_string) boolean_of_string,
      s_"Set whether a domain autostarts at boot.",
      [dom_help; "on|off",s_"new autostart status of domain"];
    "capabilities",
      cmd0 print_endline (with_readonly_connection C.get_capabilities),
      s_"Returns capabilities of hypervisor/driver.",
      [];
    "close",
      cmd0 no_return close_connection,
      s_"Close an existing hypervisor connection.",
      [];
    "connect",
      cmd12 no_return
	(fun name readonly ->
	   close_connection ();
	   match readonly with
	   | None | Some false -> conn := RW (C.connect ~name ())
	   | Some true -> conn := RO (C.connect_readonly ~name ())
	) string_of_string string_of_readonly,
      s_"Open a new hypervisor connection.",
      [s_"uri", s_"connection URI"; readonly_help];
    "create",
      cmd1 no_return
	(fun xml -> D.create_linux (get_full_connection ()) xml) input_file,
      s_"Create a domain from an XML file.",
      [s_"file",s_"domain XML file"];
    "define",
      cmd1 no_return
	(fun xml -> D.define_xml (get_full_connection ()) xml) input_file,
      s_"Define (but don't start) a domain from an XML file.",
      [s_"file",s_"domain XML file"];
    "detach-device",
      cmd2 no_return D.detach_device
	(arg_full_connection domain_of_string) input_file,
      s_"Detach device from domain.",
      [dom_help; s_"file",s_"XML file describing device"];
    "destroy",
      cmd1 no_return D.destroy (arg_full_connection domain_of_string),
      s_"Destroy a domain.",
      [dom_help];
    "domblkpeek",
      cmd4 print_string
	(fun dom path offset size ->
	   let buf = String.create size in
	   let max_peek = D.max_peek dom in
	   let rec loop i =
	     let remaining = size-i in
	     if remaining > 0 then (
	       let size = min remaining max_peek in
	       D.block_peek dom path
		 (Int64.add offset (Int64.of_int i)) size buf i;
	       loop (i+size)
	     )
	   in
	   loop 0;
	   buf)
	(arg_full_connection domain_of_string)
	string_of_string Int64.of_string int_of_string,
      s_"Peek into a block device of a domain.",
      [dom_help; s_"path",s_"Path to block device";
       s_"offset",s_"Offset in device"; s_"size",s_"Size in bytes to read"];
    "domblkstat",
      cmd2 print_block_stats D.block_stats
	(arg_readonly_connection domain_of_string) string_of_string,
      s_"Display the block device statistics for a domain.",
      [dom_help; s_"path",s_"Path to block device"];
    "domid",
      cmd1 print_int D.get_id (arg_readonly_connection domain_of_string),
      s_"Print the ID of a domain.",
      [dom_help];
    "domifstat",
      cmd2 print_interface_stats D.interface_stats
	(arg_readonly_connection domain_of_string) string_of_string,
      s_"Display the network interface statistics for a domain.",
      [dom_help; s_"path",s_"Path or name of network interface"];
    "dominfo",
      cmd1 print_domain_info D.get_info
	(arg_readonly_connection domain_of_string),
      s_"Print the domain info.",
      [dom_help];
    "dommaxmem",
      cmd1 print_int64 D.get_max_memory
	(arg_readonly_connection domain_of_string),
      s_"Print the max memory (in kilobytes) of a domain.",
      [dom_help];
    "dommaxvcpus",
      cmd1 print_int D.get_max_vcpus
	(arg_readonly_connection domain_of_string),
      s_"Print the max VCPUs of a domain.",
      [dom_help];
    "dommempeek",
      cmd3 print_string
	(fun dom offset size ->
	   let buf = String.create size in
	   let max_peek = D.max_peek dom in
	   let rec loop i =
	     let remaining = size-i in
	     if remaining > 0 then (
	       let size = min remaining max_peek in
	       D.memory_peek dom [D.Virtual]
		 (Int64.add offset (Int64.of_int i)) size buf i;
	       loop (i+size)
	     )
	   in
	   loop 0;
	   buf)
	(arg_full_connection domain_of_string)
	Int64.of_string int_of_string,
      s_"Peek into memory of a device.",
      [dom_help; s_"offset",s_"Offset in memory";
       s_"size",s_"Size in bytes to read"];
    "domname",
      cmd1 print_endline D.get_name
	(arg_readonly_connection domain_of_string),
      s_"Print the name of a domain.",
      [dom_help];
    "domostype",
      cmd1 print_endline D.get_os_type
	(arg_readonly_connection domain_of_string),
      s_"Print the OS type of a domain.",
      [dom_help];
    "domstate",
      cmd1 print_domain_state D.get_info
	(arg_readonly_connection domain_of_string),
      s_"Print the domain state.",
      [dom_help];
    "domuuid",
      cmd1 print_endline D.get_uuid_string
	(arg_readonly_connection domain_of_string),
      s_"Print the UUID of a domain.",
      [dom_help];
    "dump",
      cmd2 no_return D.core_dump
	(arg_full_connection domain_of_string) string_of_string,
      s_"Core dump a domain to a file for analysis.",
      [dom_help; s_"file",s_"Output filename"];
    "dumpxml",
      cmd1 print_endline D.get_xml_desc
	(arg_full_connection domain_of_string),
      s_"Print the XML description of a domain.",
      [dom_help];
    "freecell",
      cmd012 print_int64_array (
	fun start max ->
	  let conn = get_readonly_connection () in
	  match start, max with
	  | None, _ ->
	      [| C.node_get_free_memory conn |]
	  | Some start, None ->
	      C.node_get_cells_free_memory conn start 1
	  | Some start, Some max ->
	      C.node_get_cells_free_memory conn start max
          ) int_of_string int_of_string,
      s_"Display free memory for machine, NUMA cell or range of cells",
      [s_"start",s_"Start cell (optional)";
       s_"max",s_"Maximum cells to display (optional)"];
    "get-autostart",
      cmd1 print_bool D.get_autostart
	(arg_readonly_connection domain_of_string),
      s_"Print whether a domain autostarts at boot.",
      [dom_help];
    "hostname",
      cmd0 print_endline (with_readonly_connection C.get_hostname),
      s_"Print the hostname.",
      [];
    "list",
      cmd0 print_domain_list
	(fun () ->
	   let c = get_readonly_connection () in
	   D.get_domains_and_infos c [D.ListActive]),
      s_"List the running domains.",
      [];
    "list-all",
      cmd0 print_domain_list
	(fun () ->
	   let c = get_readonly_connection () in
	   D.get_domains_and_infos c [D.ListAll]),
      s_"List the running domains.",
      [];
    "list-defined",
      cmd0 print_domain_list
	(fun () ->
	   let c = get_readonly_connection () in
	   D.get_domains_and_infos c [D.ListInactive]),
      s_"List the defined but not running domains.",
      [];
    "quit",
      cmd0 no_return (fun () -> exit 0),
      s_"Quit the interactive terminal.",
      [];
    "maxvcpus",
      cmd0 print_int
	(fun () -> C.get_max_vcpus (get_readonly_connection ()) ()),
      s_"Print the max VCPUs available.",
      [];
    "net-autostart",
      cmd2 no_return N.set_autostart
	(arg_full_connection network_of_string) boolean_of_string,
      s_"Set whether a network autostarts at boot.",
      [net_help; "on|off", s_"new autostart status of network"];
    "net-bridgename",
      cmd1 print_endline N.get_bridge_name
	(arg_readonly_connection network_of_string),
      s_"Print the bridge name of a network.",
      [net_help];
    "net-create",
      cmd1 no_return
	(fun xml -> N.create_xml (get_full_connection ()) xml) input_file,
      s_"Create a network from an XML file.",
      [s_"file",s_"XML file describing network"];
    "net-define",
      cmd1 no_return
	(fun xml -> N.define_xml (get_full_connection ()) xml) input_file,
      s_"Define (but don't start) a network from an XML file.",
      [s_"file",s_"XML file describing network"];
    "net-destroy",
      cmd1 no_return N.destroy (arg_full_connection network_of_string),
      s_"Destroy a network.",
      [net_help];
    "net-dumpxml",
      cmd1 print_endline N.get_xml_desc
	(arg_full_connection network_of_string),
      s_"Print the XML description of a network.",
      [net_help];
    "net-get-autostart",
      cmd1 print_bool N.get_autostart
	(arg_full_connection network_of_string),
      s_"Print whether a network autostarts at boot.",
      [net_help];
    "net-list",
      cmd0 print_network_array
	(fun () ->
	   let c = get_readonly_connection () in
	   let n = C.num_of_networks c in
	   let nets = C.list_networks c n in
	   Array.map (N.lookup_by_name c) nets),
      s_"List the active networks.",
      [];
    "net-list-defined",
      cmd0 print_network_array
	(fun () ->
	   let c = get_readonly_connection () in
	   let n = C.num_of_defined_networks c in
	   let nets = C.list_defined_networks c n in
	   Array.map (N.lookup_by_name c) nets),
      s_"List the defined but inactive networks.",
      [];
    "net-name",
      cmd1 print_endline N.get_name
	(arg_readonly_connection network_of_string),
      s_"Print the name of a network.",
      [net_help];
    "net-start",
      cmd1 no_return N.create
	(arg_full_connection network_of_string),
      s_"Start a previously defined inactive network.",
      [net_help];
    "net-undefine",
      cmd1 no_return N.undefine
	(arg_full_connection network_of_string),
      s_"Undefine an inactive network.",
      [net_help];
    "net-uuid",
      cmd1 print_endline N.get_uuid_string
	(arg_readonly_connection network_of_string),
      s_"Print the UUID of a network.",
      [net_help];
    "nodeinfo",
      cmd0 print_node_info (with_readonly_connection C.get_node_info),
      s_"Print node information.",
      [];
    "reboot",
      cmd1 no_return D.reboot (arg_full_connection domain_of_string),
      s_"Reboot a domain.",
      [dom_help];
    "restore",
      cmd1 no_return (
	fun path -> D.restore (get_full_connection ()) path
        ) string_of_string,
      s_"Restore a domain from the named file.",
      [dom_help; s_"file",s_"Domain image file"];
    "resume",
      cmd1 no_return D.resume (arg_full_connection domain_of_string),
      s_"Resume a domain.",
      [dom_help];
    "save",
      cmd2 no_return D.save
	(arg_full_connection domain_of_string) string_of_string,
      s_"Save a domain to a file.",
      [dom_help; s_"file",s_"Domain image file"];
    "schedparams",
      cmd1 print_sched_param_array (
	fun dom ->
	  let n = snd (D.get_scheduler_type dom) in
	  D.get_scheduler_parameters dom n
        ) (arg_readonly_connection domain_of_string),
      s_"Get the current scheduler parameters for a domain.",
      [dom_help];
    "schedparamset",
      cmdN no_return (
	function
	| [] -> failwith (s_"expecting domain followed by field value pairs")
	| dom :: pairs ->
	    let conn = get_full_connection () in
	    let dom = domain_of_string conn dom in
	    let params = parse_sched_params pairs in
	    let params = Array.of_list params in
	    D.set_scheduler_parameters dom params
        ),
      s_"Set the scheduler parameters for a domain.",
      [dom_help];
    "schedtype",
      cmd1 print_endline
	(fun dom -> fst (D.get_scheduler_type dom))
	(arg_readonly_connection domain_of_string),
      s_"Get the scheduler type.",
      [dom_help];
    "setmem",
      cmd2 no_return D.set_memory
	(arg_full_connection domain_of_string) Int64.of_string,
      s_"Set the memory used by the domain (in kilobytes).",
      [dom_help; s_"mem",s_"memory to use (in KB)"];
    "setmaxmem",
      cmd2 no_return D.set_max_memory
	(arg_full_connection domain_of_string) Int64.of_string,
      s_"Set the maximum memory used by the domain (in kilobytes).",
      [dom_help; s_"mem",s_"maximum memory to use (in KB)"];
    "shutdown",
      cmd1 no_return D.shutdown
	(arg_full_connection domain_of_string),
      s_"Gracefully shutdown a domain.",
      [dom_help];
    "start",
      cmd1 no_return D.create
	(arg_full_connection domain_of_string),
      s_"Start a previously defined inactive domain.",
      [dom_help];
    "suspend",
      cmd1 no_return D.suspend
	(arg_full_connection domain_of_string),
      s_"Suspend a domain.",
      [dom_help];
    "type",
      cmd0 print_endline (with_readonly_connection C.get_type),
      s_"Print the driver name",
      [];
    "undefine",
      cmd1 no_return D.undefine
	(arg_full_connection domain_of_string),
      s_"Undefine an inactive domain.",
      [dom_help];
    "uri",
      cmd0 print_endline (with_readonly_connection C.get_uri),
      s_"Print the canonical URI.",
      [];
    "vcpuinfo",
      cmd1 print_vcpu_info (
	fun dom ->
	  let c = get_readonly_connection () in
	  let info = C.get_node_info c in
	  let dominfo = D.get_info dom in
	  let maxcpus = C.maxcpus_of_node_info info in
	  let maplen = C.cpumaplen maxcpus in
	  let maxinfo = dominfo.D.nr_virt_cpu in
	  let ncpus, vcpu_infos, cpumaps = D.get_vcpus dom maxinfo maplen in
	  ncpus, vcpu_infos, cpumaps, maplen, maxcpus
        ) (arg_readonly_connection domain_of_string),
      s_"Pin domain VCPU to a list of physical CPUs.",
      [dom_help];
    "vcpupin",
      cmd3 no_return D.pin_vcpu
	(arg_full_connection domain_of_string) int_of_string cpumap_of_string,
      s_"Pin domain VCPU to a list of physical CPUs.",
      [dom_help; s_"vcpu",s_"Virtual CPU number";
       s_"pcpus",s_"Comma-separated list of physical CPUs"];
    "vcpus",
      cmd2 no_return D.set_vcpus
	(arg_full_connection domain_of_string) int_of_string,
      s_"Set the number of virtual CPUs assigned to a domain.",
      [dom_help; s_"nrvcpus",s_"Number of virtual CPUs"];
    "version",
      cmd0 print_version (with_readonly_connection C.get_version),
      s_"Print the driver version",
      [];
  ] in

  (* Command help. *)
  let help = function
    | None ->				(* List of commands. *)
	String.concat "\n" (
	  List.map (
	    fun (cmd, _, description, _) ->
	      sprintf "%-16s %s" cmd description
	  ) commands
	) ^
	"\n\n" ^
	sprintf (f_"Use '%s help command' for help on a command.")
	  program_name

    | Some command ->			(* Full description of one command. *)
	try
	  let command, _, description, args =
	    List.find (fun (c, _, _, _) -> c = command) commands in

	  let arg_names = String.concat " " (List.map fst args) in
	  let args =
	    String.concat "" (
	      List.map (
		fun (name, help) -> sprintf "    %-12s %s\n" name help
	      ) args) in

	  sprintf "%s %s %s\n\n%s\n\n%s"
	    program_name command arg_names description args
	with
	  Not_found ->
	    sprintf (f_"help: %s: command not found\n") command;
  in

  let commands =
    ("help",
     cmd01 print_string help string_of_string,
     s_"Print list of commands or full description of one command.",
     [s_"cmd",s_"Show help for 'mlvirsh cmd' (optional)"];
    ) :: commands in

  (* Execute a command. *)
  let do_command command args =
    try
      let _, cmd, _, _ =
	List.find (fun (c, _, _, _) -> c = command) commands in
      cmd args
    with
      Not_found ->
	failwith (sprintf (f_"%s: command not found") command);
  in

  do_command

(* Interactive mode. *)
let rec interactive_mode () =
  let prompt =
    match !conn with
    | No_connection -> s_"mlvirsh(no connection)" ^ "$ "
    | RO _ -> s_"mlvirsh(ro)" ^ "$ "
    | RW _ -> s_"mlvirsh" ^ "# " in
  print_string prompt;
  let command = read_line () in
  (match str_nsplit command " " with
   | [] -> ()
   | command :: args ->
       do_command command args
  );
  Gc.full_major (); (* Free up all unreachable domain and network objects. *)
  interactive_mode ()

(* Connect to hypervisor.  Allow the connection to fail. *)
let () =
  conn :=
    try
      if readonly then RO (C.connect_readonly ?name ())
      else RW (C.connect ?name ())
    with
      Libvirt.Virterror err ->
	eprintf "%s: %s\n" program_name (Libvirt.Virterror.to_string err);
	No_connection

let () =
  try
    (* Execute the command on the command line, if there was one.
     * Otherwise go into interactive mode.
     *)
    (match extra_args with
     | command :: args ->
	 do_command command args
     | [] ->
	 try interactive_mode () with End_of_file -> ()
    );

    (* If we are connected to a hypervisor, close the connection. *)
    close_connection ();

    (* A good way to find heap bugs: *)
    Gc.compact ()
  with
  | Libvirt.Virterror err ->
      eprintf "%s: %s\n" program_name (Libvirt.Virterror.to_string err)
  | Failure msg ->
      eprintf "%s: %s\n" program_name msg
