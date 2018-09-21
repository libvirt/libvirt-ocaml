(* Example of using Domain.get_all_domain_stats (virConnectGetAllDomainStats).
 * Usage: get_all_domain_stats
 * https://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain

let print_stats conn stats =
  try
    Array.iter (
      fun { D.dom_uuid = uuid; D.params = params } ->
        let dom = D.lookup_by_uuid conn uuid in
        printf "domain %s:\n" (D.get_name dom);
        Array.iteri (
          fun i (field, value) ->
            printf "\t%-20s = " field;
            (match value with
             | D.TypedFieldInt32 i -> printf "%ld" i
             | D.TypedFieldUInt32 i -> printf "%ld" i
             | D.TypedFieldInt64 i -> printf "%Ld" i
             | D.TypedFieldUInt64 i -> printf "%Ld" i
             | D.TypedFieldFloat f -> printf "%g" f
             | D.TypedFieldBool b -> printf "%b" b
             | D.TypedFieldString s -> printf "%S" s);
            printf "\n";
        ) params;
        printf "\n"
    ) stats
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  if Array.length Sys.argv <> 1 then (
    eprintf "error: get_all_domain_stats\n";
    exit 1
  );

  let conn = C.connect_auth_readonly (C.get_auth_default ()) in

  let what = [
    D.StatsState;
    D.StatsCpuTotal;
    D.StatsBalloon;
    D.StatsVcpu;
    D.StatsInterface;
    D.StatsBlock;
  ] in
  let who = [] in (* empty list means returns all domains *)

  let quit = ref false in

  while not !quit do
    let stats = D.get_all_domain_stats conn what who in

    if stats <> [||] then print_stats conn stats
    else (
      printf "no guests found\n";
      quit := true
    );
    flush stdout;

    (* Run the garbage collector which is a good way to check for
     * memory corruption errors and reference counting issues in
     * libvirt.  You shouldn't do this in ordinary programs.
     *)
    Gc.compact ();

    if not !quit then Unix.sleep 3
  done
