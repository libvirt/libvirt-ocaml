(* OCaml bindings for libvirt.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   https://libvirt.org/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version,
   with the OCaml linking exception described in ../COPYING.LIB.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*)

(** Information on OCaml-libvirt itself. *)

val package : string
(** The name of the OCaml libvirt bindings. *)
val version : string
(** The version of the OCaml libvirt bindings.

    (To get the version of libvirt C library itself
     use {!Libvirt.get_version}). *)
