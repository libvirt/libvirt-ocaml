(** OCaml bindings for libvirt.
    (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
    http://libvirt.org/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   Please see the file ../COPYING.LIB.
*)

val package : string
val version : string
(** The name and version of the OCaml libvirt bindings.

    (To get the version of libvirt C library itself
     use {!Libvirt.get_version}). *)
