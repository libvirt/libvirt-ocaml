# ocaml-libvirt
# Copyright (C) 2007-2015 Red Hat Inc., Richard W.M. Jones
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

dnl Process this file with autoconf to produce a configure script.

AC_INIT(ocaml-libvirt,0.6.1.5)
AC_CONFIG_MACRO_DIRS([m4])

dnl Check for basic C environment.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_CPP

AC_C_PROTOTYPES
test "x$U" != "x" && AC_MSG_ERROR(Compiler not ANSI compliant)

AC_PROG_CC_C_O

dnl Select some C flags based on the host type.
AC_CANONICAL_HOST

DEBUG="-g"
WARNINGS="-Wall -Werror"
CFLAGS_FPIC="-fPIC"
WIN32=no
case "$host" in
  *-*-mingw*)
    WARNINGS="$WARNINGS -Wno-unused"
    CFLAGS_FPIC=""
    WIN32=yes
esac
AC_SUBST(DEBUG)
AC_SUBST(WARNINGS)
AC_SUBST(CFLAGS_FPIC)
AC_SUBST(WIN32)

dnl Check for perl (required).
AC_CHECK_PROG([PERL],[perl],[perl],[no])
if test "x$PERL" = "xno"; then
    AC_MSG_ERROR([Perl must be installed to build])
fi

dnl Check for libvirt development environment.
PKG_PROG_PKG_CONFIG
PKG_CHECK_MODULES([LIBVIRT], [libvirt >= 1.2.8])

dnl Check for basic OCaml environment & findlib.
AC_PROG_OCAML
AC_PROG_FINDLIB

if test "x$OCAMLFIND" = "x"; then
    AC_MSG_ERROR([OCaml findlib is required])
fi

dnl Check for required OCaml packages.
OCAML_PKG_unix=no
AC_CHECK_OCAML_PKG(unix)
if test "x$OCAML_PKG_unix" = "xno"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'unix'])
fi

dnl Summary.
echo "------------------------------------------------------------"
echo "Thanks for downloading" $PACKAGE_STRING
echo "------------------------------------------------------------"

dnl Produce output files.
AC_CONFIG_FILES([META
	libvirt/libvirt_version.ml
	Makefile
	Make.rules
	libvirt/Makefile
	examples/Makefile
	])
AC_OUTPUT
