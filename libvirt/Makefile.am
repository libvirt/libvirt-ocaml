# ocaml-libvirt
# Copyright (C) 2007 Red Hat Inc., Richard W.M. Jones
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

include ../Make.rules

EXTRA_DIST = \
	.depend \
	generator.pl \
	libvirt.ml \
	libvirt.mli \
	libvirt_version.mli

OCAMLCPACKAGES	:= -package unix
OCAMLCFLAGS	:= -g -warn-error +C+D+E+F+L+M+P+S+U+V+Y+Z+X-3 -safe-string
OCAMLCLIBS	:= -linkpkg

OCAMLOPTFLAGS	:= $(OCAMLCFLAGS)
OCAMLOPTPACKAGES := $(OCAMLCPACKAGES)
OCAMLOPTLIBS	:= $(OCAMLCLIBS)

# Build the C part into a library so that automake handles the C
# compilation step.  We don't use this library, only the object files.
noinst_LIBRARIES = libvirtocaml.a
libvirtocaml_a_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I$(OCAMLLIB) \
	$(LIBVIRT_CFLAGS)
libvirtocaml_a_CFLAGS = -Wall -fPIC
libvirtocaml_a_SOURCES = \
	libvirt_c.h \
	libvirt_generated.c \
	libvirt_c_common.c \
	libvirt_c_oneoffs.c
libvirtocaml_a_DEPENDENCIES = libvirt_c.h generator.pl

# Automatically generate the C code from a Perl script 'generator.pl'.
BUILT_SOURCES = libvirt_generated.c
CLEANFILES += libvirt_generated.c
libvirt_generated.c: generator.pl
	$(PERL) -w $<

# Build the real OCaml bindings using ocamlmklib.
noinst_DATA = mllibvirt.cma

COBJS := libvirt.cmo libvirt_version.cmo
OPTOBJS := libvirt.cmx libvirt_version.cmx

mllibvirt.cma: $(COBJS)
	$(OCAMLMKLIB) -o mllibvirt \
	    -ldopt '$(LDFLAGS)' \
	    $(libvirtocaml_a_OBJECTS) $(COBJS) \
	    $(LIBVIRT_LIBS)

if HAVE_OCAMLOPT

noinst_DATA += mllibvirt.cmxa

mllibvirt.cmxa: $(OPTOBJS)
	$(OCAMLMKLIB) -o mllibvirt \
	    -ldopt '$(LDFLAGS)' \
	    $(libvirtocaml_a_OBJECTS) $(OPTOBJS) \
	    $(LIBVIRT_LIBS)

endif

# Run ocamlfind to perform the install.
data_hook_files = META *.so *.a *.cma *.cmi $(srcdir)/*.mli
if HAVE_OCAMLOPT
data_hook_files += *.cmx *.cmxa
endif

install-data-hook:
	mkdir -p $(DESTDIR)$(OCAMLLIB)
	mkdir -p $(DESTDIR)$(OCAMLLIB)/stublibs
	rm -rf $(DESTDIR)$(OCAMLLIB)/libvirt
	rm -rf $(DESTDIR)$(OCAMLLIB)/stublibs/dllmllibvirt.so*
	$(OCAMLFIND) install \
	  -ldconf ignore -destdir $(DESTDIR)$(OCAMLLIB) \
	  libvirt \
	  $(data_hook_files)
	rm -f $(DESTDIR)$(OCAMLLIB)/libvirt/bindtests.*
	rm $(DESTDIR)$(OCAMLLIB)/libvirt/libvirtocaml.a

# Status of automatically generated bindings.
maintainer-status: libvirt_generated.c
	@echo -n "Functions which have manual bindings:    "
	@grep ^ocaml_libvirt_ libvirt_c_oneoffs.c  | wc -l
	@echo -n "Functions which have automatic bindings: "
	@grep ^ocaml_libvirt_ libvirt_generated.c  | wc -l
	@echo -n "LOC in manual bindings:    "
	@wc -l < libvirt_c_oneoffs.c
	@echo -n "LOC in automatic bindings: "
	@wc -l < libvirt_generated.c

# We have to use this because simultaneous calls to ocamlmklib above
# will both create libmllibvirt.a and if run simultaneously will stomp
# on each other.  This is an easy fix.  There is a more complicated
# fix used in libguestfs involving stamp files, but that project has a
# lot more source files.
.NOTPARALLEL:
