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
	domain_events.ml \
	get_all_domain_stats.ml \
	get_cpu_stats.ml \
	list_domains.ml \
	list_secrets.ml \
	node_info.ml

OCAMLCPACKAGES	:= -package unix -I ../libvirt
OCAMLCFLAGS	:= -g -warn-error +C+D+E+F+L+M+P+S+U+V+Y+Z+X-3
OCAMLCLIBS	:= -linkpkg
OCAMLOPTPACKAGES := $(OCAMLCPACKAGES)
OCAMLOPTFLAGS	:= -g -warn-error +C+D+E+F+L+M+P+S+U+V+Y+Z+X-3
OCAMLOPTLIBS	:= $(OCAMLCLIBS)

BYTE_TARGETS	:= list_domains list_secrets node_info get_cpu_stats \
		   get_all_domain_stats domain_events
OPT_TARGETS	:= $(BYTE_TARGETS:%=%.opt)

CLEANFILES	+= $(BYTE_TARGETS)

noinst_SCRIPTS = $(BYTE_TARGETS)

list_domains: list_domains.cmo
	$(OCAMLFIND) ocamlc \
	  $(OCAMLCPACKAGES) $(OCAMLCFLAGS) $(OCAMLCLIBS) \
	  ../libvirt/mllibvirt.cma -o $@ $<

list_secrets: list_secrets.cmo
	$(OCAMLFIND) ocamlc \
	  $(OCAMLCPACKAGES) $(OCAMLCFLAGS) $(OCAMLCLIBS) \
	  ../libvirt/mllibvirt.cma -o $@ $<

node_info: node_info.cmo
	$(OCAMLFIND) ocamlc \
	  $(OCAMLCPACKAGES) $(OCAMLCFLAGS) $(OCAMLCLIBS) \
	  ../libvirt/mllibvirt.cma -o $@ $<

get_cpu_stats: get_cpu_stats.cmo
	$(OCAMLFIND) ocamlc \
	  $(OCAMLCPACKAGES) $(OCAMLCFLAGS) $(OCAMLCLIBS) \
	  ../libvirt/mllibvirt.cma -o $@ $<

get_all_domain_stats: get_all_domain_stats.cmo
	$(OCAMLFIND) ocamlc \
	  $(OCAMLCPACKAGES) $(OCAMLCFLAGS) $(OCAMLCLIBS) \
	  ../libvirt/mllibvirt.cma -o $@ $<

domain_events: domain_events.cmo
	$(OCAMLFIND) ocamlc \
	  $(OCAMLCPACKAGES) $(OCAMLCFLAGS) $(OCAMLCLIBS) \
	  ../libvirt/mllibvirt.cma -o $@ $<

if HAVE_OCAMLOPT

noinst_SCRIPTS += $(OPT_TARGETS)

list_domains.opt: list_domains.cmx
	$(OCAMLFIND) ocamlopt \
	  $(OCAMLOPTPACKAGES) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) \
	  ../libvirt/mllibvirt.cmxa -o $@ $<

list_secrets.opt: list_secrets.cmx
	$(OCAMLFIND) ocamlopt \
	  $(OCAMLOPTPACKAGES) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) \
	  ../libvirt/mllibvirt.cmxa -o $@ $<

node_info.opt: node_info.cmx
	$(OCAMLFIND) ocamlopt \
	  $(OCAMLOPTPACKAGES) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) \
	  ../libvirt/mllibvirt.cmxa -o $@ $<

get_cpu_stats.opt: get_cpu_stats.cmx
	$(OCAMLFIND) ocamlopt \
	  $(OCAMLOPTPACKAGES) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) \
	  ../libvirt/mllibvirt.cmxa -o $@ $<

get_all_domain_stats.opt: get_all_domain_stats.cmx
	$(OCAMLFIND) ocamlopt \
	  $(OCAMLOPTPACKAGES) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) \
	  ../libvirt/mllibvirt.cmxa -o $@ $<

domain_events.opt: domain_events.cmx
	$(OCAMLFIND) ocamlopt \
	  $(OCAMLOPTPACKAGES) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) \
	  ../libvirt/mllibvirt.cmxa -o $@ $<

endif
