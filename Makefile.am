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

include Make.rules

EXTRA_DIST = \
	CONTRIBUTING.rst \
	.depend \
	.github/workflows/lockdown.yml \
	.gitignore \
	TODO.libvirt

OCAMLDOCFLAGS	= -html -sort -colorize-code

SUBDIRS		= libvirt examples

# Developer documentation (in html/ subdirectory).

if HAVE_OCAMLDOC
doc:
	rm -rf html
	mkdir html
	-cd libvirt; \
	$(OCAMLDOC) $(OCAMLDOCFLAGS) -d ../html \
	  libvirt.{ml,mli} libvirt_version.{ml,mli}
endif

# Do a release

release: configure
	$(MAKE) release_stage_2

release_stage_2: clean all doc dist

# Upload to main website.

upload:
	scp $(PACKAGE)-$(VERSION).tar.gz download.libvirt.org:/data/ftp/libvirt/ocaml/

#----------------------------------------------------------------------
# Maintainers only!

# Check no files are missing from EXTRA_DIST.
# (You must have done 'make dist' first)
maintainer-check-extra-dist:
	@zcat $(PACKAGE_NAME)-$(VERSION).tar.gz | tar tf - | sort | \
	  sed 's,^$(PACKAGE_NAME)-$(VERSION)/,,' > tarfiles
	@git ls-files | \
	  grep -v \
	    -e '^\.cirrus.yml' \
	    -e '^\.gitlab-ci.yml' \
	    -e '^web/' \
	    -e '^ci/' | \
	    sort > gitfiles
	@comm -13 tarfiles gitfiles > comm-out
	@echo Checking for differences between EXTRA_DIST and git ...
	@cat comm-out
	@[ ! -s comm-out ]
	@rm tarfiles gitfiles comm-out
	@echo PASS: EXTRA_DIST tests

# Commit everything in the current directory and set the commit
# message to the current version number.
maintainer-commit:
	git commit -a -m "Version $(VERSION)."

# Tag HEAD with the current version.
maintainer-tag:
	git tag -a v$(VERSION) -m "Version $(VERSION)." -f
