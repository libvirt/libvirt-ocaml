# THIS FILE WAS AUTO-GENERATED
#
#  $ lcitool manifest ci/manifest.yml
#
# https://gitlab.com/libvirt/libvirt-ci

function install_buildenv() {
    dnf --quiet distro-sync -y
    dnf --quiet install 'dnf-command(config-manager)' -y
    dnf --quiet config-manager --set-enabled -y crb
    dnf --quiet install -y epel-release
    dnf --quiet install -y epel-next-release
    dnf --quiet install -y \
                autoconf \
                automake \
                ca-certificates \
                ccache \
                diffutils \
                gawk \
                gcc \
                gettext-devel \
                git \
                glibc-devel \
                glibc-langpack-en \
                gzip \
                libtool \
                libvirt-devel \
                make \
                ocaml \
                ocaml-findlib \
                perl-base \
                pkgconfig \
                tar
    rpm -qa | sort > /packages.txt
    mkdir -p /usr/libexec/ccache-wrappers
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/cc
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/gcc
}

export CCACHE_WRAPPERSDIR="/usr/libexec/ccache-wrappers"
export LANG="en_US.UTF-8"
export MAKE="/usr/bin/make"
