# THIS FILE WAS AUTO-GENERATED
#
#  $ lcitool manifest ci/manifest.yml
#
# https://gitlab.com/libvirt/libvirt-ci

function install_buildenv() {
    zypper update -y
    zypper install -y \
           autoconf \
           automake \
           awk \
           ca-certificates \
           ccache \
           diffutils \
           gcc \
           gettext-devel \
           git \
           glibc-devel \
           glibc-locale \
           gzip \
           libtool \
           libvirt-devel \
           make \
           ocaml \
           ocaml-findlib \
           perl-base \
           pkgconfig
    rpm -qa | sort > /packages.txt
    mkdir -p /usr/libexec/ccache-wrappers
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/cc
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/gcc
}

export CCACHE_WRAPPERSDIR="/usr/libexec/ccache-wrappers"
export LANG="en_US.UTF-8"
export MAKE="/usr/bin/make"
