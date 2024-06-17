# THIS FILE WAS AUTO-GENERATED
#
#  $ lcitool manifest ci/manifest.yml
#
# https://gitlab.com/libvirt/libvirt-ci

function install_buildenv() {
    apk update
    apk upgrade
    apk add \
        autoconf \
        automake \
        busybox \
        ca-certificates \
        ccache \
        diffutils \
        gcc \
        gettext \
        git \
        gzip \
        libtool \
        libvirt-dev \
        make \
        musl-dev \
        ocaml \
        ocaml-findlib-dev \
        perl \
        pkgconf
    apk list --installed | sort > /packages.txt
    mkdir -p /usr/libexec/ccache-wrappers
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/cc
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/gcc
}

export CCACHE_WRAPPERSDIR="/usr/libexec/ccache-wrappers"
export LANG="en_US.UTF-8"
export MAKE="/usr/bin/make"
