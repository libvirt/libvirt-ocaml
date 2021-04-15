# THIS FILE WAS AUTO-GENERATED
#
#  $ lcitool dockerfile centos-stream libvirt+dist,libvirt-ocaml
#
# https://gitlab.com/libvirt/libvirt-ci/-/commit/6552fd8885423cfc383a58255eca542937f7d4ea

FROM docker.io/library/centos:8

RUN dnf install -y centos-release-stream && \
    dnf install -y centos-stream-release && \
    dnf update -y && \
    dnf install 'dnf-command(config-manager)' -y && \
    dnf config-manager --set-enabled -y powertools && \
    dnf install -y centos-release-advanced-virtualization && \
    dnf install -y epel-release && \
    dnf install -y \
        autoconf \
        automake \
        ca-certificates \
        ccache \
        diffutils \
        gcc \
        gettext-devel \
        git \
        glibc-langpack-en \
        libtool \
        libvirt-devel \
        make \
        ocaml \
        ocaml-findlib \
        perl \
        pkgconfig && \
    dnf autoremove -y && \
    dnf clean all -y && \
    rpm -qa | sort > /packages.txt && \
    mkdir -p /usr/libexec/ccache-wrappers && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/cc && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/gcc

ENV LANG "en_US.UTF-8"
ENV MAKE "/usr/bin/make"
ENV CCACHE_WRAPPERSDIR "/usr/libexec/ccache-wrappers"
