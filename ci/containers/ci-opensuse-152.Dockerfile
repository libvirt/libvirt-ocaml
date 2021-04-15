# THIS FILE WAS AUTO-GENERATED
#
#  $ lcitool dockerfile opensuse-152 libvirt+dist,libvirt-ocaml
#
# https://gitlab.com/libvirt/libvirt-ci/-/commit/6552fd8885423cfc383a58255eca542937f7d4ea

FROM registry.opensuse.org/opensuse/leap:15.2

RUN zypper update -y && \
    zypper install -y \
           autoconf \
           automake \
           ca-certificates \
           ccache \
           diffutils \
           gcc \
           gettext-devel \
           git \
           glibc-locale \
           libtool \
           libvirt-devel \
           make \
           ocaml \
           ocaml-findlib \
           perl \
           pkgconfig && \
    zypper clean --all && \
    rpm -qa | sort > /packages.txt && \
    mkdir -p /usr/libexec/ccache-wrappers && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/cc && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/gcc

ENV LANG "en_US.UTF-8"
ENV MAKE "/usr/bin/make"
ENV CCACHE_WRAPPERSDIR "/usr/libexec/ccache-wrappers"
