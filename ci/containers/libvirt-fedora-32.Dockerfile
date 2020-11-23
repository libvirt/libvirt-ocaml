FROM registry.fedoraproject.org/fedora:32

RUN dnf update -y && \
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
    mkdir -p /usr/libexec/ccache-wrappers && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/cc && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/$(basename /usr/bin/gcc)

ENV LANG "en_US.UTF-8"
ENV MAKE "/usr/bin/make"
ENV CCACHE_WRAPPERSDIR "/usr/libexec/ccache-wrappers"
