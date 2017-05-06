FROM ubuntu:16.04
RUN mkdir /topl-related
WORKDIR /topl-related
RUN apt-get -y update && apt-get -y install \
  autoconf \
  cppo \
  default-jdk \
  emacs24-nox \
  git \
  libcamomile-ocaml-dev \
  libzip-ocaml-dev \
  make \
  menhir \
  ocaml-findlib \
  ocaml-native-compilers \
  ocaml-nox \
  vim
RUN git clone https://github.com/rgrig/barista.git
RUN git clone https://github.com/rgrig/topl.git
WORKDIR /topl-related/barista
RUN bash configure
RUN make all
RUN make install
WORKDIR /topl-related/topl
RUN make
ENV PATH /topl-related/topl:$PATH
CMD /bin/bash
