FROM ubuntu:15.10
RUN mkdir /topl-related
WORKDIR /topl-related
RUN apt-get -y update && apt-get -y install \
  autoconf \
  camlzip \
  cppo \
  git \
  make \
  menhir \
  ocaml-findlib \
  ocaml-native-compilers \
  ocaml-nox \
  openjdk-7-jre
RUN git clone https://github.com/rgrig/camomile.git
RUN git clone https://github.com/rgrig/barista.git
RUN git clone https://github.com/rgrig/topl.git
WORKDIR /topl-related/camomile/Camomile
RUN autoconf
RUN ./configure
RUN make
RUN make install
WORKDIR /topl-related/barista
RUN bash configure
RUN make all
RUN make install
WORKDIR /topl-related/topl
RUN make
ENV PATH /topl-related/topl:$PATH
CMD ./toplc -help
