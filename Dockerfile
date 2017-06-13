FROM ocaml/opam:alpine
RUN cd /home/opam/opam-repository && git pull origin master && opam update -uy
RUN cd /tmp && curl -OL https://test.docker.com/builds/Linux/x86_64/docker-17.04.0-ce-rc1.tgz && tar -zxvf docker-17.04.0-ce-rc1.tgz docker/docker && sudo mv docker/docker /usr/bin && rm -f docker-17.04.0-ce-rc1.tgz
RUN opam pin add cmdliner 0.9.8
RUN opam pin add -n datakit-ci.dev --dev
RUN opam depext -uivy -j 4 datakit-ci conf-libev
RUN opam depext -uivy -j 4 toml irmin-unix ezjsonm bos ptime fmt
RUN opam pin add -n dockerfile.3.0.0 https://github.com/avsm/ocaml-dockerfile.git
RUN opam pin add -n dockerfile-opam.3.0.0 https://github.com/avsm/ocaml-dockerfile.git
ADD . /home/opam/src
RUN sudo chown -R opam /home/opam/src
RUN opam pin add -n mirage-ci /home/opam/src
RUN opam install -vy -j 4 mirage-ci
ENV CONDUIT_TLS=native
ENV OCAMLRUNPARAM=b
USER root
CMD []
