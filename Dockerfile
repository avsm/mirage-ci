FROM ocaml/opam:alpine_ocaml-4.04.0
RUN sudo apk add docker
RUN opam remote add dev git://github.com/mirage/mirage-dev
RUN opam pin add -n datakit git://github.com/docker/datakit
RUN opam pin add -n datakit-github git://github.com/docker/datakit
RUN opam pin add -n datakit-client git://github.com/docker/datakit
RUN opam pin add -n datakit-server git://github.com/docker/datakit
RUN opam pin add -n datakit-ci git://github.com/docker/datakit
RUN opam depext -uivy -j 4 datakit-ci
ADD . /home/opam/src
RUN sudo chown -R opam /home/opam/src
RUN opam pin add -n mirage-ci /home/opam/src
RUN opam install -vy -j 4 mirage-ci
USER root
ENTRYPOINT ["/home/opam/.opam/4.04.0/bin/mirageCI"]
CMD []
