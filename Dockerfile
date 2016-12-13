FROM ocaml/opam:alpine_ocaml-4.03.0
RUN cd /home/opam/opam-repository && git pull origin master && opam update -uy
RUN cd /tmp && curl -OL https://test.docker.com/builds/Linux/x86_64/docker-1.13.0-rc3.tgz && tar -zxvf docker-1.13.0-rc3.tgz docker/docker && sudo mv docker/docker /usr/bin && rm -f docker-1.13.0-rc2.tgz
RUN opam pin add -n session git://github.com/talex5/ocaml-session#redis
RUN opam pin add -n datakit-ci git://github.com/docker/datakit
RUN opam depext -uivy -j 4 datakit-ci conf-libev
ADD . /home/opam/src
RUN sudo chown -R opam /home/opam/src
RUN opam pin add -n mirage-ci /home/opam/src
RUN opam install -vy -j 4 mirage-ci
ENV CONDUIT_TLS=native
ENV OCAMLRUNPARAM=b
USER root
CMD []
