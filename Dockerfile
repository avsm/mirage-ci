FROM ocaml/opam:alpine
RUN cd /tmp && curl -OL https://get.docker.com/builds/Linux/x86_64/docker-17.05.0-ce.tgz && tar -zxvf docker-17.05.0-ce.tgz docker/docker && sudo mv docker/docker /usr/bin && rm -f docker-17.05.0-ce.tgz
RUN cd /home/opam/opam-repository && git pull origin master && opam update -uy
RUN opam depext -uivy -j 4 irmin-unix ezjsonm bos ptime fmt datakit-ci conf-libev cohttp.0.22.0 session.0.3.2
ADD . /home/opam/src
RUN sudo chown -R opam /home/opam/src
RUN opam pin add -n mirage-ci /home/opam/src
RUN opam install -vy -j 4 mirage-ci
ENV CONDUIT_TLS=native
ENV OCAMLRUNPARAM=b
USER root
CMD []
