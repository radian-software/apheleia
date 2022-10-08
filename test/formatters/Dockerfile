# Ubuntu 20.04 LTS supported until April 2025
FROM ubuntu:20.04

WORKDIR /build
COPY install-common.bash /build/
RUN ./install-common.bash

ARG FORMATTERS
COPY install-formatters.bash /build/
COPY installers /build/installers/
RUN ./install-formatters.bash

WORKDIR /src
