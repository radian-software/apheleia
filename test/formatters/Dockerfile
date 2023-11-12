# Ubuntu 22.04 LTS supported until April 2027
FROM ubuntu:22.04 AS common

WORKDIR /build
COPY install-common.bash /build/
RUN ./install-common.bash

# Add an intermediate tag so that it is possible to execute debugging
# code before formatter installation. This is necessary because with
# the newer docker build based on buildkit, you can't access
# intermediate image layers by default anymore.
FROM common
ARG FORMATTERS
COPY install-formatters.bash /build/
COPY installers /build/installers/
RUN ./install-formatters.bash

WORKDIR /src
