ARG VERSION
FROM silex/emacs:$VERSION

ARG SHARED_UID

COPY scripts/docker-install.bash /tmp/
RUN /tmp/docker-install.bash

USER $SHARED_UID
WORKDIR /home/docker/src

CMD bash
