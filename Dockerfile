FROM scratch

ARG REL_NAME
ARG REL_VSN
ARG ERTS_VSN

ENV BINDIR /erts-${ERTS_VSN}/bin
ENV BOOT /releases/${REL_VSN}/${REL_NAME}
ENV CONFIG /releases/${REL_VSN}/sys.config

# Runtime config from outside
ENV ERLANG_NODE ""
ENV ERLANG_COOKIE ""
ENV WEBSERVER_PORT ""
ENV DATABASE_HOST ""
ENV DATABASE_USER ""
ENV DATABASE_PASSWORD ""
ENV DATABASE_NAME ""

ENTRYPOINT exec ${BINDIR}/erlexec \
           -name "${ERLANG_NODE}" \
           -setcookie "${ERLANG_COOKIE}" \
           -kernel inetrc '"/inet.config"' \
           -boot ${BOOT} \
           -busbudcc webserver_port ${WEBSERVER_PORT} \
           -busbudcc database_host "<<\"${DATABASE_HOST}\">>" \
           -busbudcc database_user "<<\"${DATABASE_USER}\">>" \
           -busbudcc database_password "<<\"${DATABASE_PASSWORD}\">>" \
           -busbudcc database_name "<<\"${DATABASE_NAME}\">>" \
           -inet "{lookup, [file, dns]}" \
           -noinput \
           -config ${CONFIG}

ADD rel/${REL_NAME}/ /

COPY inet.config /inet.config
