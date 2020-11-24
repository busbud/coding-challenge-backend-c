FROM postgres:10.4

COPY *.sql /docker-entrypoint-initdb.d/

RUN chmod -R 777 /docker-entrypoint-initdb.d

EXPOSE 5432