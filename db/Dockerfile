FROM postgres:14.7

ADD scripts/init.sql /docker-entrypoint-initdb.d

RUN chmod a+r /docker-entrypoint-initdb.d/*

EXPOSE 6666
