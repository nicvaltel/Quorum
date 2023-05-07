FROM postgres:14.7


ADD scripts/init.sql /docker-entrypoint-initdb.d

# Expose the default Postgres port
EXPOSE 5432

RUN chmod a+r /docker-entrypoint-initdb.d/*
