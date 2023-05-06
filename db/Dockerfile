FROM debian

# Install required packages for Postgres
RUN apt-get update && \
    apt-get install -y postgresql postgresql-contrib

# Install required packages for Redis
#RUN apt-get update && \
#    apt-get install -y redis-server

# Expose the default Postgres and Redis ports
EXPOSE 5432
# EXPOSE 6379

# Start Postgres and Redis services
# CMD service postgresql start && service redis-server start
CMD service postgresql start
