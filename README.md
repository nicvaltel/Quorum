# Quorum

1. (Optional) If postgresql is already running on local machine, stop it:
$ sudo service postgresql stop

2.
$ sudo docker-compose up



-------------------
If there are problems with libpq.so.5

1. install python3-psycopg2, libpq-dev, libghc-postgresql-libpq-dev, libpq5

$ sudo find / -name libpq.so.5
(e.g. result will be usr/lib/x86_64-linux-gnu/libpq.so.5 )
$ ln -s usr/lib/x86_64-linux-gnu/libpq.so.5 /usr/lib/libpq.so.5
$ ln -s usr/lib/x86_64-linux-gnu/libpq.so.5 /usr/lib64/libpq.so.5
