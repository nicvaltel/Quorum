import os
import subprocess


os.system('sudo service postgresql stop')
os.system('sudo service redis-server stop')
# os.system('sudo docker-compose build')
# os.system('sudo docker-compose up')

processes = []

p_db = subprocess.Popen(['sudo', 'docker-compose', 'up'], cwd='./db')
processes.append(p_db)
p_redis = subprocess.Popen(['sudo', 'docker-compose', 'up'], cwd='./redis')
processes.append(p_redis)

for p in processes:
    p.wait()
