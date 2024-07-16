import os
import subprocess


# os.system('sudo service postgresql stop')
# os.system('sudo service redis-server stop')
# os.system('sudo  service rabbitmq-server stop')

processes = []

processes.append(subprocess.Popen(['docker-compose', 'up'], cwd='./db'))
processes.append(subprocess.Popen(['docker-compose', 'up'], cwd='./redis'))
processes.append(subprocess.Popen(['docker-compose', 'up'], cwd='./rabbitmq'))

for p in processes:
    p.wait()
