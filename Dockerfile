FROM erlang:latest
RUN apt-get update
RUN apt-get install -y openssh-server
RUN apt-get install -y sshpass