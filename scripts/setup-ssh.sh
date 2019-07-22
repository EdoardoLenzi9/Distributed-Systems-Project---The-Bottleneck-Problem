#!/bin/bash

sudo apt-get install openssh-server
sudo apt-get install sshpass
sudo systemctl enable ssh
sudo systemctl start ssh

sudo passwd
sudo sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
sudo service ssh restart