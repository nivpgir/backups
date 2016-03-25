#!/bin/bash

#first download all necessary tools for nfs:
sudo apt-get install nfs-kernel-server nfs-common portmap nmap

#next create the shared dir if necessary
if [[ $1 = ""]]
then
	echo "must give a path to the shared directory"
	exit 1
fi

#if diretory does not exist then create it
if [[ ! -d $1 ]]
then
	mkdir -p $1;
fi

#then add to /etc/exports the shared directory

echo -e  "\n $1 10.0.0.0/24(rw,async,no_subtree_check)\n" >> /etc/exports

#finally, restart the nfs service:

sudo service nfs-kernel-service restart


echo "now ssh to the osmc (username:osmc password:osmc) and add to /etc/fstab the following line:"
echo "<raspi_ip>:$1	/mnt/kodi	nfs		_netdev,defaults,user,auto,noatime,intr,x-systemd.automount 0 0"


echo "\n finding you the ip of the pi:"
nmap -sn 10.0.0.0/24
