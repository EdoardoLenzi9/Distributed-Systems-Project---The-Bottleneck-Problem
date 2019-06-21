# Docker

1. Create a `Dockerfile` from `erlang:latest` image 
    * Or alternatively create your own image in order to 
        get the latest erlang distribution (use kerl) and 
        snapshot the container
2. Build the image using
`sudo docker build -t <image_name>:<version> .`
3. Launch a new container from the image using
`sudo docker run --entrypoint "erl" <image_name>:<version> -sname <process_name> -run helloworld start`
> In this way you are going to override the default entrypoint of the image and you are able to launch any erlang process inside the container. 


## Commands

* build and run
    * **build** `sudo docker build -t name:tag .`     
    * **image list** `sudo docker image ls`                
    * **container list** `sudo docker container ls`           
    * **runs image** `sudo docker run name:tag`           
    * **launch escript** `sudo docker run -it --rm --name erlang-inst1 -v "$PWD":/usr/src/myapp -w /usr/src/myapp erlang escript helloworld.erl`
    * **launch script** `sudo docker run --entrypoint "erl" myapp:v1 -sname ciao -run helloworld start`

* Kill a container
```{sh}
sudo docker ps                      # container list
sudo docker stop <container_id>     # stop running container
sudo docker rm <container_id>       # delete container
```

* Delete image
```{sh}
    sudo docker rmi <image_id>
```
