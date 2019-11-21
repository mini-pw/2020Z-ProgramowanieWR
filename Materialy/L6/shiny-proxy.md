## Instalacja

Wymagana java i docker.

Instalacja shinyproxy: https://www.shinyproxy.io/downloads/

## Praca z shinyproxy 

`sudo docker pull openanalytics/shinyproxy-demo`

`sudo docker network create sp-net`

`sudo docker build . -t shinyproxy-example`

`sudo docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-net -p 8080:8080 shinyproxy-example`
