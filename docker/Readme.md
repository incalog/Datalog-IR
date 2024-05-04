# Update Container Registry 

Use the `Dockerfile` in this folder  to build a linux docker image with all dependencies for inca-scala and all Datalog backends installed.
This image can be uploaded to gitlabs Container Registry, which is then used by the CI pipeline.  
To update / upload an imgae in the Container Registry perform the following steps:

1. Login using: `docker login registry.gitlab.rlp.net` and enter your gitlab username and password (or authentication token). 
   
2. Build the new docker image: `docker build --platform linux/amd64 -t registry.gitlab.rlp.net/plmz/inca-scala .`. Make sure to build the image for the correct platform if you are not using an amd64 computer (e.g an MX mac)

3. Upload the new image to the registry: `docker push registry.gitlab.rlp.net/plmz/inca-scala`