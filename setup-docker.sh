#!/bin/sh

# Build the docker image
docker build -t react-trace .

# Run the docker container (mapping port 3000 for the frontend)
docker run -p 3000:3000 --rm -it react-trace
