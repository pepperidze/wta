FROM erlang:23-alpine

# system deps
RUN apk --no-cache add git python3 openssh-client make bash g++

# set working directory
RUN mkdir /data
WORKDIR /data

RUN ln -sf /usr/bin/python3 /usr/bin/python
