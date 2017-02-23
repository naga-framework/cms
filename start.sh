#!/bin/bash

COOKIE=cookie_cms
NODE_NAME=cms@127.0.0.1
CONFIG=sys.config
export ERL_EPMD_ADDRESS="127.0.0.1"
./mad deps comp repl -name $NODE_NAME


