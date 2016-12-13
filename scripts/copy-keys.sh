#!/bin/sh -e

. ./.env
sudo cp /etc/letsencrypt/live/${CI_DOMAIN_NAME}/privkey.pem data/ci/secrets/server.key 
sudo cp /etc/letsencrypt/live/${CI_DOMAIN_NAME}/fullchain.pem data/ci/secrets/server.crt
