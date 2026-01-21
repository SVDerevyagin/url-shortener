#!/usr/bin/env sh

echo "
bind 127.0.0.1  -::1
protected-mode yes
port $R_PORT
dir '$PWD/data'
logfile '$PWD/data/redis.log'
databases 2
set-proc-title no
maxmemory 3GB
maxmemory-policy allkeys-lru
" > redis.conf
