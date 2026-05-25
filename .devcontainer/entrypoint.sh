#!/bin/bash
set -e

python3 -m http.server 8000 --directory /workspaces/homepage >/tmp/http-server.log 2>&1 &

exec "$@"
