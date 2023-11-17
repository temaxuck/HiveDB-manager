#!/bin/bash

# Specify the Redis server connection details
REDIS_HOST="localhost"
REDIS_PORT="6379"

# Set the input directory for dump files
DUMP_DIR="./redis_dumps"

# Restore each key
for DUMP_FILE in "$DUMP_DIR"/*.dump
do
    KEY=$(basename "$DUMP_FILE" .dump)
    redis-cli -h "$REDIS_HOST" -p "$REDIS_PORT" RESTORE "$KEY" 0 "$(cat "$DUMP_FILE")" --replace
    echo "Restored key: $KEY from $DUMP_FILE"
done

echo "Restoration complete."
