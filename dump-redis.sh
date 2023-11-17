#!/bin/bash

# Specify the Redis server connection details
REDIS_HOST="localhost"
REDIS_PORT="6379"

# Set the output directory for dump files
DUMP_DIR="./redis_dumps"

# Create the dump directory if it doesn't exist
mkdir -p "$DUMP_DIR"

# Fetch all keys from Redis
KEYS=$(redis-cli -h "$REDIS_HOST" -p "$REDIS_PORT" KEYS "*")

# Dump each key
for KEY in $KEYS
do
    DUMP_FILE="$DUMP_DIR/$KEY.dump"
    redis-cli -h "$REDIS_HOST" -p "$REDIS_PORT" DUMP "$KEY" > "$DUMP_FILE"
    echo "Dumped key: $KEY to $DUMP_FILE"
done

echo "Dumping complete."
