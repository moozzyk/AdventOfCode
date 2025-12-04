#!/bin/bash
set -euxo pipefail

# Check for exactly 3 arguments
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <day-directory> <puzzle-input-file>"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR" || exit 1

docker run -it --rm -v "$PWD":/opt/app -w /opt/app eclipse-temurin javac -d bin $1/Problems.java utils/*.java
docker run -it --rm -v "$PWD":/opt/app -w /opt/app eclipse-temurin java -cp bin Problems $1/$2