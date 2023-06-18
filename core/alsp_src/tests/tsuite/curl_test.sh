#!/bin/bash

if [[ ! -z "$LP64_PARTIAL_TEST" ]]
then
echo "TODO: restore curl_test.sh" >> /dev/stderr
exit
fi

set -eux

: ${debug:=0}

ALSPRO=$1

TSUITE=$(dirname "$0")
TESTDIR=$(dirname "$TSUITE")

trap 'pkill -P $$ || true' EXIT

"$TSUITE"/echo/serve &

# Wait for server to respond
curl --silent --retry-connrefused --retry 5 http://localhost:8888

if (( $debug ))
then
"$ALSPRO" -no_obp -s "$TESTDIR" "$TSUITE"/curl_test.pro -g 'test ; true'
else
"$ALSPRO" -no_obp -s "$TESTDIR" "$TSUITE"/curl_test.pro -g test < /dev/null
fi
