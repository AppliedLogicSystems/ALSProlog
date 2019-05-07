#!/bin/bash

set -eux

: ${debug:=0}

ALSPRO=$1

TSUITE=$(dirname "$0")
TESTDIR=$(dirname "$TSUITE")

trap 'pkill -P $$ || true' EXIT

"$TSUITE"/echo/serve &
sleep 2

if (( $debug ))
then
"$ALSPRO" -no_obp -s "$TESTDIR" "$TSUITE"/curl_test.pro -g 'test ; true'
else
"$ALSPRO" -no_obp -s "$TESTDIR" "$TSUITE"/curl_test.pro -g test < /dev/null
fi
