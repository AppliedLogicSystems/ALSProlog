#!/bin/bash

set -eux

: ${debug:=0}

ALSPRO=$1

TESTDIR=$(dirname "$0")

trap 'pkill -P $$ || true' EXIT

"$TESTDIR"/echo/serve &
sleep 2

if (( $debug ))
then
"$ALSPRO" "$TESTDIR"/curl_test.pro -g 'test ; true'
else
"$ALSPRO" "$TESTDIR"/curl_test.pro -g test < /dev/null
fi
