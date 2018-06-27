#!/bin/bash

set -eux

: ${debug:=0}

ALSPRO=$1

TESTDIR=$(dirname "$0")

trap 'pkill -P $$' EXIT

php --server localhost:8888 --docroot "$TESTDIR"/echo &

if (( $debug ))
then
"$ALSPRO" "$TESTDIR"/curl_test.pro -g 'test ; true'
else
"$ALSPRO" "$TESTDIR"/curl_test.pro -g test
fi
