#!/bin/bash

set -eux

ALSPRO=$1

TESTDIR=$(dirname "$0")

trap 'pkill -P $$' EXIT

php --server localhost:8888 --docroot "$TESTDIR"/echo &

"$ALSPRO" "$TESTDIR"/curl_test.pro -g test
