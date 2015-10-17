#!/bin/bash
set -eux

cd ..; tar -cf ./alsprolog.com.tar ./alsprolog.com ; gzip ./alsprolog.com.tar; sh alsprolog.com/2bakha alsprolog.com.tar.gz ; rm alsprolog.com.tar.gz
