#!/bin/bash
set -eux

cd ..; mv ftp/alsprolog.com.tar.gz . ; gunzip ./alsprolog.com.tar.gz ; tar -xf ./alsprolog.com.tar ; rm ./alsprolog.com.tar
