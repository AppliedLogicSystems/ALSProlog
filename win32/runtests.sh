#!/bin/bash
#
TESTSRC=../core/alsp_src/tests

mkdir testdir
cp -r $TESTSRC/* testdir
cd testdir
echo `pwd`
#ls $TESTSRC

../ALS\ Prolog/alspro.exe atest_db.pro autotest.pro -g run_tests



