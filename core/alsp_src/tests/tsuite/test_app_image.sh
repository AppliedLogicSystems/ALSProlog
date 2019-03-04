#!/bin/bash
#
# Testing for save_image functionality on unix/linux/macOS
#
if [ -f "./app_image0" ]; then
    rm -f app_image0 app_image0.pst 
fi
if [ -f "./app_image1" ]; then
    rm -f app_image1 app_image1.pst 
fi
if [ -f "./app_image2" ]; then
    rm -f app_image2 app_image2.pst 
fi

SIPATH=../../alsp_src/tests/tsuite/savedimages/

echo "Begin saved images tests"

# app_image0:
TEST_C_OUT=$(./alspro $SIPATH/app_image_test0.pro -q -g mk_app_image0 -b)

if [ $TEST_C_OUT != "app_image0_saved" ];
then
    echo "  >>FAIL: app_image0_save_image error: GOT $TEST_C_OUT"
    exit 1
else
    echo "  >> app_image0_save_image ok"
fi

if [ ! -f "./app_image0" ]; then
    echo "  >>FAIL: app_image0 does not exist"
    exit 1
fi
if [ ! -f "./app_image0.pst" ]; then
    echo "  >>FAIL: app_image0.pst does not exist"
    exit 1
fi

TEST_C_OUT=$(./app_image0)

if [ $TEST_C_OUT != "app_image0_running" ];
then
    echo "  >>FAIL: app_image0_running error: GOT $TEST_C_OUT"
    exit 1
else
    echo "  >> app_image0_running ok"
fi

# app_image1:

TEST_C_OUT=$(./alspro $SIPATH/app_image_test1.pro -q -g mk_app_image1 -b)

if [ $TEST_C_OUT != "app_image1_saved" ];
then
    echo "  >>FAIL: app_image1_saved error: GOT $TEST_C_OUT"
    exit 1
else
    echo "  >> app_image1_save_image ok"
fi

if [ ! -f "./app_image1" ]; then
    echo "  >>FAIL: app_image1 does not exist"
    exit 1
fi
if [ ! -f "./app_image1.pst" ]; then
    echo "  >>FAIL: app_image1.pst does not exist"
    exit 1
fi

# app_image1 running & making app_image2

TEST_C_OUT=$(./app_image1)

if [ $TEST_C_OUT != "app_image1_making_app_image2" ];
then
    echo "  >>FAIL: app_image1_made_app_image2 error: GOT $TEST_C_OUT"
    exit 1
else
    echo "  >> app_image1_made_app_image2 ok"
fi

if [ ! -f "./app_image2" ]; then
    echo "  >>FAIL: app_image2 does not exist"
    exit 1
fi
if [ ! -f "./app_image2.pst" ]; then
    echo "  >>FAIL: app_image2.pst does not exist"
    exit 1
fi

TEST_C_OUT=$(./app_image2)

if [ $TEST_C_OUT != "app_image2_running" ];
then
    echo "  >>FAIL: app_image2_running error: GOT $TEST_C_OUT"
    exit 1
else
    echo "  >> app_image2_running ok"
fi

echo "  >>OK: All saved image apps ran correctly"
