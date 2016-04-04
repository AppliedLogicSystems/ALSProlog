SOURCE_DIR=../../alsp_src
OS=unix
SUBOS=darwin
PROC=port
SDK:=$(shell xcrun --show-sdk-path)
TK_HEADERS:=$(shell dirname `readlink $(SDK)/usr/include/tk.h`)
BUILD_FLAGS=-I$(SDK)/usr/include/$(TK_HEADERS)
include $(SOURCE_DIR)/generic/gnu_makefile
