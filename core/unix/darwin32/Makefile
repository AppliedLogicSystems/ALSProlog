# TODO LP64: For testing 32 vs 64 bit builds
SOURCE_DIR=../../alsp_src
OS=unix
SUBOS=darwin
PROC=port
CFLAGS=-m32
LDFLAGS=-m32

# Include Tk Framework headers on system path for X11 header access
SDK:=$(shell xcrun --show-sdk-path)
BUILD_FLAGS=-isystem $(SDK)/System/Library/Frameworks/Tk.framework/Versions/Current/Headers

include $(SOURCE_DIR)/generic/gnu_makefile
