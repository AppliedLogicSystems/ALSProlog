SOURCE_DIR=../../alsp_src
OS=unix
SUBOS=darwin
PROC=port

# Include Tk Framework headers on system path for X11 header access
SDK:=$(shell xcrun --show-sdk-path)
BUILD_FLAGS=-isystem $(SDK)/System/Library/Frameworks/Tk.framework/Versions/Current/Headers

include $(SOURCE_DIR)/generic/gnu_makefile
