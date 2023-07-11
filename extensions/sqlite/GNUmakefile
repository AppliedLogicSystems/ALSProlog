SDK_DIR = ../../foreign_sdk/unix/ALS_Prolog_Foreign_SDK
ALSPRO = ../../core/unix/darwin/alspro

sqlite3_intf.psl: sqlite3_intf.c
	gcc -fpic -shared  $(PSL_LINK) \
	-o sqlite3_intf.psl -I$(SDK_DIR)/include \
	    sqlite3_intf.c $(SDK_DIR)/source/alspi_slib.c -lc -lsqlite3

run: sqlite3_intf.psl
	$(ALSPRO) sqlite3_intf.pro

test: sqlite3_intf.psl
	$(ALSPRO) ../../core/alsp_src/tests/test.pro tests_sqlite3_intf.pro -g test_sqlite3 -b

clean:
	rm -f sqlite3_intf.psl
	rm -rf darwin/
