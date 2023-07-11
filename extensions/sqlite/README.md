SQLite Extension
================

This folder provides an extension which allows ALS Prolog programs to interface to Sqlite3 databases.


Files inventory
---------------
GNUmakefile
README.m		-	this file
sqlite3_intf.c		- 	C source code for the interface
sqlite3_intf.pro	-	prolog code linked to functions in sqlite3_intf.c
sqlite3_intf.psl*	-	(ALS) prolog sharable library code created from sqlite3_intf.c
doc_examples.pro	-	prolog code for examples in prolog_sqlite3_doc.txt
prolog_sqlite3_doc.txt	- 	manual documenting interface predicates in sqlite3_intf.pro
tests_sqlite3_intf.pro	-	test predicates for interface predicates in sqlite3_intf.pro
simple_db_setup.pro	-	prolog code supporting db development using the sqlite3 interface
simple_db_doc.txt	-	manual for using simple_db_setup.pro
simple_db_examples/	-	examples of using simple_db_setup.pro

Building the library and running tests and examples
---------------------------------------------------
The GNUmakefile specifies the various commands.  The commands reference the tree:

SDK_DIR = ../../foreign_sdk/unix/ALS_Prolog_Foreign_SDK
ALSPRO = ../../core/unix/darwin/alspro

**make sqlite3_intf.psl**

will simply update the library file if need.  However, it is not necessary to run this normally since the other commands (e.g., run, test) will check whether sqlite3_intf.c has been modified, and will update sqlite3_intf.psl accordingly.

**make run**

starts an ALS Prolog image loading sqlite3_intf.pro from the commandline.

**make test** 

starts an ALS Prolog image loading sqlite3_intf.pro and tests_sqlite3_intf.pro from the commandline, and executes the predicate test_sqlite3/0.

clean:
	rm -f sqlite3_intf.psl
	rm -rf darwin/
