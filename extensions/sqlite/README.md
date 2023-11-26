SQLite Extension
================

This folder provides an extension which allows ALS Prolog programs to interface to Sqlite3 databases.


Files inventory
---------------
GNUmakefile		- 	build instructions for sqlite3_intf.psl from sqlite3_intf.c
README.md		-	this file
sqlite3_intf.c		- 	C source code for the interface
sqlite3_intf.pro	-	prolog code linked to functions in sqlite3_intf.c
sqlite3_intf.psl*	-	(ALS) prolog sharable library code created from sqlite3_intf.c
doc_examples.pro	-	prolog code for examples in prolog_sqlite3_doc.txt
prolog_sqlite3_doc.txt	- 	manual documenting interface predicates in sqlite3_intf.pro
tests_sqlite3_intf.pro	-	test sequence for interface predicates in sqlite3_intf.pro
run_tests_examples.pro	-	toplevel test runner; executes tests_sqlite3_intf.pro together
				    with tests in examples/, tools/singers and tools/bp
examples/		-	several realistic examples of using the interface
    AA_Lincoln.txt		- HTML (fragement) source for Anthonies Awards database
    extract_anthonies.pro	- extracts data from AA_Lincoln.txt and builds database
    ca_vt_births_2016_2021.csv	- CSV source for mother/child birth data for california & vermont
				    (subset of full US database from Kaggle.com)
    load_csv.pro	- 	generic csv loader; used on ca_vt_births_2016_2021.csv
tools/
    dbsetup.pro		- 	a tool for generating a db and supporting code from a specification
tools/bp		-	a simple but realistic example of using the dbsetup.pro tool
tools/bp/bp.db_spec	-	specification for the example database
tools/bp/raw.brainy_puzzles.txt	-  raw data to be loaded into the specified database
tools/bp/bp_load.pro	-  	loads the specified database using the generated code
tools/singers		-	a simple example showing the use of foreign key constraints
tools/singers/singers.db_spec	- specification for the example database
tools/singers/crooners.pro	- test code for the generated example database
tools/singers/inj_test.pro	- test code for the generated example database


Building the library and running tests and examples
---------------------------------------------------
The GNUmakefile specifies the various commands.  The commands reference the ALS build tree:

SDK_DIR = ../../foreign_sdk/unix/ALS_Prolog_Foreign_SDK
ALSPRO = ../../core/unix/darwin/alspro

**make sqlite3_intf.psl**

will simply update the library file if needed.  However, it is not necessary to run this normally since the other commands (e.g., run, test) will check whether sqlite3_intf.c has been modified, and will update sqlite3_intf.psl accordingly.

**make run**

starts an ALS Prolog image loading sqlite3_intf.pro from the commandline.

**make test** 

starts an ALS Prolog image loading sqlite3_intf.pro and tests_sqlite3_intf.pro from the commandline, and executes the predicate test_sqlite3/0.

clean:
	rm -f sqlite3_intf.psl
	rm -rf darwin/

