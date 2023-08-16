/*=======================================================================*
 |                      sqlite3_intf.c
 |		Copyright (c) 2023 Applied Logic Systems, Inc.
 |
 |              -- Prolog <--> Sqlite interface
 |              sqlite3_open    prolog_sqlite3_open
 |              sqlite3_close   prolog_sqlite3_close
 |              sqlite3_exec    prolog_sqlite3_exec
 |			prolog_sqlite3_exec_norows
 |			prolog_sqlite3_exec_rows
 |	------ todo:
 |		support foreign keys (in sqlite3_intf.pro)
 |		add create index : https://www.sqlite.org/lang_createindex.html
 |		add max90, other aggregates: https://www.sqlite.org/lang_aggfunc.html
 *=======================================================================*/

#include "alspi.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <limits.h>

#include <sqlite3.h>


/* Prolog types:
#define PI_VAR          0 unbound variables
#define PI_LIST         1
#define PI_STRUCT       2 functor with args
#define PI_SYM          3 atoms
#define PI_INT          4
#define PI_UIA          5 uninterned atoms
#define PI_DOUBLE       6
 */

static PWord nil_sym;

static int prolog_sqlite3_open(void)
{
	PWord val1, val2;      
	int type1, type2, returnv; 
	char buf[PATH_MAX];
	sqlite3 *dbHandle;

	PI_getan(&val1,&type1,1);
	PI_getan(&val2,&type2,2);

	switch(type1) {
		case PI_SYM:
			PI_getsymname(buf,val1,PATH_MAX);
			break;
		case PI_UIA:
			PI_getuianame(buf,val1,PATH_MAX);
			break;
		default:
			PI_FAIL;
	}
	returnv = sqlite3_open(buf, &dbHandle);
	if (returnv == SQLITE_OK && PI_unify(val2, type2, (PWord)dbHandle, PI_INT)) {
		PI_SUCCEED;
	} else {
		PI_FAIL;
	}
}

static int prolog_sqlite3_close(void)
{
	PWord val1, val2;
	int type1, type2, returnv;
	sqlite3 *dbHandle;

	PI_getan(&val1,&type1,1);
	dbHandle = (sqlite3 *)val1;
	returnv = sqlite3_close(dbHandle);
	if (returnv != SQLITE_OK) {
		PI_FAIL;
	} else {
		PI_SUCCEED;
	}
}

static int prolog_sqlite3_exec_norows(void) {
	PWord val1, val2;
	int type1, type2, returnv, handle_opened=0;
	char buf[PATH_MAX];
	sqlite3 *dbHandle;
	char *err_msg = 0;

	PI_getan(&val1,&type1,1);
	PI_getan(&val2,&type2,2);

	if (type1 == PI_INT) {
		dbHandle = (sqlite3 *)val1;
fprintf(stderr, "norows-val1-dbHandle: %d\n", dbHandle);
	} else {
		switch(type1) {
			case PI_SYM:
				PI_getsymname(buf,val1,PATH_MAX);
				break;
			case PI_UIA:
				PI_getuianame(buf,val1,PATH_MAX);
				break;
			default:
				PI_FAIL;
		}
		returnv = sqlite3_open(buf, &dbHandle);
		if (returnv != SQLITE_OK) {
			fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(dbHandle));
			sqlite3_close(dbHandle);
			PI_FAIL;
		} else {
			handle_opened = 1;
		}
	}
	switch(type2) {
		case PI_SYM:
			PI_getsymname(buf,val2,PATH_MAX);
			break;
		case PI_UIA:
			PI_getuianame(buf,val2,PATH_MAX);
			break;
		default:
			if (handle_opened == 1){
				sqlite3_close(dbHandle);
			}
			PI_FAIL;
	}

fprintf(stderr, "norows - val2 - buf: %s\n", buf);
	returnv = sqlite3_exec(dbHandle, buf, 0, 0, &err_msg);

	if (returnv != SQLITE_OK ) {
		fprintf(stderr, "SQL error: %s\n", err_msg);
		sqlite3_free(err_msg);        
		if (handle_opened == 1){
			sqlite3_close(dbHandle);
		}
	PI_FAIL;
	}
	if (handle_opened == 1){
		sqlite3_close(dbHandle);
	}
	PI_SUCCEED;
}


static int prolog_sqlite3_exec_rows(void) {
	PWord 	val, 	   /* original incoming variable, arg 4 of sqlite3_exec_rows in prolog */
		temp_list, /* temporary list */
		head; 	   /* head of temp_list */
	int val_type, temp_listtype, headtype;
	PWord val1, val2, val3, val4;
	int type1, type2, type3, type4, returnv, handle_opened=0;
	int howMany = 0;

	char buf[PATH_MAX];
	char sql[PATH_MAX];
	sqlite3 *dbHandle;
	sqlite3_stmt *pstmt;

	PWord struct_val, struct_functor;
	int struct_val_type, struct_functor_type, struct_functor_arity;
	PWord arg;
	int argtype;

	PI_getan(&val1,&type1,1);
	PI_getan(&val2,&type2,2);
	PI_getan(&val3,&type3,3);

		// Open a connection to the db (arg 1)
	if (type1 == PI_INT) {
		dbHandle = (sqlite3 *)val1;
	} else {
		switch(type1) {
			case PI_SYM:
				PI_getsymname(buf,val1,PATH_MAX);
				break;
			case PI_UIA:
				PI_getuianame(buf,val1,PATH_MAX);
				break;
			default:
				PI_FAIL;
		}
		returnv = sqlite3_open(buf, &dbHandle);
		if (returnv != SQLITE_OK) {
			fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(dbHandle));
			sqlite3_close(dbHandle);
			PI_FAIL;
		} else {
			handle_opened = 1;
		}
	}
	switch(type2) {
		case PI_SYM:
			PI_getsymname(sql,val2,PATH_MAX);
			break;
		case PI_UIA:
			PI_getuianame(sql,val2,PATH_MAX);
			break;
		default:
		{
			if (handle_opened == 1){ sqlite3_close(dbHandle); }
			PI_FAIL;
		}
	}

        if (type3 == PI_INT) {
                howMany = (int)val3;
        } else {
                switch(type3) {
                        case PI_SYM:
                                PI_getsymname(buf,val3,PATH_MAX);
                                break;
                        case PI_UIA:
                                PI_getuianame(buf,val3,PATH_MAX);
                                break;
                        default:
			{
				if (handle_opened == 1){ sqlite3_close(dbHandle); }
                                PI_FAIL;
			}
                }
                if (memcmp(buf, "all", 3) == 0  ) {
                        howMany = INT_MAX;
                } else {
                        fprintf(stderr,"Error: can't understand howMany= %s\n", buf);
                        sqlite3_close(dbHandle);
                        PI_FAIL;
                }
        }

	PI_getan(&val,&val_type,4); 

	returnv = sqlite3_prepare_v2(dbHandle, sql, -1, &pstmt, NULL);
	if (returnv != SQLITE_OK) {
		if (handle_opened == 1){ sqlite3_close(dbHandle); }
		fprintf(stderr, "Cannot prepare sql: %s\n", buf);
		PI_FAIL;
	} 

    int rowCount = 0;
    returnv = sqlite3_step(pstmt);

	/*
	 * Loop through all the rows returned, creating a row entity struct r(...) for each; 
	 * For each row, loop through all the columns in the row returned, setting the
	 * values of the nth arg of r(...) to the value of the nth arg of the returned row;
	 * Create a Prolog list with its elements unified with the row structs r(...) in
	 * the order the rows are returned.
	 */

    while (returnv != SQLITE_DONE && returnv != SQLITE_OK && rowCount < howMany)
    {
	rowCount++;
		// Make a structure r(...) to contain the values from a row
        int numCols = sqlite3_column_count(pstmt);
	struct_functor_arity = numCols;
	PI_makesym(&struct_functor,&struct_functor_type, "r");
	PI_makestruct(&struct_val,&struct_val_type, struct_functor, struct_functor_arity);

		// SQLITE_INTEGER=1 SQLITE_FLOAT=2 SQLITE_TEXT=3
        for (int colIndex = 0; colIndex < numCols; colIndex++)
        {
	    PI_getargn(&arg, &argtype, struct_val, colIndex+1);
            int type = sqlite3_column_type(pstmt, colIndex);
            const char * columnName = sqlite3_column_name(pstmt, colIndex);

		// NOTE: Below, Sqlite colIndex starts at 0; prolog structure arg index starts at 1
            if (type == SQLITE_INTEGER)
            {
                int valInt = sqlite3_column_int(pstmt, colIndex);
		if (!PI_unify(arg, argtype, valInt, PI_INT))
		{
			if (handle_opened == 1){ sqlite3_close(dbHandle); }
			PI_FAIL;
		}
            }
            else if (type == SQLITE_FLOAT)
            {
		PWord dbl;
		int dbltype;
                double valDouble = sqlite3_column_double(pstmt, colIndex);
		PI_makedouble(&dbl, &dbltype, valDouble);
		if (!PI_unify(arg, argtype, dbl, dbltype))
		{
			if (handle_opened == 1){ sqlite3_close(dbHandle); }
			PI_FAIL;
		}
            }
            else if (type == SQLITE_TEXT)
            {
		PWord uia;
		int uiatype;
		const unsigned char * valChar = sqlite3_column_text(pstmt, colIndex);
		PI_makesym(&struct_functor,&struct_functor_type, "r");
		PI_makeuia(&uia, &uiatype, (const char *)valChar);
		if (!PI_unify(arg, argtype, uia, uiatype))
		{
			if (handle_opened == 1){ sqlite3_close(dbHandle); }
			PI_FAIL;
		}
            }
            else if (type == SQLITE_BLOB)
		// store blobs as UIAs on the prolog side
            {
		PWord uia;
		int uiatype;
		const unsigned char * valBlog = sqlite3_column_text(pstmt, colIndex);
		PI_makesym(&struct_functor,&struct_functor_type, "r");
//		PI_makeuia(&uia, &uiatype, (const char *)valBlog);
                	fprintf(stderr, "columnName = %s, BLOB\n", columnName);
            }
            else if (type == SQLITE_NULL)
		// should this be nil_sym or '' or ...?
            {
//                	fprintf(stderr, "columnName = %s, NULL\n", columnName);
            }
        }  // end for-loop over columns


		PI_makelist(&temp_list,&temp_listtype);
		if (!PI_unify(val,val_type,temp_list,temp_listtype))
		{
			if (handle_opened == 1){ sqlite3_close(dbHandle); }
			PI_FAIL; 
		}

		PI_gethead(&head,&headtype,temp_list); 
		if (!PI_unify(head,headtype,struct_val,struct_val_type))
		{
			if (handle_opened == 1){ sqlite3_close(dbHandle); }
			PI_FAIL; 
		}
		PI_gettail(&val,&val_type,temp_list);

        returnv = sqlite3_step(pstmt);
    }

	if (handle_opened == 1){
		sqlite3_close(dbHandle);
	}
	if (!PI_unify(val,val_type,nil_sym,PI_SYM))
		PI_FAIL; 
	PI_SUCCEED;
}



PI_BEGIN
	PI_DEFINE("sqlite3_open", 2, prolog_sqlite3_open)
	PI_DEFINE("sqlite3_close", 1, prolog_sqlite3_close)
	PI_DEFINE("sqlite3_exec_norows", 2, prolog_sqlite3_exec_norows)
	PI_DEFINE("sqlite3_exec_rows", 4, prolog_sqlite3_exec_rows)
PI_END

void pi_init(void) 
{
	int niltype; /* we don’t use this anywhere else */
	PI_makesym(&nil_sym,&niltype,"[]");

	PI_INIT; 
}
