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

static int prolog_sqlite3_open_x(void)
{
        PWord val1, val2, val3;
        int type1, type2, type3, returnv;
        char buf[PATH_MAX];
        const char *dbName = NULL;
        sqlite3 *dbHandle = NULL;

        PI_getan(&val1,&type1,1);
        PI_getan(&val2,&type2,2);
        PI_getan(&val3,&type3,3);

        switch(type1) {
                case PI_SYM:
                        dbName = PI_getsymname(buf,val1,PATH_MAX);
                        break;
                case PI_UIA:
                        dbName = PI_getuianame(buf,val1,PATH_MAX);
                        break;
                default:
                        dbName = NULL;
                        break;
        }

        returnv = sqlite3_open(dbName, &dbHandle);

        PI_unify(val2, type2, (PWord)dbHandle, PI_INT);
        PI_unify(val3, type3, returnv, PI_INT);
        PI_SUCCEED;
}

static int prolog_sqlite3_close_x(void)
{
        PWord val1, val2;
        int type1, type2, returnv;
        sqlite3 *dbHandle = NULL;
        sqlite3_stmt *pstmt;

        PI_getan(&val1,&type1,1);
        PI_getan(&val2,&type2,2);
        dbHandle = (sqlite3 *)val1;
        returnv = sqlite3_close(dbHandle);
        PI_unify(val2, type2, returnv, PI_INT);
        PI_SUCCEED;
}

static int prolog_sqlite3_exec_norows_x(void) {
        PWord val1, val2, val3; 
        int type1, type2, type3, returnv;
        char buf[PATH_MAX];
        char sql[PATH_MAX];
        sqlite3 *dbHandle = NULL;
        sqlite3_stmt *pstmt = NULL;
        char *err_msg = 0;
        
        PI_getan(&val1,&type1,1);
        PI_getan(&val2,&type2,2);
        PI_getan(&val3,&type3,3);

        if (type1 == PI_INT) {
                dbHandle = (sqlite3 *)val1;
        } else {
                dbHandle = NULL;
	}

        switch(type2) {
                case PI_SYM:
                        PI_getsymname(sql,val2,PATH_MAX);
                        break;
                case PI_UIA:
                        PI_getuianame(sql,val2,PATH_MAX);
                        break;
                default:
			sql[0] = '\0';
	}
	returnv = sqlite3_prepare_v2(dbHandle, sql, -1, &pstmt, NULL);
	returnv = sqlite3_step(pstmt);

        PI_unify(val3, type3, returnv, PI_INT);
        PI_SUCCEED;
}  

static int prolog_sqlite3_exec_rows_x(void) {
	PWord 	val, 	   /* original incoming variable, arg 4 of sqlite3_exec_rows in prolog */
		temp_list, /* temporary list */
		head; 	   /* head of temp_list */
	int val_type, temp_listtype, headtype;
	PWord val1, val2, val3, val4, val5;
	int type1, type2, type3, type4, type5, returnv, handle_opened=0;
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
	PI_getan(&val,&val_type,4); 
       	PI_getan(&val5,&type5,5);

		// Open a connection to the db (arg 1)
	if (type1 == PI_INT) {
		dbHandle = (sqlite3 *)val1;
        } else {
                dbHandle = NULL;
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
			sql[0] = '\0';
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
				buf[0] = '\0';
			}
                }
                if (memcmp(buf, "all", 3) == 0  ) {
                        howMany = INT_MAX;
                } else {
                        fprintf(stderr,"Error: can't understand howMany= %s\n", buf);
                        sqlite3_close(dbHandle);
			returnv = SQLITE_ERROR;
                }
        }


	returnv = sqlite3_prepare_v2(dbHandle, sql, -1, &pstmt, NULL);
	if (returnv != SQLITE_OK) {
		if (handle_opened == 1){ sqlite3_close(dbHandle); }
        	PI_unify(val5, type5, returnv, PI_INT);
        	PI_SUCCEED;
	} 

    int rowCount = 0;
    returnv = sqlite3_step(pstmt);
    if ( returnv != SQLITE_OK && returnv != SQLITE_ROW && returnv != SQLITE_DONE ) {
	if (handle_opened == 1){ sqlite3_close(dbHandle); }
        PI_unify(val5, type5, returnv, PI_INT);
        PI_SUCCEED;
    } 

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
			// PI_makeuia(&uia, &uiatype, (const char *)valBlog);
			// fprintf(stderr, "columnName = %s, BLOB\n", columnName);
            }
            else if (type == SQLITE_NULL)
		// should this be nil_sym or '' or ...?
            {
			// fprintf(stderr, "columnName = %s, NULL\n", columnName);
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
//	if (returnv != SQLITE_OK) {
        if ( returnv != SQLITE_OK && returnv != SQLITE_ROW && returnv != SQLITE_DONE ) {
		if (handle_opened == 1){ sqlite3_close(dbHandle); }
        	PI_unify(val5, type5, returnv, PI_INT);
        	PI_SUCCEED;
	} 
    }   // end while

    if (returnv != SQLITE_OK && returnv != 100 && returnv != 101) {
		if (handle_opened == 1){ sqlite3_close(dbHandle); }
        	PI_unify(val5, type5, returnv, PI_INT);
        	PI_SUCCEED;
    }

//fprintf(stderr, "AFTER last check on returnv\n");
        PI_unify(val5, type5, returnv, PI_INT);
	if (!PI_unify(val,val_type,nil_sym,PI_SYM))
		PI_FAIL; 

	PI_SUCCEED;

}

/*-----------------------------------------------*
	const char *sqlite3_errstr(int);
	sqlite3_errstr() returns the English-language text that describes the result code
 *-----------------------------------------------*/
static int prolog_sqlite3_errstr_x(void)
{
        PWord val1, val2;
        int type1, type2;

        PI_getan(&val1,&type1,1);
	if (type1 != PI_INT) {
		PI_FAIL;
	}
        PI_getan(&val2,&type2,2);

	PWord uia;
	int uiatype;
    	const char * valChar = sqlite3_errstr(val1);
	PI_makeuia(&uia, &uiatype, (const char *)valChar);

	if (!PI_unify(val2, type2, uia, uiatype))
		PI_FAIL;

        PI_SUCCEED;
}

/*-----------------------------------------------*
	const char *sqlite3_errmsg(sqlite3*);
	sqlite3_errmsg() returns English-language text that describes the error
 *-----------------------------------------------*/
static int prolog_sqlite3_errmsg_x(void)
{
        PWord val1, val2;
        int type1, type2;

        PI_getan(&val1,&type1,1);
	if (type1 != PI_INT) 
		PI_FAIL;
        PI_getan(&val2,&type2,2);
	PWord uia;
	int uiatype;
        const char * valChar = sqlite3_errmsg((sqlite3 *)val1);
	PI_makeuia(&uia, &uiatype, (const char *)valChar);

	if (!PI_unify(val2, type2, uia, uiatype))
		PI_FAIL;

        PI_SUCCEED;
}

PI_BEGIN
	PI_DEFINE("sqlite3_errstr_x", 2, prolog_sqlite3_errstr_x)
	PI_DEFINE("sqlite3_errmsg_x", 2, prolog_sqlite3_errmsg_x)
	PI_DEFINE("sqlite3_open_x", 3, prolog_sqlite3_open_x)
	PI_DEFINE("sqlite3_close_x", 2, prolog_sqlite3_close_x)
	PI_DEFINE("sqlite3_exec_norows_x", 3, prolog_sqlite3_exec_norows_x)
	PI_DEFINE("sqlite3_exec_rows_x", 5, prolog_sqlite3_exec_rows_x)
PI_END

void pi_init(void) 
{
	int niltype; /* we don’t use this anywhere else */
	PI_makesym(&nil_sym,&niltype,"[]");

	PI_INIT; 
}
