/*=======================================================================*
 |			bcurl.c
 |		Copyright (c) 2018-2019 Applied Logic Systems, Inc.
 |
 |		-- "Direct to curl" prolog interface
 |
 | Author: Chuck Houpt, Ken Bowen
 |
 | See live samples at end of ~builtins/blt_curl.pro
 *=======================================================================*/

#include "alspi.h"
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include <curl/curl.h>

/* Prolog types:
#define PI_VAR          0 unbound variables
#define PI_LIST         1
#define PI_STRUCT       2 functor with args
#define PI_SYM          3 atoms
#define PI_INT          4
#define PI_UIA          5 uninterned atoms
#define PI_DOUBLE       6
*/

int	curl_c_builtin	( void );
int	lookup_opt_info	( void );
char* 	normalize_opt(char* option_str);
char* normalize_info(char* option_str);
int lookup_code(const char *opt_name);

#define BUFSIZE 1024

/* -------------------------------------------------------------------------------
 | Taken from https://curl.haxx.se/libcurl/c/getinmemory.html
 | utilizing
 | https://stackoverflow.com/questions/2329571/c-libcurl-get-output-into-a-string
 * ------------------------------------------------------------------------------*/

/* Replaced by MemoryWithFileStruct, WriteMemWithFileCallback:
struct MemoryStruct {
    char *memory;
    size_t size;
};

static size_t
WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    size_t realsize = size * nmemb;
    struct MemoryStruct *mem = (struct MemoryStruct *)userp;

    mem->memory = realloc(mem->memory, mem->size + realsize + 1);
    if(mem->memory == NULL) {
// out of memory! :::: NEED TO BUBBLE THIS UP TO PROLOG 
      printf("not enough memory (realloc returned NULL)\n");
      return 0;
    }

    memcpy(&(mem->memory[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;

    return realsize;
}    
*/



struct MemoryWithFileStruct {
    char *memory;
    size_t size;
    FILE *pagefile;
    char *filename;
};

static size_t
WriteMemWithFileCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    size_t realsize = size * nmemb;
    struct MemoryWithFileStruct *memf = (struct MemoryWithFileStruct *)userp;

    memf->memory = realloc(memf->memory, memf->size + realsize + 1);
    if(memf->memory == NULL) {
      /* out of memory! :::: NEED TO BUBBLE THIS UP TO PROLOG */
      printf("not enough memory (realloc returned NULL)\n");
      return 0;
    }

    memcpy(&(memf->memory[memf->size]), contents, realsize);
    memf->size += realsize;
    memf->memory[memf->size] = 0;

    char *ff;
    ff = memf->filename;
    if (ff != NULL){
        FILE *pf;
        pf = fopen(ff, "ab");
        if (pf){
//            should coordinate written2file # with realsize:
//            size_t written2file = fwrite(contents, size, nmemb, (FILE *)pf);
            fwrite(contents, size, nmemb, (FILE *)pf);
            fclose(pf);
    }
    }

    return realsize;
}    

/* -------------------------------------------------------------------------------
 | Based on https://curl.haxx.se/libcurl/c/post-callback.html
 * ------------------------------------------------------------------------------*/

struct WriteThis {
  const char *readptr;
  size_t sizeleft;
};

static size_t read_callback(void *dest, size_t size, size_t nmemb, void *userp)
{
  struct WriteThis *wt = (struct WriteThis *)userp;
  size_t buffer_size = size*nmemb;
 
  if(wt->sizeleft) {
    /* copy as much as possible from the source to the destination */ 
    size_t copy_this_much = wt->sizeleft;
    if(copy_this_much > buffer_size)
      copy_this_much = buffer_size;
    memcpy(dest, wt->readptr, copy_this_much);
 
    wt->readptr += copy_this_much;
    wt->sizeleft -= copy_this_much;
    return copy_this_much; /* we copied this many bytes */ 
  }
 
  return 0; /* no more data left to deliver */ 
}

static PWord nil_sym;
int niltype;

/* adjust_opts/2 in blt_curl.pro guarantees that option_str is all upper case */

/* memory was malloc'd for the result here; do we need to release it?? */
char* 
normalize_opt(char* option_str)
{
    if ( strncmp("CURLOPT_", option_str, 8) != 0 ){
		/* option_str = "CURLOPT_" + option_str; */
	char *tstr = (char *)malloc(1 + strlen(option_str) + 8);
	strcpy(tstr, "CURLOPT_");
	strcat(tstr, option_str);
	option_str = tstr;
    }
    return option_str;
}
char* 
normalize_info(char* option_str)
{
    if ( strncmp("CURLINFO_", option_str, 9) != 0 ){
		/* option_str = "CURLINFO_" + option_str; */
	char *tstr = (char *)malloc(1 + strlen(option_str) + 9);
	strcpy(tstr, "CURLINFO_");
	strcat(tstr, option_str);
	option_str = tstr;
    }
    return option_str;
}

#define name2value(symbol) { #symbol, symbol }

/* Below, in the included curlopt_list.h,
	name2value(CURLOPT_URL), will expand to { "CURLOPT_URL", CURLOPT_URL },
	etc.
 */

#include "curlopt_list.h"

	/* optimize this with binary search */
int 
lookup_code(const char *opt_name) {
    size_t length_nv = sizeof(option_name_value_list) / sizeof(option_name_value_list[0]);
    for (int i = 0; i < length_nv; i++) 
    {
	if (0 == strcmp(opt_name, option_name_value_list[i].name)){
	        return option_name_value_list[i].value;
	}
    }
    return 1;
}

int 
lookup_opt_info(void)
{
    PWord op_str;
    int op_str_t;
    char op_strbuf[BUFSIZE];

    PI_getan(&op_str, &op_str_t, 1);

    if (op_str_t == PI_SYM){
	PI_getsymname(op_strbuf,op_str,BUFSIZE);
    } else if (op_str_t == PI_UIA ){
	PI_getuianame(op_strbuf,op_str,BUFSIZE);
    } 

	    /* check CURLOPT first */
    char *xstr = normalize_opt(op_strbuf);
    int opt_val = lookup_code(xstr);
    if (opt_val > 1){
	PI_SUCCEED;
    } else {
	    /* not an opt, so check CURLINFO next */
	char *ystr = normalize_info(op_strbuf);
	opt_val = lookup_code(ystr);
	if (opt_val > 1) {
	    PI_SUCCEED;
	} else {
	    PI_FAIL;
	}
    }
}

/* -------------------------------------------------------------------------------
 |	A struct in which to save info about an incoming CURLINFO equation, for
 |	processing after the call to     
 |		curl_easy_perform(easyhandle)     
 * ------------------------------------------------------------------------------*/
struct CurlInfoIn {
    int info_code;
    int info_type;
    PWord arg2;
    int arg2_t;
};

/* -------------------------------------------------------------------------------
	curl_c_builtin(void)
 * ------------------------------------------------------------------------------*/

int
curl_c_builtin(void)
{
    PWord oplist, error, head, tail, arg1, arg2, result_var;
    int oplist_t, error_t, head_t, tail_t, arg1_t, arg2_t, result_var_t;
    int have_result_var=1, have_result_file=1; 
    PWord uia_var;
    int uia_var_t;
    int opt_action;
    int ret = 0;
    char *filename;
    FILE *pagefile;

//    struct stat file_info;
    
    struct CurlInfoIn CIIArray[30];
    int ciiCtr = 1;

    char *curlinfo_string;
    long curlinfo_long;
    double curlinfo_double;

	/* This should be an incoming uninstantiated var, used for returning error info to prolog */
    PI_getan(&error, &error_t, 2);

        /* In windows, this will init the winsock stuff */
    ret = curl_global_init(CURL_GLOBAL_ALL);
    if(ret != CURLE_OK) {
        fprintf(stderr, "curl_global_init() failed: %s\n", curl_easy_strerror(ret));
			/* Report error to prolog */
	char perrmsg[120];
   	sprintf(perrmsg, "curl_global_init() failed: %s\n", curl_easy_strerror(ret));
        PI_makeuia(&uia_var, &uia_var_t, perrmsg);
	if (PI_unify(error, error_t, uia_var, uia_var_t)) 
            PI_SUCCEED;
        else
            PI_FAIL;
    }


    PI_makesym(&nil_sym,&niltype,"[]");
    PI_getan(&oplist, &oplist_t, 1);

    CURL *easyhandle = curl_easy_init();

    struct MemoryWithFileStruct chunk;
    chunk.filename = NULL;

    struct WriteThis wt;
    wt.sizeleft = 0;

        /* Process the options list: */
    if(oplist_t != PI_LIST) {
	/* This error is handled in curl/3 in blt_curl.pro. */
        PI_FAIL;
    }
    else if(oplist_t==PI_SYM && oplist==nil_sym) {
        /* nothing to do: list = [] */
    }
    else { 	/* optlist has at least one element */
                /* process options on the list */
	char a1buf[BUFSIZE];
	char a2buf[BUFSIZE];

	for (; oplist_t==PI_LIST;)
	{
	    PI_gethead(&head,&head_t,oplist);
	    PI_gettail(&tail,&tail_t,oplist);

		/* adjust_opts/2 in blt_curl.pro guarantees that head is an eqn: */
	    PI_getargn(&arg1, &arg1_t, head, 1);
            PI_getargn(&arg2, &arg2_t, head, 2);

		/* the option (arg1) might be a sym or a uia, but /is/ one or the other: */
	    if (arg1_t == PI_SYM){
                PI_getsymname(a1buf,arg1,BUFSIZE);
	    } else if (arg1_t == PI_UIA ){
                PI_getuianame(a1buf,arg1,BUFSIZE);
	    } 
//printf("i=%d arg1=%s\n",i,a1buf);

		/* ------ First handle any non-CURLOPT cases ------ */
		/* Save any incoming prolog variables for RESULT */
	    
	    if (0==strcmp("RESULT", a1buf) || 0==strcmp("RESULTFILE", a1buf) || 0==strcmp("UPLOADDATA", a1buf)) 
	    { 
	        if (0==strcmp("RESULT", a1buf)) { 
		    /* save the prolog result var */
		    have_result_var = 0;
		    result_var = arg2;
		    result_var_t = arg2_t;

        	    chunk.memory = malloc(1);  /* will be grown as needed by the realloc above */
        	    chunk.size = 0;    /* no data at this point */
        	    chunk.memory[0] = '\0';
		    if (have_result_file != 0){
        	        chunk.pagefile = NULL;
		    }
                                /* send all data to this function  */
        	    curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemWithFileCallback);
                                /* we pass our 'chunk' struct to the callback function */
        	    curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);
	        } 
	        if (0==strcmp("RESULTFILE", a1buf)){
		    have_result_file = 0;

		    char h2buf[BUFSIZE];    /* if a2buf is used below, var filename gets overwritten */
	    	    if (arg2_t == PI_SYM){
                	   PI_getsymname(h2buf,arg2,BUFSIZE);
	    	    } else if (arg2_t == PI_UIA ){
                	   PI_getuianame(h2buf,arg2,BUFSIZE);
		    }
			/* save the result file path */
    		    filename = h2buf;
			/* send all data to this function  */
        	    curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemWithFileCallback);
		        /* open the file */
		    pagefile = fopen(filename, "wb");
		    if (pagefile) {
        		chunk.memory = malloc(1);  /* will be grown as needed by the realloc above */
        		chunk.size = 0;    /* no data at this point */
        		chunk.memory[0] = '\0';
        		chunk.pagefile = pagefile;
    			chunk.filename = filename;
			    /* write the incoming page body to this file handle */
        		curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);
		    }
	        } 
	        if (0==strcmp("UPLOADDATA", a1buf)){
		    char g2buf[BUFSIZE];    /* if a2buf is used below, var filename gets overwritten */
		    if (arg2_t == PI_SYM){
			PI_getsymname(g2buf,arg2,BUFSIZE);
		    } else if (arg2_t == PI_UIA ){
			PI_getuianame(g2buf,arg2,BUFSIZE);
		    }
		    wt.readptr = g2buf;
  		    wt.sizeleft = strlen(g2buf);
        	    curl_easy_setopt(easyhandle, CURLOPT_READFUNCTION, read_callback);
		 	    /* pointer to pass to our read function */ 
    		    curl_easy_setopt(easyhandle, CURLOPT_READDATA, &wt);
        	    curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDSIZE, (long)wt.sizeleft);
	        }
	    } else /* Finished non-CURLOPT cases */
	    {
		    /* Normalize the option/info, and distinguish which it is: */
		char *nni = normalize_info(a1buf);
		int info_code = lookup_code(nni);
		int info_type = info_code & CURLINFO_TYPEMASK;

		char *nopt = normalize_opt(a1buf);
	        int opt_code = lookup_code(nopt);

		/* First check if it is a CURLINFO, and handle if it is */
		/* CURLINFO types are all > 0x100000  */
		if (info_code != 1){
		     /* This (arg1 / a1buf) is a request for something CURLINFO;
			So presumably (arg2,arg2_t) is a prolog variable (bound or unbound)
			which needs to be unified against the given CURLINFO
			/after/ the   ret = curl_easy_perform(easyhandle)   call below.
		      */
		    struct CurlInfoIn cii;
		    cii.info_code = info_code;
		    cii.info_type = info_type;
		    cii.arg2 = arg2;
		    cii.arg2_t = arg2_t;

		    CIIArray[ciiCtr] = cii;
    		    ciiCtr += 1;

		} else   /* Must be CURLOPT */
		{

		/* ------ Now handle all the CURLOPT cases ------ */

			/* Do any required option-specific setup -- based on opt_code */
		switch (opt_code) {
		    case CURLOPT_HTTPGET: 
		    {
		        opt_action = CURLOPT_HTTPGET;
        		curl_easy_setopt(easyhandle, CURLOPT_HTTPGET, arg2);

			break;
		    }

		    case CURLOPT_POST: 
		    case CURLOPT_PUT: 
	   	    { 
		        opt_action = opt_code;
   			curl_easy_setopt(easyhandle, opt_action, arg2);

			if (wt.sizeleft > 0){
        		curl_easy_setopt(easyhandle, CURLOPT_READFUNCTION, read_callback);
			 	/* pointer to pass to our read function */ 
    			curl_easy_setopt(easyhandle, CURLOPT_READDATA, &wt);
        		curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDSIZE, (long)wt.sizeleft);
			}

			break;
		    }
		    case CURLOPT_POSTFIELDS: 
	   	    { 
			if (arg2_t==5)
            		    PI_getuianame(a2buf,arg2,BUFSIZE);
        		else
            		    PI_getsymname(a2buf,arg2,BUFSIZE);
        
        		curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDS, a2buf);
        		curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDSIZE, (long)strlen(a2buf));

			break;
		    }

		    default: 
		    {
			/* normalize-the-option approach */
		
	    	        int val_type = 10000 * (opt_code / 10000);

			/* Do the uniform setup of options on the handle */
  	    	        switch (val_type) {
  			    case CURLOPTTYPE_LONG: 
			    {
		    	        long opt_long = (long)arg2;   //... grab long from prolog ...
  		    	        curl_easy_setopt(easyhandle, opt_code, opt_long);
		    	        break;
			    }
			    case CURLOPTTYPE_OBJECTPOINT: 
			    {
				char g2buf[BUFSIZE];    /* if a2buf is used below, var filename gets overwritten */
	    	    		if (arg2_t == PI_SYM){
                	    	    PI_getsymname(g2buf,arg2,BUFSIZE);
	    	    		} else if (arg2_t == PI_UIA ){
                	    	    PI_getuianame(g2buf,arg2,BUFSIZE);
		    		}
		    		char *opt_str = g2buf;  //... grab string from prolog ...
		    		curl_easy_setopt(easyhandle, opt_code, opt_str);
       		    		break;
			    }

  	    	    	}  /* switch on val_type */

 		    }   /* default case in opt_code switch */

		}    	/* option-specific setup: finish switch on opt_code */

		} 	/* end else for if(info_code != 1) */

	    } 	/* Finish else for: handle all the CURLOPT cases */

		/* Finished this list element; go on to the next */
            oplist = tail;
            oplist_t = tail_t;

	} 	/* Finish list processing for-loop */

	}   	/* end else containing option_list for-loop processing */

	    	/* options all set; try to perform the GET/POST/WHATEVER...*/

        ret = curl_easy_perform(easyhandle);

	    	/* check for errors */

        if(ret != CURLE_OK) 
	{		/* Report error to prolog */
	    char perrmsg[120];
   	    sprintf(perrmsg, "curl_easy_perform() failed: %s\n", curl_easy_strerror(ret));
            PI_makeuia(&uia_var, &uia_var_t, perrmsg);
	    if (PI_unify(error, error_t, uia_var, uia_var_t)) 
                PI_SUCCEED;
            else
                PI_FAIL;

        } else {  /* curl_easy_perform succeeded */
	    for (int j=1; j<ciiCtr; j++)
	    {
		    struct CurlInfoIn cii = CIIArray[j];

		    switch(cii.info_type) {
			case CURLINFO_STRING:
			{
            		    curl_easy_getinfo(easyhandle, cii.info_code, &curlinfo_string);
                	    PI_makeuia(&uia_var, &uia_var_t, (char *)curlinfo_string);
	        	    if (!PI_unify(cii.arg2, cii.arg2_t, uia_var, uia_var_t)) {
                    		PI_FAIL;
			    }
			    break;
			}
			case CURLINFO_LONG:
			{
            		    curl_easy_getinfo(easyhandle, cii.info_code, &curlinfo_long);
                	    if(!PI_unify(cii.arg2, cii.arg2_t, curlinfo_long, PI_INT)) {
                    		PI_FAIL;
			    }
			    break;
			}
			case CURLINFO_DOUBLE:
			{
			    PWord dbl_var;
			    int dbl_var_t;

            		    curl_easy_getinfo(easyhandle, cii.info_code, &curlinfo_double);
			    PI_makedouble(&dbl_var, &dbl_var_t, curlinfo_double);
                	    if(!PI_unify(cii.arg2, cii.arg2_t, dbl_var, dbl_var_t)) {
				PI_FAIL;
			    }
			    break;
			}
			default:
			{
			    /* Make into prolog error */
			    fprintf(stderr,"CURLINFO proc. error: Unknown type: %d\n", cii.info_type);
			}
		    }		/* end switch on cii.info_type */
	    }			/* end for (int j=1; j<ciiCtr; loop */

		/* if result_var was set up, get the data from chunk.memory and unify with 
		   the supplied result */
	    if (have_result_var == 0){
               	PI_makeuia(&uia_var, &uia_var_t, (char *)chunk.memory);
	       	if (PI_unify(result_var, result_var_t, uia_var, uia_var_t))
                    PI_SUCCEED;
                else
                    PI_FAIL;
	    }

	    if (have_result_file == 0){
                        /* close the header file */
		if(pagefile) {
                    fclose(pagefile);
		}
	    }



	    if (opt_action == CURLOPT_HTTPGET) {
	    } else if (opt_action == CURLOPT_POST || opt_action == CURLOPT_PUT || opt_action == CURLOPT_UPLOAD)
	    {
	        if (have_result_var == 0){
                    PI_makeuia(&uia_var, &uia_var_t, (char *)chunk.memory);
	            if (PI_unify(result_var, result_var_t, uia_var, uia_var_t))
                        PI_SUCCEED;
                    else
                    	PI_FAIL;
		}
	    }		/* end: opt_action==CURLOPT_READDATA || opt_action==CURLOPT_READDAT */

        }	/* curl_easy_perform succeeded */

	/* shouldn't get here: SUCCEED OR FAIL SHOULD OCCUPY EVERY PATH */
return 1;
}
