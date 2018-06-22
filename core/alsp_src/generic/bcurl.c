/*=======================================================================*
 |			bcurl.c
 |		Copyright (c) 2018 Applied Logic Systems, Inc.
 |
 |		-- "Direct to curl" prolog interface
 |
 | Author: Chuck Houpt, Ken Bowen
 |
 | See live samples at end of ~builtins/blt_curl.pro
 *=======================================================================*/

#include "alspi.h"
#include <stdlib.h>
#include <strings.h>

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


#define BUFSIZE 1024

/* -------------------------------------------------------------------------------
 | Taken from https://curl.haxx.se/libcurl/c/getinmemory.html
 | utilizing
 | https://stackoverflow.com/questions/2329571/c-libcurl-get-output-into-a-string
 * ------------------------------------------------------------------------------*/

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
      /* out of memory! :::: NEED TO BUBBLE THIS UP TO PROLOG */
      printf("not enough memory (realloc returned NULL)\n");
      return 0;
    }

    memcpy(&(mem->memory[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;
    mem->memory[mem->size + 1] = '\0';

    return realsize;
}    

/* -------------------------------------------------------------------------------
 | Based on https://curl.haxx.se/libcurl/c/url2file.html 
 * ------------------------------------------------------------------------------*/

static size_t 
write_data2file(void *ptr, size_t size, size_t nmemb, void *stream)
{
  size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
  return written;
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
	if (0 == strcmp(opt_name, option_name_value_list[i].name)) 
	        return option_name_value_list[i].value;
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

	    /* check CURLINFO first */
	char *xstr = normalize_info(op_strbuf);
	int opt_val = lookup_code(xstr);
	if (opt_val > 1){
		PI_SUCCEED;
	} else {
	    *xstr = normalize_opt(op_strbuf);
	    opt_val = lookup_code(xstr);
	    if (opt_val > 1)
		PI_SUCCEED;
	    else
		PI_FAIL;
	}
}

	/* struct in which to save info about incoming CURLINFO equations, for
	   processing after     curl_easy_perform(easyhandle)     call  */
	   
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
    int oplist_t, error_t, head_t, tail_t, arg1_t, arg2_t, result_var_t, i, have_result_var=1; 
    PWord uia_var;
    int uia_var_t;
    int opt_action;
    int ret = 0;
    int downtype = 0; /* 0 = uia, 1 = file target */
    char *filename;
    FILE *pagefile;
    
    struct CurlInfoIn CIIArray[30];
    int ciiCtr = 1;

    char *curlinfo_string;
    long curlinfo_long;
    double curlinfo_double;

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

	/* This should be an uninstantiated var, used for returning error info to prolog */
    PI_getan(&error, &error_t, 2);

    PI_makesym(&nil_sym,&niltype,"[]");
    PI_getan(&oplist, &oplist_t, 1);

    CURL *easyhandle = curl_easy_init();

    struct MemoryStruct chunk;

    struct WriteThis wt;

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

	for (i=1; oplist_t==PI_LIST; i++)
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

		/* ------ First handle any non-CURLOPT cases ------ */
		/* Save any incoming prolog variables for RESULT */

	    if (0==strcmp("RESULT", a1buf)){
		/* save the prolog result var */
		have_result_var = 0;
		result_var = arg2;
		result_var_t = arg2_t;
	    } else 
	    {
		char *nni = normalize_info(a1buf);
		int info_code = lookup_code(nni);
		int info_type = info_code & CURLINFO_TYPEMASK;

		char *nopt = normalize_opt(a1buf);
	        int opt_code = lookup_code(nopt);

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

		} else 
		{

		/* ------ Now handle all the CURLOPT cases ------ */

			/* Do any required option-specific setup -- based on opt_code */
		switch (opt_code) {
		    case CURLOPT_WRITEFUNCTION: 
		    case CURLOPT_WRITEDATA: 
		    {
		        opt_action = CURLOPT_WRITEDATA;

	    	    	if (arg2_t == PI_SYM){
                	   PI_getsymname(a2buf,arg2,BUFSIZE);
	    	    	} else if (arg2_t == PI_UIA ){
                	   PI_getuianame(a2buf,arg2,BUFSIZE);
		    	}

				/* WRITEDATA=true says: result should be a uia */
			if (0==strcmp("true", a2buf))
			{
			    downtype = 0;
        	            chunk.memory = malloc(1);  /* will be grown as needed by the realloc above */
        	            chunk.size = 0;    /* no data at this point */
        	            chunk.memory[0] = '\0';

                                /* send all data to this function  */
        	            curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

                                /* we pass our 'chunk' struct to the callback function */
        	            curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);

			} else 
			{
				/* WRITEDATA=anything else: try to open the anything as a file to take the result */
			    downtype = 1;
    			    filename = a2buf;

                		/* send all data to this function  */
        		    curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, write_data2file);
                		/* open the file */
        	    pagefile = fopen(filename, "wb");
		    if(pagefile) 
		    {
                        	/* write the page body to this file handle */
                	curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, pagefile);
		    }

			}
		        break;
		    }	/* end case CURLOPT_WRITEDATA, both subcases */

		    case CURLOPT_POSTFIELDSIZE: 
		    case CURLOPT_POSTFIELDS: 
	   	    { 
		        opt_action = CURLOPT_POSTFIELDS;

			if (arg2_t==5)
            		    PI_getuianame(a2buf,arg2,BUFSIZE);
        		else
            		    PI_getsymname(a2buf,arg2,BUFSIZE);
        
        		curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDSIZE, (long)strlen(a2buf));
        		curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDS, a2buf);

				/* like WRITEDATA=true: capture result & turn into a uia */

        	        chunk.memory = malloc(1); 	/* will be grown as needed by the realloc above */
        	        chunk.size = 0;    		/* no data at this point */
        	        chunk.memory[0] = '\0';

                                /* send all data to this function  */
        	        curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

                                /* we pass our 'chunk' struct to the callback function */
        	        curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);

			break;
		    }

		    case CURLOPT_READFUNCTION:
		    case CURLOPT_READDATA:
		    {
		        opt_action = CURLOPT_READDATA;

			if (arg2_t==5)
            		    PI_getuianame(a2buf,arg2,BUFSIZE);
        		else
            		    PI_getsymname(a2buf,arg2,BUFSIZE);

			wt.readptr = a2buf;
  			wt.sizeleft = strlen(a2buf);

        		curl_easy_setopt(easyhandle, CURLOPT_POST, 1L);

        		curl_easy_setopt(easyhandle, CURLOPT_READFUNCTION, read_callback);

        		curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDSIZE, (long)wt.sizeleft);

			 	/* pointer to pass to our read function */ 
    			curl_easy_setopt(easyhandle, CURLOPT_READDATA, &wt);

				/* like WRITEDATA=true: capture result & turn into a uia */

        	        chunk.memory = malloc(1); 	/* will be grown as needed by the realloc above */
        	        chunk.size = 0;    		/* no data at this point */
        	        chunk.memory[0] = '\0';

                                /* send all data to this function  */
        	        curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

                                /* we pass our 'chunk' struct to the callback function */
        	        curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);

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
			    case CURLOPTTYPE_STRINGPOINT: 
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

	    if (opt_action == CURLOPT_WRITEDATA)
	    {
		if (downtype == 0)
		{
                       /*
                 	* Now, our chunk.memory points to a memory block that is chunk.size
                 	* bytes big and contains the remote file.
                 	*/
				/* turn the returned chunk.memory into a uia */
                	PI_makeuia(&uia_var, &uia_var_t, (char *)chunk.memory);
	        	if (PI_unify(result_var, result_var_t, uia_var, uia_var_t))
                    	    PI_SUCCEED;
                	else
                    	    PI_FAIL;
		} else /* downtype == 1 */
		{
                        /* close the header file */
		    if(pagefile) {
                	fclose(pagefile);
		    }
        	}	/* end: downtype = 1 */
	    }	/* end: opt_action == CURLOPT_WRITEDATA */

	    else if (opt_action == CURLOPT_POSTFIELDS || opt_action == CURLOPT_READDATA)
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
