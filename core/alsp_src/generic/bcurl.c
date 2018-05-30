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

/* #include <curl/curl.h> must preceed #include "defs.h" */
#include <curl/curl.h>
#include "defs.h"

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

#define name2value(symbol) { ##symbol, symbol }

/* needs to be corrected & fully filled in 
static struct {char *name; long value;} option_name_value_list[] = {
	name2value("CURLOPT_URL"),  // This expands to { "CURLOPT_URL", CURLOPT_URL }
	name2value("CURLOPT_HTTPGET")
};
*/

/* -------------------------------------------------------------------------------
 | Based on https://curl.haxx.se/libcurl/c/url2file.html 
 * ------------------------------------------------------------------------------*/

static size_t 
write_data2file(void *ptr, size_t size, size_t nmemb, void *stream)
{
  size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
  return written;
}

static PWord nil_sym;
int niltype;

/*
function normalize_opt(option_str) {
   if (option_str doesn't start with "CURLOPT_") {
        option_str = "CURLOPT_" + uppercase(option_str)
  }
  return option_str
}
*/

/* adjust_opt/2 in blt_curl.pro guarantees that option_str is all upper case */

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

/* -------------------------------------------------------------------------------
	curl_c_builtin(void)
 * ------------------------------------------------------------------------------*/

int
curl_c_builtin(void)
{
    PWord oplist, error, head, tail, arg1, arg2;
    int oplist_t, error_t, head_t, tail_t, arg1_t, arg2_t, i, arity;
    PWord unb_var, response_code, prc, uia_var;
		/* if there is an RC=YY in the options list, prc_t gets set to 0 in 
		   	} else if (0 == strcmp(a1buf, "RC")){  
		   so that after curl_easy_perform, prc,prc_t is unified with the status */
    int unb_var_t, response_code_t, prc_t = -1, uia_var_t;
    int ret = 0;
    int mode=0;
    char *urltgt;

	/*
	 mode==1: [uia=VAR] GET with result wrapped up as UIA & bound to result/result_t
	 mode==2: [filename=Filename] GET with result read into local file 
	 mode==3: POST via postfields [postfields='a=b&...']
	 mode==4: POST via readcallback 
	 */

        /* In windows, this will init the winsock stuff */
    ret = curl_global_init(CURL_GLOBAL_ALL);
    if(ret != CURLE_OK) {
        fprintf(stderr, "curl_global_init() failed: %s\n", curl_easy_strerror(ret));
        FAIL;
    }

    PI_getan(&error, &error_t, 2);
    PI_makesym(&nil_sym,&niltype,"[]");
    PI_getan(&oplist, &oplist_t, 1);

    CURL *easyhandle = curl_easy_init();

    struct MemoryStruct chunk;

        /* set "standard" common options 
    ret += curl_easy_setopt(easyhandle, CURLOPT_MAXREDIRS, 50L);
    ret += curl_easy_setopt(easyhandle, CURLOPT_TCP_KEEPALIVE, 1L);
    ret += curl_easy_setopt(easyhandle, CURLOPT_USERAGENT, "curl/7.54.0");
	*/

        /* Process the options list: */
    if(oplist_t != PI_LIST) {
        fprintf(stderr, "curl_intf() arg #5 not a list\n");
        FAIL;
    }
    else if(oplist_t==PI_SYM && oplist==nil_sym) {
        /* nothing to do: list = [] */
    }
    else { 	/* optlist has at least one element */
                /* process options on the list */
	char a1buf[BUFSIZE];
	char a2buf[BUFSIZE];
	char lbuf[BUFSIZE];
	PWord functor;
	for (i=1; oplist_t==PI_LIST; i++){
	    PI_gethead(&head,&head_t,oplist);
	    PI_gettail(&tail,&tail_t,oplist);
		/* adjust_opt/2 in blt_curl.pro guarantees that head is an eqn: */

		/*skip************** adjust_opt/2 in blt_curl.pro guarantees that head is an eqn: 
		printf("List element #%d: headtype=%d\n",i,head_t);
		            PI_getstruct(&functor,&arity,head);
		            PI_getsymname(lbuf,functor,BUFSIZE);
		            int ss = strcmp(lbuf, "=");
		            if (ss != 0) 
			    {
		//                printf("functor is NOT equality\n");
		//                FAIL;
			        char perrmsg[120];
		   	        sprintf(perrmsg, "option functor %s not equality\n", lbuf);
		                PI_makeuia(&uia_var, &uia_var_t, perrmsg);
			        if (w_unify(error, error_t, uia_var, uia_var_t))
		                    SUCCEED;
		                else
		                    FAIL;
		            }
		* ss == 0: functor is equality 
		//printf("functor is equality\n");
		**************/

	    PI_getargn(&arg1, &arg1_t, head, 1);
//printf("arg1_t=%d\n",arg1_t);
            PI_getargn(&arg2, &arg2_t, head, 2);
//printf("arg2_t=%d\n",arg2_t);

	    if (arg1_t == PI_SYM){
                PI_getsymname(a1buf,arg1,BUFSIZE);
	    } else if (arg1_t == PI_UIA ){
                PI_getuianame(a1buf,arg1,BUFSIZE);
	    } else {
		/* error */
	    }
printf("arg1_t=%d a1=%s arg2_t=%d \n",arg1_t,a1buf,arg2_t);

		/* handle the special cases first before the normalize case */
	    if (0 == strcmp(a1buf, "UIA")){
		/* GET with result wrapped up as UIA & 
		   bound to var passed in as result/result_t */
		mode = 1;
		    /* save the incoming unbound variable to unify with the uia later: */
		unb_var = arg2;
		unb_var_t = arg2_t;
                PI_getuianame(a2buf,arg2,BUFSIZE);
		urltgt = a2buf;

        	chunk.memory = malloc(1);  /* will be grown as needed by the realloc above */
        	chunk.size = 0;    /* no data at this point */
        	chunk.memory[0] = '\0';

                    /* send all data to this function  */
        	curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

                /* we pass our 'chunk' struct to the callback function */
        	curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);

	    } else if (0 == strcmp(a1buf, "FILENAME")){
	    } else if (0 == strcmp(a1buf, "POSTFIELDS")){
	    } else if (0 == strcmp(a1buf, "POSTDATA")){
	    } else if (0 == strcmp(a1buf, "RC")){
		prc = arg2;
		prc_t = arg2_t;
	    } else {	/* normalize-the-option approach */
		
		    /* memory was malloc'd for the result here: */
		char *nopt = normalize_opt(a1buf);
printf("nopt = %s  a1buf=%s\n", nopt, a1buf);

		/* TOTAL TEST HACK -- ONLY HANDLES CURLOPT_URL */
	    if (arg2_t == PI_SYM){
                PI_getsymname(a2buf,arg2,BUFSIZE);
	    } else if (arg2_t == PI_UIA ){
                PI_getuianame(a2buf,arg2,BUFSIZE);
	    } else {
		/* error */
	    }

		if (0 == strcmp("CURLOPT_URL", nopt)){
		{
			long code = CURLOPT_URL;
			curl_easy_setopt(easyhandle, code, a2buf);
		}
	    }
	}

		/* Finished this list element; go on to the next */
            oplist = tail;
            oplist_t = tail_t;

	}   /* end option_list for-loop processing all the options */

		/* try to perform the GET/POST/WHATEVER...*/
        ret = curl_easy_perform(easyhandle);

	               /* check for errors */
        if(ret != CURLE_OK) {
	    char perrmsg[120];
   	    sprintf(perrmsg, "curl_easy_perform() failed: %s\n", curl_easy_strerror(ret));
            PI_makeuia(&uia_var, &uia_var_t, perrmsg);
	    if (w_unify(error, error_t, uia_var, uia_var_t))
                SUCCEED;
            else
                FAIL;

        } else { 	/* curl_easy_perform succeeded */
		/* set up to return the http response status code */
            curl_easy_getinfo(easyhandle, CURLINFO_RESPONSE_CODE, &response_code);

	    /* Need to have incoming variable for response code to unify prc,prc_t against: */
	    if (prc_t == 0){
                if(!w_unify(prc, prc_t, response_code, WTP_INTEGER)) {
                    FAIL;
	        }
            }

	    if (mode == 1) 
	    {
                /*
                 * Now, our chunk.memory points to a memory block that is chunk.size
                 * bytes big and contains the remote file.
                 */

//printf("RESULT: %s\n",chunk.memory);
			/* turn the returned chunk.memory into a uia */
                PI_makeuia(&uia_var, &uia_var_t, (char *)chunk.memory);
	        if (w_unify(unb_var, unb_var_t, uia_var, uia_var_t))
                    SUCCEED;
                else
                    FAIL;
            } else if (mode == 2) 
	    {

            } else if (mode == 3) 
	    {

            } else if (mode == 4) 
	    {
	    } else {
		/* error, but shouldn't happen */
	    }
        }
	/* shouldn't get here: SUCCEED OR FAIL SHOULD OCCUPY EVERY PATH */
return 1;
    }    /* end }else{ for non-empty options list processing */

}
