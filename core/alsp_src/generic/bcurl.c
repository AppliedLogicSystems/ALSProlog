/*=======================================================================*
 |			bcurl.c
 |		Copyright (c) 2018 Applied Logic Systems, Inc.
 |
 |		-- "Direct to curl" prolog interface
 |
 | Author: Ken Bowen
 | "Direct to curl" suggested by Chuck Houpt
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
	curl_intf(void)

    PI_getan(&mode, &mode_t, 1);  -- incoming:
       mode==0: GET with result wrapped up as UIA & bound to result/result_t
       mode==1: GET with result read into local file (passed in arg#5)
       mode==2: POST via postfields  (passed in arg#5)
       mode==3: POST via readcallback   (not yet completed: 5/24/18)
	
    PI_getan(&url, &url_t, 2);	  -- incoming
    PI_getan(&result, &result_t, 3);	-- outgoing
    PI_getan(&prc, &prc_t, 4);		-- outgoing (HTTP response status code)

    PI_getan(&pro_filename, &pro_filename_t, 5); -- incoming (mode 1)
    PI_getan(&fields, &fields_t, 5);             -- incoming (mode 2)

	TO BE HANDLED:

	Use arg#6 for handling error messages back to prolog

	Use arg#7 for processing a generalized list of options, perhaps
	mixed prolog-style and C-style:
		[userpwd='...', writedata=X]
		[CURLOPT_USERPWD'='...', 'CURLOPT_WRITEDATA'=X]
	Use prolog-style for options that might be moderately often used,
	and C-style for the rest.  Map the prolog-style to C with a
	C-side table like:
	    static struct {char *name, int value} option_name_value_list[] = {
		{"url", CURLOPT_URL},
		{"httpget", CURLOPT_HTTPGET},

 * ------------------------------------------------------------------------------*/

int
curl_intf(void)
{
    PWord mode, url, result, prc, val, pro_filename, fields;
    int   mode_t, url_t, result_t, prc_t, val_t, pro_filename_t, fields_t;
    char urlbuf[BUFSIZE];
    UCHAR *filename;
    FILE *pagefile;
    char postbuf[BUFSIZE];
    int ret = 0;
    long response_code;

	/* In windows, this will init the winsock stuff */ 
    curl_global_init(CURL_GLOBAL_ALL);
	/* CONVERT TO THROWING ERROR TO PROLOG: */
    if(ret != CURLE_OK) {
        fprintf(stderr, "curl_global_init() failed: %s\n",
              curl_easy_strerror(ret));
        FAIL;
    }

    PI_getan(&mode, &mode_t, 1);
    PI_getan(&url, &url_t, 2);
    if (url_t==5)
    	PI_getuianame(urlbuf,url,BUFSIZE);
    else
    	PI_getsymname(urlbuf,url,BUFSIZE);

    PI_getan(&result, &result_t, 3);
    PI_getan(&prc, &prc_t, 4);

printf(">>curl_intf: result_t=%d prc_t=%d \n",result_t, prc_t);
printf(">>curl_intf: mode_t=%d mode=%ld url_t=%d url=%s\n",mode_t,mode,url_t,urlbuf);

    CURL *easyhandle = curl_easy_init();

	/* set "standard" common options */
    ret += curl_easy_setopt(easyhandle, CURLOPT_NOPROGRESS, 1L);
    ret += curl_easy_setopt(easyhandle, CURLOPT_MAXREDIRS, 50L);
    ret += curl_easy_setopt(easyhandle, CURLOPT_TCP_KEEPALIVE, 1L);
    ret += curl_easy_setopt(easyhandle, CURLOPT_USERAGENT, "curl/7.54.0");

    ret += curl_easy_setopt(easyhandle, CURLOPT_URL, urlbuf);

//printf("set_CURLOPT_URL=%d urlbuf=%s\n",ret,urlbuf);

    if (mode==0) {
	/* GET with result wrapped up as UIA & bound to var passed in as result/result_t */

	struct MemoryStruct chunk;
	chunk.memory = malloc(1);  /* will be grown as needed by the realloc above */
	chunk.size = 0;    /* no data at this point */
	chunk.memory[0] = '\0';

		/* send all data to this function  */
	curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

		/* we pass our 'chunk' struct to the callback function */
	curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);
		/* try to perform the GET */
	ret = curl_easy_perform(easyhandle);
//printf("mode_0:curl_easy_perform-ret=%d\n",ret);

	    	/* check for errors 
		   :::: MUST BE CONVERTED TO SENDING ERRORS TO PROLOG*/
	if(ret != CURLE_OK) {
	    fprintf(stderr, "curl_easy_perform() failed: %s\n",
			curl_easy_strerror(ret));
	} else {
		/*
		 * Now, our chunk.memory points to a memory block that is chunk.size
		 * bytes big and contains the remote file.
		 */

            curl_easy_getinfo(easyhandle, CURLINFO_RESPONSE_CODE, &response_code);
    	    if(!w_unify(prc, prc_t, response_code, WTP_INTEGER)) {
		    FAIL;
    	    }
	    PI_makeuia(&val, &val_t, (char *)chunk.memory);
	    if (w_unify(result, result_t, val, val_t))
                SUCCEED;
	    else
                FAIL;
	}

    } else if (mode==1) {
	/* GET with result read into local file */
	/* var passed in as result/result_t is left unbound */

    	PI_getan(&pro_filename, &pro_filename_t, 5); 
	if (!getstring(&filename, pro_filename, pro_filename_t))
            FAIL;

printf(">>curl_intf mode_1: pro_filename_t=%d filename=%s\n",pro_filename_t,filename);

	        /* send all data to this function  */
	curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, write_data2file);
    
        	/* open the file */
    	pagefile = fopen(filename, "wb");
    	if(pagefile) {
                	/* write the page body to this file handle */
        	curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, pagefile);
        
                	/* get the remote file & write to local file */ 
        	ret = curl_easy_perform(easyhandle);

		if(ret != CURLE_OK) {
	    	    fprintf(stderr, "curl_easy_perform() failed: %s\n",
				curl_easy_strerror(ret));
	    	    FAIL;
		} else {
            	    curl_easy_getinfo(easyhandle, CURLINFO_RESPONSE_CODE, &response_code);
    	    	    if(!w_unify(prc, prc_t, response_code, WTP_INTEGER)) {
		    	FAIL;
    	    	    }
	    	}
                	/* close the header file */
        	fclose(pagefile);
	}     

    } else if (mode==2) {
	/* POST via postfields */
	/* result from server is wrapped as a UIA (a' la mode 0) and bound to
	   the var passed in as result/result_t */

    	PI_getan(&fields, &fields_t, 5); 

    	if (url_t==5)
    	    PI_getuianame(postbuf,fields,BUFSIZE);
    	else
    	    PI_getsymname(postbuf,fields,BUFSIZE);

	curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDSIZE, (long)strlen(postbuf));
	curl_easy_setopt(easyhandle, CURLOPT_POSTFIELDS, postbuf);

	struct MemoryStruct chunk;
	chunk.memory = malloc(1);  /* will be grown as needed by the realloc above */
	chunk.size = 0;    /* no data at this point */
	chunk.memory[0] = '\0';

		/* send all data to this function  */
	curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

		/* we pass our 'chunk' struct to the callback function */
	curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, (void *)&chunk);

	ret = curl_easy_perform(easyhandle);

        	/* Check for errors -- CONVERT TO THROW ERROR TO PROLOG*/
    	if(ret != CURLE_OK) {
      	    fprintf(stderr, "curl_easy_perform() failed: %s\n",
             		curl_easy_strerror(ret));
	} else {
	    PI_makeuia(&val, &val_t, (char *)chunk.memory);
            curl_easy_getinfo(easyhandle, CURLINFO_RESPONSE_CODE, &response_code);

	    if (w_unify(result, result_t, val, val_t) && w_unify(prc, prc_t, response_code, WTP_INTEGER))
                SUCCEED;
	    else
                FAIL;
	}

    } else {
	/* make it fail below */
	ret=1;
    }
    curl_easy_cleanup(easyhandle);
    curl_global_cleanup();

    if (ret==0) {
        SUCCEED;
    } else {
        FAIL;
    }
}


/*
        curl_easy_setopt(handle, CURLOPT_VERBOSE, longoptval);
*/
