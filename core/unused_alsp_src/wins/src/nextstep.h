/*
 * nextstep.h
 */

/*
  Note: /usr/local/lib/gcc-lib/m68k-next-bsd/2.5.8/include/appkit/appkit.h
	and the others internally include the 
	  #ifndef _FOO_H_INCLUDED
  #define _FOO_H_INCLUDED
  ... <real contents of file> ...
  #endif ------ Not _FOO_H_INCLUDED
	construction, so I'm changing the #import's to #includes's --
	-- K.Bowen 3/26/95
		
#import <appkit/appkit.h>
#import <dpsclient/dpsclient.h>
#import <dbkit/dbkit.h>
#import <objc/objc-runtime.h>
*/
#include <appkit/appkit.h>
#include <dpsclient/dpsclient.h>
#include <dbkit/dbkit.h>
#include <objc/objc-runtime.h>
