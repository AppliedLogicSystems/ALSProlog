#ifdef DEMO
#include <string.h>
#include "tcl.h"
#include "tk.h"

extern void (*demo_error)(const char *s);
extern void (*get_demo_key)(char *key, int max);

Tcl_Interp *demo_interp;

static void alsdev_demo_error(const char *s)
{

	Tcl_VarEval(demo_interp, "tk_messageBox -type ok -message \"", s, "\"", NULL);
	Tcl_Exit(0);
}

static char *get_key_script = "\
set w .get_demo_key\n\
set key {}\n\
\n\
toplevel $w\n\
\n\
label $w.text -justify left -text {ALS Prolog Evaluation System\n\
\n\
This evaluation copy requires a serial number key in order to run.\n\
If you do not have a key, please contact Applied Logic Systems to\n\
receive an evaluation key.\n\
\n\
Web    http://www.als.com\n\
E-Mail info@als.com\n\
Voice  +1 (617) 497-0100\n\
Fax    +1 (617) 497-3963\n\
Mail   Applied Logic Systems, Inc.\n\
       P.O. Box 175\n\
       Cambridge, MA 02140\n\
       USA\n\
}\n\
label $w.label -text \"Demo Key:\"\n\
entry $w.key -textvar key\n\
button $w.button -text \"OK\" -command \"destroy $w\"\n\
pack $w.text -padx 2m\n\
pack $w.label $w.key $w.button -side left -padx 2m -pady 2m\n\
\n\
tkwait window $w\n\
\n\
return $key\n\
";

static void alsdev_get_demo_key(char *key, int max)
{
	int r;
	
	r = Tcl_Eval(demo_interp, get_key_script);
	if (r == TCL_OK) {
		strncpy(key, Tcl_GetStringResult(demo_interp), max);
		key[max-1] = 0; 
	}
}

void setup_alsdev_demo(void);
void setup_alsdev_demo(void)
{
	demo_error = alsdev_demo_error;
	get_demo_key = alsdev_get_demo_key;
	
	demo_interp = Tcl_CreateInterp();
	Tcl_Init(demo_interp);
	Tk_Init(demo_interp);
	Tcl_Eval(demo_interp, "wm withdraw .");
}

void shutdown_alsdev_demo(void);
void shutdown_alsdev_demo(void)
{
	Tcl_DeleteInterp(demo_interp);
}

#endif