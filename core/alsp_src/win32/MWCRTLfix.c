/* MWCRTLfix.c */

/* ALS: The following is just header info we need. */

#include <windows.h>
#include <crtl.h>
#include <ThreadLocalData.h>
// Declarations from ExceptionX86.h

typedef struct FunctionTableEntry
{
    char *Pc;
    long offsets;
} FunctionTableEntry;

typedef struct ExceptionTableHeader
{
	FunctionTableEntry *First;
	FunctionTableEntry *Last;
	struct ExceptionTableHeader *Next;
} ExceptionTableHeader;

extern void _RegisterExceptionTables(ExceptionTableHeader *header);

extern FunctionTableEntry _PcToActionStart, _PcToActionEnd;

// C++ exception tables

static ExceptionTableHeader ExceptionTables =
{
    &_PcToActionStart,
    &_PcToActionEnd,
    0
};

/* ALS FIX 1: The conversion of the Win32 command line to argc, argv for C
   does not correctly handle quotation marks.  Quotation marks can appear
   explicitly on the command line, or inserted by the Win95 Explorer when
   an application has spaces in its name.
   
   a b c		->	"a", "b", "c"
   "a b" c		->	"a b", "c"
   "a b" c"		->	"a b", "c"
   
   processing of command line arguments is not working properly
   The exception handling asm code at the beginning of
   the CRT startup functions is broken, so ifdef it out. */
   
// Copy of the command line, so we can mangle it.
static char *cmdline;

void _SetupArgsFix();
void _SetupArgsFix()
{
    char *cp;
    
    cp = GetCommandLine();
    
    cmdline = (char *)malloc(strlen(cp)+1);
    strcpy(cmdline, cp);
    
    // first count number of args

	__argc = 0;
	while (*cp)
	{
	    // skip spaces
	    
	    while (*cp == ' ' || *cp == '\t')
	    {
	        cp++;
	    }
	    
	    // see if we have another arg
	    
	    if (*cp)
	    {
	        // count one
	        
	        __argc++;
	        
	        switch (*cp) {
	        case '\'':
	        	cp++;
	        	
		        // and skip non-quotes
		        
		        while (*cp && *cp != '\'')
		        {
		            cp++;
		        }
		        
		        if (*cp) cp++;
				break;
	        case '"':
	        	cp++;
	        	
		        // and skip non-double-quotes
		        
		        while (*cp && *cp != '"')
		        {
		            cp++;
		        }
		        
		        if (*cp) cp++;
				break;
	        default:
		        // and skip non-space
		        
		        while (*cp && *cp != ' ' && *cp != '\t')
		        {
		            cp++;
		        }
		        break;
		    }
	    }
	}    
    
    // allocate a vector for the arguments
    
    __argv = (char **)malloc(__argc * sizeof(char *));
    
    // and fill it in

	__argc = 0;
	cp = cmdline;
	while (*cp)
	{
	    // skip spaces
	    
	    while (*cp == ' ' || *cp == '\t')
	    {
	        cp++;
	    }
	    
	    // see if we have another arg
	    
	    if (*cp)
	    {
	        
	        switch (*cp) {
	        case '\'':
	        	cp++;
				__argv[__argc] = cp;

		        // and skip non-quotes
		        
		        while (*cp && *cp != '\'')
		        {
		            cp++;
		      	}
				break;
	        case '"':
	        	cp++;
				__argv[__argc] = cp;
	        	
		        // and skip non-double-quotes
		        
		        while (*cp && *cp != '"')
		        {
		            cp++;
		        }
				break;
	        default:
				__argv[__argc] = cp;

		        // and skip non-space
		        
		        while (*cp && *cp != ' ' && *cp != '\t')
		        {
		            cp++;
		        }
		        break;
		    }
	        
	        // terminate this arg if it is not alrady the end
	     
	     	if (*cp)
	     	{   
	            *cp = 0;
	            cp++;
	        }
	        
	        // count one
	        
	        __argc++;
	    }
	}    
}


/* ALS FIX 2: The exception handling asm code at the beginning of
   the mainCRTStartup and winMainCRTStartup is broken, so ifdef it out. */


// The main entry point for Win32 console apps.

int main(int, char **);

void mainCRTStartup();
void mainCRTStartup()
{    
#if 0
    // Set up excpetion handler
    
    asm
    {
    		push	ebp
    		mov		eax, -1
    		push	eax
    		push	eax
    		push	OFFSET _MWCHandler
    		push	fs:[0]
    		mov		fs:[0], esp
    }
#endif

    // Register the exception C++ handling tables
    
    _RegisterExceptionTables(&ExceptionTables);
        
    // Perform library initialization
    
    _CRTStartup();
    
    // Set up the command line arguments
    
    _SetupArgs();
    
    // Run the init code (which runs static C++ constructors)
    
    _RunInit();
    
	// Initialize the thread local data structure.
    if (!_InitializeMainThreadData()) {
    	// Initialization failed.
    	exit(-1);
    }

    // Call the main program and exit with the status it returns
    
    exit(main(__argc, __argv));
}

/* ALS FIX 3: In addition to Fix 2, WinMainCRTStartup needs a fix for
   building the command line for WinMain.  Same issue of quotation marks
   as above in __SetupArgsFix. */

// This is the real main entry point for Win32 GUI applications

void WinMainCRTStartup();
void WinMainCRTStartup()
{
    char *CmdPtr;
    STARTUPINFO si;
    int show;

#if 0    
    // Set up excpetion handler
    
    asm
    {
    		
    		push	ebp
    		mov		eax, -1
    		push	eax
    		push	eax
    		push	OFFSET _MWCHandler
    		push	fs:[0]
    		mov		fs:[0], esp
    		
    }
#endif

    // Register the exception C++ handling tables
    
    _RegisterExceptionTables(&ExceptionTables);
        
    // Do C runtime library startup
    
    _CRTStartup();
    
    // Run the init code
    
    _RunInit();
    
    // Setup the globals __argc and __argv.
    _SetupArgs();

	// Initialize the thread local data structure.
    if (!_InitializeMainThreadData()) {
    	// Initialization failed.
    	exit(-1);
    }

    // Get the command line
    CmdPtr = GetCommandLine();
    
    // Skip spaces

    while (*CmdPtr == ' ' || *CmdPtr == '\t')
    {
        CmdPtr++;
    }
 
 	// Skip first argument (the program name)
 	   
    if (*CmdPtr)
    {
        // count one
        
        __argc++;
        
        switch (*CmdPtr) {
        case '\'':
        	CmdPtr++;
        	
	        // and skip non-quotes
	        
	        while (*CmdPtr && *CmdPtr != '\'')
	        {
	            CmdPtr++;
	        }
	        
	        if (*CmdPtr) CmdPtr++;
			break;
        case '"':
        	CmdPtr++;
        	
	        // and skip non-double-quotes
	        
	        while (*CmdPtr && *CmdPtr != '"')
	        {
	            CmdPtr++;
	        }
	        
	        if (*CmdPtr) CmdPtr++;
			break;
        default:
	        // and skip non-space
	        
	        while (*CmdPtr && *CmdPtr != ' ' && *CmdPtr != '\t')
	        {
	            CmdPtr++;
	        }
	        break;
	    }
    }
    
    // Skip spaces
 
     while (*CmdPtr == ' ' || *CmdPtr == '\t')
    {
        CmdPtr++;
    }
   
    // Get the startup info for the show command
    
    GetStartupInfo(&si);
    if (si.dwFlags & STARTF_USESHOWWINDOW)
    {
        show = si.wShowWindow;
    }
    else
    {
        show = SW_SHOWDEFAULT;
    }
    
    // Call the WinMain and exit
    
    exit(WinMain(GetModuleHandle(0), 0, CmdPtr, show));
}


