REM   Builds and initializes the distribution directory, such as:
REM
REM			i386_mswin32-N.NN
REM
REM	where:		$1 = alsp_src directory and $2 = N.NN 
REM	Assumes:
REM	A. Running in the main dist dir: als_dist
REM	B. Running on a system with ALS Prolog packaged (e.g. skolem)
REM	C. The paths are as follows:
REM
REM		path to main build dir			= ../builds
REM		path to main source dir			= ../alsp_src
REM		path to main windows source dir = ../windows
REM

alspro -b %1\bin\mkdstdir.pro -g mkdstdir -p -ver %2
