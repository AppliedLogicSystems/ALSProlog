$ ! 
$ ! Makefile for VAX system
$ !
$ !
$ set verify
$ on error then exit
$ !
$ ! build script for the vax system
$ !
$ ! build the C files
$ cc alloc.c
$ cc arith.c
$ cc bdb.c
$ cc bdbg.c
$ cc bcinter.c
$ cc bgv.c
$ cc bio.c
$ cc bmeta.c
$ cc bmisc.c
$ cc bos.c
$ cc bparser.c
$ cc bpckg.c
$ cc bsystem.c
$ cc built.c
$ cc butil.c
$ cc cmp.c
$ cc compile.c
$ cc compmath.c
$ cc disassem.c
$ cc display.c
$ cc dummy.c
$ cc expand.c
$ cc fatal.c
$ cc fileio.c
$ cc foreign.c
$ cc gc.c
$ cc gv.c
$ cc icode1.c
$ cc icode2.c
$ cc index.c
$ cc int.c
$ cc intinit.c
$ cc lexan.c
$ cc lforeign.c
$ cc loadfile.c
$ cc main.c
$ cc mapsym.c
$ cc module.c
$ cc nopckg.c
$ cc paction.c
$ cc parser.c
$ cc pckgcoff.c
$ cc pckgload.c
$ cc pckgmake.c
$ cc pimain.c
$ cc rinfo.c
$ cc sig.c
$ cc symtab.c
$ cc uia.c
$ cc varproc.c
$ cc vprintf.c
$ cc wdisp.c
$ cc wintcode.c
$ cc winter.c
$ !
$ !
$ ! build the macro assembler files
$ !
$ macro atomic
$ macro call
$ macro chpt
$ macro compare
$ macro functor
$ macro ident
$ macro interface
$ macro interrupt
$ macro unify
$ !
$ !
$ ! create the object library
$ !
$ lib/create alspro alloc
$ lib/rep alspro arith
$ lib/rep alspro bdb
$ lib/rep alspro bdbg
$ lib/rep alspro bcinter
$ lib/rep alspro bgv
$ lib/rep alspro bio
$ lib/rep alspro bmeta
$ lib/rep alspro bmisc
$ lib/rep alspro bos
$ lib/rep alspro bparser
$ lib/rep alspro bpckg
$ lib/rep alspro bsystem
$ lib/rep alspro built
$ lib/rep alspro butil
$ lib/rep alspro cmp
$ lib/rep alspro compile
$ lib/rep alspro compmath
$ lib/rep alspro disassem
$ lib/rep alspro display
$ lib/rep alspro dummy
$ lib/rep alspro expand
$ lib/rep alspro fatal
$ lib/rep alspro fileio
$ lib/rep alspro foreign
$ lib/rep alspro gc
$ lib/rep alspro gv
$ lib/rep alspro icode1
$ lib/rep alspro icode2
$ lib/rep alspro index
$ lib/rep alspro int
$ lib/rep alspro intinit
$ lib/rep alspro lexan
$ lib/rep alspro lforeign
$ lib/rep alspro loadfile
$ lib/rep alspro main
$ lib/rep alspro mapsym
$ lib/rep alspro module
$ lib/rep alspro nopckg
$ lib/rep alspro paction
$ lib/rep alspro parser
$ lib/rep alspro pckgcoff
$ lib/rep alspro pckgload
$ lib/rep alspro pckgmake
$ ! DONT put into the library lib/rep alspro pimain
$ lib/rep alspro rinfo
$ lib/rep alspro sig
$ lib/rep alspro symtab
$ lib/rep alspro uia
$ lib/rep alspro varproc
$ lib/rep alspro vprintf
$ lib/rep alspro wdisp
$ lib/rep alspro wintcode
$ lib/rep alspro winter
$ lib/rep alspro atomic
$ lib/rep alspro call
$ lib/rep alspro chpt
$ lib/rep alspro compare
$ lib/rep alspro functor
$ lib/rep alspro ident
$ lib/rep alspro interface
$ lib/rep alspro interrupt
$ lib/rep alspro unify
$ !
$ !
$ ! link the suckers
$ !
$ link/exec=alspro pimain,alspro/lib/include=main
$ !
$ exit
