$! set terminal characteristics
$!
$ set terminal/insert
$ set prompt = "TRON :"
$!
$! set up command aliases
$!
$ cd :== set default
$ pwd :== show default
$ ls :== dir
$ ll :== dir/size/date/prot
$ alias :== define
$ ss :== show symbol
$ rm :== delete/confirm
$ cp :== copy/confirm
$ mv :== rename/confirm
$ cat :== type
$ m :== type/page
$!
$! define logical names for directories
$!
$ define home sys$login
$!
$! cc libraries
$ define lnk$library sys$library:vaxcrtl.olb
$
$ alspro :== $$users:[kev.new]alspro.exe
