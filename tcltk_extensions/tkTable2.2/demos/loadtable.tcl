# loadtable.tcl
#
# Ensures that the table library extension is loaded

set table(library) Tktable[info sharedlibextension]
if {
    [string match {} [info commands table]]
    && [catch {package require Tktable} err]
    && [catch {load [file join [pwd] .. src $table(library)]} err]
    && [catch {load [file join [pwd] src $table(library)]} err]
} {
    error $err
}
