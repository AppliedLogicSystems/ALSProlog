Security Extension: security.psl
================================

The security.psl file provides the check_security/2 and
check_sysinfo_security/1 predicates described below.
Currently the Unikey and Unikey/S hardware keys are supported on
Windows and Solaris.  Hardware serial numbers are supported on
Irix through the check_sysinfo_security/1 predicate.

Please see the Software Security (now Rainbow) documentation
for information about installing the hardware keys.

On Irix, the hardware serial number ca be determined with the
"/sbin/sysinfo -s" command.  Use the TCL script "gen_checksum.tcl"
to generate a checksum, with the following command line:

gen_checksum.tcl <serial-number>



check_security/2                          Hardware key testing
==============================================================

Tests to determine if a hardare key is present.

How to use it:
--------------

check_security(@device, ?result)

Example: check_security('/dev/ttyb', R).

Description: 
------------

check_security(Device, Result)

On Unix, Device is the path name of the device to which the
hardware key is attached.  Device is ignored on other platforms.

Result is unified with an atom that describes the outcome of the
security test.  Result will be unified with one of the following
atoms:

Atom                        Description
-----------------------------------------------------------------
no_error                    Security check was successful.
general_error               A general error occured.
device_name_error           Device is not an atom.
device_permisson_error      Insufficient permission to access
                            or missing Device.
hardware_key_error          Missing or incorrect hardware key.
system_service_error        Hardware key system services are not
                            correctly installed (Windows only).

Example:
-------

allow :- 
      getenv('KEY_DEVICE', Device),
      check_security(Device, Result),
      handle_result(Result).

handle_result(no_error) :- true.
handle_result(hardware_key_error) :-
	write('Incorrect or Missing hardware key!'), nl, exit.
handle_result(_) :- exit.


check_sysinfo_security/1        Hardware serial number testing
==============================================================

Tests to determine if a check-sum matches the hardware serial
number.

How to use it:
--------------

check_sysinfo_security(+checksum)

Example: check_security('23423').

Description: 
------------

check_security(Checksum)

Suceeds if the atom in Checksum matches the checksum for the
hardware serial number.

Example:
-------

allow :- 
      getenv('CHECK_SUM', Sum),
      check_sysinfo_security(Sum).
allow :- exit.
