security.psl
============

The security.psl file provides the check_security/2 predicate
described below.  Currently the Unikey and Unikey/S hardware keys
are supported.

In order to use Unikey keys on Windows, you may need to install
drivers/device



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
device_permisson_error      Insufficient permission to access Device.
hardware_key_error          Missing or incorrect hardware key.
system_service_error        Hardware key system services are not
                            correctly installed (Windows only).


