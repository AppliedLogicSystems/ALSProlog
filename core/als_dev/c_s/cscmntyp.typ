/*-------------------------------------------------------------*
 |                   cscmntyp.type
 |
 |		Common stuff for client & server:
 |		- login info
 *-------------------------------------------------------------*/


module socket_comms.

defStruct(login_info, [
		propertiesList = [
			user_name,
			password ],
		accessPred = access_login_info,
		setPred =    set_login_info,
		makePred =   make_login_info,
		structLabel = login_info
	] ).

endmod.
