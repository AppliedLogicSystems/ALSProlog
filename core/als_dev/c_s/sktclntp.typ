/*======================================================================*
 | 			sktclntp.typ 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |
 |		Minimal client information structure type defintion
 |		-- includes minimal slots:
 |
 | Author: Ken Bowen
 | Started: November, 1995
 *======================================================================*/

module sampserv.

    %% For the client side :
defStruct(flipStructs,
  	[propertiesList = [
  	 in_stream/user_input,       %%
  	 out_stream/user_output,     %%
  	 error_stream/user_output,   %%
  	 rem_read_stream,
  	 rem_write_stream,
  	 counter/0,
  	 history/[]                  %%
  	],
 		accessPred = access<APP>,
		setPred =    set<APP>,
		makePred =   make<APP>,
		structLabel = crf<APP>
	] ).

endmod.
