
bldit(CorePath, SVIm)
	:- 
	split_path(CorePath, CPL),
	append(CPL,[als_dev,alsdev,'demo15.pro'], Demo15List), 
	join_path(Demo15List, Demo15),
		simple_load(Demo15),
	append(CPL,[als_dev,alsdev,'serial_cmn.pro'], SerialList), 
	join_path(SerialList, Serial),
		simple_load(Serial),
	abolish(bldit,2),
	call(SVIm).


module builtins.
dvf.
endmod.
