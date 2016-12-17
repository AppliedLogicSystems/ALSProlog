
%--- crefStructs defStruct ---

module cref.
use utilities.

defStruct(crfCRF, [
        propertiesList = [
		pred,
		arity,
		mod,
		files,
		exported,
		calls,
		basis,
		calledby,
		calledcount,
		factcount,
		clausecount,
		depthcount,
		callinmod,
		importto,
		dependson,
		whereasserted
	],
	accessPred =    accessCRF,
        setPred =       setCRF,
        makePred =      makeCRF,
        structLabel =   crfCRF
    ]).

defStruct(crfCRF, [
        propertiesList = [
		files,
		files_mods,
		files_d_preds,
		files_c_preds,
		mods,
		mods_files,
		mods_use,
		mods_d_preds,
		mods_c_preds,
		mods_exp_preds,
		mods_imp_preds
	],
	accessPred =    accessMI,
        setPred =       setMI,
        makePred =      makeMI,
        structLabel =   crfMI
    ]).



endmod.

