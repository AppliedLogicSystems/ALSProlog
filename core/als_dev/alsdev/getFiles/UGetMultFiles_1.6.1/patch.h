#pragma once

enum {
	kInstall = 0,
	kRemove = 1
};

typedef pascal void (*PatchRouterProcPtr)(short remove, ListHandle* listaddres);

#if GENERATINGCFM
typedef UniversalProcPtr PatchRouterUPP;
#else
typedef PatchRouterProcPtr PatchRouterUPP;
#endif

enum {
	uppPatchRouterProcInfo = kPascalStackBased
		 | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(short)))
		 | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(ListHandle*)))
};

#if GENERATINGCFM
#define NewPatchRouterProc(userRoutine)		\
		(PatchRouterUPP) NewRoutineDescriptor((ProcPtr)(userRoutine), uppPatchRouterProcInfo, kM68kISA)
#else
#define NewPatchRouterProc(userRoutine)		\
		((PatchRouterUPP) (userRoutine))
#endif

#if GENERATINGCFM
#define CallPatchRouterProc(userRoutine, remove, listaddres)		\
		CallUniversalProc((UniversalProcPtr)(userRoutine), uppPatchRouterProcInfo, (remove), (listaddres))
#else
#define CallPatchRouterProc(userRoutine, remove, listaddres)		\
		(*(userRoutine))((remove), (listaddres))
#endif
