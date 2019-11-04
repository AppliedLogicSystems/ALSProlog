---
title: 'save_image/2'
predicates:
- {sig: 'save_image/2', desc: 'package an application'}
---

## FORMS

```
save_image(NewImage, Options)
```

## DESCRIPTION

`save_image/2` is called to package up the Prolog's code areas, symbol table, and module information into a single image. Any Prolog library code which the image depends on will not by default be packaged into the image.

`NewImage` should be bound to an atom which represents the pathname to the new image to be created. `Options` is a list of options which control the disposition and startup characteristics for the new image. The forms which may be on an options list to `save_image/2` are:

`start_goal(SGoal)` -- `SGoal` is a goal which is accessible from the user module. This goal is run in place of the current starting goal (usually the Prolog shell) when the application starts up. If the `start_goal(SGoal)` form is not specified in the options list, then the current starting goal is retained as the starting goal for the new image.

`init_goals(IGoal)` -- `IGoal` is either a single goal or a conjunction of goals to be run prior to the starting goal (see above). `IGoal` will be executed after the initialization goals added by previous packages including the ALS Prolog system itself. This form provides a mechanism for performing initializations which the present environment requires and would be required should any packages be built upon the newly saved image.

`libload(Bool)` -- `Bool` is either true or false. If true, the Prolog library is loaded as part of the package. This is necessary since the Prolog library is demand loaded and may not be part of the development environment when the image is saved. If false, the Prolog library is not loaded as part of the package. Once created, however, the library may still be (demand) loaded by the new image, provided the library files are still accessible to the new image. In general, applications which require the Prolog library and will leave the machine on which the development environment exists should specify `Bool` as true. Applications which may need the library but will be run on the same machine as the development environment can specify `Bool` as false if it is necessary to keep the space requirements for the new image as small as possible.

`select_lib(FilesList)` -- `FilesList` is a list library file names from the Prolog library. Each of the listed library files is loaded as part of the package.

The process of creating a new image consists of the following steps:


1. Process the options, changing the starting goal, extending the initialization goals, or forcefully loading the Prolog library as specified by the options.
2. Create a saved code state which is put into a temporary directory. The directory in which this saved code state is put may be influenced by changing the TMPDIR variable.
3. Merge the saved code state and the current Prolog image together into a new image file.

## EXAMPLES

```
?- save_image(hello, [start_goal(printf('Hello world\n',[]))]).
Executing /max4/kev/merge3/M88k/Mot-SVR4/obj/./alsdir/als-mics /max4/kev/merge3/M88k/Mot-SVR4/obj/alspro /var/tmp/aptAAAa000un hello
yes.

?- halt.

max:scratch$ hello
Hello world
```

## NOTES

The `als-mics` program is required to merge the code state with a working ALS Prolog image. If this program does not exist or is inaccessible, an image will not be saved. The place where `als-mics` is searched for can be obtained by entering the following query:

`?- builtins:sys_searchdir(Where).`

`Where` will be bound to the directory where `save_image` expects to find the `als-mics` program.

Global variable values and database assertions dealing with environmental issues should be initialized (or reinitialized) via a goal passed to `init_goals`.

`save_image/2` prints diagnostic messages when something goes wrong. This procedure will eventually be updated to throw errors in a manner compatible with the standard.
