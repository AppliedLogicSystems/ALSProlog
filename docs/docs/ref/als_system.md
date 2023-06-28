---
title: 'als_system/1'
predicates:
- {sig: 'als_system/1', desc: 'Provides system environmental information.'}
- {sig: 'sys_env/3', desc: 'Provides brief system environmental information.'}
---

## FORMS

```
als_system(INFO_LIST)

sys_env(OS, OS_Variation, Processor)
```

## DESCRIPTION

Portable programs which interact with the operating system must take account of the variations in the system environment. `als_system/1` provides a method of achieving this goal. When the ALS Prolog system initializes itself, the underlying C substrate asserts a single fact

```
als_system(INFO_LIST)
```

in the module `builtins` in the Prolog database. The argument of this fact is a list of equations of the form

```
property = value
```

Each property appears at most once. The properties and their possible values are listed in the table below.




|**Property Tag**|**Value Examples**|
|-------------|---------------|
| os | unix, dos, macos, mswins32, vms | 
| os_variation |(unix) : solaris2.4 | 
| processor | port_thread, port_byte, i386, m68k, m88k, sparc, powerpc | 
| manufacturer | generic, sun, motorola, dec, | 
| prologVersion | `nnn-mm` | 
| wins | nowins, motif, macos, ... | 


For most purposes, knowing the operating system(OS), and possibly the Processor, is what matters. Consequently, another small fact,

```
sys_env(OS, OS_Variation, Processor)
```

is asserted during initialization, recording the values of the os, the os_variation, and the processor properties from the als_system list description.


## EXAMPLES

On a Sun SPARC running Solaris 2.4, TTY portable version :

```
?- als_system(X).

X = [os = unix, os_variation = solaris2.4, processor = port_thread, 
manufacturer = generic, prologVersion = '1-76 ', wins = nowins ]
```
