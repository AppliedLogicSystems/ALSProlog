---
title: About
---

# About ALS Prolog

## Overview

ALS Prolog is a Prolog compiler and development environment,
available for most desktop platforms.  It is a project of [Applied Logic Systems, Inc.](https://applied-logic-systems.com)
, and is an open source version of the original commercial ALS Prolog active in the 1980-90's.
Command-line and GUI versions are available for modern Macintosh, Linux, and Windows systems.


## Brief History

In the early 1980's, K. Bowen and the Logic Programming Research Group at Syracuse University (H. Bacha, K. Buettner, I. Cicekli, K. Hughes, A. Turk) were exploring meta-level extensions to Logic Programming systems, and to Prolog in particular. Desiring a Prolog implementation on which to experiment, they worked co-operatively with a group at Argonne National Laboratories (T. Lindholm, R. Lusk, R. Overbeek), and initially developed a Prolog byte-code interpreter running on a Data General MV/8000, and later on a VAX 780.
They found the system performance disappointing, and in late 1984, set out to develop a portable compiler-based system utilizing a byte-code interpreter for D. Warren's WAM design, implemented over C.  This came on line by fall, 1985.
It contained a resident incremental compiler (from Prolog to WAM), and supported a module system, garbage compaction, and implemented assert/retract using on-the-fly compilation and decompilation techniques, the latter also utilized in a four-port debugger. The system performance was markedly better than the original byte-code interpreter.  It was used as the starting point for two versions of a meta-prolog extension.

A number of the group members decided to explore commercial possibilites in the PC arena and set out to re-implement the Research Group system design, targeting the then-current 16 bit systems, later extended to 32 bits.
Applied Logic Systems, Inc. was formed in 1984 to carry this out.
The initial version, for the IBM PC/DOS, appeared in 1985, and implemented the compiler/WAM byte-code interpreter design from the research group.
This was followed by a version for the Macintosh/MacOS.
A native code compiler optimizing the WAM was developed for the IBM PC/DOS, and partially built for the Macintosh/MacOS.
At the time, the native code compiler for the 386 chip was quite fast, and temporarily won the "naive reverse LIPS (Logical Inferences Per Second)" speed wars.
However, the difficulty of maintaining and porting these native code compilers led to the development of a respectably efficient portable threaded code system which was subsequently used on all platforms.
These PC and Macintosh systems were followed relatively quickly (1987 onward) by versions for Unix workstations (Sun/SunOS/Solaris[m68000, later SPARC], Silcon Graphics/IRIX, HP/HPUX, IBM/AIX), VAX (a new version), NeXT/NeXTStep, and systems utilizing the Motorola 88k chip.
An embeddable logic engine was developed under contract for Apple, but was never released.
Later, versions for Windows were built.
The system has been converted to open source, and now is accessible on GitHub at https://github.com/AppliedLogicSystems/ALSProlog.

## Ephemera

- ["Infer Different." Parody Ad Campaign](/infer-different/)
