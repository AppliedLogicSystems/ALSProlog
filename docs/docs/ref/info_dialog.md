---
title: 'info_dialog/[1,2,3]'
package: ALSDev
group: Gui Library
module: alsdev
predicates:
- {sig: 'info_dialog/ [1,2,3]', desc: 'present an information dialog'}
---

## FORMS
```
info_dialog(Msg)

info_dialog(Msg, Title)

info_dialog(Interp, Msg, Title)
```
## DESCRIPTION

These information dialogs present popup dialogs, with information and a single button, to the user. The shorter versions are defined by :
```
info_dialog(Msg) 
    :-
    info_dialog(Msg, 'Info').

info_dialog(Msg, Title) 
    :-
    info_dialog(tcli, Msg, Title).
```

Here `Msg, Title,` and `Interp` are all prolog atoms.

## EXAMPLES

The call  
```
?- info_dialog('Message for the User', 'Dialog Box Title').
```
produces this information dialog:

![](images/info_dialog_box.gif)

## NOTES

The default Tcl interpreter for `info_dialog/[1,2]` is `tcli` as shown above.  This interpreter is *_NOT_* automatically initialized by alsdev.  You must first run [`init_tk_alslib/0` or `init_tk_alslib/1`, or run `init_tk_alslib/2` with `Interp = tcli`](init_tk_alslib.html).  If you wish to run `info_dialog/3` with `Interp` bound to a Tcl interpreter `I` other than `tcli`, you must first run `init_tk_alslib/2` with `Interp` bound to this same `I` (only once is necessary).

## SEE ALSO

- [`init_tk_alslib/[0,1,2]`](init_tk_alslib.html)
