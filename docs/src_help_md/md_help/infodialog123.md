---
title: 'info_dialog/[1,2,3]'
predicates:
 - 'info_dialog/ [1, 2, 3]' : present an information dialog
---
`info_dialog/ [1, 2, 3]` — present an information dialog


## FORMS

info_dialog(Msg)

info_dialog(Msg, Title)

info_dialog(Interp, Msg, Title)


## DESCRIPTION

These information
dialogs present popup dialogs with information and a single button to the user. The shorter versions are defined by :

info_dialog(Msg) :-

info_dialog(Msg, ' Info ') .

info_dialog(Msg, Title) :-

info_dialog(tcli, Msg, Title) .

Here Msg, Title, and Interp are all prolog atoms.


## EXAMPLES

The call

? -info_dialog(' Message for the User ',

' Dialog Box Title ') .

produces the information dialog :

![](images/info_dialog_box.gif)


