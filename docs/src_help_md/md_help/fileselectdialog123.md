—-
title: 'file_select_dialog/[1,2,3]'
predicates:
 - 'file_select_dialog/ [1, 2, 3]' : select a file
—-
`file_select_dialog/ [1, 2, 3]` `—` select a file


## FORMS

file_select_dialog(FileName)

file_select_dialog(Options, FileName)

file_select_dialog(Interp, Options, FileName)


## DESCRIPTION

The predicates allow the user to select a file using the native file selection dialogs on each platform. The shorter versions are defined by :

file_select_dialog(FileName)

:-

file_select_dialog(tcli, [title = ' Select File ' ], FileName) .

file_select_dialog(Options, FileName)

:-

file_select_dialog(tcli, Options, FileName) .

For the general call

file_select_dialog(Interp, Options, FileName)

Interp should be an atom naming a Tcl interpreter, Filename should be an uninstantiated variable, and Options should be a list of options as follows :

defaultname = DefaultName
**_[-,default,file,name]
_**
ext = Ext
**_[to,either,add,,,or,use,for,selection]
_**
mode = new/select/save_as
**_[(,default,=,select,)]
_**
initialdir = Initial
**_[dir,in,which,to,begin...]
_**
title = WindowTitle

filetypes = FileTypesList

where FileTypesList is a list of the form

[ [Desc1 [ext1] ], [Desc2, [ext2], ... ]

and the Desci and exti are all atoms.


## EXAMPLES

The call

? - file_select_dialog(File) .

would produce this popup :

![](images/file_sel-1.gif)

The call

? - file_select_dialog(

[title = ' Testing File Selection for Open ',

filetypes = [ [zip, [zip ] ],

[ ' Prolog ', [ ' pro ' ] ],

[ ' All Files ', [ ' * ' ] ] ]

], File) .

would produce

![](alshelp00000022.gif)


