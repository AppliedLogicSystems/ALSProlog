label .know -text "know("
entry .a -textvariable a
label .comma -text ","
entry .b -textvariable b
label .close -text ")"
pack .know .a .comma .b .close -side left

bind .a <Return> {prolog_call [struct know $a [var b]]}
bind .b <Return> {prolog_call [struct know [var a] $b]}
