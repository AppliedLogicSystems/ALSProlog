label .know -text "know("
entry .a -textvariable a
label .comma -text ","
entry .b -textvariable b
label .close -text ")"
pack .know .a .comma .b .close -side left

bind .a <Return> {prolog call user know -atom $a -var b}
bind .b <Return> {prolog call user know -var a -atom $b}

