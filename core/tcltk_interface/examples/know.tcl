label .know -text "know("
entry .a -textvariable a
label .comma -text ","
entry .b -textvariable b
label .close -text ")"
pack .know .a .comma .b .close -side left

bind .a <Return> {prolog "user:know($a, B)." b}
bind .b <Return> {prolog "user:know(A, $b)." a}
