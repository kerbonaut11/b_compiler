format ELF
extrn malloc
extrn printf
globals: dd 0,10,strings+0,strings+6,0
strings: db "hello",0,"b",0,"World",0,0
