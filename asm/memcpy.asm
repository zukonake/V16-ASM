$SRCREG 0x0001
$DSTREG 0x0002
$SIZREG 0x0003

mov M:0x0000 R:ITEREG ; iterator = 0
.loop
cpy A@SRCREG A@DSTREG ; copy from source to destination and increment iterators
igq B:ITEREG R:SIZREG ; if ++iterator >= size; return
jmp .loop
ret

