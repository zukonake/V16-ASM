$HWCHNL 0x0001
$WRTCMD 0x0001

.start
mov M:HWCHNL R:0x0000
mov .data_bg R:ITEREG

.loop
adi A@ITEREG M:HWCHNL
igq R:ITEREG .data_end
jmp .loop

.print
adi M:0x0001 M:HWCHNL
ret

.data_bg
0x6548 0x6C6C 0x206F
0x6F57 0x6C72 0x2164
0x000A
.data_end

