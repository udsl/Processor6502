clr	; clear all lthe existing data

; Simple code loop should run from reset
; Code is at 512 imadiatly after stack space
; start address is placed at the reset vector.

def reset $FFFC
orig 512

start:
LDX #$ff
TXS        ;Initialise stack pointer
LDX #$08
decrement:
  DEX
  BNE decrement
brk
byt 100 ; dont break excution

orig 600
brkcall:
brk ; break excution, got to single step mode
byt 0
LDX #$99
STX $20

orig reset
addr start,brkcall


