clr	; clear all lthe existing data

; Simple code loop should run from reset
; Code is at 512 imadiatly after stack space
; start address is placed at the reset vector.

def reset $FFFC
orig 512

start:
LDX #$08
decrement:
  DEX
  BNE decrement


orig reset
addr start


