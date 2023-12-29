; Zero page values start at $20
orig $20
;
; FROM = zero page pointer start
FROM:	addr $2000
;   TO = zero page pointer destination
TO:	addr	$3000

SIZE: wrd 257 ; the word containing the number of bytes to move
 ; the word containing the number of bytes to move
def SIZEH SIZE; SIZEH = high byte of word bytes to move
def SIZEL SIZE+1 ; SIZEL = low byte of word bytes to move

orig $200
; Move memory down page at a time
MOVEDOWN: LDY #0
         LDX SIZEH
         BEQ MD2
MD1:     LDA (FROM),Y ; move a page at a time
         STA (TO),Y
         INY
         BNE MD1
         INC FROM+1
         INC TO+1
         DEX
         BNE MD1
MD2:     LDX SIZEL
         BEQ MD4
MD3:     LDA (FROM),Y ; move the remaining bytes
         STA (TO),Y
         INY
         DEX
         BNE MD3
MD4:     RTS

; Move memory up page at a time
MOVEUP:  LDX SIZEH    ; the last byte must be moved first
         CLC          ; start at the final pages of FROM and TO
         TXA
         ADC FROM+1
         STA FROM+1
         CLC
         TXA
         ADC TO+1
         STA TO+1
         INX          ; allows the use of BNE after the DEX below
         LDY SIZEL
         BEQ MU3
         DEY          ; move bytes on the last page first
         BEQ MU2
MU1:     LDA (FROM),Y
         STA (TO),Y
         DEY
         BNE MU1
MU2:     LDA (FROM),Y ; handle Y = 0 separately
         STA (TO),Y
MU3:     DEY
         DEC FROM+1   ; move the next page (if any)
         DEC TO+1
         DEX
         BNE MU1
         RTS

; Move memory up using ZP pointers
;
; FROM = 1 + source end address
; TO   = 1 + destination end address
; SIZE = number of bytes to move
;
MOVEUPA: LDY #$FF
         LDX SIZEH
         BEQ MU3A
MU1A:    DEC FROM+1
         DEC TO+1
MU2A:    LDA (FROM),Y ; move a page at a time
         STA (TO),Y
         DEY
         BNE MU2A
         LDA (FROM),Y ; handle Y = 0 separately
         STA (TO),Y
         DEY
         DEX
         BNE MU1A
MU3A:    LDX SIZEL
         BEQ MU5A
         DEC FROM+1
         DEC TO+1
MU4A:    LDA (FROM),Y ; move the remaining bytes
         STA (TO),Y
         DEY
         DEX
         BNE MU4A
MU5A:    RTS

; Move memory up using ZP pointers
;
; FROM = source end address
; TO   = destination end address
; SIZE = number of bytes to move
;
MOVEUPB: LDY #0
         LDX SIZEH
         BEQ MU3B
MU1B:    LDA (FROM),Y ; handle Y = 0 separately
         STA (TO),Y
         DEY
         DEC FROM+1
         DEC TO+1
MU2B:    LDA (FROM),Y ; move a page at a time
         STA (TO),Y
         DEY
         BNE MU2B
         DEX
         BNE MU1B
MU3B:    LDX SIZEL
         BEQ MU5B
         LDA (FROM),Y ; handle Y = 0 separately
         STA (TO),Y
         DEY
         DEX
         BEQ MU5B
         DEC FROM+1
         DEC TO+1
MU4B:    LDA (FROM),Y ; move the remaining bytes
         STA (TO),Y
         DEY
         DEX
         BNE MU4B
MU5B:    RTS

TEXTBLOCK:
		 TXT 'A block of text so the move methods can be tested.'
		 TXT 'NOTE new line of text but no CR'
		 BYT 13
		 TXT 'Above is a charage return you can also add CR/LF like this:'
         BYT 13,10		 