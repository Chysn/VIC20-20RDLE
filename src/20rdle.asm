;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                    20RDLE
;                            (c)2022, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4622
* = $1201
Launcher:   .byte $0b,$10,$2a,$00,$9e,$34,$36,$32
            .byte $32,$00,$00,$00,$00

WORD        = $033c             ; The current puzzle (5 bytes)
MOVE        = $0341             ; The current move (5 bytes)
PACKED      = $0346             ; The packed current move (3 bytes)
WORD_PTR    = $fa               ; Word pointer (2 bytes)
P_RAND      = $0349             ; Pseudorandom seed (2 bytes)
RNDNUM      = $0b               ; Random number tmp
TMP         = $fc               ; Temporary pointer (2 bytes)
CUR_FIRST   = $fe
MOVENUM     = $b0               ; Move number
SCORE       = $b1               ; Evaluation score (5 to win)
POSITION    = $ff               ; Character position
TIMER       = $a1               ; Timer
KBSIZE      = $c6               ;   advancing the assembly address
DISPLAY     = $018b             ; Position of the helper alphabet
CASECT      = $0291             ; Disable Commodore case

; KERNAL and BASIC Routines
PRTSTR      = $cb1e             ; Print to 0, A=low Y=high
CHROUT      = $ffd2             ; Print A to screen
PLOT        = $fff0             ; Get or set screen location
GETIN       = $ffe4             ; Get character from buffer
SCNKEY      = $ff9f             ; Scan keyboard

; Pick a Puzzle Word
Begin:      lda TIMER+1         ; Seed random number generator.
            ora #$01            ; ,,
            sta P_RAND          ; ,,
            lda TIMER           ; ,,
            ora #$80            ; ,,
            sta P_RAND+1        ; ,,
PickWord:   jsr Rand255         ; Get low byte of word number ($00-$2f)
            sta TMP             ; ,,
            jsr Rand63          ; Get high byte of word number ($00-$ff)
            sta TMP+1           ; ,,
            cmp ListSize+1      ; Check the list size high byte
            bcc w_ok            ; If the high byte is lower, the range is good
            bne PickWord        ; If the high byte is higher, need a new value
            lda ListSize        ; ,,
            cmp TMP             ; ,,
            bcs PickWord        ; ,,
w_ok:       lda #0              ; Initialize WORD_PTR
            sta WORD_PTR        ;   ,,
            sta WORD_PTR+1      ;   ,,
            ldy #3              ; Then add the word number (TMP) into WORD_PTR
-loop:      lda WORD_PTR        ;   three times to get the word offset, which
            clc                 ;   may be used to find the first letter.
            adc TMP             ;   ,,
            sta WORD_PTR        ;   ,,
            lda WORD_PTR+1      ;   ,,
            adc TMP+1           ;   ,,
            sta WORD_PTR+1      ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
            lda #<WordList      ; Now add the WordList address to the word
            clc                 ;   pointer offset to get the actual address of
            adc WORD_PTR        ;   the word data.
            sta WORD_PTR        ;   ,,
            lda #>WordList      ;   ,,
            adc WORD_PTR+1      ;   ,,
            sta WORD_PTR+1      ;   ,,
-loop:      ldy #2              ; Get the third byte of the word pointer
            lda (WORD_PTR),y    ; ,,
            cmp #$ff            ; If #$ff, the end of the word list has been
            beq PickWord        ;   reached. Go back for a new random word.
            and #$01            ; If bit 0 is set, it's a common word, and may
            bne found_word      ;   be selected as a puzzle
            jsr IncPtr          ; If not a common word, increment the word
            jmp loop            ;   pointer look at the next word
found_word: sec                 ; Subtract the actual address of WordList from
            lda WORD_PTR        ;   the word pointer to get a word offset in
            sbc #<WordList      ;   TMP. This will be used to find the first
            sta TMP             ;   letter of the word in alphabet offset
            lda WORD_PTR+1      ;   table AlphInd
            sbc #>WordList      ;   ,,   
            sta TMP+1           ;   ,,
            lda #1              ; Start with the letter A
-loop:      asl                 ; Double the index (2 bytes per letter)
            tay                 ; Convert A to index
            lda AlphInd+1,y     ; Has the high byte of the word reached the
            cmp TMP+1           ;   alphabet index yet?
            bcs maybe           ;   If so, it could be done. Check the low byte.
next_ltr:   tya                 ; Otherwise, advance to the next letter
            lsr                 ; ,,
            clc                 ; ,,
            adc #1              ; ,,
            jmp loop            ; And get its index
maybe:      bne found_ltr       ; If the high was greater, the letter was found
            lda AlphInd,y       ; Otherwise, it was equal, so check the low
            cmp TMP             ;   byte
            bcc next_ltr        ; If still too low, get the next letter
found_ltr:  tya                 ; A letter has been found. The index (Lx2)
            lsr                 ;   is moved to A, and halved to get the letter
            sta CUR_FIRST       ;   index, which is stored in CUR_FIRST. A is
            dec CUR_FIRST       ;   1, so decrement the letter
            jsr UnpackWord      ;
            ; Fall through to BoardSetup
            
BoardSetup: lda #$08            ; Set screen color
            sta $900f           ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda #<Intro         ; Show intro banner
            ldy #>Intro         ; ,,
            jsr PRTSTR          ; ,,
            ldx #6              ; Draw six input lines
            stx TMP             ; ,,
-loop:      lda #<InputLine     ; ,,
            ldy #>InputLine     ; ,,
            jsr PRTSTR          ; ,,
            dec TMP             ; ,,
            bne loop            ; ,,
            lda #<Bottom        ; Draw the bottom of the board
            ldy #>Bottom        ; ,,
            jsr PRTSTR          ; ,,
            ldy #1
-loop:      tya                 ; Draw an alphabet on the screen to help
            sta DISPLAY+$1000,y ;   the player out
            lda #3              ;   ,,
            sta DISPLAY+$9400,y ;   ,,
            iny                 ;   ,,
            cpy #27             ;   ,,
            bne loop            ;   ,,
            lda #0              ; Reset move number, position, and move
            sta MOVENUM         ; ,,
            sta POSITION        ; ,,
            sta MOVE+4          ; ,, (only the last byte matters for submit)
            ; Fall through to Main
            
Main:       jsr DrawCursor
debounce:   ldy #$40            ; Debounce the keyboard by waiting for all keys
-loop:      cpy $c5             ;   to be released
            bne loop            ;   ,,
wait:       jsr GETIN           ;   getting a character code as input
            cmp #0              ;   ,,
            beq wait            ;   ,,
            cmp #$0d            ; RETURN, submit move
            beq Submit          ; ,,
            ldy #8              ; Set screen color (recover from invalid word)
            sty $900f           ; ,,
            cmp #$14            ; DEL, remove last character
            beq Backspace       ; ,,
            cmp #133            ; If F1 is pressed, restart game
            bne ch_letters      ; ,,
            jmp Begin           ; ,,
ch_letters: cmp #"A"            ; If <A, do nothing
            bcc debounce        ; ,,
            cmp #"Z"+1          ; if >Z, do nothing
            bcs debounce        ; ,,
add_char:   ldy POSITION        ; Add the pressed letter to the move string
            sta MOVE,y          ; ,,
            pha
            jsr Pos             ; Position the cursor
            lda #18             ; Reverse on
            jsr CHROUT          ; ,,
            pla                 ; Get the pressed key character back and
            jsr CHROUT          ;   output it to the matrix
            lda POSITION        ; Increment the position if not already in
            cmp #04             ;   the final position
            beq debounce        ;   ,,
            inc POSITION        ;   ,,
            jmp Main            ; Going back to main redraws the cursor

; Handle DEL            
Backspace:  lda POSITION        ; If the player is already in the first
            beq to_main         ;   position, just go back
            cmp #4              ; If the player is in the last position, there
            beq bk_last         ;   are a couple different DEL scenarios
            tay                 ; Clear the position (0 = no letter there)
            lda #0              ; ,,
            sta MOVE,y          ; ,,
dec_pos:    dec POSITION        ; Go back to the previous position
to_main:    jmp Main            ; And back for more input
bk_last:    lda MOVE+4          ; If there's no letter here yet, just move
            beq dec_pos         ;   backwards
            lda #0              ; If there is a letter here, clear it, but
            sta MOVE+4          ;   keep the cursor in the last position
            jmp Main            ;   and go back for more input

; Handle RETURN
Submit:     lda POSITION        ; Is the cursor in the last position?
            cmp #4              ; If not, back to Main
            bne to_main         ; ,,
            tay                 ; If there's no letter in the last position,
            lda MOVE,y          ;   go back for more input
            beq to_main         ;   ,,
            jsr Validate        ; Make sure the word is in the word list
            bcs Eval
            lda #15             ; This is an invalid word. Turn the border
            sta $900f           ;   color yellow for a few jiffies
            jmp wait            ; And go back for more input (a new word, etc.)

; Evaluate Move
; Correct letters in correct positions are green
; Correct letters in incorrect positions are yellow            
Eval:       lda #0              ; Reset the score, the number of green positions
            sta SCORE           ; ,,
            ldy #0              ; Y = position
-loop:      lda MOVE,y          ; Remove the letter from the helper display
            and #$3f            ; ,,
            tax                 ; ,,
            lda #" "            ; ,,
            sta DISPLAY+$1000,x ; ,,
            lda MOVE,y          ; Get the move at the position
            cmp WORD,y          ; Is it the same as the word at this position?
            beq Green           ; If so, light it up green
            ldx #4              ; Search for the letter anywhere else
in_puzz:    cmp WORD,x          ;   It's okay to search the same position
            beq Yellow          ;   again because the green check handles it
            dex                 ;   ,,
            bpl in_puzz         ;   ,,
next_pos:   iny                 ; Go to the next position
            cpy #5
            bne loop            ;   and loop, if necessary
            lda SCORE           ; If the player got all five, the game is over
            cmp #5              ; ,,
            beq Winner          ; ,,
            inc MOVENUM         ; Increment the move number. If it's reached the
            lda MOVENUM         ;   seventh move, the game is over       
            cmp #6              ;   ,,
            beq Loser           ;   ,,
            lda #0              ; Reset the position for the next move
            sta POSITION        ; ,,
            jmp Main

; Handle Game Over Stuff
Loser:      ldx #13             ; Show word
            ldy #8              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            lda #28             ; ,, (in red)
            jsr CHROUT          ; ,,
            lda #18             ; ,, (and reverse)
            jsr CHROUT          ; ,,
            ldy #0              ; ,,
-loop:      lda WORD,y          ; ,,
            jsr CHROUT          ; ,,
            iny                 ; ,,
            cpy #5              ; ,,
            bne loop            ; ,,
            lda #10             ; Set screen border to red if lost
            .byte $3c           ; (skip next word)
Winner:     lda #13             ; Set screen border to green if won
            sta $900f           ; ,,
-debounce:  lda $c5             ; Debounce the keyboard, and then wait
            cmp #$40            ;   for any key
            bne debounce        ;   ,,
-wait:      lda $c5             ;   ,,
            cmp #$40            ;   ,,
            beq wait            ;   ,,
            jmp Begin           ; Start the game over

; Handle Indicators            
Green:      inc SCORE           ; Increment the score for a green position
            lda #30             ; ,,
            .byte $3c           ; (Skip the next word)
Yellow:     lda #158            ; ,,
            pha                 ; Save the color
            sty POSITION        ; Save Y
            jsr Pos             ; Position the cursor
            pla                 ; Get back the color
            jsr CHROUT          ; Write the color
            lda #18             ; Reverse on
            jsr CHROUT          ; ,,
            ldy POSITION        ; Write the selected letter
            lda MOVE,y          ; ,,
            jsr CHROUT          ; ,,
            lda #146            ; Reverse off
            jsr CHROUT          ; ,,
            jmp next_pos
                   
; Increment Word Pointer          
IncPtr:     lda #3              ; Each word record takes three bytes, so add
            clc                 ;   3 to the word pointer
            adc WORD_PTR        ;   ,,
            sta WORD_PTR        ;   ,,
            bcc inc_r           ;   ,,
            inc WORD_PTR+1      ;   ,,
inc_r:      rts 

; Pseudo Random
Rand63:     lda #%00000100      ; 6-bit
            .byte $3c
Rand255:    lda #%00000001      ; 8-bit
PRand:      sta RNDNUM
-loop:      lsr P_RAND
            ror P_RAND+1
            bcc shift_rnd
            lda P_RAND
            eor #$aa
            sta P_RAND
            lda P_RAND+1
            eor #$2b
            sta P_RAND+1
shift_rnd:  rol RNDNUM
            bcc loop
            lda RNDNUM
            rts      

; Draw Cursor
DrawCursor: jsr Pos             ; Set position on screen via PLOT
            lda #5              ; Set color to white
            jsr CHROUT          ; ,,
            lda #18             ; Reverse on
            jsr CHROUT          ; ,,
            lda #"*"            ; Asterisk
            jsr CHROUT          ; ,,
            lda POSITION        ; If this isn't the last position, draw a
            cmp #4              ;   cell border to the right
            bcs crsr_r
            lda #146            ; Reverse off
            jsr CHROUT          ; ,,
            lda #$cf            ; Cell border
            jsr CHROUT          ; ,,
crsr_r:     rts            

; Position Character
Pos:        lda POSITION
            clc                 ; Add 8 for left-hand padding
            adc #8              ; ,,
            tay                 ; Transfer POSITION to Y for x position
            lda MOVENUM
            clc                 ; Add 7 for top padding
            adc #7              ; ,,
            tax                 ; Transfer GUESS to X for y position
            clc
            jmp PLOT            ; KERNAL PLOT  

; Validate Move           
Validate:   lda MOVE            ; Convert the first letter into an index to
            and #$3f            ;   the AlphInd offset lookup table
            asl                 ;   ,,
            tax                 ;   ,,
            lda AlphInd,x       ; Put the alphabetic offset into the word
            sta WORD_PTR        ;   pointer
            lda AlphInd+1,x     ;   ,,
            sta WORD_PTR+1      ;   ,,
            lda #<WordList      ; Add the word list address to the alphabetic
            clc                 ;   offset. This is where we'll start looking
            adc WORD_PTR        ;   for the last four letters of the word for
            sta WORD_PTR        ;   validation
            lda #>WordList      ;   ,,
            adc WORD_PTR+1      ;   ,,
            sta WORD_PTR+1      ;   ,,
            jsr PackWord        ; Pack the word into 3 bytes for search
search:     ldy #2              ; Compare packed bytes to word list bytes
-loop:      lda (WORD_PTR),y    ;   against the current word list entry.
            cpy #2              ; The third byte is a special case, because it
            bne no_mask         ;   contains the "common word" bit 0, which
            and #$fe            ;   must be masked off.
no_mask:    cmp PACKED,y        ; If any bytes are wrong, check for end and go
            bne srch_next       ;   to the next entry
            dey
            bpl loop
            sec                 ; Set carry - Word is valid
            rts                 ; ,,
srch_next:  jsr IncPtr          ; Increment the word pointer
            ldy #0              ; If bit 7 is set, either the end of the word
            lda (WORD_PTR),y    ;   list has been reached, or the start of the
            bpl search          ;   next first letter's data has been reached
unfound:    clc
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UNPACKING AND PACKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unpack Word
; Unpack the word at the word pointer, and store it into WORD  
; The word is stored like this
;   Byte 1   1bbbbbcc
;   Byte 2   0cccdddd
;   Byte 3   0deeeee0
UnpackWord: ldx #4              ; Clear out the word storage location
            lda #0              ; ,,
-loop:      sta WORD,x          ; ,,
            dex                 ; ,,
            bpl loop            ; ,,
L1:         lda CUR_FIRST       ; LETTER 1. Decode the first letter to the
            sta WORD            ;   first location directly
            ldy #0              ; Get the next byte of the word pointer
            lda (WORD_PTR),y    ; ,,
            asl                 ; Shift off the word align bit
L2:         ldx #5              ; LETTER 2. Rotate the leftmost 5 bits
-loop:      asl                 ;  into second letter (WORD+1) by using carry
            rol WORD+1          ;   ,,
            dex                 ;   ,,
            bne loop            ;   ,,
L3:         asl                 ; LETTER 3. Rotate the leftmost 2 bits
            rol WORD+2          ;   into third letter (WORD+2)
            asl                 ;   ,,
            rol WORD+2          ;   ,,
            iny                 ;   Get the second packed byte
            lda (WORD_PTR),y    ;   ,,
            asl                 ;   Shift off high 0
            ldx #3              ;   Rotate leftmost 3 bits
-loop:      asl                 ;   ,,
            rol WORD+2          ;   ,,
            dex                 ;   ,,
L4:         bne loop            ;   ,,
            ldx #4              ; LETTER 4. Rotate the leftmost 4 bits
-loop:      asl                 ;   into the fourth letter (WORD+3)
            rol WORD+3          ;   ,,
            dex                 ;   ,,
            bne loop            ;   ,,
            iny                 ;   Get the third packed byte
            lda (WORD_PTR),y    ;   ,,
            asl                 ;   Shift off high 0
            asl                 ;   Rotate the last bit into WORD+3
            rol WORD+3          ;   ,,
L5:         ldx #5              ; LETTER 5. Rotate the five bits into
-loop:      asl                 ;   the fifth letter (WORD+4)
            rol WORD+4          ;   ,,
            dex                 ;   ,,
            bne loop            ;   ,,
unpack_r:   ldx #4              ; Convert the letters to PETSCII
-loop:      lda WORD,x          ; ,,
            ora #$40            ; ,,
            sta WORD,x          ; ,,
            dex                 ; ,,
            bpl loop            ; ,,
            rts                 ; All done

; Pack Word
; Pack the word at the MOVE+1 address, and store it into PACKED  
; The word is stored like this
;   Byte 1   1bbbbbcc
;   Byte 2   0cccdddd
;   Byte 3   0deeeee0
PackWord:   lda #0              ; Clear out all packed bytes. They're going
            ldy #3              ;   to be shifted into by the Carry flag
-loop:      sta PACKED,y        ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            lda MOVE+1          ; LETTER 2. For all letters, we'll AND #$3F to
            jsr prep            ;   make 5-byte values (a=1, z=26)
            ldy #5              ; Shift all five bits of the second letter
-loop:      asl                 ;   into the first packed byte
            rol PACKED          ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
            lda MOVE+2          ; LETTER 3. Move the high two bits into PACKED
            jsr prep            ;   ,,
            asl                 ;   ,,
            rol PACKED          ;   ,,
            asl                 ;   ,,
            rol PACKED          ;   ,,
            ldy #3              ;  The next three bits of Letter 3 go into
-loop:      asl                 ;   PACKED+1
            rol PACKED+1        ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
            lda MOVE+3          ; LETTER 4. Move the high four bits into
            jsr prep            ;   PACKED+1
            ldy #4              ;   ,,
-loop:      asl                 ;   ,,
            rol PACKED+1        ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
            asl                 ; Now bit 0 of Letter 4 goes into PACKED+2
            rol PACKED+2        ; ,,
            lda MOVE+4          ; LETTER 5. All five bits get rotated into
            jsr prep            ;   PACKED+2
            ldy #5              ;   ,,
-loop:      asl                 ;   ,,
            rol PACKED+2        ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
            asl PACKED+2        ; One more bit, the "Common Word" bit
            rts
prep:       and #$3f            ; Convert A to a 5-bit byte (a=1 ~ z=26) and
            asl                 ;   then set them up for carry flag shifting
            asl                 ;   ,,
            asl                 ;   ,,
            rts                 ;   ,,
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Intro:      .asc $93,$0d,$9e,"        20RDLE",$0d,$0d
            .asc "  BEIGE MAZE VIC LAB",$0d,$0d,$0d
            .asc "        ",$05,$af,$af,$af,$af,$af,$af,$00
InputLine:  .asc $05,$0d,"        ",$cf,$cf,$cf,$cf,$cf,18,$20,146,$00
Bottom:     .asc $0d,"        ",18,$20,$20,$20,$20,$20,146,$a9,$00

#include "./src/word_list.asm"