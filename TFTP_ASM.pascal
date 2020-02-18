        mname asmcksm
* by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
* This module implements doing an IP checksum on a data block it is passed.
* docksm(mem, len) must be called with mem being a ord(pointer) to the data
* block, and len being the integer length of the data.  len can be even or odd,
* pointer must be even (this is taken care of by the compiler).  The checksum 
* is returned as an integer.  I assume that len isn't zero on entry.
* The checksum algorithm is the one complement of the ones complement sum of 
* the data.  An all zero checksum should be returned as all ones, but this is
* not implemented (yet).
*

        src module asmcksm;
        src export
        src  FUNCTION docksm(mem, len : INTEGER) : INTEGER;
        src end;
        
        def asmcksm_docksm
        def asmcksm_asmcksm
        
        rorg 0

*
* A dummy initialization procedure.  Must be present.  I think it is called at 
* the start of the program.
*
asmcksm_asmcksm rts (dummy proc)

*
* The real checksum routine
*
asmcksm_docksm link a6,#16
        movem   d0-d3,-16(a6)
mem     equ     12
len     equ     8
retaddr equ     4

        moveq   #0,d2           (d2 is sum)
        moveq   #2,d3           (save some cycles over an addq #2,Dx)
        movea.l mem(a6),a1      (a1 is the pointer to the data block)
        move.l  len(a6),d1      (d1 is the len)
        

while   moveq   #0,d0           (set up to get just a word or byte)
if      cmp.l   #1,d1           (test for an odd last data address)
        ble.s   else
then    move.w  (a1)+,d0        (get the word)
endw    add.l   d0,d2           (end while, do the sum)
        sub.l   d3,d1           (len -= 2)
        bgt     while           (continue while)
        bra.s   adjust          (end the while)

else    move.b  (a1),d0         (get just a byte)
        lsl.w   #8,d0           (move the byte to the right place in the word)
        add.l   d0,d2           (end the while also)

adjust  move.l  d2,d0
        moveq   #16,d1
        asr.l   d1,d0
        move.w  d2,d1
        add.l   d1,d0
        move.l  d0,d2
        
        moveq   #16,d1
        asr.l   d1,d0
        add.l   d0,d2
        
        not.w   d2
        moveq   #0,d0
        movea.w d2,a1
* here's where an all zeros to all ones conversion should go eventually        
        movem   -16(a6),d0-d3
        unlk    a6
        movea.l (sp)+,a0        (save the return address)
        adda.l  #16,sp
        move.l  a1,-(sp)        (push the checksum on the stack)
        jmp     (a0)
        end

