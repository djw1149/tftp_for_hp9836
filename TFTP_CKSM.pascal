{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This is the front end for the checksum routine.
}

$search 'TFTP_ASM'$
MODULE TFTP_CKSM;
IMPORT TFTP_DEC, ASMCKSM;

EXPORT
 FUNCTION CkSum(VAR m : MBuf_t; plen : INTEGER) : INTEGER;
 { call with m being an mbuf of data, and plen being the length of the data to 
   be checksummed. 
 }
IMPLEMENT

FUNCTION CkSum;
VAR SUM : INTEGER;
BEGIN
    SUM := docksm(ord(MtoD(m)), plen); { call the assembly checksum routine }
    CkSum := SUM;
END;

END; { Module }
