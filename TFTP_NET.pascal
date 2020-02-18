{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This is the net layer of tftp.  It hides the Udp and lower layers from
  the TFTP layer.
}

MODULE TFTP_NET;
IMPORT TFTP_DEC, TFTP_UDP, TFTP_IP, TFTP_RSC;

EXPORT
 PROCEDURE Net_Close (VAR up : Upcb);
 FUNCTION  Net_Open  (VAR addr : In_addr) : Upcb_Ptr;
 FUNCTION  Net_Read  (VAR up : Upcb; VAR MBuf : MBuf_t; VAR n : short)
          : BOOLEAN;
 PROCEDURE Net_Write (VAR up : Upcb; VAR MBuf : Mbuf_t; len : short);
 PROCEDURE Net_Port  (VAR up : Upcb; d, s : INTEGER);

IMPLEMENT

PROCEDURE Net_Close;
{ Closes the net connection }
BEGIN
    fillchar(up, sizeof(up), #0);     { zero all the bytes in up. }
    RSC_Close;
END;

FUNCTION Net_Open;
{ Open the net connection }
VAR up : Upcb_Ptr;
BEGIN
    RSC_open;
    up := Udp_Create;
    IF (up <> nil) THEN up^.his_ipaddr := addr;
    Net_Open := up;
END;

FUNCTION Net_Read;
{ Read a block with timeouts.  Calls procedures to check each layer of the
  protocols.  Returns true if it was able to read in a block else it tries
  again, eventually returning false after a timeout.  n is set to the number
  of bytes read in.  If this procedures fails (returns false), then n is set
  to 0.
}
VAR got_good  : BOOLEAN;
    Timer     : TimeType;
BEGIN
    got_good := false;
    Timer    := RSC_TIMEOUT;
    WHILE ((NOT got_good) and (Timer > 0))
      DO IF (RSC_read(MBuf, Timer))     { Returns when SOME packet is read. }
           THEN IF (Ip_Input(MBuf))     { Matches one for us in IP layer?   }
             THEN IF (Udp_Input(MBuf))  { Matches one for us in UDP layer?  }
               THEN BEGIN
                 got_good := true;      { A match! }
                 n := up.count;
                 up.count := 0;		{ no need to bother, but... }
               END;

    Net_Read := got_good;
    IF (NOT got_good) THEN n := 0; { For safety }
END;

PROCEDURE Net_Write;
{ Writes out the block in MBuf that is len bytes long. }
BEGIN
    Udp_Output(up, MBuf, len);
END;

PROCEDURE Net_Port;
{ Sets the port numbers in up. }
BEGIN
    Udp_Port(up, d, s);
END;

END;


