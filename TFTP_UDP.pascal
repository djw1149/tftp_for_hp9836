{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This implements the UDP layer of tftp.
}

MODULE TFTP_UDP;
IMPORT TFTP_DEC, TFTP_CKSM, TFTP_IP;

EXPORT
 FUNCTION  Udp_Create : Upcb_Ptr;
 PROCEDURE Udp_Port   (VAR up : upcb; d, s : INTEGER);
 PROCEDURE Udp_Output (VAR up : upcb; VAR m : MBuf_t; len : INTEGER);
 FUNCTION  Udp_Input  (VAR m : Mbuf_t) : BOOLEAN;


IMPLEMENT

FUNCTION Udp_Create : Upcb_Ptr;
{ Returns a pointer to the first unused Upcb.  If all are used, returns nil. 
  I think that this is only needed in a multi connection tftp daemon. }
LABEL 999;
VAR i : 1..UDP_ARRSIZE;
BEGIN
    FOR i := 1 to UDP_ARRSIZE
      DO IF (NOT Udp_Var[i].active)
           THEN BEGIN
             Udp_Var[i].active := true;
             { Now, dangerously index into the array. }
             Udp_Create := addr(Udp_Var, sizeof(Upcb) * (i - 1));
             GOTO 999; { break }
           END;
    Udp_Create := nil;
999: ;
END;

PROCEDURE Udp_Port(VAR up : upcb; d, s : INTEGER);
{ adjust the port numbers }
BEGIN
    IF (d <> 0) THEN up.his_port := d;
    IF (s <> 0)
      THEN up.my_port := s
      ELSE WITH Ip_Var DO
             BEGIN
               IF (ports < 1024) THEN ports := 1024; { default }
               up.my_port := ports;
               ports := ports + 1;
             END;
END;

PROCEDURE Udp_Output(VAR up : upcb; VAR m : Mbuf_t; len : INTEGER);
VAR u : ^UdpIpHdr;
BEGIN
    m.offset := m.offset - sizeof(UdpIpHdr);
    m.length := sizeof(UdpIpHdr) + len;
    u := MtoD(m);
    WITH u^.ui_i, u^.ui_u DO
      BEGIN
        ih_next  := nil;
        ih_prev  := nil;
        ih_x1    := 0;
        ih_pr    := IPPROTO_UDP;
        ih_len   := len + sizeof(u^.ui_u);
        ih_dst   := up.his_ipaddr;
        ih_src   := Ip_Var.my_ipaddr;
        uh_sport := up.my_port;
        uh_ulen  := ih_len;
        uh_dport := up.his_port;
        uh_sum   := 0;
        uh_sum   := CkSum(m, sizeof(UdpIpHdr) + len);
        Ip_Output(up.his_ipaddr, m);
     END;
END;

FUNCTION Udp_Input(VAR m : Mbuf_t);
{ could be made (a little) faster by getting rid of multiple upcb's }
LABEL 999;
VAR u : ^UdpIpHdr;
    i : 1..UDP_ARRSIZE;
BEGIN
    u := MtoD(m);
    FOR i := 1 to UDP_ARRSIZE
      DO WITH Udp_Var[i], u^.ui_i, u^.ui_u
           DO IF ( (active) AND ((his_ipaddr.S_un.S_addr = 0) OR
                   (his_ipaddr.S_un.S_addr = ih_src.S_un.S_addr))
                   AND (my_port = uh_dport) )
                THEN BEGIN
                  IF (peer_port = 0) THEN peer_port := uh_sport;
                  m.offset  := m.offset + sizeof(UdpIpHdr);
                  m.length  := uh_ulen - sizeof(u^.ui_u);
                  count     := m.length;
                  Udp_Input := true;
                  GOTO 999;  { break }
                END;
    Udp_Input := false;
999:;
END;

END; { module }



