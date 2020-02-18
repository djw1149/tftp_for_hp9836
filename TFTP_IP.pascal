{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This is the IP layer of tftp.
}

MODULE TFTP_IP;
IMPORT TFTP_DEC, TFTP_RSC, TFTP_CKSM;

EXPORT
 FUNCTION  Ip_Input  (VAR Mbuf : MBuf_t) : BOOLEAN;
 PROCEDURE Ip_Output (VAR to_addr : In_Addr; VAR Mbuf : MBuf_t);

IMPLEMENT

FUNCTION Ip_Input;
{ This should ignore a zero checksum }
VAR inp :^ Ip_t;
    n,
    sum : short;
BEGIN
    Ip_Input := false;
    inp := MtoD(Mbuf);
    WITH inp^
      DO
        IF (ip_v = 4)
          THEN
            IF (ip_ttl > 0) { how could a packet get here after expiring ttl? }
              THEN BEGIN
                sum    := ip_sum;
                ip_sum := 0;
                n      := CkSum(Mbuf, 20);
                IF (n = sum)
                  THEN BEGIN
                    IF ((ip_des.S_un.S_addr = Ip_Var.my_ipaddr.S_un.S_addr)
                        AND (ip_p = IPPROTO_UDP))
                      THEN Ip_Input := true
                  END
                  ELSE
                    writeln('Ip checksum error: received =', sum:5,
                            ' but calculated =', n:5);
              END;
END;

PROCEDURE Ip_Output;
VAR inp :^ Ip_t;
BEGIN
    inp := MtoD(Mbuf);
    WITH inp^ DO
      BEGIN
        ip_v    := 4;
        ip_hl   := 5;
        ip_tos  := 0;
        ip_len  := MBuf.length;
        ip_id   := 0;
        ip_off  := 0;
        ip_ttl  := 103;
        ip_sum  := 0;
        ip_sum  := CkSum(MBuf, 20);
        RSC_write(MBuf);
      END;
END;

END; { Module }


