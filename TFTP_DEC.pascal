{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This module contains the shared declarations used by tftp (it is the pascal
  version of a header file).  It also contains a little shared code.  It is
  imported by almost every other module.
  There are references to the original C header files that some things came 
  from.
}

MODULE TFTP_DEC;
EXPORT

{****************************************************************************}

{ types.h }
{ There may be some sign extension problems as C supports genuine unsigned math
  and a greater dynamic range for a given bit count than unpacked pascal vars.
}
CONST MAX_STR_LEN = 255;                    { maximum length of a string }
      MAX_short   = 32767;
      MIN_short   = -32768;
TYPE  short       = MIN_short..MAX_short;
      long        = INTEGER;
      u_short     = 0..MAX_short;  { note unpacked 0..65535 is 32 bits long }
      u_long      = 0..MAXINT;
      u_char      = CHAR;
      max_str     = STRING[MAX_STR_LEN];
      byte        = 0..255;        { unpacked takes 16 bits }

{****************************************************************************}
     
{ declarations defining the parser command table }
CONST NumCommands = 13;        {Must be set to the number of commands.}
TYPE Cmd = RECORD
               Name : STRING[10];
               Help : STRING[80];
               Handler : PROCEDURE;
            END;
     CmdTableType = ARRAY [1..NumCommands] OF Cmd;
VAR  CmdTable : CmdTableType;

{ global status vars }
CONST TIMEOUT   = 5; { retries between rexmt's by default }

TYPE TimeType   = u_short;

VAR Hashes, 
    Trace, 
    Verbose,
    Connected,
    UdpCkSum    : BOOLEAN; {The status vars}
    RexmtVal,
    MaxTimeOut  : TimeType;

CONST MODECNT = 3;      { The number of file transfer modes implemented. }
TYPE  ModeStrType = STRING[32];
      MR =  RECORD
              M_name, M_mode : ModeStrType;
              M_help : STRING[60];
            END;
      ModeArrayType = ARRAY[1..MODECNT] OF MR;
CONST ModeArray
         = ModeArrayType[
              MR[M_name:'binary',
                 M_mode:'netascii',
                 M_help:'code files'],
              MR[M_name:'raw_data',
                 M_mode:'netascii',
                 M_help:'binary type data, no cr/lf conversions are done'],
{ must be 3 } MR[M_name:'converted_data',
                 M_mode:'netascii',
                 M_help:'text and ascii data, cr<-->lf conversions   are done']
             ];
VAR   Modes : ModeArrayType;
      Mode  : 1..MODECNT;

{****************************************************************************}
{
 TFTP.H 4.2 --> Pascal 2.1  1/10/85 Trivial File Transfer Protocol (IEN-133)
}
CONST TFTP_SEG_SIZE = 512; { data segment size }

{
  Packet types.
}
CONST RRQ      = 1; { read request }
      WRQ      = 2; { write request }
      DATA     = 3; { data packet }
      ACK      = 4; { acknowledgment }
      ERROR_T  = 5; { error code }
      
TYPE th_u_type = PACKED RECORD CASE INTEGER OF
                   1: (tu_block : short);
                   2: (tu_code  : short);
                   3: (tu_stuff : PACKED ARRAY[1..1] OF CHAR);
                 END;
       tftphdr = PACKED RECORD
                  th_opcode : short;
                  th_u      : th_u_type;
                  th_data   : PACKED ARRAY[1..TFTP_SEG_SIZE] OF CHAR;
                 END;

{
***  **** Not Used: ****  ****
 Error codes.
}
    CONST ENDEF       = 0; { not defined }
          ENOTFOUND   = 1; { file not found }
          EACCESS     = 2; { access violation }
          ENOSPACE    = 3; { disk full or allocation exceeded }
          EBADOP      = 4; { illegal TFTP operation }
          EBADID      = 5; { unknown transfer ID }
          EEXISTS     = 6; { file already exists }
          ENOUSER     = 7; { no such user }

{
***  **** Not Used: ****  ****

  * CONST N_TFTP_ERR  = 8;

  * TYPE  Tftp_Err_mes_t = ARRAY[0..N_TFTP_ERR - 1] OF STRING[34];
  * CONST TFTP_ERR_MES_C = Tftp_Err_mes_t['Not defined',
                                          'File not found',
                                          'Access violation',
                                          'Disk full or allocation exceeded',
                                          'Illegal TFTP operation', 
                                          'Unknown transfer ID',
                                          'File already exists',
                                          'No such user'];
  * VAR   Tftp_Err_Mes   : Tftp_Err_Mes_t;
  **** End Not Used ****
}
{****************************************************************************}

{netdb.h}
{ some host table entry information.  more in TFTP_HOST }
CONST H_MAX_NAME_LEN = 40;
TYPE  HostStrType = STRING[H_MAX_NAME_LEN]; { This is an assumption }
      IpAddrStr   = PACKED ARRAY[1..4] OF CHAR;
      HostEnt = RECORD
                  h_name      : HostStrType;     { offical name of host }
                  h_addr      : IpAddrStr;       { address }
                END;
      HostEntPtr = ^HostEnt;
VAR   The_Host : HostEntPtr;

{****************************************************************************}

{ in.h }
TYPE In_addr = 
       PACKED RECORD   { Internet Address (old style.. should be updated. }
         S_un : RECORD CASE INTEGER OF
                  1: (S_un_b : PACKED RECORD s_b1, s_b2, s_b3, s_b4 : char END);
                  2: (S_un_w : PACKED RECORD s_w1, s_w2 : short END);
                  3: (S_addr : long);
                  4: (S_str  : IpAddrStr);  { added by djw }
                END;
       END;
     
     SockAddr_In = RECORD
                     sin_family : short;
                     sin_port   : u_short;
                     sin_addr   : In_addr;
                   END;
VAR Sin : SockAddr_In;
CONST AF_INET = 2;      { for internet            }
      IPPROTO_UDP = 17; { user datagram protocol  }
      IPPORT_TFTP = 69; { the defined tftp socket }
{****************************************************************************}

{ ip.h as per RFC 791 Sep 81 }

TYPE Ipcb = RECORD
              my_ipaddr : In_addr;
              ports     : INTEGER;
            END;
VAR Ip_Var : Ipcb;

CONST MAXTTL = 255; { Maximum time to live for a packet. }
TYPE Ip_t = PACKED RECORD
              ip_v      : 0..15;        { Ip protocol version }
              ip_hl     : 0..15;        { header length in 32 bit words }
              ip_tos    : byte;         { type of service }
              ip_len    : short;        { total length in octets }
              ip_id     : short;        { identification }
              ip_off    : short;        { fragment offset field }
              ip_ttl    : byte;         { time to live (seconds) }
              ip_p      : byte;         { protocol }
              ip_sum    : short;        { checksum }
              ip_src    : In_addr;      { source address }
              ip_des    : In_addr;      { destination address }
            END;
{****************************************************************************}

{ udp.h 6.1 }

TYPE Upcb = RECORD
              his_ipaddr : In_addr;
              my_port,
              his_port,
              peer_port  : short; 
              active     : BOOLEAN;
              count      : INTEGER;    { may not be needed }
              ip         : ^Ipcb;
            END;
     Upcb_Ptr = ^Upcb;
CONST UDP_ARRSIZE = 4;
VAR Udp_Var : ARRAY[1..UDP_ARRSIZE] OF Upcb;
              { From /usr/router/ombyte/bmon/global.h }
              
TYPE IpOvly = PACKED RECORD
                ih_next,
                ih_prev   : ^CHAR;   { For protocol sequence q's (used now) }
                ih_x1     : byte;    { unused }
                ih_pr     : byte;    { protocol }
                ih_len    : short;   { protocol length }
                ih_src    : In_addr; { source internet address }
                ih_dst    : In_addr; { destination internet address }
              END;
     UdpHdr = PACKED RECORD
                uh_sport  : short;     { source port }
                uh_dport  : short;     { destination port }
                uh_ulen   : short;     { udp length }
                uh_sum    : short;     { udp checksum }
              END;
     UdpIpHdr = PACKED RECORD
                  ui_i : IpOvly;     { overlaid Ip structure }
                  ui_u : UdpHdr;     { udp header }
                END;
                
                
{****************************************************************************}
{ ib.h  }

{ ib call values }
CONST IB_ETHER      = 0;
      IB_ETHERTYPE  = 1;
      IB_IPTYPE     = 2;
      IB_TESTTYPE   = 3;    { never used here }
      
{ ib function values }
      IB_NACK       = -1;
      IB_ACK        = 0;
      IB_OPEN       = 1;
      IB_CLOSE      = 2;
      IB_READ       = 3;
      IB_WRITE      = 4;
      IB_SEEK       = 5;
      IB_IOCTL      = 6;
      IB_SIGNAL     = 7;
      IB_ADDR       = 8;
      
{ set values for the count field of an ib_header }
      IB_OPEN_SIZE  = 6;
      IB_CLOSE_SIZE = 2;
      IB_READ_SIZE  = 4;
      IB_WRITE_SIZE = 2;
      IB_ACK_SIZE   = 2;
      IB_HEAD_SIZE  = 6;
      IB_ADDR_SIZE  = 4;
      IB_MAXCOUNT   = (794 * 2); { what is this based on? }
      
      ETHER_LEN     = 1500; { why do I care? }
      
      IB_ISC        = 7;  { the IB bus interface code }
      His_Ib_Addr   = IB_ISC * 100 + 5;  { Wire-in!!! }
      IB_TIMEOUT    = 2.00;  { The real number of seconds to wait for IB i/o }

TYPE OverLay_t = PACKED ARRAY[0..749] OF short;
     Ib_Packet = PACKED RECORD
                   rsc_pro,
                   rsc_type,
                   rsc_size  : short;
                   rsc_data  : OverLay_t;
                 END;
     

{****************************************************************************}
{ mbuf.h }
{ This defines the heavily used mbuf structure.  Through lots of trashy code,
  all kinds of headers and stuff will go on this.
}
CONST MLEN  = 1024;
      MHEAD = 2 * sizeof({u_**}short);
      MDATA = MLEN - MHEAD;
TYPE  MBuf_t = RECORD
                 offset,              
                 length  : 0..MLEN;
                 mdata   : PACKED ARRAY[0..MLEN - 1 - 2 * sizeof(u_short)]
                             OF CHAR;
               END;
TYPE bluff = PACKED array[1..1000] of -128..127;

FUNCTION MtoD(VAR MBuf : MBuf_t) : ANYPTR;

PROCEDURE print_buf (ANYVAR buf : bluff; j : INTEGER);

{****************************************************************************}

IMPLEMENT

FUNCTION MtoD;
BEGIN
   { #define mtod(m,t) ((t) ((int)(m) + (m)->offset)) }
   MtoD := addr(MBuf, MBuf.offset);
END;

PROCEDURE print_buf (ANYVAR buf : bluff; j : INTEGER);
{ just used for debugging.  prints out j bytes of buf.  remember to 
  dereference pointers to blocks when calling this.
}
VAR n : INTEGER;
BEGIN
    writeln('size = ',j, ' address of data is ', ord(addr(buf)));
    FOR n := 1 TO j DO
      BEGIN
        WRITE (output, buf[n] : 4);
        IF buf[n] in [32..127] THEN WRITE (output, CHR(buf[n]) : 2)
                ELSE WRITE(OUTPUT,' _');
        IF n mod 10 = 0
          THEN WRITELN (output)
          ELSE WRITE (output, ' ');
      END;
    WRITELN;
END;

END; { Module }

