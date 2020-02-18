{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This module talks to the packet forwarder over the IB interface.
}

MODULE TFTP_RSC;
IMPORT TFTP_DEC, GENERAL_1, IODECLARATIONS, GENERAL_4, HPIB_2, HPIB_3;
{ modules GENERAL_1, IODECLARATIONS, GENERAL_4, HPIB_2 & HPIB_3 are supplied
  by HP.
}
EXPORT
 CONST RSC_TIMEOUT = 100;  { The number of IB NACKs that I'll accept before 
                             resending.  I get an IB NACK when I ask the 
                             packet forwarder for a packet from the host
                             over the ethernet and the packet hasn't come yet.
                             I usually recieve two to three NACKs before a
                             packet comes.  Maybe I should wait a sec. after my
                             first send to reduce the load on the forwarder. }
 
 PROCEDURE RSC_open;
 PROCEDURE RSC_close;
 FUNCTION  RSC_read  (VAR MBuf : MBuf_t; VAR Timer : TimeType) : BOOLEAN;
  { Timer is decremented by 1 each time I get any packet from the forwarder. }
 PROCEDURE RSC_write (VAR MBuf : MBuf_t);
 PROCEDURE RSC_Set_Ip_Addr; { gets ip_var.my_ipaddr from the forwarder }
 
IMPLEMENT

CONST BUFSIZE = 2000; { Fake size }
VAR Buf         : buf_info_type; { an i/o buffer type defined by hp.  Used all
                                   over RSC. }
    wpacket     : Ib_Packet;     { a handy packet }

FUNCTION Mkoffset (ptr: anyptr; num: INTEGER): anyptr;
{
  indexes by num into the block pointed to by ptr.  It is assumed to be a block
  of 1 byte entries (i.e. it doesn't index by object size like C does).
}
TYPE conv_type = RECORD                    { true dirt }
                   CASE INTEGER OF
                     0: (value: INTEGER);
                     1: (address: anyptr);
                 END;
VAR conv: conv_type;
BEGIN
    conv.address := ptr;
    conv.value   := conv.value + num;
    mkoffset     := conv.address;
END;

FUNCTION Ib_Read_trans(count : INTEGER) : BOOLEAN;
{
 Reads data from the IB into Buf.
 returns true if count matches the actual the # of bytes actually read
 else returns false.
}
BEGIN
    buffer_reset(buf);
    transfer_end(His_Ib_Addr, serial_dma, to_memory, buf);
    unlisten(IB_ISC);
    IF (buffer_data(Buf) <> count)
      THEN Ib_Read_trans := false
      ELSE Ib_Read_trans := true;
END;


PROCEDURE RSC_open;
BEGIN
    ioinitialize;
    set_timeout(7, IB_TIMEOUT);   { this doesn't seem to do anything, since
                                    I'll still hang no matter what if the
                                    forwarder isn't being friendly. }
    iobuffer(Buf, 0);             { say that Buf is an i/o buffer of length 0 }
    WITH Buf, wpacket DO
      BEGIN
        buf_size := BUFSIZE;
        rsc_pro  := IB_IPTYPE;
        rsc_type := IB_OPEN;
        rsc_size := 0;

        buf_ptr := addr(wpacket);
        buffer_reset (Buf);
        buf_fill := mkoffset (buf_ptr, IB_HEAD_SIZE);
      END;
    transfer_end(His_Ib_Addr, serial_dma, from_memory, buf);
    unlisten(IB_ISC);
 
    IF (not (Ib_Read_trans(IB_HEAD_SIZE + IB_ACK_SIZE + 2))
        or (wpacket.rsc_type <> IB_ACK))
      THEN BEGIN
        writeln('TFTP: Forwarder is not responding correctly');
        ESCAPE(2);    { if the forwarder is really down we'll never get here
                        because we'll hang.  I don't think this is therefore
                        ever taken. }
      END;
END;

PROCEDURE RSC_close;
BEGIN
    WITH wpacket DO
      BEGIN
        rsc_pro  := IB_IPTYPE;
        rsc_type := IB_CLOSE;
        rsc_size := 0;     { this constant changed from ftp.pascal to tftp.ux }
      END;
    WITH Buf DO
      BEGIN
        buf_ptr  := addr(wpacket);
        buffer_reset (Buf);
        buf_fill := mkoffset (buf_ptr, IB_HEAD_SIZE);
      END;
    
    transfer_end(His_Ib_Addr, serial_dma, from_memory, Buf);
    unlisten(IB_ISC);

    IF (not Ib_Read_trans(6)) { should use the CONST instead of just 6 }
      THEN writeln('Ib_Close: Receive count <> Wanted Count');
END;

FUNCTION RSC_read(VAR MBuf : MBuf_t; VAR Timer : TimeType) : BOOLEAN;
LABEL 999;
VAR count,
    i      : INTEGER;
    packet : ^Ib_Packet;
BEGIN
    RSC_read  := false;
    WITH wpacket do
        BEGIN
            rsc_pro     := IB_IPTYPE;
            rsc_type    := IB_READ;
            rsc_size    := IB_READ_SIZE;
            rsc_data[0] := ETHER_LEN;
        END;
    MBuf.offset := MHEAD + 2;
    packet := MtoD(MBuf);
    

    { now loop until a timeout or an indirect exit from the while }
    WHILE (Timer > 0) DO
      BEGIN
        Timer := Timer - 1;
        WITH Buf DO
          BEGIN
            buf_ptr := addr(wpacket);
            buffer_reset(Buf);
            buf_fill := mkoffset(buf_ptr, IB_READ_SIZE + IB_HEAD_SIZE);
          END;
        transfer_end(His_Ib_Addr, serial_dma, from_memory, Buf);
        unlisten(IB_ISC);
        Buf.buf_ptr := mkoffset(packet,0);
        buffer_reset (Buf);
        transfer_end(His_Ib_Addr, serial_dma, to_memory, Buf);
        unlisten(IB_ISC);
        
        IF ((buffer_data(Buf) < 1) or ((packet^.RSC_type <> IB_ACK)
                                        and (packet^.RSC_type <> IB_NACK)))
          THEN BEGIN
            writeln('RSC_Read: Bad first transfer received');
            writeln(' got result ', packet^.rsc_type:4);
            writeln(' byte count in buffer was ',buffer_data(Buf):4);
            GOTO 999;
          END;
        IF (packet^.RSC_type = IB_ACK)
          THEN BEGIN
          {
           The returned packet^.RSC_size is the # of routed bytes that the
           forwarder would like to transfer to here.  I will take bytes from
           the forwarder in whatever # of transfers it sends them.  I will fail
           if I ever receive zero bytes in a transfer.  I can't take more bytes
           than the MBuf structure can hold.
          }
            count := packet^.RSC_size - buffer_data(Buf) + IB_HEAD_SIZE;
            IF (packet^.RSC_size > MDATA)
              THEN BEGIN
                writeln('RSC_Read: Forwarder wants to send ', packet^.RSC_size,
                        ' bytes, but can only handle ',MDATA,' bytes.');
                ESCAPE(2);
              END;
            MBuf.length := packet^.RSC_size - IB_HEAD_SIZE;
            MBuf.offset := MBuf.offset + IB_HEAD_SIZE;
            WHILE (count > 0) DO
              BEGIN
                transfer_end(His_Ib_Addr, serial_dma, to_memory, buf);
                unlisten(IB_ISC);
                IF (buffer_data(Buf) < 1)
                  THEN BEGIN
                    writeln('RSC_Read: Zero length transfer received');
                    ESCAPE(2);
                  END;
                count := count - buffer_data(Buf);
              END;
            RSC_Read := true;
            goto 999;
          END; { if then }
      END; { while }
999:;
END;

PROCEDURE RSC_write(VAR MBuf : MBuf_t);
VAR i : INTEGER;
    packet : ^Ib_Packet;
BEGIN
    MBuf.offset := MBuf.offset - IB_HEAD_SIZE;
    packet := MtoD(MBuf);
    Buf.buf_ptr := mkoffset(packet,0);
    buffer_reset (Buf);
    WITH packet^ DO
      BEGIN
        rsc_pro  := IB_IPTYPE;
        rsc_type := IB_WRITE;
        rsc_size := MBuf.length;
      END;
    MBuf.length  := MBuf.length + IB_HEAD_SIZE;
    Buf.buf_fill := mkoffset (Buf.buf_ptr, MBuf.length);
    transfer_end(His_Ib_Addr, serial_dma, from_memory, Buf);
    unlisten(IB_ISC);
    Buf.buf_ptr := addr(wpacket);
    buffer_reset (Buf);
    IF (not (Ib_Read_trans(IB_HEAD_SIZE + IB_ACK_SIZE))
        or (wpacket.rsc_type <> IB_ACK))
      THEN BEGIN
        writeln('RSC_write: Forwarder didn''t ACK transfer, giving ',
                 wpacket.rsc_type, ' not ', IB_ACK);
        ESCAPE(2);
      END;
END;


PROCEDURE RSC_Set_Ip_Addr;
{ Get my Ip address from the forwarder and sets Ip_var.my_ipaddr to it. }
{ VAR i : integer; }
BEGIN
    WITH wpacket DO
      BEGIN
        rsc_pro  := IB_IPTYPE;
        rsc_type := IB_ADDR;
        rsc_size := 0;     { this constant changed from ftp.pascal to tftp.ux }
      END;
    WITH Buf DO
      BEGIN
        buf_ptr  := addr(wpacket);
        buffer_reset (Buf);
        buf_fill := mkoffset (buf_ptr, IB_HEAD_SIZE);
      END;
    
    transfer_end(His_Ib_Addr, serial_dma, from_memory, Buf);
    unlisten(IB_ISC);
    
    IF ((not Ib_Read_trans(IB_ADDR_SIZE + IB_HEAD_SIZE))
        or (wpacket.rsc_type <> IB_ACK))
      THEN BEGIN
        writeln('TFTP: Forwarder won''t give its IP address.');
        ESCAPE(2);
      END;
    moveleft(wpacket.RSC_data[0], Ip_Var.my_ipaddr, wpacket.RSC_size); 
{ The typical user wouldn't care about the following:
    write('Received :');
    for i:= 1 to 4 do write(ord(ip_var.my_ipaddr.s_un.s_str[i]):1,'.');
    writeln(#008' as my IP address');
}
END;

END; { Module }


