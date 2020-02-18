MODULE TFTP_TFTP;
{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This is the tftp layer of the program.  It is a little weak on the file kinds
  it allows you to transfer.  It refuses to transfer .TEXT or .ASC files, but
  really should let you do so in binary mode.  More ideally it should know
  about those file formats and convert correctly.  It also should convert tabs
  correctly, though this can be quite hard because of tftp block boundaries 
  being corrupted.
}
IMPORT TFTP_DEC, TFTP_NET;

EXPORT
 PROCEDURE Recvfile(src, des : max_str);
 { receives src into des. }
 PROCEDURE Sendfile(src, des : max_str);
 { sends src to des. }

IMPLEMENT
 
TYPE
     { The kinds of files that I know about }
     F_Types = (f_code, f_text, f_asc, f_data, f_err);

     { A dummy buffer used by Write_Zero_Str() }
     Big_Pac = PACKED ARRAY[1..TFTP_SEG_SIZE] OF CHAR;

     { Flags for Punt(). }
     Tftp_Op_t = (recv_f, send_f);

VAR
    { The file descriptor }
    fd : FILE OF CHAR;

    { The file conversion flag 
         if true then
                   on incoming files, turn \n -> cr
                   on outgoing files, turn cr -> \n
    }
    Trans_cr_to_lf : BOOLEAN; 

PROCEDURE Handle_Bad_IO(iores : INTEGER; filename : max_str; fileop : max_str);
{ Screams at user a message about a failed file operation.  Tries slightly to 
  close the file.  fileop is a string the failed operation.
}
BEGIN
    writeln;
    write('TFTP: Unable to ', fileop);
    IF (filename <> '')
      THEN writeln(' file ', filename)
      ELSE writeln;
    writeln('An ioresult of ', iores : 4, ' was received.');
    writeln;
    writeln('            Transfer aborted');
$IOCHECK OFF$
    close(fd, 'normal');
$IOCHECK ON$
END;

PROCEDURE Write_Zero_Str(ANYVAR str : Big_Pac);
{ Writes out the null terminated string in str. }
VAR i : u_short;
BEGIN
    i := 1;
    WHILE (str[i] <> #000)
      DO BEGIN
        write(str[i]);
        i := i + 1;
      END;
    writeln;
END;


FUNCTION FileType(VAR filename : STRING) : F_Types;
{ Returns the file type of the file called filename.  It determines the
  type of the file by looking at the end of the file name.
}
VAR pos, len : short;
    temp_str : max_str;
BEGIN
    len := strlen(filename);
    { do a character in string search from the RIGHT end of the string }
    pos := len + scan(-len, ='.', filename[len]);
    { pos = (0 if no '.') | (the index of the '.') }
    IF (pos = 0)
      THEN FileType := f_data
      ELSE BEGIN
        temp_str := str(filename, pos + 1, len - pos);
        IF (temp_str = 'CODE')
          THEN FileType := f_code
          ELSE IF (temp_str = 'TEXT')
                 THEN FileType := f_text 
                 ELSE IF (temp_str = 'ASC')
                        THEN FileType := f_asc
                        ELSE FileType := f_data;
      END;
END;


FUNCTION MakeRequest(request: short; VAR MBuf : MBuf_t; fname : max_str):short;
{ Sets up a makerequest tftp block.  Returns the size of the request block }
VAR tp    : ^tftphdr;
    len,
    r     : short;
BEGIN
     tp := MtoD(MBuf);
     tp^.th_opcode := request;
     fname := fname + #000 + Modes[Mode].M_mode + #000;
     len := strlen(fname);
     { Copy over the file name.  There may be a better way to do this, but
       I couldn't find it. }
     for r := 1 to len
       do
         $RANGE OFF$ tp^.th_u.tu_stuff[r] := fname[r]; $RANGE ON$ 
     MakeRequest := len + 2;
END;


PROCEDURE Close_File(how : max_str);
{ Attempts to close the file.  how is the kind of close to do
  (lock, crunch, etc)
}
VAR iores_save : INTEGER;
BEGIN
$IOCHECK OFF$
    close(fd, how);
    iores_save := ioresult;
$IOCHECK ON$
    IF (iores_save <> 0)
      THEN BEGIN
        Handle_Bad_IO(iores_save, '', 'close');
        ESCAPE(1);
      END;
END;


FUNCTION Get_from_Disk(ANYVAR stuff : BIG_PAC; VAR size : short) : BOOLEAN;
{ Reads in data from the source file on a pascal-system disk. returns
  true if unable to read, else returns false.  Size is set to the number of 
  bytes put into stuff.
  Will try to get blocks of TFTP_SEG_SIZE. 
}
LABEL 999;
VAR iores_save : INTEGER;
BEGIN
$IOCHECK OFF$
    size := 0;
    REPEAT
      get(fd);
      IF (not eof(fd))
        THEN BEGIN
          size := size + 1;
          stuff[size] := fd^;    { Must be inside of the iocheck off region. }
          iores_save := ioresult;
          IF (iores_save <> 0)
            THEN BEGIN
              Handle_Bad_IO(iores_save,'', 'read');
              Get_From_Disk := true;
              GOTO 999;
            END;
          IF ((Trans_cr_to_lf) AND (stuff[size] = #M))
            THEN stuff[size] := #J; 
        END;
    UNTIL ((size = TFTP_SEG_SIZE) OR eof(fd));
    Get_From_Disk := false;
999:;
$IOCHECK ON$
END;

FUNCTION Put_to_Disk(ANYVAR stuff : BIG_PAC; size : short) : BOOLEAN;
{ Write the data is stuff to the file on a pascal-system disk.
  returns true if unable to write, else returns false.  Size must be set to
  the number of bytes to write from stuff.}
LABEL 999;
VAR i : short;
    iores_save : INTEGER;
BEGIN
$IOCHECK OFF$
    FOR I := 1 TO size DO
      BEGIN
        IF ((Trans_cr_to_lf) AND (stuff[i] = #J))
          THEN stuff[i] := #M;
        fd^ := stuff[i];
        put(fd);
        iores_save := ioresult;
        IF (iores_save <> 0)
          THEN BEGIN
            Handle_Bad_IO(iores_save,'', 'write');
            Put_to_Disk := true;
            GOTO 999;
          END;
      END;
   Put_to_Disk := false;
999:;
$IOCHECK ON$
END;

PROCEDURE Tftp_Recv(VAR fname : STRING);
{ Receives the file fname from the current host and writes it to the current
  destination file.
}
LABEL 999;
VAR up          : Upcb_Ptr;
    tp          : ^tftphdr;
    block,                  { the current block number (1..inf)             }
    size,                   { the packet size                               }
    n,                      { number of bytes read over the net             }
    t_retries,              { the total number of retries for this transfer }
    retries     : short;    { the number of retries for this packet         }
    err         : BOOLEAN;  { an error flag that is set but not used        }
    MBuf        : MBuf_t;
BEGIN
    block := 0;
    t_retries := 0;
    err := true; 
    up := Net_Open(Sin.sin_addr);
    IF (up = nil)
      THEN BEGIN
        { This should never happen, and this code may be a remanent from 4.2
          Unix C code for this stuff }
        writeln('TFTP: panic- resources exceeded for UDP buffers in net_open');
        ESCAPE(0); { signal a fatal error }
      END;
    Net_Port(up^, Sin.sin_port, 0);
    retries := 0;
    

    REPEAT
      MBuf.offset := MLEN - TFTP_SEG_SIZE - 4;
      tp := MtoD(MBuf);
      IF (block = 0)
        THEN size := MakeRequest(RRQ, MBuf, fname)
        ELSE BEGIN
          tp^.th_opcode := ACK;
          tp^.th_u.tu_block := block;
          size := 4;
        END;
      Net_Write(up^, MBuf, size);
      IF (Trace)
        THEN writeln('Sent packet for block ', block:5, ',', size:3, ' bytes long.');
      { Note that n = 4 is the minimum for an error opcode, and null message. }
      IF (Net_Read(up^, MBuf, n) AND (n > 3))
        THEN BEGIN
          IF (trace) THEN writeln('Net_Read ',n,' bytes');
          tp := MtoD(Mbuf);
          n := n - 4;        { walk over the header length }
          WITH tp^, tp^.th_u
            DO
              IF ((th_opcode = DATA) AND (tu_block = (block + 1)))
                THEN BEGIN
                  retries := 0;
                  IF (Hashes) THEN write('#');
                  IF (Put_to_Disk(th_data, n)) THEN GOTO 999;
                  IF (n <> TFTP_SEG_SIZE)
                    THEN BEGIN
                      err := false;
                      GOTO 999;
                    END;
                  block := block + 1;
                  IF (block = 1)  { Set their port to what they say it is. }
                    THEN Net_Port(up^, up^.peer_port, up^.my_port);
                END
                ELSE IF (th_opcode = ERROR_T)
                       THEN BEGIN
                         write('Error from remote TFTP: ');
                         Write_Zero_Str(th_data);
                         GOTO 999;
                       END;
        END
        ELSE BEGIN
          retries := retries + 1;
          t_retries := t_retries + 1;
          IF (retries >= RexmtVal) OR (t_retries >= MaxTimeOut)
            THEN BEGIN
              writeln('TFTP: Timeout, the host is not responding');
              writeln('Transfer aborted');
              GOTO 999;
            END;
          IF (Hashes) THEN write('.');
        END;
    UNTIL FALSE;

999: MBuf.offset := MLEN - TFTP_SEG_SIZE - 4;
     tp := MtoD(MBuf);
     tp^.th_opcode := ERROR_T;
     tp^.th_u.tu_stuff := #000; { A zero length message ending with a null }
     Net_Write(up^, MBuf, 3);
     Net_Close(up^);
     writeln;
END;

PROCEDURE Tftp_Send(VAR fname : STRING);
{ Send the current source file to the file fname on the current host.
  This implementation of tftp_send maintains two data buffers, one for sending
  data packets and one for reading ACKs.
}  
LABEL 999;
VAR up          : Upcb_Ptr;
    tp          : ^tftphdr;
    block,
    size,
    n,
    t_retries,
    retries     : short;
    resend_data : BOOLEAN;         { Flags if to resend a data packet. }
    MBuf_write,
    MBuf_read   : MBuf_t;
BEGIN
    block := 0;
    t_retries := 0;
    up := Net_Open(Sin.sin_addr);
    resend_data := false;
    IF (up = nil)
      THEN BEGIN
        writeln('TFTP: panic- resources exceeded for UDP buffers in net_open');
        writeln('      Transfer aborted');
        ESCAPE(0);  { signal a fatal error }
      END;
    Net_Port(up^, Sin.sin_port, 0);
    

    REPEAT
      MBuf_write.offset := MLEN - TFTP_SEG_SIZE - 4;
              {Should the length be set also? }
      tp := MtoD(MBuf_write);
      IF (NOT resend_data)
        THEN BEGIN
          IF (block = 0)
            THEN size := MakeRequest(WRQ, MBuf_write, fname)
            ELSE BEGIN
              tp^.th_opcode := DATA;
              tp^.th_u.tu_block := block;
              { Now try to read in from the disk file.
                If there was an error, abort the send. }
              IF (Get_from_Disk(tp^.th_data, size)) THEN GOTO 999;
              size := size + 4;
            END;
          retries := 0;
        END
        ELSE BEGIN
          retries := retries + 1;
          t_retries := t_retries + 1;
          IF (retries >= RexmtVal) OR (t_retries >= MaxTimeOut)
            THEN BEGIN
              writeln('TFTP: Timeout, the host is not responding');
              writeln('Transfer aborted');
              GOTO 999;
            END;
        END;
      Net_Write(up^, MBuf_write, size);
      IF (Trace) THEN writeln('Sent packet for block ',
                               block:3, ', ',(size - 4):3, ' bytes long.');
      IF (Net_Read(up^, MBuf_read, n) AND (n > 0))
        THEN BEGIN
          tp := MtoD(MBuf_read);
          WITH tp^, tp^.th_u DO
            BEGIN
              IF (trace) THEN writeln ('Received packet type ',th_opcode:4,
                                       ' bytes ',(n - 4):4);
              IF ((th_opcode = ACK) AND (tu_block = block))
                THEN BEGIN
                  resend_data := false;  { signal for new data. }
                  IF (Hashes) THEN write('#');
                  block := block + 1;
                END
                ELSE IF (th_opcode = ERROR_T)
                       THEN BEGIN
                         write('Error from remote TFTP: ');
                         Write_Zero_Str(th_data);
                         GOTO 999;
                       END;
            END;
        END
        ELSE BEGIN
          IF (Hashes) THEN write('.');
          resend_data := true;  { resend the data }
        END;
    UNTIL ((size <> TFTP_SEG_SIZE + 4) AND (block > 1) AND not resend_data);

999: Net_Close(up^);
     writeln;
END;

PROCEDURE Open_File(VAR filename : STRING; Operation : Tftp_Op_t);
{ Tries to open the file in filename.  Checks the file type and punts if the
  user wants to transfer an illegal kind of file.  Sets the cr<-->lf conversion
  flag if needed.
}
LABEL 999;
TYPE  FileExt = STRING[10];
VAR   iores_save : INTEGER;

   PROCEDURE Punt(filetype : FileExt);
   BEGIN
       IF (Operation = recv_f)
         THEN BEGIN
           writeln('TFTP: Unable to tranfer to files of type ', filetype,'.');
           writeln('Use a different destination file name.');
           writeln('You may need to (T)ranslate the file using the (F)iler' +
                   ' once it is gotten.');
           ESCAPE(1); { really should abort transfering just that file if doing
                        a multiple file transfer }
         END
         ELSE BEGIN
           writeln('TFTP: Unable to tranfer files of type ', filetype,
                   ' without their being (T)ranslated');
           writeln(' in the (F)iler to a data type file first.');
           ESCAPE(1); { really should abort transfering just that file if doing
                        a multiple file transfer }
         END;
   END;

BEGIN
    CASE FileType(filename) OF
      f_asc  : BEGIN
                 Punt('ASC');  { Can't transfer ascii }
                 GOTO 999;
               END;
      f_text : BEGIN
                 Punt('TEXT'); { Can't transfer text }
                 GOTO 999;
               END;
      f_data : Trans_cr_to_lf := (Mode = 3); { Do conversion.   (wire in!) }
      f_code : Trans_cr_to_lf := false;
      otherwise BEGIN
                  writeln('TFTP: panic- bad file type computed');
                  ESCAPE(0);  { signal a fatal error }
                END;
    END;
$IOCHECK OFF$
    open(fd, filename);
    iores_save := ioresult;
$IOCHECK ON$
    IF (iores_save <> 0)
      THEN BEGIN
        Handle_Bad_IO(iores_save, filename, 'open');
        ESCAPE(1);
      END;
999:;
END;


PROCEDURE Recvfile;
BEGIN
    Open_File(des, recv_f);
    Tftp_Recv(src);
    Close_File('crunch'); { 'crunch' to chop off what was in the file before }
END;

PROCEDURE Sendfile;
BEGIN
    Open_File(src, send_f);
    Tftp_Send(des);
    Close_File('normal'); { 'normal' to save everything in the file }
END;

END; { Module }

