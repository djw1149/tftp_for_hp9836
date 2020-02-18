$PARTIAL_EVAL ON$ { this will really die horribly if PARTIAL_EVAL is off }

MODULE TFTP_HOST;
{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  Knows about host tables and will translate for you from a name to its
  Ip address.  Also given an ascii Ip address returns the decimal equivelent.
  Could be adapted to get names from a name server.
  This likes a host table to exist, but can live without one if you know
  ascii Ip addresses.
  This should allow tabs in the host table.

 Danger: Side effects are predominant in this code.  (I wanted to see what
         it was like, and to make the code faster.)  Don't reorder boolean
         expressions without careful thought.
         Must be compiled with $PARTIAL_EVAL ON$.
}
IMPORT TFTP_DEC;

EXPORT

FUNCTION GetHostByName(Host : max_str) : HostEntPtr;
{ Host may not contain more than 253 characters (but can have a max len = 255)}

FUNCTION INet_Addr(in_string : max_str) : IpAddrStr;
{ Converts an ascii ip address in in_string to a 4 byte ip address returned in
  IpAddrStr.  Returns BadIpAddr if in_string doesn't represent a valid ipaddr.}

CONST BadIpAddr = #255#255#255#255;      { The error value returned by
                                           INet_Addr.  A long integer = -1 }


IMPLEMENT

CONST HostFileName = '*HOSTS';                { Wired-In!!!   }
VAR   HostFile : TEXT;                        { the host file }
    
FUNCTION INet_Addr { (in_string : max_str) : IpAddrStr };

TYPE Trick = RECORD CASE BOOLEAN OF     { Used for pascal type casts. }
               FALSE : (i : INTEGER);
               TRUE  : (c : PACKED ARRAY[1..4] OF CHAR);
             END;
VAR val        : Trick;
    part_count : short;
    parts      : ARRAY[1..4] OF Trick;
    ok         : BOOLEAN;


FUNCTION Get_Num(VAR val : INTEGER) : BOOLEAN;
{ Returns true if a valid number is found in front of in_string, else
  it returns false.  val is set to the valid number.  in_string must not
  be empty when this is called.  The number is deleted from in_string.
  valid number formats: octal has a leading 0
                        hex has a leading [0]x or [0]X
                        decimal is the default
}
VAR per_pos      : short;
    last_pos     : INTEGER;
    num_is_octal : BOOLEAN;
    temp_str     : max_str; { temp_str is set = the first number in in_string }

BEGIN
    Get_Num := true;
    per_pos := pos('.', in_string) - 1;
    IF (per_pos = -1)
      THEN BEGIN
        per_pos := strlen(in_string);
        temp_str := str(in_string, 1, per_pos);
        delete(in_string, 1, per_pos);
      END
      ELSE BEGIN
        temp_str := str(in_string, 1, per_pos);
        delete(in_string, 1, per_pos + 1); { delete + 1 to kill the '.' also}
      END;
    IF (strlen(temp_str) = 0)
      THEN Get_Num := false
      ELSE BEGIN
        num_is_octal := false;
        IF (temp_str[1] = '0')                      { Is the number octal? }
          THEN BEGIN
            num_is_octal := true;
            delete(temp_str, 1, 1); { delete the 0 }
          END;
        IF (strlen(temp_str) = 0)
          THEN val := 0
          ELSE IF ((temp_str[1] = 'x') OR (temp_str[1] = 'X'))
                 { Is the number hex? }
                 THEN BEGIN
                   delete(temp_str, 1, 1); { delete the x or X }
                   IF (strlen(temp_str) = 0)
                     { Nothing coming after the x or X is invalid. }
                     THEN Get_Num := false
                     ELSE TRY val := hex(temp_str);
                          RECOVER IF (ESCAPECODE = -8)
                                    { The number wasn't a valid hex number }
                                    { escapecode -8 is a value range error }
                                    THEN Get_Num := false
                                    ELSE ESCAPE(ESCAPECODE);
                 END
                 ELSE IF (num_is_octal)
                       THEN TRY val := octal (temp_str);
                            RECOVER IF (ESCAPECODE = -8)
                                      THEN Get_Num := false
                                      ELSE ESCAPE(ESCAPECODE)
                       ELSE BEGIN  { The number is decimal }
                         TRY strread(temp_str, 1, last_pos, val);
                         RECOVER IF ((ESCAPECODE = -10) OR (ESCAPECODE = -8))
                                   { escapecode -10 is an i/o error }
                                   THEN Get_Num := false
                                   ELSE ESCAPE(ESCAPECODE);
                         { Now check for cases like temp_str = '123!!@$@#$@#',
                           which wouldn't cause an exception in strread.  Make
                           sure we strread to the end of temp_str. }
                         IF (last_pos <> (strlen(temp_str) + 1))
                           THEN Get_Num := false;
                       END;
      END;
END;

BEGIN { INet_Addr }
    part_count := 1;
    ok := true;
    WHILE ((strlen(in_string) <> 0) AND ok)
      DO BEGIN
        ok := ((part_count <> 5) AND Get_Num(parts[part_count].i));
        part_count := part_count + 1;
      END;
    part_count := part_count - 1; { Adjust for the extra add. }
    IF ((NOT ok) OR (part_count = 0))
      THEN INet_Addr := BadIpAddr
      ELSE BEGIN
        CASE (part_count) OF
          { a -- 32 bits }
          1 : val.i := parts[1].i;             { a }
          { a.b 8.24 bits }
          2 : BEGIN
                val.i := parts[2].i;           { b }
                val.c[1] := parts[1].c[4];     { a }
              END;
          { a.b.c -- 8.8.16 bits }
          3 : BEGIN
                val.i := parts[3].i;           { c }
                val.c[1] := parts[1].c[4];     { a }
                val.c[2] := parts[2].c[4];     { b }
              END;
          { a.b.c.d -- 8.8.8.8 bits }
          4 : BEGIN
                val.c[1] := parts[1].c[4];     { a }
                val.c[2] := parts[2].c[4];     { b }
                val.c[3] := parts[3].c[4];     { c }
                val.c[4] := parts[4].c[4];     { d }
              END;
          OTHERWISE writeln('INet_Addr: panic- part_count =',part_count);
        END;
        INet_Addr := val.c;
      END;
END;

PROCEDURE HandleError(iores : short);
BEGIN
    writeln('The ioresult was:',iores);
    writeln('Please tell the lab assistant to check the disk, and tell him/her the ioresult');
    writeln;
END;


FUNCTION SetHostEnt : BOOLEAN;
VAR iores_save : short;
BEGIN
$IOCHECK OFF$
    reset(HostFile, HostFileName);
    iores_save := ioresult;
$IOCHECK ON$
    IF (iores_save <> 0)
      THEN BEGIN
        writeln;
        writeln('Unable to open host file ',HostFileName, '.');
        writeln('TFTP will only be able to parse IP addresses, not names.');
        writeln('If you don''t know what an IP address is, ask the lab assistant for help.');
        HandleError(iores_save);
        SetHostEnt := false;
      END
      ELSE SetHostEnt := true;
END;

PROCEDURE EndHostEnt;
VAR iores_save : short;
BEGIN
$IOCHECK OFF$
    close(HostFile, 'NORMAL');
    iores_save := ioresult;
$IOCHECK ON$
    IF (iores_save <> 0)
      THEN BEGIN
        writeln;
        writeln('Unable to close host file ',HostFileName, '.');
        HandleError(iores_save);
      END;
END;


FUNCTION GetHostByName { (Host : max_str) : HostEntPtr };
VAR p           : HostEntPtr;
    found_entry : BOOLEAN;
    Line        : max_str;

FUNCTION GetHostEnt : BOOLEAN;
VAR Raw_h_addr   : max_str;
    found_line   : BOOLEAN;
{ Sample entries:  (tabs not allowed)
10.1.0.14       cmu-cs-a.arpa cmu-cs-a cmu-10a cmua csa cmucsa cmu10a
128.2.251.3     cmu-ee-maxwell.arpa cmu-ee-maxwell maxwell eem
}

PROCEDURE ReadInLine;
VAR iores_save : short;
BEGIN
$IOCHECK OFF$
    readln(HostFile, Line);
    iores_save := ioresult;
$IOCHECK ON$
    IF (iores_save <> 0)
      THEN BEGIN
        writeln;
        writeln('An error occured while reading host file ',
                 HostFileName, '.');
        HandleError(iores_save);
        halt;
      END;
END;

PROCEDURE BreakUpLine;
VAR i, spcpos : short;
    state     : (read_nothing, read_addr, read_enough);
    temp_str  : max_str;

PROCEDURE Break_Error;
BEGIN
    writeln('An error was found in host table format.');
    writeln('As best as possible, the error will be ignored.');
    writeln('The host table should be fixed, please tell the lab assistant.');
    state := read_enough;
END;

BEGIN
    state := read_nothing;
    WITH p^ DO
      BEGIN
        WHILE ( NOT((strlen(Line) = 0) OR (state = read_enough)) ) DO
          BEGIN
            spcpos := pos(' ', Line) - 1;
            IF (spcpos = -1)
              THEN spcpos := strlen(Line);
            { Move a word from Line to the host entry. }
            temp_str := str(Line, 1, spcpos);
            { a finite state machine }
            CASE state OF
              read_nothing : BEGIN
                                 Raw_h_addr := temp_str;
                                 state := succ(state);
                               END;
              read_addr    : IF (strlen(temp_str) > H_MAX_NAME_LEN)
                               THEN Break_Error
                               ELSE BEGIN
                                 h_name := temp_str;
                                 state := succ(state);
                               END;
              read_enough  : ;
            END;
            delete(Line, 1, spcpos);
            Line := strltrim(Line);
          END;
    END;
END;

BEGIN { GetHostEnt }
    found_line := false;
    WITH p^ DO
      BEGIN
        h_name := '';
        WHILE ( NOT(eof(HostFile) OR found_line) ) DO
          BEGIN
            ReadInLine;
            IF ((strlen(Line) > 0) AND (Line[1] <> '#')
                AND (pos(' ', Line) > 0))
                { Note we ignore lines that don't have any spaces,
                  this may be bad to ignore without warning the user the
                  host table is bad.
                }
              THEN BEGIN
                found_line := true;
                BreakUpLine;
                h_addr := INet_Addr(Raw_h_addr);
              END;
          END;
      END;
    GetHostEnt := found_line;
END;

BEGIN { GetHostByName }
    found_entry := false;
    new(p);
    WITH p^ DO
      IF (NOT SetHostEnt)
        THEN BEGIN
              h_addr := INet_Addr(Host);
              IF (h_addr <> BadIpAddr)
                THEN BEGIN
                  found_entry := true;
                  h_name := Host;
                END;
        END
        ELSE BEGIN
          WHILE ((NOT found_entry) AND GetHostEnt)
            DO 
                IF (h_name = Host)
                  THEN found_entry := true
                  ELSE found_entry := (pos(' ' + Host + ' ',
                                           ' ' + Line + ' ') <> 0);
          EndHostEnt;
        END;
      IF ((found_entry) AND (p^.h_addr <> BadIpAddr))
        THEN GetHostByName := p
        ELSE BEGIN                { we didn't get a good Host }
          GetHostByName := nil;
          dispose(p);
        END;
END;
  
END; { Module }

