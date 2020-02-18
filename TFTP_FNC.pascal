{
  by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
  This is the user interface module of tftp.
  Bugs: while you can use 'connect 128.2.251.3' and 'get eem:f *t', you can use
        'get 128.2.251.3:f *t'.  This should be (easily) fixed.
}
MODULE TFTP_FNC;

IMPORT TFTP_DEC, TFTP_HOST, TFTP_TFTP, TFTP_RSC;
            
EXPORT PROCEDURE Driver;
 
IMPLEMENT

CONST MAXWORDLEN = 255;
      MAXWORDS = MAXWORDLEN DIV 2 + 1; { If the maximum length of a string is
                                         255, and each word is separated by a
                                         space, then you can only have 128
                                         (one letter long) words. }
TYPE  WordStrType = STRING[MAXWORDLEN];
VAR
      { A user command line is broken down by word into Argv numbered from
        word 1 to word Argc. }
      Argv : ARRAY[1..MAXWORDS] OF ^WordStrType;
      Argc : 0..MAXWORDS;
      HostName : WordStrType;    { The english host name }

PROCEDURE BreakLineUp(Line : max_str);
{ breaks Line up into Argv }
VAR i, spcpos : INTEGER;
BEGIN
    WHILE (strlen(Line) <> 0) DO
      BEGIN
        spcpos := pos(' ',Line) - 1;
        IF (spcpos = -1)
          THEN spcpos := strlen(Line);
        { Move a word from Line to the Argv array. }
        Argc := Argc + 1;
        new(Argv[Argc]);
        setstrlen(Argv[Argc]^,0);
        strmove(spcpos,Line,1,Argv[Argc]^,1);
        delete(Line,1,spcpos);
        Line := strltrim(Line);
     END;
END;
 
PROCEDURE DisposeArgv;
{ disposes of the space taken up by argv.  Must be done after each command
  line is used.  A bug: isn't done if the command failed with an ESCAPE().
}
VAR i : INTEGER;
BEGIN
    FOR i := 1 to Argc
      DO dispose(Argv[i]);
END;

FUNCTION GetCmd(testw : WordStrType) : INTEGER;
{ This function matches testw to the commands names in the command table.
  Returns -1 if an ambigous match, 0 if no match, or the CmdTable[] index if
  the word matches ok.  Uses a linear search as the command table isn't
  ordered.  This doesn't need to be fast anyway.
}
VAR i : 1..NumCommands;
    temp : -1..NumCommands;
BEGIN
    temp := 0;
    FOR i := 1 TO NumCommands
      DO
        IF (pos(testw, CmdTable[i].Name) = 1)
          THEN IF (temp = 0)
                 THEN temp := i
                 ELSE temp := -1;
     GetCmd := temp;
END;

PROCEDURE Quit;
BEGIN
    writeln;
    writeln('Bye...  You can immediately press ''U'' to restart TFTP');
    halt;
END;

PROCEDURE HelpCom;
{ Implements the help command.  '? ti p q' is a valid help command and will
  give help for each of the commands listed.
}
VAR i : 1..NumCommands;
    argvplc : 1..MaxWords;
    ComNum : -1..NumCommands;
BEGIN
    IF Argc = 1
      THEN BEGIN
        writeln('Commands may be abbreviated.  Commands are:');
        writeln;
        FOR i := 1 TO NumCommands
          DO WITH CmdTable[i]
               DO writeln(Name, strrpt(' ',15 - strlen(Name)), Help);
      END
      ELSE
        FOR argvplc := 2 TO Argc DO
          BEGIN
            ComNum := GetCmd(Argv[argvplc]^);
            IF ComNum = -1
              THEN writeln(#007'Ambiguous help command ' +
                           Argv[argvplc]^ +
                           '.  Please spell out more of the command name')
              ELSE IF ComNum = 0
                     THEN writeln(#007'Unknown command ',Argv[argvplc]^)  
                     ELSE WITH CmdTable[ComNum]
                            DO writeln(Name,
                                       strrpt(' ',15 - strlen(Name)), Help);
          END;
END;
            
        
PROCEDURE WriteYesOrNo(val : BOOLEAN);
BEGIN
    IF (val)   THEN writeln(' on.')   ELSE writeln(' off.');
END;

PROCEDURE SetVerbose;
BEGIN
    Verbose := NOT Verbose;
    write('Verbose mode is now');
    WriteYesOrNo(Verbose);
END;

PROCEDURE SetTrace;
BEGIN
    Trace := NOT Trace;
    write('Packet tracing is now');
    WriteYesOrNo(Trace);
END;

PROCEDURE SetHashes;
BEGIN
    Hashes := NOT Hashes;
    write('Hash mark printing mode is now');
    WriteYesOrNo(Hashes);
END;

PROCEDURE SetUdpCheck;
{ This command doesn't matter since due to bugs in 4.2 Unix the checksum must 
  always be there, so I will always calculate the checksum.  This is really 
  easy to change, though.
}
BEGIN
    UdpCkSum := NOT UdpCkSum;
    write('UDP check sum mode is now');
    WriteYesOrNo(UdpCkSum);
    writeln('It doesn''t matter, since the checksum must always be calculated');
END;

PROCEDURE Status;
{ Displays a verbose status listing }
VAR i : 1..4;
BEGIN
    IF (Connected)
      THEN BEGIN
        write('Connected to : ',HostName,
              '  Port: ', Sin.sin_port:0, '  IP Address: ');
        FOR i := 1 TO 4 DO
          BEGIN
            write(ord(Sin.sin_addr.S_un.S_str[i]):0);
            IF (i <> 4) THEN write('.') ELSE writeln;
          END;
      END 
      ELSE writeln('Not connected');
    writeln('Mode : ', Modes[Mode].M_name);
    write('Verbose'); WriteYesOrNo(Verbose);
    write('Tracing'); WriteYesOrNo(Trace);
    write('UDP check sum'); WriteYesOrNo(UdpCkSum);
    write('Hash mark printing'); WriteYesOrNo(Hashes);
    writeln('ReXmt Interval  :', RexmtVal : 3, ' retries');
    writeln('Maximum Timeout :', MaxTimeOut : 3, ' retries');
END;

FUNCTION Tail(filename : max_str) : max_str;
{ Given a unix pathname filename, returns the last filename in the path,
  a null string if the filename is null.
  i.e.: Tail('/usr/djw/.login') returns '.login'
        Tail('/usr/djw/')       returns 'djw'
        Tail('')                returns ''
        Tail('vmunix')          returns 'vmunix'
}
LABEL 999,2;
VAR pos, len : short;
BEGIN
    len := strlen(filename);
    WHILE (len > 0) DO
      BEGIN
        pos := len + scan(-len, ='/', filename[len]);
        { pos is zero if no '/', or the index of the '/'. }
        IF (pos = 0) THEN GOTO 2;
        IF (len > pos) {if there is something after the '/'}
          THEN BEGIN
            Tail := str(filename, pos + 1, len - pos);
            GOTO 999;
          END;
        len := len - 1;
        setstrlen(filename, len); { Delete the last charecter, a '/' }
      END;
2 : Tail := filename;
999 : ;
END;

PROCEDURE GetFile;
LABEL 999, 2;
VAR Line, src   : WordStrType;
    chpos, n      : u_short;
    dirname,
    filename    : max_str;
    iores_save  : INTEGER;

PROCEDURE GetUsage;
BEGIN
    writeln('usage : get host:file {[host]:file} target, or');
    writeln('        get file {file} target (when already connected)');
    writeln('You must specify both file(s) and target.');
    writeln('If multiple files are specified, target must be a volume');
    writeln('(* is allowed, and a colon will be appended if needed for the');
    writeln('value name).');
END;

BEGIN
    IF (Argc < 2)
      THEN BEGIN
        DisposeArgv;
        write('(file list) ');
        readln(Line);
        Line := strrtrim(strltrim(Line)); {Remove leading and trailing blanks.}
        Argc := 1; { Fudging! Argv[1] is where 'get' should go. }
        new(Argv[1]); { Even if it shouldn't exist, it'll later be disposed. }
        BreakLineUp(Line);
      END;
    IF (Argc < 3)
      THEN BEGIN
        GetUsage;
        GOTO 999;
      END;
    { We need to get a host.  If we're not connected, than at least the first
      source file must have a host specified with the colon notation.  That
      host will be used until another host is specified. }
    IF ((NOT Connected) AND (pos(':',Argv[2]^) = 0))      
      THEN BEGIN
        GetUsage;
        GOTO 999;
      END;
    IF (Argc > 3)
      THEN IF (Argv[Argc]^[strlen(Argv[Argc]^)] IN [':','*'])
             THEN dirname := Argv[Argc]^
             ELSE dirname := Argv[Argc]^ + ':';

    FOR n := 2 TO Argc -1 DO
      { for each file in the file list given: }
      BEGIN
        { Check for a host being given along with the file name }  
        chpos := pos(':', Argv[n]^);
        IF (chpos = 0)
          THEN src := Argv[n]^
          ELSE BEGIN
            IF (chpos = 1)
              THEN BEGIN
                writeln('get- No host name before : in ', Argv[n]^);
                GOTO 2;
              END;
            { Get the file name after the host name and colon }
            src := str(Argv[n]^, chpos + 1, strlen(Argv[n]^) - chpos);
            IF (strlen(src) = 0)
              THEN BEGIN
                writeln('get- No file name after : in ', Argv[n]^);
                GOTO 2;
              END;
            The_Host := GetHostByName(str(Argv[n]^, 1, chpos - 1));
            IF (The_Host = nil)
              THEN BEGIN
                writeln('get- Unknown host:', str(Argv[n]^, 1, chpos - 1));
                GOTO 2;
              END;
            moveleft(The_Host^.h_addr, Sin.sin_addr, 4);
            Sin.sin_port := IPPORT_TFTP;
            Connected := true;
            HostName := The_Host^.h_name;
          END;
        IF (Argc = 3)    { just one file to transfer? }
          THEN BEGIN
            filename := Argv[3]^;
            IF (filename[strlen(filename)] IN [':','*'])
              THEN filename := filename + Tail(src);
            RecvFile(src, filename);
            GOTO 999;
          END;
        filename := dirname + Tail(src);
        RecvFile(src, filename);
2:    END;
999: ;
END;

PROCEDURE PutFile;
LABEL 999;
VAR Line        : WordStrType;
    chpos, n, len : u_short;
    targ,
    targ_front,
    filename    : max_str;
    iores_save  : INTEGER;

PROCEDURE PutUsage;
BEGIN
    writeln('usage : put file {file} host:target, or');
    writeln('        put file {file} target (when already connected)');
    writeln('You must specify both file(s) and target.');
    writeln('Only one host may be specified in a single put command, and it');
    writeln('must come at the end of the line, before the target.');
    writeln('If multiple files are specified, target should be a directory.');
END;

BEGIN
    IF (Argc < 2)
      THEN BEGIN
        DisposeArgv;
        write('(file list) ');
        readln(Line);
        Line := strrtrim(strltrim(Line)); {Remove leading and trailing blanks.}
        Argc := 1; { Fudging! Argv[1] is where 'put' should go. }
        new(Argv[1]); { Even if it shouldn't exist, it'll later be disposed. }
        setstrlen(Argv[1]^, 0);
        BreakLineUp(Line);
      END;
    IF (Argc < 3)
      THEN BEGIN
        PutUsage;
        GOTO 999;
      END;
    targ_front := Argv[Argc]^;
    IF ((NOT Connected) AND (pos(':', targ_front) = 0))
      THEN BEGIN
        writeln('No target machine specified');
        GOTO 999;
      END;
      
    chpos := pos(':',targ_front);
    IF (chpos <> 0)
      THEN BEGIN                                      { They specified a host }
        IF (chpos = 1)
          THEN BEGIN
            writeln('get- No host name before : in ', targ_front);
            GOTO 999;
          END;
        The_Host := GetHostByName(str(targ_front, 1, chpos - 1));
        IF (The_Host = nil)
          THEN BEGIN
            writeln('put- Unknown host:', targ_front);
            GOTO 999;
          END;
        strdelete(targ_front, 1, chpos);    {get rid of the host name and colon }
        moveleft(The_Host^.h_addr, Sin.sin_addr, 4);
        Sin.sin_port := IPPORT_TFTP;
        Connected := true;
        HostName := The_Host^.h_name;
      END;
    
    FOR n := 2 TO Argc - 1 DO
      BEGIN
        filename := Argv[n]^;   { The Pascal-system filename. }
        IF (Argc > 3)
          THEN BEGIN
            { Now make the connected-to machine's filename, but exclude any
              Pascal-system directory stuff from the filename. targ_front is
              really a pathname, now we give it the file name. }
            chpos := pos(':', filename);
            IF (chpos = 0) THEN chpos := pos('*', filename);
            IF (chpos = 0)
              THEN targ := targ_front + filename
              ELSE targ := targ_front 
                            + str(filename, chpos + 1, strlen(filename) - chpos);
          END
          ELSE targ := targ_front; { If only two names were given, the connect-
                                     ed-to machine's filename is absolute.}
        SendFile(filename, targ);
      END;
999: ;
END;

PROCEDURE SetPeer;
{ Implements the connect command }
LABEL 999;
VAR Line         : WordStrType;
    temp_addr    : IpAddrStr;
    temp_pos     : INTEGER;
BEGIN
    Connected := true;
    IF (Argc = 1)
      THEN BEGIN
        DisposeArgv;
        write('(to) ');
        readln(Line);
        Line := strrtrim(strltrim(Line)); {Remove leading and trailing blanks.}
        Argc := 1; { Fudging! Argv[1] is where 'connect' should go. }
        new(Argv[1]); { Even if it shouldn't exist, it'll later be disposed. }
        BreakLineUp(Line);
      END;
    IF ((Argc > 3) OR (Argc < 2))
      THEN BEGIN
        writeln('usage : connect host-name [port]');
        Connected := false;
        GOTO 999;
      END;
    The_Host := GetHostByName(Argv[2]^);
    IF (The_Host <> nil)
      THEN BEGIN
        HostName := The_Host^.h_name;
        moveleft(The_Host^.h_addr, Sin.sin_addr, 4);
      END
      ELSE BEGIN
        temp_addr := INet_Addr(Argv[2]^);
        IF (temp_addr = BadIpAddr)
          THEN BEGIN
            Connected := false;
            writeln('connect: unknown host ', Argv[2]^);
            GOTO 999;
          END;
        moveleft(temp_addr, Sin.sin_addr, 4);
        HostName := Argv[2]^;
      END;
    Sin.sin_port := IPPORT_TFTP;
    IF (Argc = 3)
      THEN BEGIN
        TRY strread(Argv[3]^, 1, temp_pos, Sin.sin_port);
        RECOVER IF ((ESCAPECODE = -10) OR (ESCAPECODE = -8))
                { escapecode -10 is an i/o error, -8 is a range error. }
                THEN BEGIN
                  writeln('connect: Bad port number ', Argv[3]^);
                  Connected := false;
                  GOTO 999;
                END
                ELSE ESCAPE(ESCAPECODE);
                { Now check for cases like '123!!@$@#$@#',
                  which wouldn't cause an exception in strread.  Make
                  sure we strread to the end of the string. }
        IF (temp_pos <> (strlen(Argv[3]^) + 1))
          THEN BEGIN
                  writeln('connect: Bad port number ', Argv[3]^);
                  Connected := false;
                END;
      END;
999 : ;  { for emulating return statements }
END;

PROCEDURE ModeHelp;
VAR p : 1..ModeCnt + 1;
    sep : STRING[3];
BEGIN
    write('usage: ',argv[1]^,' [ ');
    sep := '';
    FOR p := 1 TO ModeCnt
      DO BEGIN
        write(sep, Modes[p].M_name);
        IF (strlen(sep) = 0) THEN sep := ' | ';
      END;
    writeln(' ]');
    writeln;
    FOR p := 1 TO ModeCnt
      DO writeln('Use ',Modes[p].M_name,' mode to transfer ',Modes[p].M_help);
END;

PROCEDURE SetMode;
VAR p : 1..ModeCnt + 1;
BEGIN
    IF (Argc > 2)
      THEN ModeHelp
      ELSE IF (Argc < 2)
             THEN writeln('Using ', Modes[Mode].M_name,
                          ' mode to transfer files')
             ELSE BEGIN
               IF (Argv[2]^ = '?')
                 THEN ModeHelp
                 ELSE BEGIN
                   p := 1;
                   WHILE ((p <= ModeCnt)
                          AND (pos(argv[2]^,Modes[p].M_name)<>1))
                     DO p := p + 1;
                   IF (p > ModeCnt)
                     THEN writeln('Unknown mode ',argv[2]^)
                     ELSE Mode := p;
                 END;
             END;
END;

FUNCTION GetTimeVal(s : max_str; VAR time : TimeType) : BOOLEAN;
{ Gets a time for command 's' and puts it in 't'.  Returns false if it
  fails to parse a good value.
}
VAR Line     : WordStrType;
    temp_pos : INTEGER;
    bad_time : BOOLEAN;
BEGIN
    GetTimeVal := false;
    bad_time := false;
IF (Argc < 2)
      THEN BEGIN
        DisposeArgv;
        write('(value) ');
        readln(Line);
        Line := strrtrim(strltrim(Line)); {Remove leading and trailing blanks.}
        Argc := 1; { Fudging! Argv[1]^ is where command should go. }
        new(Argv[1]); { Even if it shouldn't exist, it'll later be disposed. }
        BreakLineUp(Line);
      END;
    IF (Argc <> 2)
      THEN writeln('usage : ', s, ' value')
      ELSE BEGIN
        TRY strread(Argv[2]^, 1, temp_pos, time);
        RECOVER IF ((ESCAPECODE = -10) OR (ESCAPECODE = -8))
                { escapecode -10 is an i/o error, -8 is a range error. }
                THEN bad_time := true
                ELSE ESCAPE(ESCAPECODE);
                { Now check for cases like '123!!@$@#$@#',
                  which wouldn't cause an exception in strread.  Make
                  sure we strread to the end of the string. }
        IF ((NOT bad_time) AND (temp_pos = (strlen(Argv[2]^) + 1))
            AND (time >= 0))
          THEN GetTimeVal := true
          ELSE writeln(s, ': bad time ', Argv[2]^);
      END;
END;

PROCEDURE SetRexmt;
VAR temp : TimeType;
BEGIN
    IF (GetTimeVal('Rexmt-timeout', temp))
      THEN RexmtVal := temp;
END;

PROCEDURE SetTimeout;
VAR temp : TimeType;
BEGIN
    IF (GetTimeVal('Maximum-timeout', temp))
      THEN MaxTimeOut := temp;
END;

PROCEDURE InitGlobals;
BEGIN
    WITH CmdTable[1] DO
        BEGIN
            Name := 'connect';
            Help := 'connect to remote tftp';
            Handler := setpeer;
        END;
    WITH CmdTable[2] DO
        BEGIN
            Name := 'mode';
            Help := 'set file transfer mode (use "mode ?" for more help)';
            Handler := setmode;
        END;
    WITH CmdTable[3] DO
        BEGIN
            Name := 'put';
            Help := 'send file';
            Handler := putfile;
        END;
    WITH CmdTable[4] DO
        BEGIN
            Name := 'get';
            Help := 'receive file';
            Handler := getfile;
        END;
    WITH CmdTable[5] DO
        BEGIN
            Name := 'quit';
            Help := 'exit tftp to operating system';
            Handler := quit;
        END;
    WITH CmdTable[6] DO
        BEGIN
            Name := 'verbose';
            Help := 'toggle verbose mode';
            Handler := setverbose;
        END;
    WITH CmdTable[7] DO
        BEGIN
            Name := 'trace';
            Help := 'toggle packet tracing';
            Handler := settrace;
        END;
    WITH CmdTable[8] DO
        BEGIN
            Name := 'status';
            Help := 'show current status and mode settings';
            Handler := status;
        END;
    WITH CmdTable[9] DO
        BEGIN
            Name := 'rexmt';
            Help := 'set per-packet retransmission timeout';
            Handler := setrexmt;
        END;
    WITH CmdTable[10] DO
        BEGIN
            Name := 'timeout';
            Help := 'set total retransmission timeout';
            Handler := settimeout;
        END;
    WITH CmdTable[11] DO
        BEGIN
            Name := 'udpsum';
            Help := 'toggle udp checksum mode';
            Handler := setudpcheck;
        END;
    WITH CmdTable[12] DO
        BEGIN
            Name := 'hashes';
            Help := 'toggle printing of hash marks';
            Handler := sethashes;
        END;
    WITH CmdTable[13] DO
        BEGIN
            Name := '?';
            Help := 'print help information about any or all commands';
            Handler := helpcom;
        END;
        
    Hashes := true;
    Trace  := false;
    Connected := false;
    Verbose := false;
    UdpCkSum := true;
    RexmtVal := TIMEOUT;
    MaxTimeOut := 5 * TIMEOUT;
    Mode := 3;
    HostName := '';
    Modes := ModeArray;
    fillchar(Sin, sizeof(Sin), #000);
    Sin.sin_family := AF_INET;
    fillchar(Udp_Var, sizeof(Udp_Var), #000);
    RSC_open;
    RSC_Set_Ip_Addr;
END;

PROCEDURE GetLine;
VAR Line   : WordStrType;
    ComNum : -1..NumCommands;
BEGIN
    write('tftp >');
    Argc := 0;
    readln(Line);
    Line := strrtrim(strltrim(Line)); { Remove leading and trailing blanks. }
    IF strlen(Line) <> 0
    THEN BEGIN
        BreakLineUp(Line);
        ComNum := GetCmd(Argv[1]^);
        IF ComNum = -1
          THEN writeln(#007'Ambiguous match.  ' +
                       'Please spell out more of the command name')
          ELSE IF ComNum = 0
                 THEN writeln(#007'Unknown command.  "?[enter]" for help')
                 ELSE call(CmdTable[ComNum].Handler);
                       
    END;
    DisposeArgv;
END;

PROCEDURE Driver;
{ Does the main work.
  It terms of handling ESCAPE(code):
   case code of
    1         : returns to main input loop after warning
    0 or -20  : quietly quits
    otherwise : noisily quits after mean messages are printed
}
BEGIN
    InitGlobals;
    writeln('Type ?[enter] for help.');
    WHILE (true)
      DO TRY GetLine;
         RECOVER IF (ESCAPECODE <> 1)
                   THEN BEGIN
                     IF ((ESCAPECODE <> 0) and (ESCAPECODE <> -20))
                       THEN BEGIN
                         writeln(#7#7'Exception in TFTP #', ESCAPECODE:4);
                         writeln('Please report it to lab assistant');
                         writeln('TFTP NOT RUNNING');
                       END;
                     ESCAPE(ESCAPECODE);
                   END
                   ELSE
                     writeln('Escape to toplevel in TFTP caught, #'
                              ,ESCAPECODE:4);
END;

END; { module }

