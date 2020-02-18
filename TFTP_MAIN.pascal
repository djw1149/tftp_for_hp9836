{
 by David Waitzman  2/25/85  for Gregg Lebovitz and CMU-ECE
 Main body of HP-Pascal TFTP (trivial file transfer program) version 1.1.
 Includes all of the needed program modules and runs the real program that's in
 TFTP_FNC.
}

$SYSPROG$             { Enable system level programming features          }
$UCSD$                { Enable UCSD pascal programming features           }
$PARTIAL_EVAL ON$     { optomize boolean expressions, don't remove        }
$HEAP_DISPOSE ON$     { really dispose memory, slows things down, but...  }
$LINES 2000000$       { suppreses autopagination of program listings      }
$RANGE ON$            { enable range checks, slows things down, but...    }
$OVFLCHECK ON$        { enable overflow checks, slows things down, but... }
$STACKCHECK OFF$      { Dangerous!!! don't check stack for overflow when  }
                      {   allocating locals                               }

PROGRAM TFTP_MAIN;

$include 'TFTP_DEC'$
$include 'TFTP_CKSM'$
$include 'TFTP_RSC'$
$include 'TFTP_IP'$
$include 'TFTP_UDP'$
$include 'TFTP_NET'$
$include 'TFTP_TFTP'$
$include 'TFTP_HOST'$
$include 'TFTP_FNC'$

IMPORT TFTP_FNC;

BEGIN
    writeln('HP Pascal TFTP Version 1.1                             2/25/85');
    writeln;
    writeln('Report bugs and comments to gripe@ampere and the lab assistant.');
    Driver;
END.
            

