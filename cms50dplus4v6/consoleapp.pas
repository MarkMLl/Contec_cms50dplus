(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit ConsoleApp;

(* This is the greater part of a console program which reads live data from a   *)
(* Contec CMS50D pulse oximeter (firmware v4.6, 9-byte messages @115k) and      *)
(* sends it to stdout.                                          MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial;

type
  TWriteLn= procedure(const s: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const portName: string; portScan: boolean= false);

(* This is the inner loop of the main function. If the second parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; var pleaseStop: boolean;
                                        writer: TWriteLn= nil): integer;

(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;


implementation

uses
  StrUtils, LocateCp210xPort, IniFilesAbout, BaseUnix, Termio;

var
  oddity: boolean= false;


(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

begin
  WriteLn();
  WriteLn(projName + ' ' + AboutText());
  WriteLn()
end { DoVersion } ;


(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const portName: string; portScan: boolean= false);

begin
  WriteLn();
  WriteLn('Usage: cms50dplus [OPTIONS]... [DEVICE]');
  WriteLn();
  WriteLn('Output readings from a Contec CMS50D+ pulse oximeter.');
  WriteLn();
  WriteLn('DEVICE is the name of a serial device such as /dev/ttyUSB0.');
  WriteLn();
{$ifdef LCL }
  WriteLn('If there is no explicit option or device an interactive GUI screen will');
  WriteLn('be presented. Supported options are as below:');
{$else      }
  WriteLn('Supported options are as below:');
{$endif LCL }
  WriteLn();
  WriteLn('  --version      Version information.');
  WriteLn();
  WriteLn('  --help         This help text, also reports default device.');
  WriteLn();
  WriteLn('  --portScan     Extends --help output with device list (might be slow).');
  WriteLn();
  WriteLn('  -F --format p  Output format for main value is defined by pattern p, which');
  WriteLn('                 is e.g. %5.2f to specify a total of five characters with two');
  WriteLn('                 digits after the decimal point. Alternatively use %x@yBzd or');
  WriteLn('                 %x@yLzd for offset x and y bytes of big/little-endian data');
  WriteLn('                 with C format %zd etc., @ by itself dumps raw data as read.');
  WriteLn();
{$ifdef LCL }
  WriteLn('  -              Dummy option, ignored.');
{$else      }
  WriteLn('  - --           Dummy options, ignored.');
{$endif LCL }
  WriteLn();
  WriteLn('Exit status:');
  WriteLn();
  WriteLn(' 0  Normal termination');
  WriteLn(' 1  Cannot parse device identifier');
  WriteLn(' 2  Named device cannot be opened');
  WriteLn(' 3  Named device is unresponsive');
  WriteLn(' 4  Data access error');
  WriteLn(' 5  Data format error');
  WriteLn(' 9  Bad command-line parameters');
  WriteLn();
  if portScan then
    DumpCachedPorts;
  WriteLn('Default port: ', portName);
  WriteLn()
end { DoHelp } ;


type
  byteArray= array of byte;


operator + (const a: byteArray; const b: byte): byteArray;

begin
  result := a;
  SetLength(result, Length(result) + 1);
  result[High(result)] := b
end { + } ;


(* Output data that we've got from a feature request or a query. This is either
  a block of hex/ASCII data in the conventional layout, or is formatted
  according to a layout string where

         %      Indicates start of layout string
         n @    Buffer is zero-based, advance to this index
  either n L    n bytes of little-endian data follow
  or     n B    n bytes of big-endian data follow
  or     n H    n bytes of host-endian data follow
         xxx    C-style format string, leading % assumed

  Assume that this requires three components required in that order, and with n
  being a decimal number. This is intended as a general aid to investigating
  HID behaviour, I'm writing it primarily to support a TEMPer sensor.
*)
procedure diagOutput(const rawBlock: byteArray; l: integer; const diagOutputFormat: string);

var
  i, j, scratchInt: integer;
  expectMaths: boolean;
  pattern, numAsString, savedStr: string;
  savedChar: char;
  accumulator: double;


  function hex(b: byte): string; inline;

  begin
    result := ' ' + LowerCase(IntToHex(b, 2))
  end { hex } ;


  (* Parse off anything that looks like a signed or unsigned integer or real
    plus the # character and space.
  *)
  function parseNum(var str: string): string;

  var
    valid: set of char;

  begin
    result := '';
    valid := [' ', '#', '.', '0'..'9'];
    if not expectMaths then
      valid += ['+', '-'];
    while (str <> '') and (str[1] in valid) do begin
      result += str[1];
      Delete(str, 1, 1)
    end
  end { parseNum } ;


  procedure accumulateData(littleEndian: boolean);

  begin
    if littleEndian then begin
      if numAsString <> '' then
        j := StrToInt(numAsString);
      case j of
        1: scratchInt := rawBlock[i];
        2: scratchInt := (rawBlock[i + 1] shl 8) + rawBlock[i];
        3: scratchInt := (rawBlock[i + 2] shl 16) + (rawBlock[i + 1] shl 8) +
                                rawBlock[i];
        4: scratchInt := (rawBlock[i + 3] shl 24) + (rawBlock[i + 2] shl 16) +
                                (rawBlock[i + 1] shl 8) + rawBlock[i]
      otherwise
      end;
      savedChar := Chr(scratchInt);
      SetLength(savedStr, j);     (* UNTESTED             *)
      Move(rawBlock[i], savedStr[1], j); (* Natural order *)
      accumulator := scratchInt
    end else begin
      if numAsString <> '' then
        j := StrToInt(numAsString);
      case j of
        1: scratchInt := rawBlock[i];
        2: scratchInt := (rawBlock[i] shl 8) + rawBlock[i + 1];
        3: scratchInt := (rawBlock[i] shl 16) + (rawBlock[i + 1] shl 8) +
                                rawBlock[i + 2];
        4: scratchInt := (rawBlock[i] shl 24) + (rawBlock[i + 1] shl 16) +
                                (rawBlock[i + 2] shl 8) + rawBlock[i + 3]
      otherwise
      end;
      savedChar := Chr(scratchInt);
      SetLength(savedStr, j);     (* UNTESTED             *)
      Move(rawBlock[i], savedStr[1], j); (* Natural order *)
      accumulator := scratchInt
    end
  end { accumulateData } ;


begin
  if diagOutputFormat = '' then begin
    for i := 0 to SizeOf(rawBlock) div 16 do
      if i * 16 < l then begin
        Write(LowerCase(IntToHex(I * 16, 4)), ' ');
        for j := 0 to 15 do
          if (i * 16) + j < l then
            Write(hex(rawBlock[(i * 16) + j]));
        Write('  ');
        for j := 0 to 15 do
          if (i * 16) + j < l then
            if (rawBlock[(i * 16) + j] >= $20) and (rawBlock[(i * 16) + j] < $7f) then
              Write(Chr(rawBlock[(i * 16) + j]))
            else
              Write('·');               (* Dot                                  *)
        WriteLn
      end
  end else begin
    pattern := diagOutputFormat;
    while pattern <> '' do
      case pattern[1] of
        '\': begin                      (* Control characters, and literal %    *)
               case pattern[2] of
                 'b': Write(#$08);
                 'n': WriteLn;
                 'r': Write(#$0d);
                 't': Write(#$09)
               otherwise                (* Second character verbatim            *)
                 Write(pattern[2])
               end;
               Delete(pattern, 1, 2)
             end;
        '%': begin                      (* Formatted output, uses a nested loop *)
               i := 0;                  (* Index into array, default zero       *)
               j := 2;                  (* Number of bytes to read, default 2   *)
               accumulator := 0.0;
               savedChar := #$00;       (* These bypass any arithmetic operations *)
               savedStr := '';
               expectMaths := false;
               Delete(pattern, 1, 1);   (* Leading %                            *)
               while pattern <> '' do begin
                 numAsString := parseNum(pattern); (* Deletes chars parsed off  *)
                 if pattern = '' then begin (* Treat this as literal output,    *)
                   Write(numAsString);  (* we'll have lost an earlier %.        *)
                   exit
                 end;
                 try
                   case pattern[1] of
                     '(': expectMaths := true; (* Are + and - maths operators   *)
                     ')': expectMaths := false; (* or part of the format string? *)
                     '@': if numAsString <> '' then
                            i := StrToInt(numAsString);
                     'L': accumulateData(true);
                     'B': accumulateData(false);
(*$IFDEF ENDIAN_LITTLE *)
                     'H': accumulateData(true);         (* Host-endian          *)
(*$ELSE                *)
                     'H': accumulateData(false);
(*$ENDIF ENDIAN_LITTLE *)
                     '+': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 0;
                            accumulator += scratchInt
                          end;
                     '-': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 0;
                            accumulator -= scratchInt
                          end;
                     '*': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator *= scratchInt
                          end;
                     '/': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator /= scratchInt
                          end;
                     '%': begin         (* Not a modulus: round to precision    *)
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator /= scratchInt;
                            accumulator := Round(accumulator);
                            accumulator *= scratchInt
                          end

(* Assume that anything else is a printf()-style type character, except that    *)
(* this does not have provision for a prefix character since that's been        *)
(* handled explicitly by the L or B command.                                    *)
(*                                                                              *)
(* I am resisting the temptation of putting in any additional operators here,   *)
(* for example some of the sexy APL ones, not so much because the sort of       *)
(* manipulation they imply is well outside the original scope of this program   *)
(* (which was, after all, a GUI to configure a mouse) but because most of them  *)
(* imply the availability of variable or array support which is quite simply    *)
(* impractical without using a completely different notation.                   *)

                   otherwise
                     case pattern[1] of
                       'c':      Write(Format('%' + numAsString + pattern[1], [savedChar]));
                       's':      Write(Format('%' + numAsString + pattern[1], [savedStr]));
                       'e', 'E', 'f',
                       'g', 'G': Write(Format('%' + numAsString + pattern[1], [accumulator]));
                       'x':      Write(LowerCase(ReplaceStr(
                                        Format('%' + numAsString + pattern[1], [Round(accumulator)]), ' ', '0')));
                       'X':      Write(UpperCase(ReplaceStr(
                                        Format('%' + numAsString + pattern[1], [Round(accumulator)]), ' ', '0')))
                     otherwise
                       Write(FormatEx('%' + numAsString + pattern[1], [Round(accumulator)], IsoTimeOnly, 2))
                     end;
                     Delete(pattern, 1, 1);
                     Break              (* Back to outer loop                   *)
                   end;
                   Delete(pattern, 1, 1) (* Stay in inner loop                  *)
                 except
                   Write(#$0d);         (* Start of line                        *)
                   diagOutput(rawBlock, l, '') (* Something wrong               *)
                 end
               end                      (* Inner loop (handles % format)        *)
             end
      otherwise                         (* Anything else                        *)
        Write(pattern[1]);
        Delete(pattern, 1, 1)
      end;
      WriteLn                           (* Outer loop ends immediately after this *)
  end
end { diagOutput } ;


type
  Tmessage= array[0..8] of byte;


(* Output either as a data dump (format is @ by itself) or with C-style
  formatting applied to each byte or word.
*)
function outputFormattedRaw(message: TMessage; const formatString: string): boolean;

begin
  result := true;
  if Length(formatString) <= 2 then
    diagOutput(message, SizeOf(message), '')
  else
    diagOutput(message, SizeOf(message), formatString)
end { outputFormattedRaw } ;


(* Output using an explicit C-style format string. If this ends with whitespace
  or a control character (tab or newline) then don't append an implicit newline.
*)
function outputFormattedRescaled(value: double; const formatString: string): boolean;

var
  ln: boolean= true;

begin
  result := true;
  try
    if Length(formatString) > 2 then begin
      if formatString[Length(formatString)] = ' ' then
        ln := true;
      if formatString[Length(formatString) - 1] = '\' then
        ln := true
    end;
    Write(FormatEx(formatString, [value]));
    if ln then
      WriteLn;
  except
    result := false
  end
end { outputFormattedRescaled } ;


(* If the format string contains an @ then process the raw buffer. Otherwise
  extract the main value (SpO2) and scale to a double, then format it.
*)
function outputFormatted(message: Tmessage; const formatString: string): boolean;

var
  value: double;

begin
  if (formatString = '') or (Pos('@', formatString) > 0) then
    result := outputFormattedRaw(message, formatString)
  else begin
    value := message[6];
    result := outputFormattedRescaled(value, formatString)
  end
end { outputFormatted } ;


(* Format a 9-byte message into text, using the scaling etc. provided by the
  meter.
*)
function autoFormatted(message: Tmessage; wrapUp: boolean= false): string;

(* There's been at least three data formats: low-, medium- and high-speed as    *)
(* described respectively at                                                    *)
(*                                                                              *)
(* https://www.atbrask.dk/?p=244  https://github.com/atbrask/CMS50Dplus         *)
(* https://github.com/InfantLab/Contec-Pulse-Oximeter-in-Matlab                 *)
(* https://gist.github.com/patrick-samy/df33e296885364f602f0c27f1eb139a8        *)
(*                                                                              *)
(* I only have one a CMS50D+ with firmware v4.6, and am only supporting the     *)
(* high-speed variant with live (not recorded) data i.e. the last of those.     *)
(*                                                                              *)
(* Data looks something like                                                    *)
(*                                                                              *)
(* 0 01                 Sync, always 0x01                                       *)
(* 1 e0                 LSB 0 if OK, 1 if finger out                            *)
(* 2 0x85 -> 0x05 5     Signal strength? Bit 6 possibly a pulse marker?         *)
(* 3 0xd5 -> 0x55 85    Infra red?                                              *)
(* 4 0x9a -> 0x1a 26    Visible?                                                *)
(* 5 0xbd -> 0x35 53    Rolling pulse rate                                      *)
(* 6 0xe3 -> 0x63 99    Rolling SpO2                                            *)
(* 7 ff                                                                         *)
(* 8 ff                                                                         *)
(*                                                                              *)
(* Nominal operation: 0000  01 e0 85 d5 9a bd e3 ff ff  ·········               *)

var
  scratch: string;

begin
  if (message[0] <> $01) or (message[7] <> $ff) or (message[8] <> $ff) then
    exit('');                           (* Bad sync byte                        *)
  result := IsoFormatDateTime(Now(), IsoTimeOnly, 2);   (* Date, time           *)

(* Pulse(?) marker and signal strength (value decreases with a layer of bin-    *)
(* liner as a filter).                                                          *)

  if (message[2] and $40) = 0 then
    result += '  |'
  else
    result += ' ^|';
  Str((message[2] and $0f):3, scratch);
  result += scratch + '|';

(* Infrared and visible signal (I think in that order, based on trying a layer  *)
(* of bin-liner as a visible-light filter).                                     *)

  Str((message[3] and $7f):3, scratch);
  result += scratch + '|';
  Str((message[4] and $7f):3, scratch);
  result += scratch + '|';

(* Rolling pulse rate and SpO2.                                                 *)

  Str((message[5] and $7f):3, scratch);
  result += scratch + '|';
  Str((message[6] and $7f):3, scratch);
  result += scratch + '|';
  if wrapUp then
    result += 'No reading'
  else
    if Odd(message[1]) then
      result += 'Finger out'
    else
      result += 'OK'
end { autoFormatted } ;


var
  debugLevel: integer= 0;
  formatString: string= '';
  onceOnly: boolean= false;


(* This is the inner loop of the main function. If the second parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; var pleaseStop: boolean;
                                        writer: TWriteLn= nil): integer;

const
  start: Tmessage= ($7d, $81, $a1, $80, $80, $80, $80, $80, $80);

var
  i, lastI: integer;
  waitingFirstSync: boolean;
  resync: jmp_buf;
  message: Tmessage;
  scratch: string;

begin
  try
    SerSetParams(portHandle, 115200, 8, NoneParity, 1, []);
    message := start;
    lastI := SetJmp(resync);
    SerWrite(portHandle, message, 9);
    waitingFirstSync := true;
    repeat
      i := -1;

(* There's two possibilities here. If we're not confident that we're seeing a   *)
(* stream of contiguous data then be prepared to wait several seconds for a     *)
(* sync byte, otherwise be less tolerant.                                       *)

      if waitingFirstSync then
        if SerReadTimeout(portHandle, message[0], 5000) = 0 then
          case lastI of
            0,
            1: exit(3);                 (* Unresponsive                         *)
          otherwise
            exit(4)                     (* Access error, message not intact     *)
          end
        else begin end                  (* Continue with check that it's $01    *)
      else
       if SerReadTimeout(portHandle, message[0], 1000) = 0 then
//          LongJmp(resync, 1);
         exit(0);

(* The MS2115B can resynchronise, the CMS50D+ can't and any attempt to recover  *)
(* using e.g. a close/open sequence would introduce an unacceptable glitch.     *)
(* Runtime appears to be a fixed 30 seconds irrespective of the validity of the *)
(* reading, I've not been able to change this by any obvious alteration to the  *)
(* start command.                                                               *)

      waitingFirstSync := false;
      if message[0] <> $01 then
        continue;

(* Anything that isn't a sync byte has been discarded. Read the remainder of    *)
(* the message.                                                                 *)

      for i := 1 to 8 do
        if SerReadTimeout(portHandle, message[i], 100) = 0 then
          LongJmp(resync, i + 1);
      if debugLevel > 1 then begin
        Write(stderr, '#');
        for i := 0 to 8 do
          Write(stderr, ' ' + HexStr(message[i], 2));
        WriteLn(stderr)
      end;
      if Assigned(writer) then begin
        scratch := autoFormatted(message);
        if scratch = '' then
          exit(5);                      (* Format error                         *)
        writer(scratch)
      end else

(* We have a 9-byte message. If there is an explicit format string then apply   *)
(* it, otherwise just try to do the right thing.                                *)

        if formatString <> '' then
          if not outputFormatted(message, formatString) then
            exit(5)                     (* Format error                         *)
          else begin end
        else begin
          scratch := autoFormatted(message);
          if scratch = '' then
            exit(5);                    (* Format error                         *)
          WriteLn(scratch)
        end;
      if onceOnly then                  (* Debugging option, -ve level          *)
        break
    until pleaseStop { Dave }           (* Or signal from keyboard              *)
  finally
    if Assigned(writer) then begin
      scratch := autoFormatted(message, true); (* New time plus last data      *)
      writer(scratch)
    end;
    SerClose(portHandle)
  end;
  result := 0
end { RunConsoleApp2 } ;


(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;

var
  portHandle: TSerialHandle= InvalidSerialHandle;
  dontStop: boolean= false;
  i: integer;
  ports: TStringList;

begin
  result := 3;                          (* Unresponsive is a good default       *)
  i := 1;
  while i <= ParamCount() do begin
    case ParamStr(i) of
      '-',                              (* Placeholder only                     *)
      '--',                             (* This doesn't work with GUI/LCL       *)
      '--ports',                        (* Used as --help modifier only         *)
      '--portscan',
      '--portsScan':  ;
      '--debug':      if i = ParamCount() then begin
                        WriteLn(stderr, 'Debug level has no parameter');
                        exit(9)         (* Missing debug level                  *)
                      end else begin
                        i += 1;
                        try
                          debugLevel := Abs(StrToInt(ParamStr(i)));
                          onceOnly := StrToInt(ParamStr(i)) < 0
                        except
                          WriteLn(stderr, 'Debug level not numeric');
                          exit(9)       (* Bad debug level                      *)
                        end
                      end;
      '-F',
      '--format':     if i = ParamCount() then begin
                        WriteLn(stderr, 'Format string has no parameter');
                        exit(9)         (* Missing format string                *)
                      end else begin
                        i += 1;
                        formatString := ParamStr(i)
                      end
    otherwise
      if i <> ParamCount() then begin
        WriteLn(stderr, 'Bad device name');
        exit(1)                         (* Bad device name                      *)
      end else
        portName := ParamStr(i)
    end;
    i += 1
  end;

(* In principle, if the debugging level were appropriate I could list the auto- *)
(* detected serial ports here. In practice telling the user anything useful     *)
(* would be quite a lot of work, since the standard code doesn't actually save  *)
(* manufacturer and driver names as it's scanning the /sys tree trying to find  *)
(* a satisfactory match.                                                        *)

  if debugLevel > 1 then begin
    ports := ListPorts;
    try
      for i := 0 to ports.Count - 1 do
        WriteLn(stderr, '# ' + ports[i])
    finally
      FreeAndNil(ports)
    end
  end;

// OK, so I did a bit but it doesn't show very much that's useful. What I'm
// inclined to do next is hang a secondary stringlist onto each line that
// represents a port, and then as properties (e.g. kernel driver) are being
// checked update indexed lines which can be subsequently walked.

  portHandle := SerOpen(portName);
{$ifdef UNIX }
  if portHandle > 0 then
    if fpIoctl(portHandle, TIOCEXCL, nil) <> 0 then begin (* Mandatory lock,    *)
      SerClose(portHandle);             (* unlike flock() (if it even works in  *)
      portHandle := -1                  (* this context) or a lock file as used *)
    end;                                (* by various gettys etc.               *)
{$endif UNIX }
  if portHandle = InvalidSerialHandle then begin
    WriteLn(stderr, 'Device ' + portName + ' cannot be opened');
    exit(2)                             (* Cannot be opened                     *)
  end;
  if debugLevel > 0 then
    WriteLn(stderr, '# Using port ', portName);
  result := RunConsoleApp2(portHandle, dontStop);
  case result of
    3: WriteLn(stderr, 'No data waiting for sync byte');
    4: WriteLn(stderr, 'No data reading message');
    5: WriteLn(stderr, 'Error formatting message')
  otherwise
  end
end { RunConsoleApp };


initialization
  Assert(SizeOf(smallint) = 2)
end.

