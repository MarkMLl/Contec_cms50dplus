(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit ConsoleApp;

(* This is the greater part of a console program which reads live data from a   *)
(* Contec CMS50D+ pulse oximeter (firmware v4.6, 9-byte messages @115k) and     *)
(* sends it to stdout.                                          MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial;

type
  TOperation= (opLive, opPlayback, opQuery, opQueryVendor, opQueryModel,
                                                        opQueryDevice, opQueryInfo);
  TWriteLn= procedure(const s: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const portName: string; portScan: boolean= false);

(* This is the inner loop of the main function. If the final parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; op: TOperation;
                        var pleaseStop: boolean; writer: TWriteLn= nil): integer;

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
  WriteLn('  --playback     Recovers recorded rather than live data.');
  WriteLn();
  WriteLn('  --query        Get model identifier in place of live or playback data.');
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

var
  lineCounter: integer= 0;


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
//        Write(LowerCase(IntToHex(I * 16, 4)), ' ');

// HACK: We know that in the current case we're dealing with short records, so
// it is more useful to emit a line number than a hex address.

        lineCounter += 1;
        Write(lineCounter:4, ' ');

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
  Tmessage9= array[0..8] of byte;


(* Output either as a data dump (format is @ by itself) or with C-style
  formatting applied to each byte or word.
*)
function outputFormattedRaw(message: TMessage9; const formatString: string): boolean;

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
function outputFormatted(message: Tmessage9; const formatString: string): boolean;

var
  value: double;

begin
  if formatString = '%%%%%%%%' then
    lineCounter := 0
  else
    if (formatString = '') or (Pos('@', formatString) > 0) then
      result := outputFormattedRaw(message, formatString)
    else begin
      value := message[6];
      result := outputFormattedRescaled(value, formatString)
    end
end { outputFormatted } ;


(* Check that the first parameter is a time followed by six values and a text
  string, return false if not. Split the parameter into three, advancing the
  time on the 2nd and 3rd by a second.
*)
function splitPlayback(const s: string; out sec0, sec1, sec2: string): boolean;

var
  baseTime: string;
  baseSecs, secs, mins, hrs: integer;


  function occur(const s: string; c: char): integer; inline;

  var
    i: integer;

  begin
    result := 0;
    for i := 1 to Length(s) do
      if s[i] = c then
        result += 1
  end { occur } ;


begin
  if occur(s, '|') <> 7 then
    exit(false);
  baseTime := Trim(ExtractWord(1, s, ['|']));
  if occur(baseTime, ':') <> 2 then
    exit(false);
  result := true;
  try
    hrs := StrToInt(Copy(baseTime, 1, 2));
    mins := StrToInt(Copy(baseTime, 4, 2));
    secs := StrToInt(Copy(baseTime, 7, 2));
    baseSecs := secs + (mins * 60) + hrs * (24 * 60);
    sec0 := baseTime + ' |';
    sec0 += ExtractWord(2, s, ['|']) + '|';
    sec0 += ExtractWord(3, s, ['|']) + '|';
    sec0 += ExtractWord(8, s, ['|']);
    secs := baseSecs + 1;
    mins := secs div 60;
    secs := secs mod 60;
    hrs := mins div 60;
    mins := mins mod 60;
    sec1 := Format('%02d:%02d:%02d', [hrs, mins, secs]);
    sec1 := ReplaceStr(sec1, ' ', '0') + ' |'; (* Don't trust FPC to obey leading 0 *)
    sec1 += ExtractWord(4, s, ['|']) + '|';
    sec1 += ExtractWord(5, s, ['|']) + '|';
    sec1 += ExtractWord(8, s, ['|']);
    secs := baseSecs + 2;
    mins := secs div 60;
    secs := secs mod 60;
    hrs := mins div 60;
    mins := mins mod 60;
    sec2 := Format('%02d:%02d:%02d', [hrs, mins, secs]);
    sec2 := ReplaceStr(sec2, ' ', '0') + ' |'; (* Don't trust FPC to obey leading 0 *)
    sec2 += ExtractWord(6, s, ['|']) + '|';
    sec2 += ExtractWord(7, s, ['|']) + '|';
    sec2 += ExtractWord(8, s, ['|'])
  except
    result := false
  end
end { splitPlayback } ;


(* Format a 9-byte message into text, using the scaling etc. provided by the
  meter.
*)
function autoFormattedPlayback(message: Tmessage9; wrapUp: boolean= false): string;

(* Playback data looks something like                                           *)
(*                                                                              *)
(* 0 0f                 Sync, always 0x0f                                       *)
(* 1 80                 Always 8x80                                             *)
(* 2 0xe2 -> 0x62 98    First SpO2                                              *)
(* 3 0xbf -> 0x3f 63    First pulse rate                                        *)
(* 4 0xe2 -> 0x62 98    Second SpO2                                             *)
(* 5 0xbf -> 0x3f 63    Second pulse rate                                       *)
(* 6 0xe2 -> 0x62 98    Third SpO2                                              *)
(* 7 0xbf -> 0x3f 63    Third pulse rate                                        *)
(* 8 0x00               Padding                                                 *)

var
  secs, mins, hrs: integer;
  scratch: string;

begin
  if (message[0] <> $0f) or (message[8] <> $00) then
    exit('');                           (* Bad sync byte                        *)
  secs := lineCounter * 3;
  lineCounter += 1;
  mins := secs div 60;
  secs := secs mod 60;
  hrs := mins div 60;
  mins := mins mod 60;
  result := Format('%02d:%02d:%02d', [hrs, mins, secs]);
  result := ReplaceStr(result, ' ', '0'); (* Don't trust FPC to obey leading 0  *)
  result += ' |';

(* Pulse rate and SpO2 for three seconds.                                       *)

  Str((message[3] and $7f):3, scratch);
  result += scratch + '|';
  Str((message[2] and $7f):3, scratch);
  result += scratch + '|';

  Str((message[5] and $7f):3, scratch);
  result += scratch + '|';
  Str((message[4] and $7f):3, scratch);
  result += scratch + '|';

  Str((message[7] and $7f):3, scratch);
  result += scratch + '|';
  Str((message[6] and $7f):3, scratch);
  result += scratch + '|';
  if wrapUp then
    result += 'No reading'
  else
    result += 'OK'
end { autoFormattedPlayback } ;


(* Format a 9-byte message into text, using the scaling etc. provided by the
  meter.
*)
function autoFormattedLive(message: Tmessage9; wrapUp: boolean= false): string;

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
(* Live data looks something like                                               *)
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
end { autoFormattedLive } ;


(* Format a 9-byte message into text, using the scaling etc. provided by the
  meter.
*)
function autoFormatted(message: Tmessage9; wrapUp: boolean= false): string;

begin
  case message[0] of
    $01: result := autoFormattedLive(message, wrapup);
    $0f: result := autoFormattedPlayback(message, wrapup)
  else
    result := ''
  end
end { autoFormatted } ;


var
  debugLevel: integer= 0;
  formatString: string= '';
  onceOnly: boolean= false;


(* This is a cut-down variant of RunConsoleApp2() which doesn't loop and
  doesn't close the port on completion. It is primarily used for trying to
  query device characteristics, and as such outputs direct to the console.
*)
function runConsoleApp3(portHandle: TSerialHandle; op: TOperation): integer;

const
  QueryVendor: Tmessage9= ($7d, $81, $a9, $80, $80, $80, $80, $80, $80);
  QueryModel: Tmessage9= ($7d, $81, $a8, $80, $80, $80, $80, $80, $80);
  QueryDevice: Tmessage9= ($7d, $81, $aa, $80, $80, $80, $80, $80, $80);
  QueryInfo: Tmessage9= ($7d, $81, $b0, $80, $80, $80, $80, $80, $80);

type
  Tmessage18= array[0..17] of byte;

var
  i, msgTop: integer;
  message: Tmessage9;
  response: Tmessage18;


  procedure writeLnStripped(topByte: integer);

  const
    lowByte= 2;

  var
    i: integer;

  begin
    for i := lowByte to topByte do
      case response[i] and $7f of
        $20..$7e: Write(Chr(response[i] and $7f))
      otherwise
        Write(' ')
      end;
    WriteLn('"')
  end { writeLnStripped } ;


begin
  case op of
    opQueryVendor:  begin
                      msgTop := 8;
                      message := queryVendor
                    end;
    opQueryModel:   begin
                      msgTop := 17;
                      message := queryModel
                    end;
    opQueryDevice:  begin
                       msgTop := 8;
                       message := queryDevice
                     end;
    opQueryInfo:    begin
                      msgTop := 8;
                      message := queryInfo
                    end
  otherwise
    exit(5)
  end;
  SerWrite(portHandle, message, 9);
  if SerReadTimeout(portHandle, response[0], 5000) = 0 then
    exit(3);                            (* Unresponsive                         *)
  case op of
    opQueryVendor: if response[0] <> $03 then
                     exit(4);
    opQueryModel:  if response[0] <> $02 then
                     exit(4);
    opQueryDevice: if response[0] <> $04 then
                     exit(4);
    opQueryInfo:   if response[0] <> $11 then
                     exit(4)
  otherwise
    exit(4)
  end;

(* Anything that isn't a sync byte has been discarded. Read the remainder of    *)
(* the message.                                                                 *)

  for i := 1 to msgTop do
    if SerReadTimeout(portHandle, response[i], 100) = 0 then
      exit(4);
  if debugLevel > 1 then begin
    Write(stderr, '#');
    for i := 0 to msgTop do
      Write(stderr, ' ' + HexStr(response[i], 2));
    WriteLn(stderr)
  end;
  case op of
    opQueryVendor: Write('Vendor: "');
    opQueryModel:  Write('Model:  "');
    opQueryDevice: Write('Device: "');
    opQueryInfo:   Write('Info:   "');
  otherwise
    exit(0)
  end;
  writeLnStripped(msgTop);
  result := 0
end { runConsoleApp3 } ;


(* This is the inner loop of the main function. If the final parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; op: TOperation;
                        var pleaseStop: boolean; writer: TWriteLn= nil): integer;

const
  startLive: Tmessage9= ($7d, $81, $a1, $80, $80, $80, $80, $80, $80);
  startPlayback: Tmessage9= ($7d, $81, $a6, $80, $80, $80, $80, $80, $80);

var
  i, lastI, msgTop: integer;
  waitingFirstSync: boolean;
  resync: jmp_buf;
  message: Tmessage9;
  scratch, sec0, sec1, sec2: string;

begin
  try
    SerSetParams(portHandle, 115200, 8, NoneParity, 1, []);
    case op of
      opLive:     begin
                    msgTop := 8;
                    message := startLive
                  end;
      opPlayback: begin
                    msgTop := 7;
                    message := startPlayback
                  end;
      opQuery:    begin
                    result := runConsoleApp3(portHandle, opQueryVendor);
                    result := runConsoleApp3(portHandle, opQueryModel);
                    result := runConsoleApp3(portHandle, opQueryDevice);
                    result := runConsoleApp3(portHandle, opQueryInfo);
                    exit
                  end
    otherwise
      exit(0)
    end;
    lastI := SetJmp(resync);
    SerWrite(portHandle, message, 9);
    waitingFirstSync := true;
    outputFormatted(message, '%%%%%%%%'); (* Initialise counters etc. as needed *)
    if Assigned(writer) then
      writer('%%%%%%%%');
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
      case op of
        opLive:        if message[0] <> $01 then   (* 0x01 for live data, 0x0f for logged  *)
                         continue;
        opPlayback:    if message[0] <> $0f then
                         continue
      otherwise
        halt
      end;

(* Anything that isn't a sync byte has been discarded. Read the remainder of    *)
(* the message.                                                                 *)

      for i := 1 to msgTop do
        if SerReadTimeout(portHandle, message[i], 100) = 0 then
          LongJmp(resync, i + 1);
      if debugLevel > 1 then begin
        Write(stderr, '#');
        for i := 0 to msgTop do
          Write(stderr, ' ' + HexStr(message[i], 2));
        WriteLn(stderr)
      end;
      if msgTop = 7 then
        message[8] := $00;
      if Assigned(writer) then begin
        scratch := autoFormatted(message); (* Looks at sync byte                *)
        if scratch = '' then
          exit(5);                      (* Format error                         *)
        case op of
          opPlayback: if splitPlayback(scratch, sec0, sec1, sec2) then begin
                        writer(sec0);
                        writer(sec1);
                        writer(sec2)
                      end else
                        exit(5)         (* Format error                         *)
        otherwise
          writer(scratch)
        end
      end else

(* We have a 9- or 8-byte message. If there is an explicit format string then   *)
(* apply it, otherwise just try to do the right thing.                          *)

        if formatString <> '' then
          if not outputFormatted(message, formatString) then
            exit(5)                     (* Format error                         *)
          else begin end
        else begin
          scratch := autoFormatted(message);
          if scratch = '' then
            exit(5);                    (* Format error                         *)
          case op of
            opPlayback: if splitPlayback(scratch, sec0, sec1, sec2) then begin
                          WriteLn(sec0);
                          WriteLn(sec1);
                          WriteLn(sec2)
                        end else
                          exit(5)       (* Format error                         *)
          otherwise
            WriteLn(scratch)
          end
        end;
      if onceOnly then                  (* Debugging option, -ve level          *)
        break
    until pleaseStop { Dave }           (* Or signal from keyboard              *)
  finally
    if Assigned(writer) then begin
      scratch := autoFormatted(message, true); (* New time plus last data       *)
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
  cmd: TOperation= opLive;
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
      '--playback',
      '--download':   cmd := opPlayback;
      '--info',
      '--query':      cmd := opQuery;
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
  result := RunConsoleApp2(portHandle, cmd, dontStop);
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


{$ifdef uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu }

Lots of people mention the live-data start command, usually without commenting
on the 30-sec limit. The only place I've seen anything about the playback is

https://github.com/grantzvolsky/cms-50d-plus-cli/blob/master/hack/download.py

although plugging that string into Google gives me command lists at

https://github.com/tobac/cms50ew/blob/master/cms50ew/cms50ew.py
https://github.com/maka89/cms50ew_datadump/blob/master/cms50ew.py

which look far more comprehensive. There's also documentation of the CMS50EW
at https://github.com/vats28/Contec-CMS50EW/blob/master/Communication%20protocol%20of%20pulse%20oximeter%20V7.0.pdf
but I don't know how reliable it is.

Below is from USB capture, I'll look at the above presently.

// The commands appear as URB_BULK out messages, I don't know whether there are
// additional messages which manipulate the serial control lines.

This sequence at start of connect to endpoint 0x02
7d 81 a2 80 80 80 80 80 80
7d 81 a7 80 80 80 80 80 80
7d 81 a8 80 80 80 80 80 80
7d 81 a9 80 80 80 80 80 80
7d 81 aa 80 80 80 80 80 80
7d 81 b0 80 80 80 80 80 80

Response from 0x82
0c 80

Response from 0x82
02 80 80 b5 b0 c4 a8 c1 a9
02 81 ff ab a0 a0 a0 a0 a0
03 80 a0 a0 a0 a0 a0 a0 a0
04 80 f5 f3 e5 f2 a0 a0 a0
0c 80 11 80 81 81 80 80 80
80 80

To 0x02
7d 81 a7 80 80 80 80 80 80

From 0x82
0c 80

To 0x02
7d 81 a2 80 80 80 80 80 80

From 0x82
0c 80

To 0x02
7d 81 a0 80 80 80 80 80 80

From 0x82
06 80 80 87

To 0x02
7d 81 b0 80 80 80 80 80 80

From 0x82
11 80 81 81 80 80 80 80 80

To 0x02
7d 81 ac 80 80 80 80 80 80

From 0x82
0e 80 81

To 0x02
7d 81 b3 80 80 80 80 80 80

From 0x82
13 80 80 80 80 80 80 80 80
14 80 80 80 80 80 80 80 80

To 0x02
7d 81 a8 80 80 80 80 80 80

From 0x02
02 80 80 b5 b0 c4 a8 c1 a9
02 81 ff ab a0 a0 a0 a0 a0

To 0x02
7d 81 aa 80 80 80 80 80 80

From 0x82
04 80 f5 f3 e5 f2 a0 a0 a0

To 0x02
7d 81 a9 80 80 80 80 80 80

From 0x82
03 80 a0 a0 a0 a0 a0 a0 a0

Request idling resonse?

To 0x02
7d 81 a1 80 80 80 80 80 80

Idling responses.

From 0x82
01 e1 84 c0 90 80 80 ff ff
01 e1 84 c0 90 80 80 ff ff
01 e1 84 c0 90 80 80 ff ff
...

This is a fairly definite shutdown sequence. I've seen it sent as two messages
with two 0c 08 responses.

To 0x02: Shutdown?
7d 81 a7 80 80 80 80 80 80
7d 81 a2 80 80 80 80 80 80

From 0x82
0c 80

Different session:

To 0x02
7d 81 af 80 80 80 80 80 80

From 0x82
01 e1 84 c0 90 80 80 ff ff

To 0x02
7d 81 a6 80 80 80 80 80 80

From 0x82

3232
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80

3233
0f 80 e2 c6 e2 c6 e2 c6
0f 80 e2 c6 e2 c6 e2 c6
0f 80 e2 c7 e2 c7 e2 c7
0f 80 e2 c6 e2 c6 e2 c5
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e2 c5 e2 c5 e2 c5

3237
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e2 c5 e2 c5 e2 c5
0f 80 e1 c5 e1 c5 e1 c5
0f 80 e1 c5 e1 c5 e1 c5
0f 80 e1 c5 e1 c5 e1 c5

...

A further 150 blocks here.

Program reports data was collected for 1 hour 2 mins.

Each full block is 8 messages, hence total is (154 x 8) + 5 = 1237
messages or 20 messages (i.e. readings logged) a minute.

For 24 hours assuming no compression that would be 225Kb

...

3470
0f 80 e2 c2 e2 c2 e2 c2
0f 80 e2 c2 e2 c2 e2 c2
0f 80 e2 c4 e2 c5 e2 c5
0f 80 e2 c5 e2 c6 e2 c6
0f 80 e2 c6 e2 c6 e2 c6
0f 80 e2 c6 e2 c6 e2 c6
0f 80 e2 c6 e2 c5 e2 c5
0f 80 e1 c5 e1 c4 e1 c4

3471
0f 80 e1 c4 e1 c4 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80
0f 80 80 80 80 80 80 80

self.cmd_hello1 = b'\x7d\x81\xa7\x80\x80\x80\x80\x80\x80'
self.cmd_hello2 = b'\x7d\x81\xa2\x80\x80\x80\x80\x80\x80'
self.cmd_hello3 = b'\x7d\x81\xa0\x80\x80\x80\x80\x80\x80'
self.cmd_session_hello = b'\x7d\x81\xad\x80\x80\x80\x80\x80\x80'
self.cmd_get_session_count = b'\x7d\x81\xa3\x80\x80\x80\x80\x80\x80'
self.cmd_get_session_time = b'\x7d\x81\xa5\x80\x80\x80\x80\x80\x80'
self.cmd_get_session_duration = b'\x7d\x81\xa4\x80\x80\x80\x80\x80\x80'
self.cmd_get_user_info = b'\x7d\x81\xab\x80\x80\x80\x80\x80\x80'
self.cmd_get_session_data = b'\x7d\x81\xa6\x80\x80\x80\x80\x80\x80'
self.cmd_get_deviceid = b'\x7d\x81\xaa\x80\x80\x80\x80\x80\x80'
self.cmd_get_info = b'\x7d\x81\xb0\x80\x80\x80\x80\x80\x80'
self.cmd_get_model = b'\x7d\x81\xa8\x80\x80\x80\x80\x80\x80'
self.cmd_get_vendor = b'\x7d\x81\xa9\x80\x80\x80\x80\x80\x80'
self.cmd_session_erase = b'\x7d\x81\xae\x80\x80\x80\x80\x80\x80'
self.cmd_custom = b'\x7d\x81\xf5\x80\x80\x80\x80\x80\x80'
self.cmd_session_stuff = b'\x7d\x81\xaf\x80\x80\x80\x80\x80\x80'
self.cmd_get_live_data = b'\x7d\x81\xa1\x80\x80\x80\x80\x80\x80'

{$endif }

