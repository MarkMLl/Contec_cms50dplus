(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

program cms50dplus;

(* Read live data from a Contec CMS50D+ pulse oximter (firmware v4.6, 9-byte    *)
(* messages @115k), either as a console program or with a GUI.  MarkMLl.        *)

{$mode objfpc}{$H+}

uses
{$ifdef LCL }
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, cms50dpluscode,
  { you can add units after this }
{$endif LCL }
  ConsoleApp, locatecp210xport;

var
  CMS50DPlusPort: string= '';
  scanPorts: boolean= false;
  i: integer;

{$R *.res}

begin
  for i := 1 to ParamCount() do
    if Pos('-ports', LowerCase(ParamStr(i))) <> 0 then
      scanPorts := true;
  for i := 1 to ParamCount() do
    if LowerCase(ParamStr(i)) = '--version' then begin
      DoVersion('CMS50D+');
      Halt(0)
    end;
  CMS50DPlusPort := FindMs2115bPort(scanPorts);  (* Builds cached ports list        *)
  for i := 1 to ParamCount() do
    if LowerCase(ParamStr(i)) = '--help' then begin
      DoHelp(CMS50DPlusPort, scanPorts);
      Halt(0)
    end;
{$ifdef LCL }
  if ParamCount() > 0 then  (* If GUI is available, activated by no parameter   *)
{$endif LCL }
    Halt(RunConsoleApp(CMS50DPlusPort));

(* The objective here is to minimise the amount of manually-inserted text so as *)
(* to give the IDE the best chance of managing form names etc. automatically. I *)
(* try, I don't always succeed...                                               *)

{$ifdef LCL }
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCMS50DPlusForm, CMS50DPlusForm);
  CMS50DPlusForm.DefaultPort := CMS50DPlusPort;
  Application.Run;
{$endif LCL }
end.

