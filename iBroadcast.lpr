program iBroadcast;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  LibDefine,
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, mainui, indylaz, dialogs,
  { you can add units after this }
  BroadcastAPI, SpectrumVis3D, uniqueinstanceraw, LoadingLibrary, About,
  taskexecution, iteminformation, ratingform, loginform, helpform,
  createplaylistform, PopupPlayForm;

{$R *.res}

var
  Param: string;
  I: integer;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;

  // Instance
  if InstanceRunning('iBroadcast-for-Linux') then
    begin
      ShowMessage('A instance of iBroadcast is already running!');
      Exit;
    end;

  // Params
  for I := 1 to ParamCount do
    begin
      Param := ParamStr(I);

      if Param = '--debug' then
        DebugMode := true;

      if Param = '--offline' then
        IsOffline := true;

      if Param = '--help' then
        begin
            Help := THelp.Create(nil);
            with Help do
              try
                ShowModal;
              finally
                Free;
              end;
            Halt;
        end;
    end;


  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

