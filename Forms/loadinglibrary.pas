unit LoadingLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, UITypes;

type

  { TLoadLib }

  TLoadLib = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Status_Text: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private

  public

  end;

var
  LoadLib: TLoadLib;

implementation

{$R *.lfm}

{ TLoadLib }

procedure TLoadLib.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrNone then
    CanClose := false;
end;

end.

