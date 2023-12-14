unit helpform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, ExtendedTabControls, LCLIntf;

type

  { THelp }

  THelp = class(TForm)
    Help_General: TScrollBox;
    Help_Login: TScrollBox;
    Help_Playback: TScrollBox;
    Help_Downloads: TScrollBox;
    Help_Keyboard: TScrollBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Page_Title: TLabel;
    Panel11: TPanel;
    Container: TPanel;
    Help_Template: TScrollBox;
    TabControl1: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private

  public

  end;

var
  Help: THelp;

implementation

{$R *.lfm}

{ THelp }

procedure THelp.FormCreate(Sender: TObject);
begin
  TabControl1.TabIndex:=0;
  Help_General.Show;
end;

procedure THelp.Label6Click(Sender: TObject);
begin
  OpenURL('https://github.com/Codrax/iBroadcast-For-Linux');
end;

procedure THelp.TabControl1Change(Sender: TObject);
var
  I: integer;
begin
  with Container do
  for I := 0 to ControlCount-1 do
    if Controls[I] is TScrollBox then
      (Controls[I] as TScrollBox).Hide;

  case TabControl1.TabIndex of
    0: Help_General.Show;
    1: Help_Login.Show;
    2: Help_Playback.Show;
    3: Help_Downloads.Show;
    4: Help_Keyboard.Show;
  end;
end;

end.

