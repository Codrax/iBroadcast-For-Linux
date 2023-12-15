unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, lclintf, BroadcastAPI, Math;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    App_Version_Label: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo_Credits: TMemo;
    Memo_License: TMemo;
    PageControl1: TPageControl;
    Content_Cover: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure LinkLeave(Sender: TObject);
    procedure LinkEnter(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Enter(Sender: TObject);
  private

  public

  end;

var
  AboutDialog: TAboutDialog;

implementation

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.LinkEnter(Sender: TObject);
begin
   TLabel(Sender).Font.Color:=clActiveCaption;
end;

procedure TAboutDialog.PageControl1Change(Sender: TObject);
const
  SPEED = 1;
var
  DestCredits,
  DestLicense: integer;
  MaxT: integer;
begin
  MaxT := App_Version_Label.Top;

  DestCredits := ClientHeight;
  DestLicense := ClientHeight;

  case PageControl1.ActivePageIndex of
    1: DestCredits := MaxT;
    2: begin
      DestCredits := MaxT;
      DestLicense := MaxT;
    end;
  end;

  repeat
    Sleep(2);
    Memo_Credits.Top := Memo_Credits.Top + Sign(DestCredits-Memo_Credits.Top) * Speed;
    Memo_License.Top := Memo_License.Top + Sign(DestLicense-Memo_License.Top) * Speed;

    Application.ProcessMessages;;
  until (Memo_Credits.Top = DestCredits) and (Memo_License.Top = DestLicense);
end;

procedure TAboutDialog.PageControl1Enter(Sender: TObject);
begin
  Content_Cover.SetFocus;
end;

procedure TAboutDialog.LinkLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Color:=clHighlight;
end;

procedure TAboutDialog.Label4Click(Sender: TObject);
begin
  OpenURL( TLabel(Sender).Hint );
end;

procedure TAboutDialog.FormCreate(Sender: TObject);
begin
  // Ver
  App_Version_Label.Caption := APP_VERSION.ToString;

  // Prep
  Memo_Credits.Top:=ClientHeight;
  Memo_License.Top:=ClientHeight;
end;

end.

