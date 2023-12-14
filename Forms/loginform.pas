unit loginform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, helpform, BroadcastAPI;

type

  { TLogin }

  TLogin = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Login_AppToken: TEdit;
    Login_AppID: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Popup_Extra: TPopupMenu;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Separator1: TMenuItem;
    procedure Button1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private

  public

  end;

var
  Login: TLogin;

implementation

{$R *.lfm}

{ TLogin }

procedure TLogin.MenuItem2Click(Sender: TObject);
begin
  Help := THelp.Create(Self);
  with Help do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TLogin.FormCreate(Sender: TObject);
begin
  Login_AppID.Text := APPLICATION_ID;
end;

procedure TLogin.MenuItem1Click(Sender: TObject);
begin
  Panel2.Visible:=TMenuItem(Sender).Checked;
end;

procedure TLogin.Button1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  with TButton(Sender) do
    begin
      P := Point(0, Height);
      P := ClientToScreen(P);
    end;

  Popup_Extra.PopUp(P.X, P.Y);
end;

procedure TLogin.Button2Click(Sender: TObject);
begin
  if Login_AppToken.Text = '' then
    begin
      MessageDlg('Please enter a valid login token', mtWarning, [mbOk], 0);
      Exit;
    end;

  ModalResult := mrOk;
end;

procedure TLogin.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
    ModalResult := mrClose;
end;

end.
