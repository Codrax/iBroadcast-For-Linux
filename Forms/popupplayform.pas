unit PopupPlayForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SpectrumVis3D, MainUI;

type

  { TPopupPlay }

  TPopupPlay = class(TForm)
    Button1: TButton;
    Music_Artist: TLabel;
    Music_Artwork: TImage;
    Music_Name: TLabel;
    Music_Next: TButton;
    Music_Play: TButton;
    Music_Prev: TButton;
    Music_Time: TLabel;
    Panel7: TPanel;
    Visualisation_Player: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ButtonsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Visualisation_PlayerPaint(Sender: TObject);
  private

  public

  end;

var
  PopupPlay: TPopupPlay;

  // Visualisations
  Spectrum_Popup: TSpectrum;

implementation

{$R *.lfm}

{ TPopupPlay }

procedure TPopupPlay.FormCreate(Sender: TObject);
begin
  // Spectrum
  Spectrum_Popup := TSpectrum.Create(round(Visualisation_Player.Width * ScaleFactor), round(Visualisation_Player.Height * ScaleFactor));
  Spectrum_Popup.Height := Visualisation_Player.Height - 20;
  Spectrum_Popup.Peak := clMenuText;
  Spectrum_Popup.Pen:= RGBToColor(255, 105, 180);
  Spectrum_Popup.BackColor := clWindow;
end;

procedure TPopupPlay.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TPopupPlay.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  PopupPlay := nil;

  // Show Main
  Main.Show;
end;

procedure TPopupPlay.ButtonsClick(Sender: TObject);
var
  C: TControl;
begin
  C := Main.Panel5.FindChildControl(TComponent(Sender).Name);

  if C <> nil then
    C.OnClick(C);
end;

procedure TPopupPlay.FormShow(Sender: TObject);
begin
  if not Visualisation_Player.Visible then
    ClientHeight:=Panel7.BoundsRect.Bottom;
end;

procedure TPopupPlay.Visualisation_PlayerPaint(Sender: TObject);
var
  Style: TTextStyle;
  ARect: TRect;
begin
  with TPaintBox(Sender).Canvas do begin
    ARect := TPaintBox(Sender).ClientRect;

    Style.Alignment:=taCenter;
    Style.Layout:=tlCenter;
    Style.Wordbreak := true;

    with TPaintBox(Sender).Canvas do begin
      Font.Assign(Self.Font);
      TextRect(ARect, 0,0, 'Play to show visualistions.', Style);
    end;
  end;
end;

end.

