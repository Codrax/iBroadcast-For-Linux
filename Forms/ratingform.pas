unit ratingform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BroadcastAPI, Types;

type

  { Trating }

  Trating = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Rate_ThumbsUp: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Rate_ThumbsDown: TButton;
    procedure SelectRate(Sender: TObject);
    procedure ButtonHoverOn(Sender: TObject);
    procedure ButtonApplyColor(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    PointOpen: TPoint;

    RateValue: integer;
    NewRating: integer;
  end;

var
  rating: Trating;

implementation

{$R *.lfm}

{ Trating }

procedure Trating.ButtonHoverOn(Sender: TObject);
begin
  // Nice
  with TButton(Sender) do
    Font.Color := $000067FF;
end;

procedure Trating.SelectRate(Sender: TObject);
begin
  NewRating := TButton(Sender).Tag;
  ModalResult := mrOk;

  // Same
  if NewRating = RateValue then
    begin
      ModalResult := mrAbort;

      Close;
      Exit;
    end;

  // Close
  CloseQuery;
end;

procedure Trating.ButtonApplyColor(Sender: TObject);
begin
  // Not nice
  with TButton(Sender) do
    if Tag <= RateValue then
      Font.Color := clYellow
    else
      Font.Color := clDefault;
end;

procedure Trating.FormCreate(Sender: TObject);
var
  I: integer;
begin
  if ValueRatingMode then
    // Number rating
    begin
      Rate_ThumbsUp.Hide;
      Rate_ThumbsDown.Hide;
    end
  else
    // Like / dislike
    begin
      for I := 0 to ControlCount-1 do
        if Controls[I] is TButton then
          with Controls[I] as TButton do
            if Tag > 0 then
              Hide;

      Rate_ThumbsUp.Show;
      Rate_ThumbsDown.Show;
    end;
end;

procedure Trating.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure Trating.FormShow(Sender: TObject);
var
  I: integer;
begin
  // Prep
  for I := 0 to ControlCount-1 do
    begin
      if not (Controls[I] is TButton) then
        Continue;

      if TButton(Controls[I]).Tag > 0 then
        ButtonApplyColor( Controls[I] );
    end;

  // Pos
  Top:=PointOpen.Y;
  Left:=PointOpen.X - Rating.Width div 2;
end;

end.

