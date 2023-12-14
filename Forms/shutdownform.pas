unit shutdownform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { Tshutdown }

  Tshutdown = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ProgressBar1: TProgressBar;
    Status_Text: TLabel;
  private

  public

  end;

var
  shutdown: Tshutdown;

implementation

{$R *.lfm}

end.

