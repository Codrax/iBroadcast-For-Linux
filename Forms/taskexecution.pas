unit taskexecution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TTaskExec }

  TTaskExec = class(TForm)
    Title: TLabel;
    Label3: TLabel;
    Progress: TProgressBar;
  private

  public

  end;

var
  TaskExec: TTaskExec;

implementation

{$R *.lfm}

end.

