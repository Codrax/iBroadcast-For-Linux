unit iteminformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ExtDlgs, taskexecution, MainUI, BroadcastAPI, ratingform;

type

  { TItemInfo }

  TItemInfo = class(TForm)
    Button2: TButton;
    SavePictureDialog1: TSavePictureDialog;
    Save_Button: TButton;
    Item_Downloaded: TButton;
    Item_Name: TEdit;
    Item_Rating: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Item_Description: TMemo;
    Item_Image: TImage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Cover_Save: TPopupMenu;
    Rate_Box: TPanel;
    procedure Cover_SavePopup(Sender: TObject);
    procedure Item_DescriptionChange(Sender: TObject);
    procedure Item_DownloadedClick(Sender: TObject);
    procedure Item_RatingClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Save_ButtonClick(Sender: TObject);
    procedure ValueChange(Sender: TObject);
  private
    ChangedName: boolean;
    ChangedDescription: boolean;
    ChangedRating: boolean;
    RateChanged: integer;

    procedure ChangesMade;

  public
    Item: ^TDrawItem;

    procedure LoadDownload;

    procedure LoadInformation;
    procedure Prepare;

  end;

var
  ItemInfo: TItemInfo;

implementation

{$R *.lfm}

{ TItemInfo }

procedure TItemInfo.Save_ButtonClick(Sender: TObject);
var
  Success: boolean;
begin
  Success := false;

  // Playlist
  if ChangedName or ChangedDescription then
    with TDialogPlaylistUpdate.Create do
      begin
        ItemID := Item^.ItemID;

        ChangeName := ChangedName;
        ChangeDescription := ChangedDescription;

        if ChangeName then
          NewName := Item_Name.Text;

        if ChangeDescription then
          NewDescription := string(Item_Description.Text).Replace(#13, '');

        Start;
      end;

  // Rating
  if ChangedRating then
    with TDialogUpdateRating.Create do
      begin
        ItemSource := Item^.Source;
        ItemID := Item^.ItemID;

        NewRating := RateChanged;

        Start;
      end;

  // Exec (only one task can be done playlist / (album+track))
  if TaskExec.ShowModal = mrOk then
    begin
      // Re-prep
      Prepare;
    end
  else
    begin
      Success := false;
      ShowMessage('Could not apply changes.');
    end;


  // Reload data
  if Success then
    begin
      // Load from Index
      Item^.ReloadSource;

      // UI
      LoadInformation;
    end;
end;

procedure TItemInfo.Cover_SavePopup(Sender: TObject);
begin
  MenuItem2.Visible := Item^.Source in [TDataSource.Tracks, TDataSource.Artists,
    TDataSource.Playlists];
end;

procedure TItemInfo.Item_DescriptionChange(Sender: TObject);
begin
  if not ChangedDescription then
    begin
      Item_Description.Text := Playlists[Item^.Index].Description;

      Item_Description.SelStart:=length(Item_Description.Text);
    end;

  ChangesMade;
  ChangedDescription := true;
end;

procedure TItemInfo.Item_DownloadedClick(Sender: TObject);
begin
  Item^.ToggleDownloaded;

  LoadDownload;
end;

procedure TItemInfo.Item_RatingClick(Sender: TObject);
begin
  Rating := Trating.Create(Self);
  with Rating do
    try
      RateValue := RateChanged;

      with TButton(Sender) do
        PointOpen := ClientToScreen(Point(Width div 2, Height));

      if ShowModal = mrOk then
        begin
          RateChanged:=NewRating;
          ChangesMade;
          ChangedRating := true;

          Item_Rating.Caption:=RatingToString(RateChanged);
        end;
    finally
      Free;
    end;
end;

procedure TItemInfo.MenuItem1Click(Sender: TObject);
begin
  // Save
  SavePictureDialog1.FileName := Item^.Title;

  if SavePictureDialog1.Execute then
    if TMenuItem(Sender).Tag = 0 then
      Item^.GetPicture.SaveToFile(SavePictureDialog1.FileName)
    else
      Item^.GetLargePicture.SaveToFile(SavePictureDialog1.FileName);

end;

procedure TItemInfo.ValueChange(Sender: TObject);
begin
  ChangesMade;
  ChangedName := true;
end;

procedure TItemInfo.ChangesMade;
begin
  Save_Button.Enabled:=true;
end;

procedure TItemInfo.LoadDownload;
begin
  if Item^.Downloaded then
    Item_Downloaded.Caption:='✓ Downloaded'
  else
    Item_Downloaded.Caption:='Download';
end;

procedure TItemInfo.LoadInformation;
begin
  // Set data
  Item_Name.Caption := Item^.Title;
  Item_Description.Text := Item^.GetPremadeInfoList;
  RateChanged := Item^.Rating;
  Item_Rating.Caption := RatingToString(Item^.Rating);
end;

procedure TItemInfo.Prepare;
var
  PlaylistUpdate: boolean;
begin
  // UI
  Caption := Format('Information about "%S"', [Item^.Title]);

  // Playlist editing
  PlaylistUpdate := false;
  if (Item^.Source = TDataSource.Playlists) and not IsOffline then
    begin
      PlaylistUpdate := Playlists[Item^.Index].PlaylistType = '';
    end;

  Item_Name.ReadOnly := not PlaylistUpdate;
  Item_Description.ReadOnly := Item_Name.ReadOnly;

  // Buttons
  Item_Downloaded.Enabled:=not IsOffline;
  Item_Rating.Enabled:=not IsOffline;

  MenuItem2.Enabled:=not IsOffline;

  // Rating
  Rate_Box.Visible := Item^.Source in [TDataSource.Tracks, TDataSource.Albums];

  // Download
  LoadDownload;

  // Status
  ChangedName := false;
  ChangedDescription := false;
  ChangedRating := false;

  Save_Button.Enabled:=false;
end;

end.

