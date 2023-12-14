unit BroadcastAPI;

{$SCOPEDENUMS ON}
{$mode Delphi}

interface
  uses
    // Required Units
    SysUtils, Classes, Graphics, IdSSLOpenSSL,
    IdHTTP, fpjson, Clipbrd, DateUtils, Cod.Types, fileutil,
    Cod.VarHelpers, Cod.ArrayHelpers, Forms,
    jsonparser, Dialogs, Cod.VersionUpdate;

  type
    // Cardinals
    TArtSize = (Small, Medium, Large);
    TWorkItem = (DownloadingImage);
    TWorkItems = set of TWorkItem;

    // Source
    TDataSource = (None, Tracks, Albums, Artists, Playlists);
    TDataSources = set of TDataSource;

    // Loading
    TLoad = (Track, Album, Artist, PlayList);
    TLoadSet = set of TLoad;

    // Procs
    TDataTypeUpdate = procedure(AUpdate: TDataSource) of object;

    { TCollageMaker }
    TCollageMaker = class
    private
      TempResult: TJPEGImage;

      procedure Build;
    public
      Image1,
      Image2,
      Image3,
      Image4: TJPEGImage;

      function Make: TJPEGImage;
    end;

    { TSaveArtClass }

    TSaveArtClass = class
    private
      procedure SaveFile;
    public
      Image: TJPEGImage;
      FilePath: string;

      procedure Save;
    end;

    // Records
    ResultType = record
      Error: boolean;
      LoggedIn: boolean;
      ServerMessage: string;

      function Success: boolean;

      procedure TerminateSession;
      procedure AnaliseFrom(JSON: TJSONObject);
    end;

    TTrackHistoryItem = record
      TrackID: integer;
      TimeStamp: TDateTime;
    end;

    TLibraryStatus = record
      TotalTracks: integer;
      TotalPlays: integer;

      TokenExpireDate: TDateTime;
      LastLibraryModified: TDateTime;
      UpdateTimestamp: TDateTime;

      (* Loading *)
      procedure LoadFrom(JSON: TJSONObject);
    end;

    TAccount = record
      Username: string;
      OneQueue: boolean;
      BitRate: string;

      UserID: integer;
      CreationDate: TDateTime;

      Verified: boolean;
      BetaTester: boolean;

      EmailAdress: string;
      Premium: boolean;
      VerificationDate: TDateTime;

      (* Loading *)
      procedure LoadFrom(JSON: TJSONObject);
    end;

    { TTrackItem }

    TTrackItem = record
      (* Song properties in their JSON order, "?" is a unknown property *)
      ID: integer;

      TrackNumber: cardinal;

      Year: cardinal;
      Title: string;

      Genre: string;

      LengthSeconds: cardinal;
      AlbumID: cardinal;
      ArtworkID: string;
      ArtistID: cardinal;

      // ??? Some ID integer
      DayUploaded: TDate;
      IsInTrash: boolean;
      FileSize: integer;

      UploadLocation: string;
      // ??? empty string

      Rating: cardinal;
      Plays: cardinal;

      StreamLocations: string;
      AudioType: string;

      ReplayGain: string;
      UploadTime: TTime;
      // ??? Tag Array

      // Extra Data
      CachedImage,
      CachedImageLarge: TJpegImage;
      Status: TWorkItems;

      function GetPlaybackURL: string;

      (* Artwork *)
      function ArtworkLoaded(Large: boolean = false): boolean;
      function GetArtwork(Large: boolean = false): TJPEGImage;

      (* Loading *)
      procedure LoadFrom(JSONPair: TJSONData; AName: string);
    end;

    TAlbumItem = record
      (* Album properties in their JSON order, "?" is a unknown property *)
      ID: integer;

      AlbumName: string;

      TracksID: TIntArray;
      ArtistID: integer;

      IsInTrash: boolean;

      Rating: cardinal;
      Disk: cardinal;
      Year: cardinal;

      // ??? - Artist_aditional
      // ??? - ICatID

      CachedImage: TJpegImage;
      Status: TWorkItems;

      (* Artwork *)
      function ArtworkLoaded: boolean;
      function GetArtwork: TJPEGImage;

      (* Loading *)
      procedure LoadFrom(JSONPair: TJSONData; AName: string);
    end;

    TArtistItem = record
      (* Album properties in their JSON order, "?" is a unknown property *)
      ID: integer;

      ArtistName: string;

      TracksID: TIntArray;
      IsInTrash: boolean;

      Rating: cardinal;
      ArtworkID: string;

      // ??? - ICatID

      // Extra Data
      HasArtwork: boolean;

      CachedImage,
      CachedImageLarge: TJpegImage;
      Status: TWorkItems;

      (* Artwork *)
      function ArtworkLoaded(Large: boolean = false): boolean;
      function GetArtwork(Large: boolean = false): TJPEGImage;

      (* Loading *)
      procedure LoadFrom(JSONPair: TJSONData; AName: string);
    end;

    TPlaylistItem = record
      (* Album properties in their JSON order, "?" is a unknown property *)
      ID: integer;

      Name: string;

      TracksID: TIntArray;
      // ??? UID
      // ??? system_created
      // ??? public_id

      PlaylistType: string;

      Description: string;
      ArtworkID: string;
      // ??? SortType

      // Extra Data
      HasArtwork: boolean;

      CachedImage,
      CachedImageLarge: TJpegImage;
      Status: TWorkItems;

      (* Artwork *)
      function ArtworkLoaded(Large: boolean = false): boolean;
      function GetArtwork(Large: boolean = false): TJPEGImage;

      (* Loading *)
      procedure LoadFrom(JSONPair: TJSONData; AName: string);
    end;

    TSession = record
      DeviceName: string;

      Joinable: boolean;
      Connected: boolean;

      Client: string;
      LastLogin: TDateTime;
      Location: string;

      (* Loading *)
      procedure LoadFrom(JSON: TJSONObject);
    end;

    // Arrays
    TArtists = TArray<TArtistItem>;
    TAlbums = TArray<TAlbumItem>;
    TTracks = TArray<TTrackItem>;
    TPlaylists = TArray<TPlaylistItem>;
    TSessions = TArray<TSession>;

  // Get Data
  function GetTrack(ID: integer): integer;
  function GetAlbum(ID: integer): integer;
  function GetArtist(ID: integer): integer;
  function GetPlaylist(ID: integer): integer;

  function GetData(ID: integer; Source: TDataSource): integer;
  function GetItemID(Index: integer; Source: TDataSource): integer;

  function GetPlaylistOfType(AType: string): integer; (* thumbsup, recently-played, recently-uploaded *)

  // Utils
  function StringToDateTime(const ADateTimeStr: string; CovertUTC: boolean = true): TDateTime;
  function StringToTime(const ADateTimeStr: string; CovertUTC: boolean = true): TTime;
  function DateTimeToString(ADateTime: TDateTime; CovertUTC: boolean = true): string;
  function DateToString(ADateTime: TDate; CovertUTC: boolean = true): string;
  function Yearify(Year: cardinal): string;

  // Main Request
  function SendClientRequest(RequestJSON: string; Endpoint: string = ''): TJSONObject;

  // API
  function ConnectedToServer: boolean;

  // User
  function LoginUser: boolean;
  procedure LogOff;

  function IsAuthenthicated: boolean;

  // Memory
  procedure APIFreeMemory;

  // Artwork Store
  procedure AddToArtworkStore(ID: integer; Cache: TJpegImage; AType: TDataSource);
  function ExistsInStore(ID: integer; AType: TDataSource): boolean;
  function GetArtStoreCache(ID: integer; AType: TDataSource): TJpegImage;
  function GetArtworkStore(AType: TDataSource = TDataSource.None): string;
  procedure ClearArtworkStore;
  procedure InitiateArtworkStore;

  // Tracks
  function UpdateTrackRating(ID: integer; Rating: integer; ReloadLibrary: boolean): boolean;
  function GetSongPlaylists(ID: integer): TIntArray;

  function TrackRatingToLikedPlaylist(ID: integer): boolean;

  // Rating
  function RatingToString(Rating: integer): string;

  // Albums
  function UpdateAlbumRating(ID: integer; Rating: integer; ReloadLibrary: boolean): boolean;

  // Artists
  function UpdateArtistRating(ID: integer; Rating: integer; ReloadLibrary: boolean): boolean;

  // Playlist
  function CreateNewPlayList(Name, Description: string; MakePublic: boolean; Tracks: TIntArray): boolean; overload;
  function CreateNewPlayList(Name, Description: string; MakePublic: boolean; Mood: string): boolean; overload;
  function AppentToPlaylist(ID: integer; Tracks: TIntArray): boolean;
  function PreappendToPlaylist(ID: integer; Tracks: TIntArray): boolean;
  function ChangePlayList(ID: integer; Tracks: TIntArray): boolean;
  function DeleteFromPlaylist(ID: integer; Tracks: TIntArray): boolean;
  function TouchupPlaylist(ID: integer): boolean;
  function UpdatePlayList(ID: integer; Name, Description: string; ReloadLibrary: boolean): boolean;
  function DeletePlayList(ID: integer): boolean;
  function DeleteTracks(Tracks: TArray<integer>): boolean;
  function DeleteTrack(ID: integer): boolean;
  function DeleteAlbum(ID: integer): boolean;
  function DeleteArtist(ID: integer): boolean;
  function RestoreTracks(Tracks: TArray<integer>): boolean;
  function RestoreTrack(ID: integer): boolean;
  function RestoreAlbum(ID: integer): boolean;
  function RestoreArtist(ID: integer): boolean;
  function EmptyTrash(Tracks: TArray<integer>): boolean;
  function CompleteEmptyTrash: boolean;

  // History
  function PushHistory(Items: TArray<TTrackHistoryItem>): boolean;

  // Library
  procedure LoadStatus;
  procedure LoadLibrary;
  procedure LoadLibraryAdvanced(LoadSet: TLoadSet);
  procedure EmptyLibrary;

  // Additional Data
  function GetSongArtwork(ID: string; Size: TArtSize = TArtSize.Small): TJpegImage;
  function SongArtCollage(ID1, ID2, ID3, ID4: integer): TJpegImage;

  // Status
  procedure SetWorkStatus(Status: string);
  procedure SetDataWorkStatus(Status: string);

  procedure ResetWork;

  // UI
  procedure ReturnToLogin;

  // Utils
  function CalculateLength(Seconds: cardinal): string;

  // JSON
  function EmptyJSON: TJSONObject;
  function EmptyJSONArray: TJSONArray;

const
  // Formattable Strings
  DEVICE_NAME_CONST = '%S' + ' iBroadcast for Windows';
  WELCOME_STRING = 'Welcome, %S';

  // Login Constants
  CLIENT_NAME = 'Cods iBroadcast';
  API_ENDPOINT = 'https://api.ibroadcast.com/';
  LIBRARY_ENDPOINT = 'https://library.ibroadcast.com/';
  ARTWORK_ENDPOINT = 'https://artwork.ibroadcast.com/artwork/%S-%U';
  STREAMING_ENDPOINT = 'https://streaming.ibroadcast.com';

  API_VERSION = '1.0.0';
  APP_VERSION: TVersionRec = (Major:1; Minor:0; Maintenance:0);

  // Artwork Store
  ART_EXT = '.jpeg';

  // Templates
  REQUEST_HEADER = '{'
    + '"user_id": "%U",'
    + '"token": "%S",'
    + '"version": "' + API_VERSION + '"';

  // Request Formats
  REQUEST_LOGIN = '{'
    + '"login_token": "%S",'
    + '"device_name": "%S",'
    + '"client": "%S",'
    + '"version": "' + API_VERSION + '",'
    + '"app_id": "%S",'
    + '"type": "account",'
    + '"mode": "login_token"'
    + '}';

  REQUEST_LOGOFF = REQUEST_HEADER + ','
    + '"mode": "logout"'
    + '}';

  // Data
  REQUEST_EMPTY = REQUEST_HEADER + ','
    + '}';

  REQUEST_DATA = REQUEST_HEADER + ','
    + '"mode": "%S"'
    + '}';

  // Playlist
  REQUEST_LIST_TEMPLATE = REQUEST_HEADER + ','
    + '"mode": "createplaylist",'
    + '"name": "%S",'
    + '"description": "%S",'
    + '"make_public": %S';

  REQUEST_LIST_CREATETRACKS = REQUEST_LIST_TEMPLATE + ','
    + '"tracks": [%S]'
    + '}';

  REQUEST_LIST_CREATEMOOD = REQUEST_LIST_TEMPLATE + ','
    + '"mood": "%S"'
    + '}';

  REQUEST_LIST_DELETE = REQUEST_HEADER + ','
    + '"mode": "deleteplaylist",'
    + '"playlist": %D'
    + '}';

  REQUEST_LIST_ADD = REQUEST_HEADER + ','
    + '"mode": "appendplaylist",'
    + '"playlist": %D,'
    + '"tracks": [%S]'
    + '}';

  REQUEST_LIST_SET = REQUEST_HEADER + ','
    + '"mode": "updateplaylist",'
    + '"playlist": %D,'
    + '"tracks": [%S]'
    + '}';

  REQUEST_LIST_UPDATE = REQUEST_HEADER + ','
    + '"mode": "updateplaylist",'
    + '"playlist": %D,'
    + '"name": "%S",'
    + '"supported_types": false,'
    + '"description": "%S"'
    + '}';

  // Track
  REQUEST_TRACK_DELETE = REQUEST_HEADER + ','
    + '"mode": "trash",'
    + '"tracks": [%S]'
    + '}';

  REQUEST_TRACK_RESTORE = REQUEST_HEADER + ','
    + '"mode": "restore",'
    + '"tracks": [%S]'
    + '}';

  REQUEST_TRACK_EMPTYTRASH = REQUEST_HEADER + ','
    + '"mode": "empty_trash",'
    + '"tracks": [%S]'
    + '}';

  // Rating
  REQUEST_RATE_TRACK = REQUEST_HEADER + ','
    + '"mode": "ratetrack",'
    + '"track_id": %D,'
    + '"rating": %D'
    + '}';

  REQUEST_RATE_ALBUM = REQUEST_HEADER + ','
    + '"mode": "ratealbum",'
    + '"album_id": %D,'
    + '"rating": %D'
    + '}';

  REQUEST_RATE_ARTIST = REQUEST_HEADER + ','
    + '"mode": "rateartist",'
    + '"artist_id": %D,'
    + '"rating": %D'
    + '}';

  // History
  (* Will be build on runtime *)

  // Library
  REQUEST_LIBRARY = REQUEST_HEADER
    + '}';


var
  // System
  DebugMode: boolean;

  // App Device token
  LOGIN_TOKEN: string;

  // Notify
  OnWorkStatusChange: procedure(Status: string);
  OnDataWorkStatusChange: procedure(Status: string);

  OnReturnToLogin: procedure;

  // Cover Settings
  DefaultArtSize: TArtSize = TArtSize.Medium;

  // Login Information
  VERSION: string;
  DEVICE_NAME: string;

  // Verbose Loggins
  WORK_STATUS: string;
  DATA_WORK_STATUS: string;

  // Work
  WorkCount: int64;
  TotalWorkCount: int64;

  // Setings
  ValueRatingMode: boolean = false; // use rating stars
  AllowArtCollage: boolean = true; { Bug fixed, error is no more }

  // Notify Events
  OnUpdateType: TDataTypeUpdate;

  // Artwork Store
  ArtworkStore: boolean = true;
  MediaStoreLocation: string;

  // Server Login Output
  TOKEN: string;
  USER_ID: integer;
  APPLICATION_ID: string = '1102';

  // Library
  LibraryStatus: TLibraryStatus;
  Account: TAccount;
  Sessions: TSessions;

  Tracks: TTracks;
  Albums: TAlbums;
  Artists: TArtists;
  Playlists: TPlaylists;

  DefaultPicture: TJPEGImage;

implementation

uses
  MainUI;

function SendClientRequest(RequestJSON, Endpoint: string): TJSONObject;
var
  Response: string;
  HTTP: TIdHTTP;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  RequestStream: TStringStream;
begin
  // Endpoint
  if Endpoint = '' then
    Endpoint := API_ENDPOINT;

  // Create HTTP and SSLIOHandler components
  HTTP := TIdHTTP.Create(nil);
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(HTTP);
  RequestStream := TStringStream.Create(RequestJSON);
  try
    // Set SSL/TLS options
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    HTTP.IOHandler := SSLIOHandler;

    // Set headers
    HTTP.Request.ContentType := 'application/json';

    // Send request and receive response
    Response := HTTP.Post(Endpoint, RequestStream);

    // Parse response and extract numbers
    Result := GetJSON(Response) as TJSONObject;
  finally
    // Free
    HTTP.Free;
    RequestStream.Free;
  end;
end;

function LoginUser: boolean;
var
  Request: string;
  SResult: ResultType;

  JSONValue: TJSONObject;
  JSONUser: TJSONObject;
begin
  // Reset values
  USER_ID := 0;
  TOKEN := '';

  // Prepare request string
  Request := Format(REQUEST_LOGIN, [LOGIN_TOKEN, DEVICE_NAME, CLIENT_NAME, APPLICATION_ID]);

  // Parse response and extract numbers
  JSONValue := SendClientRequest(Request);
  try
    SResult.AnaliseFrom(JSONValue);

    Result := SResult.Success;

    // Success
    if SResult.Success then
        begin
          // Get "user" category
          JSONUser := JSONValue.Get('user', EmptyJSON);

          // Get User ID
          USER_ID := StrToInt( JSONUser.Get('id', '') );
          TOKEN := JSONUser.Get('token', '');
        end
      else
        begin
          //raise Exception.Create(SResult.ServerMessage);
        end;

  finally
    JSONValue.Free;
  end;
end;

procedure LogOff;
var
  Request: string;
  SResult: ResultType;

  JSONValue: TJSONObject;
begin
  // Prepare request string
  Request := Format(REQUEST_LOGOFF, [USER_ID, TOKEN]);

  // Parse response and extract numbers
  JSONValue := SendClientRequest(Request);
  try
    SResult.AnaliseFrom(JSONValue);
  finally
    JSONValue.Free;
  end;

  // Reset array
  SetLength(Tracks, 0);
  SetLength(Albums, 0);
  SetLength(Artists, 0);
  SetLength(Playlists, 0);

  // Open login page
  ReturnToLogin;
end;

function IsAuthenthicated: boolean;
var
  Request: string;
  SResult: ResultType;

  JSONValue: TJSONObject;
begin
  if (USER_ID = 0) or (TOKEN = '') then
    Exit(false);

  // Prepare request string
  Request := Format(REQUEST_DATA, [USER_ID, TOKEN, 'status']);

  // Parse response and extract numbers
  JSONValue := SendClientRequest(Request);
  try
    SResult.AnaliseFrom(JSONValue);

    Result := SResult.LoggedIn;
  finally
    JSONValue.Free;
  end;
end;

procedure APIFreeMemory;
var
  I: Integer;
begin
  for I := 0 to High(Tracks) do
    begin
      if Tracks[I].CachedImage <> nil then
        Tracks[I].CachedImage.Free;
      if Tracks[I].CachedImageLarge <> nil then
        Tracks[I].CachedImageLarge.Free;
    end;
end;

procedure AddToArtworkStore(ID: integer; Cache: TJpegImage; AType: TDataSource);
var
  LifeSaver: TSaveArtClass;
begin
  // gud
  LifeSaver := TSaveArtClass.Create;
  try
    LifeSaver.Image := Cache;
    LifeSaver.FilePath:=GetArtworkStore(AType) + ID.ToString + ART_EXT;

    LifeSaver.Save;
  finally
    LifeSaver.Free;
  end;
end;

function ExistsInStore(ID: integer; AType: TDataSource): boolean;
var
  Path: string;
begin
  if not ArtworkStore then
    Exit(false);

  Path := GetArtworkStore(AType) + ID.ToString + ART_EXT;

  Result := fileexists( Path );
end;

function GetArtStoreCache(ID: integer; AType: TDataSource): TJpegImage;
var
  Path: string;
begin
  Path := GetArtworkStore(AType) + ID.ToString + ART_EXT;

  Result := TJpegImage.Create;
  Result.LoadFromFile(Path);
end;

function GetArtworkStore(AType: TDataSource): string;
begin
  Result := IncludeTrailingPathDelimiter(MediaStoreLocation);
  case AType of
    TDataSource.Tracks: Result := Result + 'tracks';
    TDataSource.Albums: Result := Result + 'albums';
    TDataSource.Artists: Result := Result + 'artists';
    TDataSource.Playlists: Result := Result + 'playlists';
  end;

  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure ClearArtworkStore;
var
  Path: string;
begin
  Path := GetArtworkStore;

  if directoryexists(Path) then
    if DeleteDirectory(Path, true) then
        RemoveDir(Path);
end;

procedure InitiateArtworkStore;
var
  ArtRoot: string;
begin
  if not ArtworkStore then
    Exit;

  ArtRoot := GetArtworkStore;

  if not DirectoryExists(ArtRoot) then
    MkDir(ArtRoot);

  MkDir(GetArtworkStore(TDataSource.Tracks));
  MkDir(GetArtworkStore(TDataSource.Albums));
  MkDir(GetArtworkStore(TDataSource.Artists));
  MkDir(GetArtworkStore(TDataSource.Playlists));
end;

function UpdateTrackRating(ID: integer; Rating: integer; ReloadLibrary: boolean): boolean;
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
begin
  // Prepare request string
  Request := Format(REQUEST_RATE_TRACK, [USER_ID, TOKEN, ID, Rating]);

  // Parse response and extract numbers
  SetWorkStatus('Updating track rating');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  if ReloadLibrary then
    LoadLibraryAdvanced([TLoad.Track]);
end;

function GetSongPlaylists(ID: integer): TIntArray;
var
  I: Integer;
begin
  // Search
  Result := [];
  for I := 0 to High(Playlists) do
    if Playlists[I].TracksID.Find(ID) <> -1 then
      Result.AddValue(Playlists[I].ID);
end;

function TrackRatingToLikedPlaylist(ID: integer): boolean;
var
  Index, SongIndex: integer;
  Fav, IsFav: boolean;
begin
  Result := false;

  SongIndex := GetTrack(ID);
  Index := GetPlaylistOfType('thumbsup');

  if (Index <> -1) and (SongIndex <> -1) then
    begin
      Fav := Playlists[Index].TracksID.Find(ID) <> -1;
      if ValueRatingMode then
        IsFav := Tracks[SongIndex].Rating = 10
      else
        IsFav := Tracks[SongIndex].Rating in [10, 5];

      if IsFav <> Fav then
        begin
          if IsFav then
            Result := PreappendToPlaylist(Playlists[Index].ID, MakeIntArray([ID]))
          else
            Result := DeleteFromPlaylist(Playlists[Index].ID, MakeIntArray([ID]));
        end;
    end;
end;

function RatingToString(Rating: integer): string;
begin
  if ValueRatingMode then
    begin
      if Rating <> 0 then
        Result := Format('%D/%D', [Rating, 10])
      else
        Result := 'Not rated';
    end
  else
    case Rating of
      10, 5: Result := 'Liked';
      1: Result := 'Disliked';
      else Result := 'Not rated';
    end;
end;

function UpdateAlbumRating(ID: integer; Rating: integer; ReloadLibrary: boolean): boolean;
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
begin
  // Prepare request string
  Request := Format(REQUEST_RATE_ALBUM, [USER_ID, TOKEN, ID, Rating]);

  // Parse response and extract numbers
  SetWorkStatus('Updating album rating');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  if ReloadLibrary then
    LoadLibraryAdvanced([TLoad.Album]);
end;

function UpdateArtistRating(ID: integer; Rating: integer; ReloadLibrary: boolean): boolean;
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
begin
  // Prepare request string
  Request := Format(REQUEST_RATE_ARTIST, [USER_ID, TOKEN, ID, Rating]);

  // Parse response and extract numbers
  SetWorkStatus('Updating artist rating');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  if ReloadLibrary then
    LoadLibraryAdvanced([TLoad.Artist]);
end;

function CreateNewPlayList(Name, Description: string; MakePublic: boolean; Tracks: TIntArray): boolean;
var
  Request: string;
  JResult: ResultType;

  ATracks: string;
  ATotal: integer;

  JSONValue: TJSONObject;
  I: Integer;
begin
  // Get Tracks
  ATracks := '';
  ATotal := High(Tracks);
  for I := 0 to ATotal do
    begin
      ATracks := ATracks + Tracks[I].ToString;

      if I < ATotal then
        ATracks := Concat(ATracks, ',');
    end;

  // Prepare request string
  Request := Format(REQUEST_LIST_CREATETRACKS, [USER_ID, TOKEN,
    Name, Description, booleantostring(MakePublic), ATracks]);

  // Parse response and extract numbers
  SetWorkStatus('Creating Playlist by Songs');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

function CreateNewPlayList(Name, Description: string; MakePublic: boolean; Mood: string): boolean;
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
begin
  // Prepare request string
  Request := Format(REQUEST_LIST_CREATEMOOD, [USER_ID, TOKEN,
    Name, Description, booleantostring(MakePublic), Mood]);

  // Parse response and extract numbers
  SetWorkStatus('Creating Playlist by Mood');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

function AppentToPlaylist(ID: integer; Tracks: TIntArray): boolean;
var
  Request: string;
  JResult: ResultType;

  ATracks: string;
  ATotal: integer;

  JSONValue: TJSONObject;
  I: Integer;
begin
  // Get Tracks
  ATracks := '';
  ATotal := High(Tracks);
  for I := 0 to ATotal do
    begin
      ATracks := ATracks + Tracks[I].ToString;

      if I < ATotal then
        ATracks := Concat(ATracks, ',');
    end;

  // Prepare request string
  Request := Format(REQUEST_LIST_ADD, [USER_ID, TOKEN,
    ID, ATracks]);

  // Parse response and extract numbers
  SetWorkStatus('Adding songs to playlist');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

function PreappendToPlaylist(ID: integer; Tracks: TIntArray): boolean;
var
  AllTracks: TIntArray;
  I: Integer;
begin
  // Get Tracks
  AllTracks := Playlists[GetPlaylist(ID)].TracksID;

  // Insert
  for I := 0 to High(Tracks) do
    AllTracks.Insert(0, Tracks[I]);

  // Change ex
  Result := ChangePlayList(ID, AllTracks);
end;

function ChangePlayList(ID: integer; Tracks: TIntArray): boolean;
var
  Request: string;
  JResult: ResultType;

  AllTracks: TArray<integer>;
  ATracks: string;
  ATotal: integer;

  JSONValue: TJSONObject;
  I: Integer;
begin
  // Delete Tracks
  AllTracks := Tracks;

  // Get Tracks
  ATracks := '';
  ATotal := High(AllTracks);
  for I := 0 to ATotal do
    begin
      ATracks := ATracks + AllTracks[I].ToString;

      if I < ATotal then
        ATracks := Concat(ATracks, ',');
    end;

  // Prepare request string
  Request := Format(REQUEST_LIST_SET, [USER_ID, TOKEN,
    ID, ATracks]);

  // Parse response and extract numbers
  SetWorkStatus('Changing songs of playlist');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

function DeleteFromPlaylist(ID: integer; Tracks: TIntArray): boolean;
var
  Request: string;
  JResult: ResultType;

  AllTracks: TIntArray;
  ATracks: string;
  ATotal: integer;

  JSONValue: TJSONObject;
  I: Integer;
begin
  // Delete Tracks
  AllTracks := Playlists[GetPlaylist(ID)].TracksID;
  for I := 0 to High(Tracks) do
    AllTracks.Delete(AllTracks.Find(Tracks[I]));

  // Get Tracks
  ATracks := '';
  ATotal := High(AllTracks);
  for I := 0 to ATotal do
    begin
      ATracks := ATracks + AllTracks[I].ToString;

      if I < ATotal then
        ATracks := Concat(ATracks, ',');
    end;

  // Prepare request string
  Request := Format(REQUEST_LIST_SET, [USER_ID, TOKEN,
    ID, ATracks]);

  // Parse response and extract numbers
  SetWorkStatus('Changing songs of playlist');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

function TouchupPlaylist(ID: integer): boolean;
var
  Request: string;
  JResult: ResultType;

  AllTracks: TIntArray;
  ATracks: string;
  ATotal: integer;

  JSONValue: TJSONObject;
  I: Integer;
begin
  // Delete Tracks
  AllTracks := Playlists[GetPlaylist(ID)].TracksID;

  // Delete invalid enteries
  for I := High(AllTracks) downto 0 do
    if GetTrack(AllTracks[I]) = -1 then
      AllTracks.Delete(I);

  // Get Tracks
  ATracks := '';
  ATotal := High(AllTracks);
  for I := 0 to ATotal do
    begin
      ATracks := ATracks + AllTracks[I].ToString;

      if I < ATotal then
        ATracks := Concat(ATracks, ',');
    end;

  // Prepare request string
  Request := Format(REQUEST_LIST_SET, [USER_ID, TOKEN,
    ID, ATracks]);

  // Parse response and extract numbers
  SetWorkStatus('Repairing playlist');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

function UpdatePlayList(ID: integer; Name, Description: string; ReloadLibrary: boolean): boolean;
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
begin
  // Prepare request string
  Request := Format(REQUEST_LIST_UPDATE, [USER_ID, TOKEN,
    ID, Name, Description]);

  // Parse response and extract numbers
  SetWorkStatus('Updating playlist');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  if ReloadLibrary then
    LoadLibraryAdvanced([TLoad.PlayList]);
end;

function DeletePlayList(ID: integer): boolean;
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
begin
  // Prepare request string
  Request := Format(REQUEST_LIST_DELETE, [USER_ID, TOKEN, ID]);

  // Parse response and extract numbers
  SetWorkStatus('Deleting playlist');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

function DeleteTracks(Tracks: TArray<integer>): boolean;
var
  Request: string;
  JResult: ResultType;

  ATracks: string;
  ATotal: integer;
  I: integer;

  JSONValue: TJSONObject;
begin
  // Get Tracks
  ATracks := '';
  ATotal := High(Tracks);
  for I := 0 to ATotal do
    ATracks := ATracks + Tracks[I].ToString + ',';

  ATracks := Copy(ATracks, 1, Length(ATracks)-1);

  // Prepare request string
  Request := Format(REQUEST_TRACK_DELETE, [USER_ID, TOKEN, ATracks]); // supports multi-delete

  // Parse response and extract numbers
  SetWorkStatus('Deleting track');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.Track, TLoad.Album, TLoad.Artist, TLoad.PlayList]);
end;

function RestoreTracks(Tracks: TArray<integer>): boolean;
var
  Request: string;
  JResult: ResultType;

  ATracks: string;
  ATotal: integer;
  I: integer;

  JSONValue: TJSONObject;
begin
  // Get Tracks
  ATracks := '';
  ATotal := High(Tracks);
  for I := 0 to ATotal do
    ATracks := ATracks + Tracks[I].ToString + ',';

  ATracks := Copy(ATracks, 1, Length(ATracks)-1);

  // Prepare request string
  Request := Format(REQUEST_TRACK_RESTORE, [USER_ID, TOKEN, ATracks]); // supports multi-delete

  // Parse response and extract numbers
  SetWorkStatus('Restoring track');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.Track, TLoad.Album, TLoad.Artist, TLoad.PlayList]);
end;

function EmptyTrash(Tracks: TArray<integer>): boolean;
var
  Request: string;
  JResult: ResultType;

  ATracks: string;
  ATotal: integer;
  I: integer;

  JSONValue: TJSONObject;
begin
  // Get Tracks
  ATracks := '';
  ATotal := High(Tracks);
  for I := 0 to ATotal do
    ATracks := ATracks + Tracks[I].ToString + ',';

  ATracks := Copy(ATracks, 1, Length(ATracks)-1);

  // Prepare request string
  Request := Format(REQUEST_TRACK_EMPTYTRASH, [USER_ID, TOKEN, ATracks]); // supports multi-delete

  // Parse response and extract numbers
  SetWorkStatus('Deleting from trash');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load playlists
  LoadLibraryAdvanced([TLoad.Track, TLoad.Album, TLoad.Artist, TLoad.PlayList]);
end;

function CompleteEmptyTrash: boolean;
var
  ATracks: TIntArray;
  I: integer;
begin
  ATracks := [];
  for I := 0 to High(Tracks) do
    if Tracks[I].IsInTrash then
      ATracks.AddValue(Tracks[I].ID);

  // Empty
  Result := EmptyTrash(ATracks);
end;

function RestoreTrack(ID: integer): boolean;
begin
  Result := RestoreTracks(MakeIntArray([ID]));
end;

function RestoreAlbum(ID: integer): boolean;
var
  Index: integer;
begin
  Result := false;
  Index := GetAlbum(ID);

  if Index <> -1 then
    Result := RestoreTracks(Albums[Index].TracksID);
end;

function RestoreArtist(ID: integer): boolean;
var
  Index: integer;
begin
  Result := false;
  Index := GetArtist(ID);

  if Index <> -1 then
    Result := RestoreTracks(Artists[Index].TracksID);
end;

function DeleteTrack(ID: integer): boolean;
begin
  Result := DeleteTracks(MakeIntArray([ID]));
end;

function DeleteAlbum(ID: integer): boolean;
var
  Index: integer;
begin
  Result := false;
  Index := GetAlbum(ID);

  if Index <> -1 then
    Result := DeleteTracks(Albums[Index].TracksID);
end;

function DeleteArtist(ID: integer): boolean;
var
  Index: integer;
begin
  Result := false;
  Index := GetArtist(ID);

  if Index <> -1 then
    Result := DeleteTracks(Artists[Index].TracksID);
end;

function PushHistory(Items: TArray<TTrackHistoryItem>): boolean;
var
  Request: string;
  JResult: ResultType;

  JSONRequest,
  JSONHist,
  JSONEvents,
  JSONItem: TJSONObject;
  JSONArray,
  JSONHistory: TJSONArray;

  JSONValue: TJSONObject;

  PlayMap,
  PlayCount: TIntArray;

  Day: TDate;

  Index, I: Integer;
begin
  if Length(Items) = 0 then
    Exit(false);

  Day := Items[0].Timestamp;

  // Calculate Count
  PlayMap := [];
  PlayCount := [];

  for I := 0 to High(Items) do
    begin
      Index := PlayMap.Find(Items[I].TrackID);

      if Index = -1 then
        begin
          PlayMap.AddValue(Items[I].TrackID);
          PlayCount.AddValue(1);
        end
      else
        begin
          Inc(PlayCount[Index]);
        end;
    end;

  // Create JSON
  JSONRequest := TJSONObject.Create;
  JSONHistory := TJSONArray.Create;
  JSONHist := TJSONObject.Create;
  try
    // Data
    JSONRequest.Add('user_id', USER_ID);
    JSONRequest.Add('token', TOKEN);
    JSONRequest.Add('version', API_VERSION);
    JSONRequest.Add('mode', 'status');

    // Overview
    JSONHist.Add('day', DateToString(Day));

    JSONItem := TJSONObject.Create;
    for I := 0 to High(PlayMap) do
      JSONItem.Add(PlayMap[I].ToString, PlayCount[I]);

    JSONHist.Add('plays', JSONItem);

    // Detail & Events
    JSONEvents := TJSONObject.Create;

    for I := 0 to High(Items) do
      begin
        JSONArray := TJsonArray.Create;
        JSONItem := TJSONObject.Create;

        JSONItem.Add('event', 'play');
        JSONItem.Add('ts', DateTimeToString(Items[I].TimeStamp));

        JSONArray.Add(JSONItem);
        JSONEvents.Add(Items[I].TrackID.ToString, JSONArray);
      end;

    JSONHist.Add('detail', JSONEvents);

    // Add
    JSONHistory.Add(JSONHist);
    JSONRequest.Add('history', JSONHistory);

    // Prepare request string
    Request := JSONRequest.AsJSON;
  finally
    JSONRequest.Free;
  end;

  // Parse response and extract numbers
  SetWorkStatus('Pushing history update to server');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    Result := JResult.Success;
  finally
    JSONValue.Free;
  end;

  // Re-load history, nah
  LoadLibraryAdvanced([TLoad.PlayList]);
end;

procedure LoadStatus;
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
  JSONAccount,
  JSONItem: TJSONObject;
  JSONSessions: TJSONArray;
  I: Integer;
begin
  // Prepare request string
  Request := Format(REQUEST_DATA, [USER_ID, TOKEN, 'status']);

  // Parse response and extract numbers
  SetWorkStatus('Contacting iBroadcast API servers...');
  JSONValue := SendClientRequest(Request);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    if JResult.Error then
      if not JResult.LoggedIn then
        JResult.TerminateSession;

    // Load status
    SetWorkStatus('Loading library status...');
    JSONItem := JSONValue.Get('status', EmptyJSON);
    LibraryStatus.LoadFrom(JSONItem);

    // Account
    SetWorkStatus('Loading your account...');
    JSONAccount := JSONValue.Get('user', EmptyJSON);

    Account.LoadFrom( JSONAccount );

    // Sessions
    SetWorkStatus('Loading sessions...');
    JSONSessions := JSONAccount.Get('session', EmptyJSON).Get('sessions', EmptyJSONArray);

    SetLength( Sessions, JSONSessions.Count );

    for I := 0 to JSONSessions.Count - 1 do
      begin
        Sessions[I].LoadFrom( JSONSessions.Items[I] as TJSONObject );
      end;
  finally
    JSONValue.Free;
  end;
end;

procedure LoadLibrary;
begin
  LoadLibraryAdvanced( [TLoad.Track, TLoad.Album, TLoad.Artist, TLoad.PlayList]);
end;

procedure LoadLibraryAdvanced(LoadSet: TLoadSet);
var
  Request: string;
  JResult: ResultType;

  JSONValue: TJSONObject;
  JSONLibrary,
  JSONItem: TJSONObject;
  JSONData: TJSONData;
  I, Index: Integer;
  Name: string;
begin
  // Prepare request string
  Request := Format(REQUEST_LIBRARY, [USER_ID, TOKEN]);

  // Work
  ResetWork;

  // Parse response and extract numbers
  SetWorkStatus('Downloading iBroadcast Library...');
  JSONValue := SendClientRequest(Request, LIBRARY_ENDPOINT);
  try
    // Error
    JResult.AnaliseFrom(JSONVALUE);

    if JResult.Error then
      if not JResult.LoggedIn then
        JResult.TerminateSession;

    // Load library
    SetWorkStatus('Loading library...');
    JSONLibrary := JSONValue.Get('library', EmptyJSON);

    // Tracks
    if TLoad.Track in LoadSet then
      begin
        SetWorkStatus('Loading tracks...');
        JSONItem := JSONLibrary.Get('tracks', EmptyJSON);
        SetLength( Tracks, 0 );

        // Work
        ResetWork;
        TotalWorkCount := JSONItem.Count;

        for I := 0 to JSONItem.Count - 1 do
          begin
            try
              Name := JSONItem.Names[I];
              JSONData := JSONItem.Items[I];
            except
              if I >= JSONItem.Count - 1 then
                Break;
              Continue;
            end;

            WorkCount := I;

            if JSONData.JSONType = jtObject then
              Continue;

            Index := Length(Tracks);
            SetLength( Tracks, Index + 1 );

            Tracks[Index].LoadFrom( JSONData, Name );
          end;

        // Updated
        if Assigned(OnUpdateType) then
          OnUpdateType(TDataSource.Tracks);
      end;

    // Albums
    if TLoad.Album in LoadSet then
      begin
        SetWorkStatus('Loading albums...');
        JSONItem := JSONLibrary.Get('albums', EmptyJSON);
        SetLength( Albums, 0 );

        // Work
        ResetWork;
        TotalWorkCount := JSONItem.Count;

        for I := 0 to JSONItem.Count - 1 do
          begin
            Name := JSONItem.Names[I];
            JSONData := JSONItem.Items[I];

            WorkCount := I;

            if JSONData.JSONType = jtObject then
              Continue;

            Index := Length(Albums);
            SetLength( Albums, Index + 1 );

            Albums[Index].LoadFrom( JSONData, Name );

            // Invalid entry, delete from index
            if Albums[Index].TracksID.Count = 0 then
              SetLength( Albums, Index );
          end;

        // Updated
        if Assigned(OnUpdateType) then
          OnUpdateType(TDataSource.Albums);
      end;

    // Artists
    if TLoad.Artist in LoadSet then
      begin
        SetWorkStatus('Loading artists...');
        JSONItem := JSONLibrary.Get('artists', EmptyJSON);
        SetLength( Artists, 0 );

        // Work
        ResetWork;
        TotalWorkCount := JSONItem.Count;

        for I := 0 to JSONItem.Count - 1 do
          begin
            Name := JSONItem.Names[I];
            JSONData := JSONItem.Items[I];

            WorkCount := I;

            if JSONData.JSONType = jtObject then
              Continue;

            Index := Length(Artists);
            SetLength( Artists, Index + 1 );

            Artists[Index].LoadFrom( JSONData, Name );

            // Invalid entry, delete from index
            if Artists[Index].TracksID.Count = 0 then
              SetLength( Artists, Index );
          end;

        // Updated
        if Assigned(OnUpdateType) then
          OnUpdateType(TDataSource.Artists);
      end;

    // PlayLists
    if TLoad.PlayList in LoadSet then
      begin
        SetWorkStatus('Loading playlists...');
        JSONItem := JSONLibrary.Get('playlists', EmptyJSON);
        SetLength( PlayLists, 0 );

        // Work
        ResetWork;
        TotalWorkCount := JSONItem.Count;

        for I := 0 to JSONItem.Count - 1 do
          begin
            Name := JSONItem.Names[I];
            JSONData := JSONItem.Items[I];

            WorkCount := I;

            if JSONData.JSONType = jtObject then
              Continue;

            Index := Length(PlayLists);
            SetLength( PlayLists, Index + 1 );

            PlayLists[Index].LoadFrom( JSONData, Name );
          end;

        // Updated
        if Assigned(OnUpdateType) then
          OnUpdateType(TDataSource.Playlists);
      end;
  finally
    JSONValue.Free;
  end;

  // Work
  ResetWork;
end;

procedure EmptyLibrary;
begin
  SetLength(Tracks, 0);
  SetLength(Albums, 0);
  SetLength(Artists, 0);
  SetLength(Playlists, 0);
end;

function GetSongArtwork(ID: string; Size: TArtSize): TJpegImage;
var
  URL: string;
  ImageSize: integer;

  IdHTTP: TIdHTTP;
  ResponseStream: TMemoryStream;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  case Size of
    TArtSize.Small: ImageSize := 150;
    TArtSize.Medium: ImageSize := 300;
    else ImageSize := 1000;
  end;

  // Prepare URL
  URL := Format(ARTWORK_ENDPOINT, [ID, ImageSize]);

  // Fetch Image
  IdHTTP := TIdHTTP.Create;
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(IdHTTP);
  try
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    IdHTTP.IOHandler := SSLIOHandler;

    // Create Stream
    ResponseStream := TMemoryStream.Create;
    try
      IdHTTP.Get(URL, ResponseStream);
      ResponseStream.Position := 0;

      // Load Picture
      Result := TJPEGImage.Create;
      Result.LoadFromStream(ResponseStream);
    finally
      // Free Steam
      ResponseStream.Free;
    end;
  finally
    // Free Net
    IdHTTP.Free;
  end;
end;

procedure ReturnToLogin;
begin
  OnReturnToLogin;
end;

function CalculateLength(Seconds: cardinal): string;
var
  Minutes, Hours: cardinal;
begin
  Minutes := Seconds div 60;
  Seconds := Seconds - Minutes * 60;

  Hours := Minutes div 60;
  Minutes := Minutes - Hours * 60;

  Result := IntToStrIncludePrefixZeros(Minutes, 2) + ':' + IntToStrIncludePrefixZeros(Seconds, 2);

  if Hours > 0 then
    Result := IntToStrIncludePrefixZeros(Hours, 2) + ':' + Result;
end;

function EmptyJSON: TJSONObject;
begin
  Result := nil;
end;

function EmptyJSONArray: TJSONArray;
begin
  Result := nil;
end;

function SongArtCollage(ID1, ID2, ID3, ID4: integer): TJpegImage;
var
  CollageMaker: TCollageMaker;
begin
  CollageMaker := TCollageMaker.Create;
  try
    // Get
    CollageMaker.Image1 := GetSongArtwork( Tracks[GetTrack( ID1 )].ArtworkID, TArtSize.Small );
    CollageMaker.Image2 := GetSongArtwork( Tracks[GetTrack( ID2 )].ArtworkID, TArtSize.Small );
    CollageMaker.Image3 := GetSongArtwork( Tracks[GetTrack( ID3 )].ArtworkID, TArtSize.Small );
    CollageMaker.Image4 := GetSongArtwork( Tracks[GetTrack( ID4 )].ArtworkID, TArtSize.Small );

    // Make
    Result := CollageMaker.Make;
  finally
    (* Free *)
    CollageMaker.Free;
  end;
end;

procedure SetWorkStatus(Status: string);
begin
  WORK_STATUS := Status;

  if Assigned(OnWorkStatusChange) then
    OnWorkStatusChange(Status);
end;

procedure SetDataWorkStatus(Status: string);
begin
  DATA_WORK_STATUS := Status;

  if Assigned(OnDataWorkStatusChange) then
    OnDataWorkStatusChange(Status);
end;

procedure ResetWork;
begin
  WorkCount := 0;
  TotalWorkCount := 0;
end;

{ TSaveArtClass }

procedure TSaveArtClass.SaveFile;
begin
  Image.SaveToFile( FilePath );
end;

procedure TSaveArtClass.Save;
begin
  TThread.Synchronize(TThread.CurrentThread, SaveFile);
end;

{ TCollageMaker }

procedure TCollageMaker.Build;
begin
  TempResult := TJPEGImage.Create;
  TempResult.Width := 300;
  TempResult.Height := 300;

  with TempResult.Canvas do
    begin
      try
        StretchDraw(Rect(0, 0, 150, 150), Image1);
        Application.ProcessMessages;
      except
      end;
      try
        StretchDraw(Rect(150, 0, 300, 150), Image2);
        Application.ProcessMessages;
      except
      end;
      try
        StretchDraw(Rect(0, 150, 150, 300), Image3);
        Application.ProcessMessages;
      except
      end;
      try
        StretchDraw(Rect(150, 150, 300, 300), Image4);
        Application.ProcessMessages;
      except
      end;
    end;
end;

function TCollageMaker.Make: TJPEGImage;
begin
  TThread.Synchronize(TThread.CurrentThread, Build);

  Result := TempResult;
end;

{ ResultType }

procedure ResultType.AnaliseFrom(JSON: TJSONObject);
var
  O:  TJSONString;
begin
  Error := not JSON.Get('result', false);

  LoggedIn := JSON.Get('authenticated', false);

  if JSON.Find('message', O) then
    try
      if O.JSONType = jtString then
        ServerMessage := O.AsString;
    except
      ServerMessage := '';
    end;
end;

function ResultType.Success: boolean;
begin
  Result := not Error;
end;

procedure ResultType.TerminateSession;
begin
  LogOff;
  ReturnToLogin;

  // Terminate Parent Function
  Abort;
end;

function GetTrack(ID: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Tracks) do
    if Tracks[I].ID = ID then
      Exit( I );
end;

function GetAlbum(ID: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Albums) do
    if Albums[I].ID = ID then
      Exit( I );
end;

function GetArtist(ID: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Artists) do
    if Artists[I].ID = ID then
      Exit( I );
end;

function GetPlaylist(ID: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Playlists) do
    if Playlists[I].ID = ID then
      Exit( I );
end;

function GetData(ID: integer; Source: TDataSource): integer;
begin
  Result := -1;
  case Source of
    TDataSource.Tracks: Exit(GetTrack(ID));
    TDataSource.Albums: Exit(GetAlbum(ID));
    TDataSource.Artists: Exit(GetArtist(ID));
    TDataSource.Playlists: Exit(GetPlaylist(ID));
  end;
end;

function GetItemID(Index: integer; Source: TDataSource): integer;
begin
  Result := -1;
  if Index <> -1 then
    case Source of
      TDataSource.Tracks: Exit(Tracks[Index].ID);
      TDataSource.Albums: Exit(Albums[Index].ID);
      TDataSource.Artists: Exit(Artists[Index].ID);
      TDataSource.Playlists: Exit(Playlists[Index].ID);
    end;
end;

function GetPlaylistOfType(AType: string): integer;
var
  I: Integer;
  ListType: string;
begin
  Result := -1;
  for I := 0 to High(Playlists) do
    begin
      ListType := Playlists[I].PlaylistType;
      if ListType = AType then
        Exit( I );
    end;
end;

function StringToDateTime(const ADateTimeStr: string; CovertUTC: boolean = true): TDateTime;
var
  DateTimeFormat: TFormatSettings;
begin
  DateTimeFormat := DefaultFormatSettings;

  DateTimeFormat.ShortDateFormat := 'yyyy-mm-dd';
  DateTimeFormat.LongTimeFormat := 'hh:nn:ss.zzzzzz';
  DateTimeFormat.DateSeparator := '-';
  DateTimeFormat.TimeSeparator := ':';
  Result := StrToDateTime(ADateTimeStr, DateTimeFormat);

  // Unversal Coordinated Time
  if CovertUTC then
    Result := UniversalTimeToLocal(Result);
end;

function StringToTime(const ADateTimeStr: string; CovertUTC: boolean = true): TTime;
var
  DateTimeFormat: TFormatSettings;
begin
  DateTimeFormat.ShortTimeFormat := 'hh:nn:ss';
  DateTimeFormat.LongTimeFormat := 'hh:nn:ss.zzzzzz';
  DateTimeFormat.DateSeparator := '-';
  DateTimeFormat.TimeSeparator:= ':';
  Result := StrToTime(ADateTimeStr, DateTimeFormat);

  // Unversal Coordinated Time
  if CovertUTC then
    Result := UniversalTimeToLocal(Result);
end;

function DateTimeToString(ADateTime: TDateTime; CovertUTC: boolean = true): string;
var
  DateTimeFormat: TFormatSettings;
begin
  DateTimeFormat.ShortDateFormat := 'yyyy-mm-dd';
  DateTimeFormat.LongTimeFormat := 'hh:nn:ss';
  DateTimeFormat.DateSeparator := '-';

  // Unversal Coordinated Time
  if CovertUTC then
    ADateTime := UniversalTimeToLocal(ADateTime);

  // Convert
  Result := DateTimeToStr(ADateTime, DateTimeFormat);
end;

function DateToString(ADateTime: TDate; CovertUTC: boolean = true): string;
var
  DateTimeFormat: TFormatSettings;
begin
  DateTimeFormat.ShortDateFormat := 'yyyy-mm-dd';
  DateTimeFormat.LongTimeFormat := 'hh:nn:ss';
  DateTimeFormat.DateSeparator := '-';

  // Unversal Coordinated Time
  if CovertUTC then
    ADateTime := UniversalTimeToLocal(ADateTime);

  // Convert
  Result := DateToStr(ADateTime, DateTimeFormat);
end;

function Yearify(Year: cardinal): string;
begin
  if Year = 0 then
    Result := 'Unknown'
  else
    Result := IntToStrIncludePrefixZeros( Year, 4 );
end;

function ConnectedToServer: boolean;
var
  Request: string;
begin
  // Prepare request string
  Request := '{"mode": "test"}';

  // Parse response and extract numbers
  try
    SendClientRequest(Request);

    Result := true;
  except
    Result := false;
  end;
end;

{ TLibraryStatus }

procedure TLibraryStatus.LoadFrom(JSON: TJSONObject);
begin
  TotalTracks := JSON.Get('available', 0);
  TotalPlays := JSON.Get('plays', 0);

  try
    TokenExpireDate := StringToDateTime( JSON.Get('expires', '') );
    LastLibraryModified := StringToDateTime( JSON.Get('lastmodified', '') );
    UpdateTimestamp := StringToDateTime( JSON.Get('timestamp', '') );
  except
  end;
end;

{ TTrackItem }

function TTrackItem.GetPlaybackURL: string;
begin
  Result := STREAMING_ENDPOINT + StreamLocations;
end;

function TTrackItem.ArtworkLoaded(Large: boolean): boolean;
begin
  if TWorkItem.DownloadingImage in Status then
    Exit(false);
  if not Large then
    Result := (CachedImage <> nil) and (not CachedImage.Empty)
  else
    Result := (CachedImageLarge <> nil) and (not CachedImageLarge.Empty);
end;

function TTrackItem.GetArtwork(Large: boolean): TJPEGImage;
begin
  Status := Status + [TWorkItem.DownloadingImage];

  if Large then
    begin
      if (CachedImageLarge = nil) or CachedImageLarge.Empty then
        CachedImageLarge := GetSongArtwork(ArtworkID, TArtSize.Large);

      Result := CachedImageLarge;
    end
  else
    begin
      if (CachedImage = nil) or ((CachedImage <> nil) and CachedImage.Empty) then
        begin
          // Load from Artwork Store
          if ExistsInStore(ID, TDataSource.Tracks) then
            CachedImage := GetArtStoreCache(ID, TDataSource.Tracks)
          else
            // Load from server, save to artowork store
            begin
              CachedImage := GetSongArtwork(ArtworkID, DefaultArtSize);

              // Save artstore
              if ArtworkStore then
                AddToArtworkStore(ID, CachedImage, TDataSource.Tracks);
            end;
        end;

      Result := CachedImage;
    end;

  Status := Status - [TWorkItem.DownloadingImage];
end;

procedure TTrackItem.LoadFrom(JSONPair: TJSONData; AName: string);
var
  JSON: TJSONArray;
begin
  JSON := JSONPair as TJSONArray;

  // Data
  ID := strtoint(AName);

  SetDataWorkStatus(Format('Loading song with ID of %D', [ID]));

  TrackNumber := (JSON.Items[0].AsInteger);
  Year := (JSON.Items[1].AsInteger);

  Title := (JSON.Items[2].AsString);
  Genre := (JSON.Items[3].AsString);

  LengthSeconds := (JSON.Items[4].AsInteger);
  // Typecast as number, then as string for legacy accounts
  try
    AlbumID := (JSON.Items[5].AsInteger);
  except
    AlbumID := strtoint(JSON.Items[5].AsString);
  end;
  try
    ArtworkID := (JSON.Items[6].AsInteger).ToString;
  except
    ArtworkID := (JSON.Items[6].AsString);
  end;
  try
    ArtistID := (JSON.Items[7].AsInteger);
  except
    ArtistID := strtoint(JSON.Items[7].AsString);
  end;

  // ?
  DayUploaded := StringToDateTime( JSON.Items[9].AsString );
  IsInTrash := (JSON.Items[10].AsBoolean);
  FileSize := (JSON.Items[11].AsInteger);

  UploadLocation := (JSON.Items[12].AsString);
  // ?

  Rating := (JSON.Items[14].AsInteger);
  Plays := (JSON.Items[15].AsInteger);

  StreamLocations := (JSON.Items[16].AsString);
  AudioType := (JSON.Items[17].AsString);

  ReplayGain := (JSON.Items[18].AsString);
  try
    UploadTime := StringToTime( (JSON.Items[19].AsString) );
  except
    UploadTime := 0;
  end;
  // ?
end;

{ TAccount }

procedure TAccount.LoadFrom(JSON: TJSONObject);
const
  BACKUP_DATE = '2023-03-05';
var
  S: string;
  O: TJSONString;
  OB: TJSONBoolean;
begin
  SetDataWorkStatus('Loading account from post request');

  if JSON.Find('username', O) then
    Username := O.AsString
  else
    Username := 'User';

  //ShowMessage(JSOn.ToString);

  JSON.Get('preferences', EmptyJSON).Find('onequeue', O);
  OneQueue := stringtoboolean(O.AsString);
  JSON.Get('preferences', EmptyJSON).Find('bitratepref', O);
  BitRate := O.AsString;

  UserID := strtoint(JSON.Get('user_id', ''));
  if JSON.Find('created_on', O) then
    S := O.AsString
  else
    S := BACKUP_DATE;
  CreationDate := StringToDateTime(S);

  if JSON.Find('verified', OB) then
    Verified := OB.AsBoolean;
  if JSON.Find('tester', OB) then
      BetaTester := OB.AsBoolean;

  EmailAdress := JSON.Get('email_address', '');
  if JSON.Find('premium', OB) then
    Premium := OB.AsBoolean;
  if JSON.Find('verified_on', O) then
    S := O.AsString
  else
    S := BACKUP_DATE;
  VerificationDate := StringToDateTime(S);
end;

{ TAlbumItem }

function TAlbumItem.ArtworkLoaded: boolean;
begin
  if TWorkItem.DownloadingImage in Status then
    Exit(false);
  Result := (CachedImage <> nil) and (not CachedImage.Empty);
end;

function TAlbumItem.GetArtwork: TJPEGImage;
var
  AIndex: integer;
begin
  Status := Status + [TWorkItem.DownloadingImage];

  if (CachedImage = nil) or CachedImage.Empty then
    begin
      if Length(TracksID) > 0 then
        begin
          // Load from Artwork Store
          if ExistsInStore(ID, TDataSource.Albums)  then
            CachedImage := GetArtStoreCache(ID, TDataSource.Albums)
          else
            // Load from server, save to artowork store
            begin
              AIndex := GetTrack( TracksID[0] );
              if AIndex <> -1 then
                begin
                  CachedImage := Tracks[AIndex].GetArtwork();

                  // Save artstore
                  if ArtworkStore then
                    AddToArtworkStore(ID, CachedImage, TDataSource.Albums);
                end
                  else
                    CachedImage := DefaultPicture;
            end;
        end
      else
        CachedImage := DefaultPicture;
    end;

  Result := CachedImage;

  Status := Status - [TWorkItem.DownloadingImage];
end;

procedure TAlbumItem.LoadFrom(JSONPair: TJSONData; AName: string);
var
  JSON, SONGS: TJSONArray;
  I: Integer;
  AID: integer;
begin
  JSON := JSONPair as TJSONArray;

  // Data
  ID := strtoint(AName);

  SetDataWorkStatus(Format('Loading album with ID of %D', [ID]));

  AlbumName := (JSON.Items[0].AsString);

  // TRACKS
  SONGS := JSON.Items[1] as TJSONArray;
  SetLength(TracksID, 0);

  for I := 0 to SONGS.Count-1 do
    begin
      AID := SONGS.Items[I].AsInteger;
      // Validate
      if GetTrack(AID) <> -1 then
        TracksID.AddValue( AID );
    end;

  // Data 2
  ArtistID := (JSON.Items[2].AsInteger);

  IsInTrash := (JSON.Items[3].AsBoolean);

  Rating := (JSON.Items[4].AsInteger);
  Disk := (JSON.Items[5].AsInteger);
  Year := (JSON.Items[6].AsInteger);
end;

{ TArtistItem }

function TArtistItem.ArtworkLoaded(Large: boolean): boolean;
begin
  if TWorkItem.DownloadingImage in Status then
    Exit(false);
  if not Large then
    Result := (CachedImage <> nil) and (not CachedImage.Empty)
  else
    Result := (CachedImageLarge <> nil) and (not CachedImageLarge.Empty);
end;

function TArtistItem.GetArtwork(Large: boolean): TJPEGImage;
var
  AIndex: integer;
begin
  Status := Status + [TWorkItem.DownloadingImage];

  if Large then
    begin
      if (CachedImageLarge = nil) or CachedImageLarge.Empty then
        CachedImageLarge := GetSongArtwork(ArtworkID, TArtSize.Large);

      Result := CachedImageLarge;
    end
  else
    begin
      if (CachedImage = nil) or CachedImage.Empty then
        begin
          // Load from Artwork Store
          if ExistsInStore(ID, TDataSource.Artists) then
            CachedImage := GetArtStoreCache(ID, TDataSource.Artists)
          else
          // Load from server, save to artowork store
            begin
              if HasArtwork then
                // Get premade
                CachedImage := GetSongArtwork(ArtworkID, DefaultArtSize)
              else
                begin
                  if (Length(TracksID) >= 4) and AllowArtCollage then
                    begin
                      CachedImage := SongArtCollage(TracksID[0], TracksID[1], TracksID[2], TracksID[3]);
                    end
                  else
                    if Length(TracksID) > 0 then
                      begin
                        AIndex := GetTrack( TracksID[0] );
                        if AIndex <> -1 then
                          CachedImage := Tracks[AIndex].GetArtwork()
                        else
                          CachedImage := DefaultPicture;
                      end
                        else
                          CachedImage := DefaultPicture;
                end;

              // Save artstore
              if ArtworkStore and (CachedImage <> DefaultPicture) then
                AddToArtworkStore(ID, CachedImage, TDataSource.Artists);
            end;
        end;

      Result := CachedImage;
  end;

  Status := Status - [TWorkItem.DownloadingImage];
end;

procedure TArtistItem.LoadFrom(JSONPair: TJSONData; AName: string);
var
  JSON, SONGS: TJSONArray;
  I: Integer;
  AID: integer;
begin
  JSON := JSONPair as TJSONArray;

  // Data
  ID := strtoint(AName);

  SetDataWorkStatus(Format('Loading artist with ID of %D', [ID]));

  ArtistName := (JSON.Items[0].AsString);

  // TRACKS
  SONGS := JSON.Items[1] as TJSONArray;
  SetLength(TracksID, 0);

  for I := 0 to SONGS.Count-1 do
    begin
      AID := SONGS.Items[I].AsInteger;
      // Validate
      if GetTrack(AID) <> -1 then
        TracksID.AddValue( AID );
    end;

  // Data 2
  IsInTrash := (JSON.Items[2].AsBoolean);
  Rating := (JSON.Items[3].AsInteger);

  HasArtwork := (JSON.Count > 4) and (JSON.Items[4].JSONType <> jtNull);
  if HasArtwork then
    begin
      ArtworkID := JSON.Items[4].AsString;

      if ArtworkID = '' then
        HasArtwork := false;
    end;
end;

{ TPlaylistItem }

function TPlaylistItem.ArtworkLoaded(Large: boolean): boolean;
begin
  if TWorkItem.DownloadingImage in Status then
    Exit(false);
  if not Large then
    Result := (CachedImage <> nil) and (not CachedImage.Empty)
  else
    Result := (CachedImageLarge <> nil) and (not CachedImageLarge.Empty);
end;

function TPlaylistItem.GetArtwork(Large: boolean): TJPEGImage;
begin
  Status := Status + [TWorkItem.DownloadingImage];

  if Large then
    begin
      if (CachedImageLarge = nil) or CachedImageLarge.Empty then
        CachedImageLarge := GetSongArtwork(ArtworkID, TArtSize.Large);

      Result := CachedImageLarge;
    end
  else
    begin
      if (CachedImage = nil) or CachedImage.Empty then
        begin
          if ExistsInStore(ID, TDataSource.Playlists) then
            CachedImage := GetArtStoreCache(ID, TDataSource.Playlists)
          else
            begin
              // Load from Artwork Store
              if HasArtwork then
                // Get premade
                CachedImage := GetSongArtwork(ArtworkID, DefaultArtSize)
              else
                // Load from server, save to artowork store
                begin
                  if (Length(TracksID) >= 4) and AllowArtCollage then
                    begin
                      CachedImage := SongArtCollage(TracksID[0], TracksID[1], TracksID[2], TracksID[3]);
                    end
                  else
                    if Length(TracksID) > 0 then
                      CachedImage := Tracks[GetTrack( TracksID[0] )].GetArtwork()
                    else
                      CachedImage := DefaultPicture;
                end;

              // Save artstore
              if ArtworkStore and (CachedImage <> DefaultPicture) then
                AddToArtworkStore(ID, CachedImage, TDataSource.Playlists);
            end;
        end;
    end;

  Result := CachedImage;

  Status := Status - [TWorkItem.DownloadingImage];
end;

procedure TPlaylistItem.LoadFrom(JSONPair: TJSONData; AName: string);
var
  JSON, SONGS: TJSONArray;
  I: Integer;
  AID: integer;
begin
  JSON := JSONPair as TJSONArray;

  // Data
  ID := strtoint(AName);

  SetDataWorkStatus(Format('Loading playlist with ID of %D', [ID]));

  Name := (JSON.Items[0].AsString);

  // TRACKS
  SONGS := JSON.Items[1] as TJSONArray;
  SetLength(TracksID, 0);

  for I := 0 to SONGS.Count-1 do
    begin
      AID := SONGS.Items[I].AsInteger;
      // Validate
      if GetTrack(AID) <> -1 then
        TracksID.AddValue( AID );
    end;

  // ?
  // ?
  // ?

  // Data 2
  if JSON.Items[5].JSONType = jtString then
    PlaylistType := JSON.Items[5].AsString
  else
    PlaylistType := '';
  if JSON.Items[6].JSONType = jtString then
    Description := JSON.Items[6].AsString
  else
    Description := '';

  HasArtwork := (JSON.Count > 7) and (JSON.Items[7].JSONType <> jtNull);
  if HasArtwork then
    begin
      ArtworkID := JSON.Items[7].AsString;

      if ArtworkID = '' then
        HasArtwork := false;
    end;

  // ?
end;

{ TSession }

procedure TSession.LoadFrom(JSON: TJSONObject);
var
  O: TJSONString;
begin
  SetDataWorkStatus('Loading session');

  DeviceName := JSON.Get('device_name', '');

  Joinable := JSON.Get('joinable', false);
  Connected := JSON.Get('connected', false);

  Client := JSON.Get('client', '');

  LastLogin := StringToDateTime(JSON.Get('last_login', ''));

  if JSON.Find('location', O) then
    Location := O.AsString;
end;

end.
