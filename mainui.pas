unit MainUI;

{$mode delphi}{$H+}
{$SCOPEDENUMS ON}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, Menus, cthreads, inifiles, LCLIntf, LCLType, Clipbrd,
  DateUtils,

  // Networking
  IdSSLOpenSSL, IdHTTP, IdComponent,

  // System
  LibDefine,

  // Cod
  Cod.ArrayHelpers, Cod.Math,  Cod.Files, Cod.Types, Cod.Graphics,
  Cod.ColorUtils, Cod.StringUtils, Cod.Internet, Cod.VersionUpdate,
  Cod.SysUtils,

  // Audio
  Bass, Cod.Audio,

  // Utils
  Math, fpjson, fileutil,

  // Libs
  BroadcastAPI, SpectrumVis3D, Types, LMessages, EditBtn, unitywsctrls;

const
  SECT_MAIN = 'Main';
  SECT_META = 'Meta';

type
  // Cardinals
  TViewStyle = (List, Cover);
  TRepeat = (Off, All, One);
  TThreadStatus = (Queued, Running, Finished, Error, Terminated);
  TSortType = (Default, Alphabetic, Year, Rating, Flipped);
  TSortTypes = set of TSortType;
  TSearchFlag = (ExactMatch, CaseSensitive, SearchInfo, SearchTrashed);
  TSearchFlags = set of TSearchFlag;
  TPlayType = (Streaming, Local, CloudDownload);
  TDownloadedKind = (None, Direct, Indirect); // for tracks downloaded from an album
  THiddenReason = (Manual, Search, Trashed, Filter);
  THiddenReasons = set of THiddenReason;
  TPage = (None, Home, Search, Queue, Albums, Songs, Artists, Playlists, Genres, Trash,
    Downloads, History, Account, Settings, ViewAlbum, ViewArtist, ViewPlaylist, ViewGenre);

  // Helper

  { TStringListHelper }

  TStringListHelper = class helper for TStringList
  public
    procedure SafeDelete(Element: string); overload;
    procedure SafeDelete(Index: integer); overload;

    procedure LoadFromString(AString, ASeparator: string);
    function ToString(ASeparator: string): string;
  end;

  { Threads }

  { TTaskThread }
  TTaskThread = class(TThread)
  protected
    procedure IncCounter;
    procedure DecCounter;

    procedure Execute; override;

  public
    Status: TThreadStatus;

    procedure DoTerminated; virtual;

    procedure DoPrepare; virtual;
    procedure DoWork; virtual;
    procedure DoFinish; virtual;

    constructor Create; virtual;
  end;

  { TDialogedTaskThread }
  TDialogedTaskThread = class(TTaskThread) // if the form is not created, these tasks can be non-dialoged
  public
    // Manual status
    Succeeded: boolean;

    // Work, defined by user
    procedure DoFinish; override;

    constructor Create; override;
  end;

  { TInstantTaskThread }
  TInstantTaskThread = class(TTaskThread)
    constructor Create; override;
  end;

  { TDownloadSongThread }
  TDownloadSongThread = class(TTaskThread)
  private
    procedure InternalWB(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure InternalW(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure SyncWork;

  public
    DownloadIndex: integer;
    FilePath: string;

    WorkCount,
    TotalWorkCount: int64;

    function Progress: integer;

    procedure DoWorkItem; virtual;
    procedure DoPrepare; override;
    procedure DoWork; override;
    procedure DoFinish; override;
  end;

  { TDialoggedDownloadSongThread }
  TDialoggedDownloadSongThread = class(TDownloadSongThread)
    LastProgress: integer;

    procedure DoWorkItem; override;
    procedure DoFinish; override;

    constructor Create; override;
  end;

  { TCloudDownloadSongThread }
  TCloudDownloadSongThread = class(TDownloadSongThread)
    LastProgress: integer;
    AutoStartPlayback: boolean;

    procedure DoTerminated; override;

    procedure DoPrepare; override;
    procedure DoWorkItem; override;
    procedure DoFinish; override;

    constructor Create; override;
  end;

  { TDownloadArtworkThread }
  TDownloadArtworkThread = class(TTaskThread)
  public
    DownloadSource: TDataSource;
    DownloadIndex: integer;
    ArtPointer: TJPEGImage;

    procedure DoWork; override;
    procedure DoFinish; override;
  end;

  { TPushHistoryThread }
  TPushHistoryThread = class(TTaskThread)
  public
    Items: TArray<TTrackHistoryItem>;

    procedure DoWork; override;
  end;

  { TLoadLibraryThread }
  TLoadLibraryThread = class(TInstantTaskThread)
    procedure DoWork; override;
    procedure DoFinish; override;
  end;

  { TLoginThread }
  TLoginThread = class(TInstantTaskThread)
    procedure DoWork; override;
    procedure DoFinish; override;
  end;

  { Custom Dialoged Funtions }

  { TDialogTashEmptyTrash }
  TDialogTashEmptyTrash = class(TDialogedTaskThread)
    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { TDialogCheckUpdatesThread }
  TDialogCheckUpdatesThread = class(TDialogedTaskThread)
    ServerVersion: TVersionRec;

    procedure DoPrepare; override;
    procedure DoWork; override;
    procedure DoFinish; override;
  end;

  { TDialogTrashItem }
  TDialogTrashItem = class(TDialogedTaskThread)
    ItemID: string;
    Source: TDataSource;
    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { TDialogRestoreItem }
  TDialogRestoreItem = class(TDialogedTaskThread)
    ItemID: string;
    Source: TDataSource;
    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { TDialogCreatePlaylist }
  TDialogCreatePlaylist = class(TDialogedTaskThread)
    Name, Description: string;
    MakePublic: boolean;
    Tracks: TStringArray;
    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { TInfoLoadArtworkThread }
  TInfoLoadArtworkThread = class(TDownloadArtworkThread)
    procedure DoFinish; override;
  end;

  { TViewLoadArtworkThread }
  TViewLoadArtworkThread = class(TDownloadArtworkThread)
    procedure DoFinish; override;
  end;

  { TPlayerLoadArtworkThread }
  TPlayerLoadArtworkThread = class(TDownloadArtworkThread)
    procedure DoFinish; override;
  end;

  { TDialogPlaylistUpdate }
  TDialogPlaylistUpdate = class(TDialogedTaskThread)
    ChangeName, ChangeDescription, ChangeTracks, PreAppendTracks, AppendTracks, DeleteTracks: boolean;
    NewName, NewDescription: string;
    NewTracks: TStringArray;
    ItemID: string;

    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { TDialogUpdateRating }
  TDialogUpdateRating = class(TDialogedTaskThread)
    ItemSource: TDataSource;
    ItemID: string;
    NewRating: integer;

    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { TDialogUpdateRating }

  { TDialogStopThreads }
  TDialogStopThreads = class(TDialogedTaskThread)
    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { TFunctionsContainer }
  TFunctionsContainer = class
    class procedure SyncStatus;
    class procedure SyncStatusData;
  end;

  { THistoryItem }
  THistoryItem = record
    Page: TPage;
    ScrollValue: integer;
    // View mode is saved with a page by page basis
  end;

  { TDrawItem }
  TDrawItem = record
    Index: integer;

    ItemID: string;

    Title: string;
    InfoShort: string;
    InfoLong: string;
    Rating: integer;

    HideReasons: THiddenReasons;
    Trashed: boolean;

    Information: TStringArray;

    Bounds: TRect;
    Source: TDataSource;

    (* Other data *)
    OnlyQueue: boolean;
    Loaded: boolean;

    (* Mix data *)
    function Hidden: boolean;
    function Downloaded: boolean;
    function Active: boolean;

    function IsDownloaded: TDownloadedKind;

    function ToggleDownloaded: boolean;
    procedure SetHidden;

    (* Data Information *)
    function GetPremadeInfoList: string;
    function GetPicture: TJPEGImage;
    function GetLargePicture: TJPEGImage; // UI thread

    (* When Clicked *)
    procedure Execute;
    procedure OpenInformation;

    (* UI *)
    function Invalid: boolean;

    (* Audo Load *)
    procedure LoadSourceID(ID: string; From: TDataSource);
    procedure LoadSource(AIndex: integer; From: TDataSource);
    procedure ReloadSource;

    (* Load Thread *)
    procedure StartPictureLoad;
  end;
  TDrawItemPointer = ^TDrawItem;

  { TDialogLibraryLoadThread }
  TDialogLibraryLoadThread = class(TDialogedTaskThread)
    procedure LoadLibraryComponent(Source: TDataSource);

    procedure DoPrepare; override;
    procedure DoWork; override;
  end;

  { Download Manager }

  { TDownloaderClass }
  TDownloaderClass = class(TTaskThread)
  private
    var
      ItemStatus: string;
      ItemStatusNice: string;
      TotalWork: integer;
      LastPercent: integer;

    (* Internal *)
    procedure TrackDownloadWB(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure TrackDownloadW(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);

  public
    var
      CurrentTrack: string; // The ID of the track that's currently being written

    (* Files *)
    procedure InitDirStructure;
    class function GetDir(DataKind: TDataSource): string;
    procedure ValidateFiles(DownKind: TDataSource);

    (* Downloading *)
    function BuildDownloadList(DownKind: TDataSource): TStringList;

    procedure DownloadType(DownKind: TDataSource);

    (* Masters *)
    procedure DoDownload;
    procedure DoUpdateFiles;

    (* Thread *)
    constructor Create; override;
    procedure DoWork; override;
    procedure DoFinish; override;
  end;

  { TDownloadManager }
  TDownloadManager = class
  private
    const
      CAT_DOWNLOAD = 'Downloads';
      SEPAR = ',';

  public
    // Threads
    Downloader: TDownloaderClass;

    // List
    MasterList: TStringList;

    // Data
    Tracks,
    Albums,
    Artists,
    Playlists: TStringList;

    // Procs
    (* Thread *)
    procedure PeacefullyCloseThread;

    (* Playback *)
    function IsLocal(Track: string; out FilePath: string): boolean;

    (* Managing *)
    procedure AddItem(ID: string; Source: TDataSource);
    procedure RemoveItem(ID: string; Source: TDataSource);

    (* Saving *)
    function GetConfigFile: TIniFile;
    procedure LoadConfig;
    procedure SaveConfig;

    procedure ValidateDownloads; { Remove invalid enteries from INDEX }
    procedure BuildMasterList;
    procedure AddToMaster(Items: TStringArray);

    (* Download *)
    procedure CheckDownloads; { Begin thread }

    // Constructors
    destructor Destroy; override;
    constructor Create;
  end;

  { TMain }
  TMain = class(TForm)
    Artwork_Storage1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    Button1: TButton;
    Button_CheckUpdate: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    ComboBox1: TComboBox;
    Control_ToggleSearch: TButton;
    Control_ViewStyle: TButton;
    Equaliser_Indic: TLabel;
    Filter_Box: TEdit;
    FlowPanel1: TFlowPanel;
    IdHTTP1: TIdHTTP;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Download_StoreUse: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label_UpdateStat: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Option_checkupdates: TCheckBox;
    Stat_Viewer: TLabel;
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
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Account_Devices: TListBox;
    MenuItem13: TMenuItem;
    Popup_ID: TMenuItem;
    Option_savequeue: TCheckBox;
    Popup_Playlist_Remove: TMenuItem;
    Music_Menu1: TButton;
    Option_datasaver: TCheckBox;
    Option_playervisualisation: TCheckBox;
    Option_playerstaytop: TCheckBox;
    Option_threads: TTrackBar;
    Option_playeropacity: TTrackBar;
    Page_Account: TPanel;
    Popup_Save: TMenuItem;
    Music_Artist: TLabel;
    Music_Artwork: TImage;
    Music_Name: TLabel;
    Music_Next: TButton;
    Music_Play: TButton;
    Music_Position: TTrackBar;
    Music_Volume: TTrackBar;
    Music_Speed: TTrackBar;
    Music_Prev: TButton;
    Music_Menu: TButton;
    Music_Time: TLabel;
    Page_ToolbarContainer: TPanel;
    Panel12: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Generic_AddPlaylist: TMenuItem;
    Popup_Queue_Now: TMenuItem;
    Popup_Queue_Bottom: TMenuItem;
    Queue_Clear: TButton;
    Queue_Jump: TButton;
    Queue_Repeat: TButton;
    Queue_Shuffle: TButton;
    SaveDialog1: TSaveDialog;
    ScrollBox2: TScrollBox;
    Search_Box: TEditButton;
    Search_Container: TPanel;
    Separator1: TMenuItem;
    Separator_Playlist: TMenuItem;
    Separator_Save: TMenuItem;
    Separator_Queue: TMenuItem;
    Popup_Queue_Remove: TMenuItem;
    Option_artwork: TCheckBox;
    Option_streaming: TCheckBox;
    Label10: TLabel;
    Label11: TLabel;
    Artwork_Storage: TLabel;
    Label9: TLabel;
    Option_rating: TCheckBox;
    DrawingBox: TPaintBox;
    Drawing_Container: TPanel;
    Generic_Menu: TPopupMenu;
    Label4: TLabel;
    Popup_AddQueue: TMenuItem;
    Add_Playlist: TMenuItem;
    Menu_Permadelete: TMenuItem;
    MenuItem15: TMenuItem;
    Separator_Download: TMenuItem;
    MenuItem17: TMenuItem;
    Menu_Trash: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    Popup_Download: TMenuItem;
    Separator_Browse: TMenuItem;
    Menu_Restore: TMenuItem;
    N1: TMenuItem;
    Page_Settings: TPanel;
    ScrollBox1: TScrollBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuIcons: TImageList;
    Page_Container: TPanel;
    Page_Draw: TPanel;
    Page_Title: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    AudioUpdate: TTimer;
    ExtrasMenu: TPopupMenu;
    ScrollBar1: TScrollBar;
    LoginDelay: TTimer;
    Sidebar_content: TPanel;
    Sidebar_Description: TLabel;
    Sidebar_full: TPanel;
    Sidebar_line: TPanel;
    Sidebar_Name: TLabel;
    Sidebar_Picture: TImage;
    Sidebar_Toggle: TSpeedButton;
    Sidebar_Vertical: TLabel;
    SpeedButton1: TSpeedButton;
    PressNow: TTimer;
    Subview_Sidebar: TPanel;
    Debugger32: TTimer;
    ToggleBox1: TToggleBox;
    ToggleBox10: TToggleBox;
    ToggleBox11: TToggleBox;
    ToggleBox12: TToggleBox;
    ToggleBox13: TToggleBox;
    ToggleBox14: TToggleBox;
    ToggleBox15: TToggleBox;
    ToggleBox16: TToggleBox;
    ToggleBox2: TToggleBox;
    ToggleBox3: TToggleBox;
    ToggleBox4: TToggleBox;
    ToggleBox5: TToggleBox;
    ToggleBox6: TToggleBox;
    ToggleBox7: TToggleBox;
    ToggleBox8: TToggleBox;
    ToggleBox9: TToggleBox;
    Toolbar_Queue: TPanel;
    Toolbar_Search: TPanel;
    Toolbar_Search1: TPanel;
    Toolbar_Search2: TPanel;
    Toolbar_Downloads: TPanel;
    Toolbar_PlaylistControls: TPanel;
    Toolbar_ViewArtist: TPanel;
    Toolbar_Search4: TPanel;
    Toolbar_TrashControls: TPanel;
    Toolbar_ViewControls: TPanel;
    TrayIcon1: TTrayIcon;
    Visualisation_Player: TPaintBox;
    VisualRender: TTimer;
    procedure AudioUpdateTimer(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button_CheckUpdateClick(Sender: TObject);
    procedure Label21Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure Music_SpeedKeyPress(Sender: TObject; var Key: char);
    procedure Music_SpeedMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Popup_IDClick(Sender: TObject);
    procedure Popup_Playlist_RemoveClick(Sender: TObject);
    procedure Music_Menu1Click(Sender: TObject);
    procedure Option_artworkClick(Sender: TObject);
    procedure Option_datasaverClick(Sender: TObject);
    procedure Option_threadsChange(Sender: TObject);
    procedure Popup_DownloadClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure Music_ArtistClick(Sender: TObject);
    procedure Music_ArtworkMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Music_VolumeChange(Sender: TObject);
    procedure Music_SpeedChange(Sender: TObject);
    procedure Music_MenuClick(Sender: TObject);
    procedure Popup_SaveClick(Sender: TObject);
    procedure Queue_ClearClick(Sender: TObject);
    procedure Queue_JumpClick(Sender: TObject);
    procedure Queue_RepeatClick(Sender: TObject);
    procedure Generic_AddPlaylistClick(Sender: TObject);
    procedure Music_NextClick(Sender: TObject);
    procedure Music_PrevClick(Sender: TObject);
    procedure Option_ratingChange(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Control_ToggleSearchClick(Sender: TObject);
    procedure Control_ViewStyleClick(Sender: TObject);
    procedure DrawingBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawingBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawingBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem20Click(Sender: TObject);
    procedure Popup_Queue_BottomClick(Sender: TObject);
    procedure Popup_Queue_NowClick(Sender: TObject);
    procedure Popup_Queue_RemoveClick(Sender: TObject);
    procedure Queue_ShuffleClick(Sender: TObject);
    procedure Search_BoxButtonClick(Sender: TObject);
    procedure Search_BoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Filter_BoxChange(Sender: TObject);
    procedure Filter_BoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure Generic_MenuPopup(Sender: TObject);
    procedure LoginDelayTimer(Sender: TObject);
    procedure MenuDeleteClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure Popup_AddQueueClick(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Menu_RestoreClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure Music_PlayClick(Sender: TObject);
    procedure Music_PositionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Music_PositionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawingBoxPaint(Sender: TObject);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PressNowTimer(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure SpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Sidebar_ToggleClick(Sender: TObject);
    procedure Debugger32Timer(Sender: TObject);
    procedure ToggleBox15Change(Sender: TObject);
    procedure ToggleBox16Change(Sender: TObject);
    procedure ToggleBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToggleBox6Change(Sender: TObject);
    procedure ToggleVisual(Sender: TObject);
    procedure VisualRenderTimer(Sender: TObject);
  private
    function GetScrollMax: integer;
    function GetScrollPosition: integer;
    procedure SetScrollMax(AValue: integer);
    procedure SetScrollPosition(AValue: integer);
  private
    var
      CloudDownloader: TCloudDownloadSongThread;

    // Utils
    function  PageToName(APage: TPage): string;
    function GetItemCount: integer;

    // System
    procedure InitiateDirectories;

    // Login
    function DoLogin: boolean;
    procedure DoReloadlibrary;
    procedure OpenLoginPage;

    procedure SaveCredentials;
    procedure LoadCredentials;
    procedure DeleteCredentials;

    // List
    procedure Sort;
    procedure FiltrateSearch(Term: string; Flags: TSearchFlags);
    function SearchCompareFound(Data1, Data2: string; Flags: TSearchFlags): boolean;

    procedure CurrentViewReload;

    // Offline
    procedure LoadOfflineMode;

    // Debug
    procedure DoDebug;

    // Queue (Track IDs)
    procedure QueueAdd(Track: string; AutoPlay: boolean = true); overload;
    procedure QueueAdd(Tracks: TStringArray; AutoPlay: boolean = true); overload;
    procedure QueueClear;
    procedure QueueMove(IndexFrom, IndexTo: integer);
    procedure QueueDelete(Index: integer);
    procedure StartDrawPlay(SongID: string); // plays the song of the draw index clicked, and adds the rest to the queue
    procedure QueueSetPos(Index: integer; StartPlay: boolean = true);
    procedure QueuePlayCurrent(StartPlay: boolean = true);
    procedure QueueNext;
    procedure QueuePrevious;

    procedure QueueUpdated(OverrideShuffleDefault: boolean = true); // ran when the queue is modified
    procedure QueueTrackChanged;
    procedure QueueUpdateUI;

    procedure QueueFindPlaying;

    procedure SaveQueueToFile;
    procedure LoadQueueFromFile;

    // Shuffle controls
    procedure LoadShuffleDefault;
    procedure SaveShuffleDefault;
    procedure ShuffleQueue;

    // Threading
    procedure WaitForThreads;

    // Drawing
    procedure DoCustomDraw(Sender: TObject);
    procedure DoCustomInteractUp(Button: TMouseButton);
    procedure DoCustomInteractMove(X, Y: integer);
    procedure LoadDrawItems;

    procedure SetViewStyle(Style: TViewStyle);

    procedure UpdateScroll;

    procedure DrawItemCanvas(Canvas: TCanvas; ARect: TRect; Item: TDrawItemPointer);

    // Visuals
    procedure RenderVisualisations;

    // Threads
    procedure StopThreads;

    // Scaling
    procedure DoScaling;
    function GetMonitorScaling: real;

    // Status
    procedure UpdateSpecialStat;
    procedure ResetSpecialStat;
    procedure SetSpecialStat(Value: string);

    // Paging
    procedure SelectPage(APage: TPage; AData: string = '');

    // Song
    function Playing: boolean;
    procedure PlaySong(ID: string; StartPlay: boolean = true);
    procedure AddSongToHistory;
    procedure SongFinished;
    procedure CloudDownloadPlay(TrackIndex: integer; StartPlay: boolean);
    procedure StopCloudDownloader;

    // Player
    procedure PlayerUpdate;
    procedure PlayerApplySettings;
    procedure SetPlayIcon(AStatus: TPlayStatus);
    procedure LoadSongInfo;
    procedure ApplyEqualisation;

    // Popup
    procedure OpenPopupPlayer;
    procedure UpdatePopupPlayIcons;
    procedure UpdatePopupPlayMusic;
    procedure UpdatePopupTime;

    // History
    procedure AddToHistory;

    // Filters
    procedure SetDrawPageFilter(ATo: TDataSource);

    // Settings
    procedure AppSettings(Load: boolean);

    // Props
    property ScrollPosition: integer read GetScrollPosition write SetScrollPosition;
    property ScrollMax: integer read GetScrollMax write SetScrollMax;

    // Update
    procedure BackendUpdate(AUpdate: TDataSource);
  public

  end;

  // Notify
  procedure APIStatusChanged(Status: string);
  procedure APIStatusDataChanged(Status: string);

  procedure DoReturnToLogin;

const
  // Audio playback
  BUFFER_INTERVAL = 1;

  // Icons
  ICON_PLAY = '▶';
  ICON_PAUSED = '⏸';
  ICON_BUFFERING = '⏳';

  ICON_LIST = '▤';
  ICON_COVER = '▦';

  ICON_MENU = '≡';
  ICON_CLOSE = '✕';
  ICON_BIGCLOSE = '✖';

  ICON_FILL = '■';
  ICON_DOWNLOAD = '▼';

  // Strings
  BLANK_PAGE = 'Crickets... Nothing is here...';

  // Page
  MAX_HISTORY = 100;

  HOME_TOTAL_ROWS = 5;

  // Page groups
  GENERAL_DRAW_PAGES = [TPage.Home, TPage.Search, TPage.Queue, TPage.Albums, TPage.Songs,
    TPage.Artists, TPage.Playlists, TPage.Genres, TPage.Trash, TPage.Downloads, TPage.History,
    TPage.ViewAlbum, TPage.ViewArtist, TPage.ViewPlaylist, TPage.ViewGenre];

  CUSTOM_DRAW = [TPage.Home, TPage.Search, TPage.Queue];

  CUSTOM_INTERACT = [TPage.Queue];
  DRAG_ABLE_PAGES = [TPage.Queue];

var
  Main: TMain;

  // System
  AppData: string;

  TempFolder: string;
  DownloadsFolder: string;

  LastUpdateCheck: TDateTime = 0;

  // Threading
  SimultanousTaskThreads: integer = 0;

  MAX_THREAD_COUNT: integer = 5;

  // Draw Settings
  CoverSpacing: integer = 10;
  CoverWidth: integer = 180;
  CoverHeight: integer = 240;
  CoverRadius: integer = 15;

  ListSpacing: integer = 10;
  ListHeight: integer = 100;
  ListRadius: integer = 15;

  ItemColor: TColor = clMenu;
  ItemActiveColor: TColor = clHighlight;

  // Dragging
  IsDragging: boolean = false;
  DraggingIndex: integer = 0;

  // Popup
  PopupIndex: integer;
  PopupItem: TDrawItem;

  // Scaling
  ScaleFactor: real;

  // Queue
  Queue: TStringList;
  QueueDefault: TStringList; // this is a way to "undo" the shuffling of a
                              // Queue, once Queue is modified, It will be the same as Queue
  QueueIndex: integer;

  // Player settings
  ShuffleMode: boolean;
  RepeatMode: TRepeat;

  // Pages filters
  DrawPageFilter: TDataSource;
  DrawPageFilters: array[Low(TPage)..High(TPage)] of TDataSource;
  SearchFilters: TDataSources;

  // Audio
  Player: TAudioPlayer;
  LastPlayStatus: TPlayStatus;
  PlayerSpeed: single = 1;
  PlayerVolume: single = 1;
  PlayerTotalLength: string;

  // Paging
  CurrentPage: TPage;
  PageData: string;
  PageHistory: TArray<THistoryItem>;

  // Offline
  IsOffline: boolean;

  // Info
  LoginScriptDone: boolean = false;

  // Application
  SPECIAL_STATUS: string;

  // Drawing
  PageIndexingError: boolean;
  DrawItems: TArray<TDrawItem>;
  DrawPage: boolean;
  CustomDraw: boolean;
  CustomInteract: boolean;
  SupportsDragging: boolean;
  ViewStyle: TViewStyle = TViewStyle.List;
  ViewStyles: array[Low(TPage)..High(TPage)] of TViewStyle;
  TrashInheritance: boolean; // shows hidden trashed items

  DrawCursorPos: TPoint;

  DrawPressed: boolean;
  IndexDown,
  Press10Stat,
  IndexHover: integer;

  SortType: TSortType;
  SortingList: TIntArray;

  // Page custom draw
  HomeColumns: integer;

  // Filter
  LastFilterQuery: string;

  // Downloading
  DownloadManager: TDownloadManager;

  // Playing
  PlayID: string = '';
  PlayType: TPlayType;

  // Components
  { Slider }
  SliderPressed: boolean;
  LastPlayState: TPlayStatus;

  // Visualisations
  Spectrum_Player: TSpectrum;

implementation

uses
  // Forms
  LoadingLibrary,
  About,
  taskexecution,
  iteminformation,
  helpform,
  loginform,
  createplaylistform,
  PopupPlayForm;

{$R *.lfm}

procedure APIStatusChanged(Status: string);
begin
  TThread.Synchronize(nil, TFunctionsContainer.SyncStatus);
end;

procedure APIStatusDataChanged(Status: string);
begin
  TThread.Synchronize(nil, TFunctionsContainer.SyncStatusData);
end;

procedure DoReturnToLogin;
begin
  // Select home
  Main.SelectPage(TPage.None);

  Main.WaitForThreads;

  // Login
  Main.OpenLoginPage;
end;

{ TStringListHelper }

procedure TStringListHelper.SafeDelete(Element: string);
var
  Index: integer;
begin
  Index := IndexOf(Element);

  SafeDelete(Index);
end;

procedure TStringListHelper.SafeDelete(Index: integer);
begin
  if Index <> -1 then
      Delete(Index);
end;

procedure TStringListHelper.LoadFromString(AString, ASeparator: string);
var
  I: integer;
  AItems: TStringArray;
begin
  Clear;

  AItems := AString.Split([ASeparator]);

  for I := 0 to System.High(AItems) do
    if AItems[I] <> '' then
      Add( AItems[I] );
end;

function TStringListHelper.ToString(ASeparator: string): string;
var
  I, ACount: integer;
begin
  Result := '';
  ACount := Count;
  for I := 0 to ACount-1 do
    Result := Concat(Result, Self[I], ASeparator);

  // Remove last
  Result := Copy(Result, 1, Length(Result) - Length(ASeparator));
end;

{ TDialogCheckUpdatesThread }

procedure TDialogCheckUpdatesThread.DoPrepare;
begin
  TaskExec.Title.Caption:='Checking for updates...';

  // UI
  Main.Button_CheckUpdate.Enabled := false;
  Main.Label_UpdateStat.Caption := 'Checking...';
end;

procedure TDialogCheckUpdatesThread.DoWork;
begin
  try
    ServerVersion.APILoad('ibroadcast-linux');

    Succeeded := true;
  except
    Succeeded := false;
  end;

  // Reduce API strain
  Sleep(3000);
end;

procedure TDialogCheckUpdatesThread.DoFinish;
var
  Newer: boolean;
begin
  if Succeeded then
    begin
      Main.Label_UpdateStat.Caption := ServerVersion.ToString();
      Newer := ServerVersion.NewerThan(APP_VERSION);

      // Status
      LastUpdateCheck := Now;

      // Update dialog
      if Newer and (MessageDlg('New version released', 'There is a new version of iBroadcast avalabile to download. Version ' + ServerVersion.ToString()
      + #13'Would you like to download It now?',
        mtInformation, [mbYes, mbNo], 0) = mrYes) then
          OpenURL('https://www.codrutsoft.com/apps/ibroadcast-linux/');
    end
      else
        Main.Label_UpdateStat.Caption:='Failed to check for updates';

  // Ui
  Main.Button_CheckUpdate.Enabled:=true;

  // Inherit
  inherited;
end;

{ TDialogLibraryLoadThread }

procedure TDialogLibraryLoadThread.LoadLibraryComponent(Source: TDataSource);
var
  Files: TStringList;
  Directory: string;
  Ini: TIniFile;
  I: integer;
  Index: integer;
  AID: string;
  DataFile: string;
function AddNew: integer;
begin
  Result := 0;
  case Source of
    TDataSource.Tracks: begin
      Result := Length(Tracks);
      SetLength(Tracks, Result+1);
    end;

    TDataSource.Albums: begin
      Result := Length(Albums);
      SetLength(Albums, Result+1);
    end;

    TDataSource.Artists: begin
      Result := Length(Artists);
      SetLength(Artists, Result+1);
    end;

    TDataSource.Playlists: begin
      Result := Length(Playlists);
      SetLength(Playlists, Result+1);
    end
  end;
end;

function StringToArray(AStr: string): TStringArray;
var
  Items: TStringArray;
  I: integer;
begin
  Result := [];
  Items := AStr.Split([',']);
  for I := 0 to High(Items) do
    if Items[I] <> '' then
      try
        Result.AddValue(Items[I]);
      except
        Exit([]);
      end;
end;

begin
  Directory := TDownloaderClass.GetDir(Source);

  Files := FindAllFiles(Directory, '*.ini', false);
  Files.Sorted:=true;

  // Load
  for I := 0 to Files.Count-1 do begin
    DataFile := Files[I];
    Ini := TIniFile.Create(DataFile);
    with Ini do
      try
        try
          AID := ChangeFileExt(ExtractFileName(DataFile), '');
        except
          Continue;
        end;

        Index := AddNew;
        case Source of
          TDataSource.Tracks: with Tracks[Index] do begin
            ID := AID;

            Title := ReadString(SECT_MAIN, 'Name', Title);
            ArtworkID := ReadString(SECT_MAIN, 'Artwork', ArtworkID);

            AlbumID := ReadString(SECT_META, 'Album', AlbumID);
            ArtistID := ReadString(SECT_META, 'Artist', ArtistID);

            Year := ReadInteger(SECT_META, 'Year', Year);
            Genre := ReadString(SECT_META, 'Genre', Genre);
            LengthSeconds := ReadInteger(SECT_META, 'Length', LengthSeconds);
            FileSize := ReadInteger(SECT_META, 'Size', FileSize);
            Rating := ReadInteger(SECT_META, 'Rating', Rating);
            Plays := ReadInteger(SECT_META, 'Plays', Plays);
            AudioType := ReadString(SECT_META, 'Audio Type', AudioType);
          end;

          TDataSource.Albums: with Albums[Index] do begin
            ID := AID;

            AlbumName := ReadString(SECT_MAIN, 'Name', AlbumName);
            TracksID := StringToArray(ReadString(SECT_MAIN, 'Tracks', ''));

            ArtistID := ReadString(SECT_META, 'Artist', ArtistID);

            Year := ReadInteger(SECT_META, 'Year', Year);
            Rating := ReadInteger(SECT_META, 'Rating', Rating);
          end;

          TDataSource.Artists: with Artists[Index] do begin
            ID := AID;

            ArtistName := ReadString(SECT_MAIN, 'Name', ArtistName);
            ArtworkID := ReadString(SECT_MAIN, 'Artwork', ArtworkID);
            TracksID := StringToArray(ReadString(SECT_MAIN, 'Tracks', ''));

            Rating := ReadInteger(SECT_META, 'Rating', Rating);
          end;

          TDataSource.Playlists: with Playlists[Index] do begin
            ID := AID;

            Name := ReadString(SECT_MAIN, 'Name', Name);
            ArtworkID := ReadString(SECT_MAIN, 'Artwork', ArtworkID);
            TracksID := StringToArray(ReadString(SECT_MAIN, 'Tracks', ''));

            Description := ReadString(SECT_META, 'Description', Description);
          end;
        end;

      finally
        Free;
      end;
  end;
end;

procedure TDialogLibraryLoadThread.DoPrepare;
begin
  TaskExec.Title.Caption:='[Offline Mode] Loading local library...';
end;

procedure TDialogLibraryLoadThread.DoWork;
begin
  EmptyLibrary;

  // Load core components
  LoadLibraryComponent(TDataSource.Tracks);
  LoadLibraryComponent(TDataSource.Albums);
  LoadLibraryComponent(TDataSource.Artists);
  LoadLibraryComponent(TDataSource.Playlists);

  // Load based data
  LoadLibraryGenres;

  Succeeded := true;
end;

{ TCloudDownloadSongThread }

procedure TCloudDownloadSongThread.DoTerminated;
begin
  Main.ResetSpecialStat;
end;

procedure TCloudDownloadSongThread.DoPrepare;
begin
  Main.SetSpecialStat(Format('Preparing to download "%S"', [Tracks[DownloadIndex].Title]));
  Main.UpdateSpecialStat;

  // Delete
  if fileexists(FilePath) then
    deletefile(FilePath);
end;

procedure TCloudDownloadSongThread.DoWorkItem;
var
  AProgress: integer;
begin
  // Stopped
  if Terminated then
    begin
      DoTerminated;
      Abort;
    end;

  // Progress
  AProgress := Progress;
  if LastProgress = AProgress then
    Exit;

  // Set
  Main.SetSpecialStat(Format('Downloading track %D%%', [AProgress]));
  Main.UpdateSpecialStat;

  // Last
  LastProgress := AProgress;
end;

procedure TCloudDownloadSongThread.DoFinish;
begin
  // Status
  Main.ResetSpecialStat;

  // Play
  if not Terminated then
    if Player.OpenFile(FilePath) then
      begin
        Main.PlayerUpdate;
        if AutoStartPlayback then
          Player.Play;
      end;
end;

constructor TCloudDownloadSongThread.Create;
begin
  inherited;
  LastProgress := -1;
  FreeOnTerminate:=false;

  FilePath := TempFolder + 'playback.mp3';
end;

{ TDialogStopThreads }

procedure TDialogStopThreads.DoPrepare;
begin
  TaskExec.Title.Caption:='Stopping threads...';
end;

procedure TDialogStopThreads.DoWork;
const
  WAIT_FOR_THREAD = 15; // Time to wait for threads (seconds)
var
  Counter: integer;
begin
  // Downloader thread
  DownloadManager.PeacefullyCloseThread;

  // Other threads
  Counter := 0;
  while SimultanousTaskThreads > 1 {This thread is added in the counter} do
    begin
      Sleep(100);
      Inc(Counter);

      // Timeout
      if Counter = WAIT_FOR_THREAD * 10 then
        Exit;
    end;

end;

{ TDownloaderClass }

procedure TDownloaderClass.TrackDownloadWB(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  TotalWork := AWorkCountMax;
  LastPercent := 0;
end;

procedure TDownloaderClass.TrackDownloadW(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
var
  APerc: integer;
begin
  APerc := round(AWorkCount / TotalWork * 1000);

  if APerc <> LastPercent then
    begin
      SetDataWorkStatus(ItemStatus + Format(', Downloading track %.2f%%', [LastPercent/10]));
      Main.SetSpecialStat(ItemStatusNice + Format(' - %.2f%%', [LastPercent/10]));
      TThread.Synchronize(TThread.CurrentThread, Main.UpdateSpecialStat);

      LastPercent := APerc;
    end;
end;

procedure TDownloaderClass.InitDirStructure;
var
  I: TDataSource;
  Dir: string;
begin
  for I in [TDataSource.Albums, TDataSource.Tracks, TDataSource.Artists, TDataSource.Playlists] do
    begin
      Dir := GetDir(I);

      if not directoryexists(Dir) then
        MkDir(Dir);
    end;
end;

class function TDownloaderClass.GetDir(DataKind: TDataSource): string;
begin
  Result := DownloadsFolder;

  case DataKind of
    TDataSource.Tracks: Result := Result + 'tracks';
    TDataSource.Albums: Result := Result + 'albums';
    TDataSource.Artists: Result := Result + 'artists';
    TDataSource.Playlists: Result := Result + 'playlists';
  end;

  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TDownloaderClass.ValidateFiles(DownKind: TDataSource);
var
  ExpectedFileCount: integer;

  I, J: integer;

  ActualFileCount: integer;
  TheEnd: boolean;
  CurrentValid, LastValid: boolean;

  Directory: string;
  Files: TStringList;
  DeleteFiles: TStringList;

  Current: string;
  Last: string;

procedure GetFilesList;
var
  I: integer;
begin
  Files := FindAllFiles(Directory, '', false);

  for I := 0 to Files.Count-1 do
    Files[I] := ExtractFileName(Files[I]);

  Files.Sorted:=true; // required for searching files
end;

function FindDownloadID(ID: string): boolean;
var
  I: integer;
begin
  case DownKind of
    TDataSource.Tracks: Result := DownloadManager.MasterList.Find(ID, I);
    TDataSource.Albums: Result := DownloadManager.Albums.Find(ID, I);
    TDataSource.Artists: Result := DownloadManager.Artists.Find(ID, I);
    TDataSource.Playlists: Result := DownloadManager.Playlists.Find(ID, I);
    else Result := false;
  end;
end;
begin
  Directory := GetDir(DownKind);

  SetWorkStatus('Validating download files');

  // Prep
  if DownKind = TDataSource.Tracks then
    ExpectedFileCount := 3
  else
    ExpectedFileCount := 2;

  // Remove files without a pair
  GetFilesList;

  DeleteFiles := TStringList.Create;
  try
    Last := '';
    I := 0;
    ActualFileCount := 0;
    if Files.Count > 0 then
      repeat
        // Status
        SetDataWorkStatus(Format('Checking download file pair %D/%D', [I+1, Files.Count]));
        if Terminated then
          Exit;

        TheEnd := I = Files.Count-1;
        Current := ChangeFileExt(Files[I], '');

        if Last = Current then
          Inc(ActualFileCount);

        if (Last <> Current) or TheEnd then
          begin
            // Delete items
            if (ActualFileCount <> 0) and (ActualFileCount <> ExpectedFileCount) then
              for J := I downto I-(ActualFileCount-1) do
                DeleteFile( Directory + Files[J]);

            Last := Current;
            ActualFileCount := 1;
          end;

        Inc(I);
      until TheEnd;
  finally
    DeleteFiles.Free;
  end;

  // Remove files not in INDEX32
  LastValid := false;
  Last := '';
  GetFilesList;

  for I := 0 to Files.Count-1 do
    begin
      // Status
      SetDataWorkStatus(Format('Validating download file index %D/%D', [I+1, Files.Count]));
      if Terminated then
        Exit;

      Current := ChangeFileExt(Files[I], '');

      // Valid
      if Last = Current then
        CurrentValid := LastValid
      else
        try
          if Current = '255182618' then
            Current := Current;

          CurrentValid := FindDownloadID(Current);
        except
          CurrentValid := false;
        end;

      // Delete
      if not CurrentValid then
        DeleteFile( Directory + Files[I] );

      // Last
      Last := Current;
      LastValid := CurrentValid;
    end;
end;

function TDownloaderClass.BuildDownloadList(DownKind: TDataSource): TStringList;
var
  DownloadDir, FilePath: string;
  I: integer;
  List: TStringList;
begin
  Result := TStringList.Create;

  // Dir
  DownloadDir := GetDir(DownKind);

  // List
  case DownKind of
    TDataSource.Tracks: List := DownloadManager.MasterList;
    TDataSource.Albums: List := DownloadManager.Albums;
    TDataSource.Artists: List := DownloadManager.Artists;
    TDataSource.Playlists: List := DownloadManager.Playlists;

    else Exit;
  end;

  // Scan
  for I := 0 to List.Count-1 do
    begin
      { .INI is the master file extension, all downloads kind have one! }
      FilePath := Format('%S%S.ini', [DownloadDir, List[I]]);

      if not fileexists(FilePath) then
        Result.Add( List[I] );
    end;
end;

procedure TDownloaderClass.DownloadType(DownKind: TDataSource);
var
  NewItems: TStringList;
  Directory, BaseFile, ExportFile: string;
  Info: TIniFile;
  Index: integer;
  I, TotalCount: integer;

  // Networking
  FileStream: TFileStream;
  HTTP: TIdHTTP;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;

procedure WaitForDownloadFinalise(var Status: TWorkItems);
var
  Count: integer;
begin
  // Wait 10 seconds for download to finalise
  Count := 0;
  while TWorkItem.DownloadingImage in Status do
    begin
      // Sleep
      Inc(Count);
      Sleep(100);

      // Timeout
      if Count = 10 * 10 then
        Break;
    end;
end;
function TracksToStr(Tracks: TStringArray): string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to High(Tracks) do
    Result := Result + Tracks[I] + ',';

  Result := Copy(Result, 1, Length(Result)-1);
end;

begin
  // Prepare
  NewItems := BuildDownloadList(DownKind);
  Directory:=GetDir(DownKind);

  // Write
  TotalCount := NewItems.Count;
  for I := 0 to TotalCount-1 do begin
    // Status
    SetDataWorkStatus(Format('Downloading record %D/%D', [I+1, TotalCount]));
    Main.SetSpecialStat(Format('Downloading %D/%D', [I+1, TotalCount]));
    TThread.Synchronize(TThread.CurrentThread, Main.UpdateSpecialStat);

    if Terminated then
      Exit;

    // Data
    BaseFile := Directory + NewItems[I];
    Info := TIniFile.Create(BaseFile + '.ini');
    Index := GetData(NewItems[I], DownKind);

    if Index = -1 then
      Exit;

    // Info
    with Info do
      try
        case DownKind of
          // Tracks
          TDataSource.Tracks: with Tracks[Index] do begin
            WriteString(SECT_MAIN, 'Name', Title);
            WriteString(SECT_MAIN, 'Artwork', ArtworkID);

            WriteString(SECT_META, 'Album', AlbumID);
            WriteString(SECT_META, 'Artist', ArtistID);

            WriteInteger(SECT_META, 'Year', Year);
            WriteString(SECT_META, 'Genre', Genre);
            WriteInteger(SECT_META, 'Length', LengthSeconds);
            WriteInteger(SECT_META, 'Size', FileSize);
            WriteInteger(SECT_META, 'Rating', Rating);
            WriteInteger(SECT_META, 'Plays', Plays);
            WriteString(SECT_META, 'Audio Type', AudioType);

            // Image
            WaitForDownloadFinalise( Status );

            ExportFile := BaseFile + '.jpg';
            try
              GetArtwork.SaveToFile(ExportFile);
            except
              DefaultPicture.SaveToFile(ExportFile);
            end;
          end;

          TDataSource.Albums: with Albums[Index] do begin
            WriteString(SECT_MAIN, 'Name', AlbumName);
            //WriteString(SECT_MAIN, 'Artwork', ArtworkID);
            WriteString(SECT_MAIN, 'Tracks', TracksToStr(TracksID));

            WriteString(SECT_META, 'Artist', ArtistID);

            WriteInteger(SECT_META, 'Year', Year);
            WriteInteger(SECT_META, 'Rating', Rating);

            // Image
            WaitForDownloadFinalise( Status );

            ExportFile := BaseFile + '.jpg';
            try
              GetArtwork.SaveToFile(ExportFile);
            except
              DefaultPicture.SaveToFile(ExportFile);
            end;
          end;

          TDataSource.Artists: with Artists[Index] do begin
            WriteString(SECT_MAIN, 'Name', ArtistName);
            WriteString(SECT_MAIN, 'Artwork', ArtworkID);
            WriteString(SECT_MAIN, 'Tracks', TracksToStr(TracksID));

            WriteInteger(SECT_META, 'Rating', Rating);

            // Image
            WaitForDownloadFinalise( Status );

            ExportFile := BaseFile + '.jpg';
            try
              GetArtwork.SaveToFile(ExportFile);
            except
              DefaultPicture.SaveToFile(ExportFile);
            end;
          end;

          TDataSource.Playlists: with Playlists[Index] do begin
            WriteString(SECT_MAIN, 'Name', Name);
            WriteString(SECT_MAIN, 'Artwork', ArtworkID);
            WriteString(SECT_MAIN, 'Tracks', TracksToStr(TracksID));

            WriteString(SECT_META, 'Description', Description);

            // Image
            WaitForDownloadFinalise( Status );

            ExportFile := BaseFile + '.jpg';
            try
              GetArtwork.SaveToFile(ExportFile);
            except
              DefaultPicture.SaveToFile(ExportFile);
            end;
          end;
        end;

      finally
        Free;
      end;

    // Data
    case DownKind of
      // Download audio
      TDataSource.Tracks: begin
        ItemStatus := DATA_WORK_STATUS;
        ItemStatusNice := SPECIAL_STATUS;
        ExportFile := BaseFile + '.mp3';
        CurrentTrack := Tracks[Index].ID;

        HTTP := TIdHTTP.Create;
        FileStream := TFileStream.Create(ExportFile, fmCreate);
        SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(HTTP);
        try
        try
          // Set SSL/TLS options
          SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
          HTTP.IOHandler := SSLIOHandler;

          // Notify events
          HTTP.OnWorkBegin := TrackDownloadWB;
          HTTP.OnWork := TrackDownloadW;

          // Request
          HTTP.Get(Tracks[Index].GetPlaybackURL, FileStream);
        finally
          HTTP.Free;
          FileStream.Free;
        end;

        except
          // Fail
          if fileexists(ExportFile) then
            deletefile(ExportFile);
        end;

        CurrentTrack := '';
      end;
    end;
  end;
end;

procedure TDownloaderClass.DoDownload;
begin
  // Download base
  DownloadType( TDataSource.Albums );
  DownloadType( TDataSource.Artists );
  DownloadType( TDataSource.Playlists );

  // Download audio + data
  DownloadType( TDataSource.Tracks );
end;

procedure TDownloaderClass.DoUpdateFiles;
begin
  ValidateFiles( TDataSource.Albums );
  ValidateFiles( TDataSource.Artists );
  ValidateFiles( TDataSource.Playlists );

  ValidateFiles( TDataSource.Tracks );
end;

constructor TDownloaderClass.Create;
begin
  inherited Create;
  FreeOnTerminate:=false;
end;

procedure TDownloaderClass.DoWork;
begin
  // Dirs
  InitDirStructure;

  // Delete and validate
  DoUpdateFiles;

  // Download
  DoDownload;
end;

procedure TDownloaderClass.DoFinish;
begin
  // Status
  Main.ResetSpecialStat;
end;

{ TDialogCreatePlaylist }

procedure TDialogCreatePlaylist.DoPrepare;
begin
  TaskExec.Title.Caption := Format('Creating playlist "%S"...', [Name]);
end;

procedure TDialogCreatePlaylist.DoWork;
begin
  Succeeded := CreateNewPlayList(Name, Description, MakePublic, Tracks);
end;

{ TDialoggedDownloadSongThread }

procedure TDialoggedDownloadSongThread.DoWorkItem;
var
  AProgress: integer;
begin
  AProgress := Progress;
  if LastProgress = AProgress then
    Exit;

  // Set
  TaskExec.Progress.Position:=AProgress;
  Application.ProcessMessages;

  // Last
  LastProgress := AProgress;
end;

procedure TDialoggedDownloadSongThread.DoFinish;
begin
  TaskExec.ModalResult:=mrOk;
  TaskExec.CloseQuery;

  Application.ProcessMessages;
  Main.SetFocus;
end;

constructor TDialoggedDownloadSongThread.Create;
begin
  inherited Create;

  LastProgress := -1;

  if TaskExec = nil then
    TaskExec := TTaskExec.Create(Application);

  // Defaults
  TaskExec.Title.Caption:='Downloading song...';
  TaskExec.Progress.Position:=0;
  TaskExec.Progress.Style:=pbstNormal;
end;

{ TDownloadSongThread }

procedure TDownloadSongThread.InternalWB(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  WorkCount := 0;
  TotalWorkCount := AWorkCountMax;

  SyncWork;
end;

procedure TDownloadSongThread.InternalW(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  WorkCount := AWorkCount;

  SyncWork
end;

procedure TDownloadSongThread.SyncWork;
begin
  Synchronize(DoWorkItem);
end;

function TDownloadSongThread.Progress: integer;
begin
  Result := round(WorkCount / TotalWorkCount * 100);
end;

procedure TDownloadSongThread.DoWorkItem;
begin
  // nothing
end;

procedure TDownloadSongThread.DoPrepare;
begin

end;

procedure TDownloadSongThread.DoWork;
var
  FileStream: TFileStream;
  HTTP: TIdHTTP;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;

  URL: string;
begin
  // Files
  URL := Tracks[DownloadIndex].GetPlaybackURL;

  // Create
  HTTP := TIdHTTP.Create;
  FileStream := TFileStream.Create(FilePath, fmCreate);
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(HTTP);
  try
    // Set SSL/TLS options
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    HTTP.IOHandler := SSLIOHandler;

    // Notify events
    HTTP.OnWorkBegin := InternalWB;
    HTTP.OnWork := InternalW;

    // Request
    HTTP.Get(URL, FileStream);
  finally
    HTTP.Free;
    FileStream.Free;
  end;
end;

procedure TDownloadSongThread.DoFinish;
begin

end;

{ TDownloadManager }

procedure TDownloadManager.PeacefullyCloseThread;
begin
  with DownloadManager do
  if (Downloader <> nil) then
    begin
      Downloader.Suspended := false;
      case Downloader.Status of
        // Stop thread
        TThreadStatus.Running, TThreadStatus.Queued: begin
          Downloader.Terminate;
          Downloader.WaitFor;
        end;
      end;

      FreeAndNil( Downloader );
    end;
end;

function TDownloadManager.IsLocal(Track: string; out FilePath: string
  ): boolean;
var
  Index, I: integer;
begin
  Index := GetTrack(Track);
  Result := (Index <> -1) and (MasterList.Find(Track, I))
    and ((Downloader = nil) or (Track <> Downloader.CurrentTrack));

  if Result then
    FilePath := Format('%S%S/%S.mp3', [DownloadsFolder, 'tracks', Track]);
end;

procedure TDownloadManager.AddItem(ID: string; Source: TDataSource);
var
  Index: integer;
  Items: TStringArray;
begin
  // Suspend
  PeacefullyCloseThread;

  // Track
  if Source = TDataSource.Tracks then
    begin
      if GetTrack(ID) = -1 then
        Exit;

      Tracks.Add(ID);
      MasterList.Add(ID);

      Exit;
    end;

  // Other
  Index := GetData(ID, Source);
  Items := [];
  if Index = -1 then
    Exit;

  case Source of
    TDataSource.Albums: begin
      Albums.Add(ID);
      Items := BroadcastAPI.Albums[Index].TracksID;
    end;

    TDataSource.Artists: begin
      Artists.Add(ID);
      Items := BroadcastAPI.Artists[Index].TracksID;
    end;

    TDataSource.Playlists: begin
      Playlists.Add(ID);
      Items := BroadcastAPI.Playlists[Index].TracksID;
    end;
  end;

  AddToMaster(Items);
end;

procedure TDownloadManager.RemoveItem(ID: string; Source: TDataSource);
var
  I, Index: integer;
  Items: TStringArray;
begin
  // Suspend
  PeacefullyCloseThread;

  // Track
  if Source = TDataSource.Tracks then
    begin
      if GetTrack(ID) = -1 then
        Exit;

      Tracks.SafeDelete(Tracks.IndexOf(ID));
      MasterList.SafeDelete(MasterList.IndexOf(ID));
    end;

  // Other
  Index := GetData(ID, Source);
  if Index = -1 then
    Exit;

  case Source of
    TDataSource.Albums: begin
      Albums.SafeDelete(Albums.IndexOf(ID));
      Items := BroadcastAPI.Albums[Index].TracksID;
    end;

    TDataSource.Artists: begin
      Artists.SafeDelete(Artists.IndexOf(ID));
      Items := BroadcastAPI.Artists[Index].TracksID;
    end;

    TDataSource.Playlists: begin
      Playlists.SafeDelete(Playlists.IndexOf(ID));
      Items := BroadcastAPI.Playlists[Index].TracksID;
    end;
  end;

  for I := 0 to High(Items) do
    MasterList.SafeDelete(MasterList.IndexOf(Items[I]));
end;

function TDownloadManager.GetConfigFile: TIniFile;
begin
  Result := TIniFile.Create(AppData + 'downloads.ini');
end;

procedure TDownloadManager.LoadConfig;
begin
  with GetConfigFile do
    try
      Tracks.LoadFromString( ReadString(CAT_DOWNLOAD, 'Tracks', ''), SEPAR);
      Albums.LoadFromString( ReadString(CAT_DOWNLOAD, 'Albums', ''), SEPAR);
      Artists.LoadFromString( ReadString(CAT_DOWNLOAD, 'Artists', ''), SEPAR);
      Playlists.LoadFromString( ReadString(CAT_DOWNLOAD, 'Playlists', ''), SEPAR);
    finally
      Free;
    end;

  // Build list
  BuildMasterList;
end;

procedure TDownloadManager.SaveConfig;
begin
  with GetConfigFile do
    try
      WriteString(CAT_DOWNLOAD, 'Tracks', Tracks.ToString(SEPAR));
      WriteString(CAT_DOWNLOAD, 'Albums', Albums.ToString(SEPAR));
      WriteString(CAT_DOWNLOAD, 'Artists', Artists.ToString(SEPAR));
      WriteString(CAT_DOWNLOAD, 'Playlists', Playlists.ToString(SEPAR));
    finally
      Free;
    end;
end;

procedure TDownloadManager.ValidateDownloads;
var
  I: integer;
begin
  // No offline mode!!
  if IsOffline then
    Exit;

  // Remove invalid
  for I := Tracks.Count-1 downto 0 do
    if GetTrack(Tracks[I]) = -1 then
      Tracks.Delete(I);

  for I := Albums.Count-1 downto 0 do
    if GetAlbum(Albums[I]) = -1 then
      Albums.Delete(I);

  for I := Artists.Count-1 downto 0 do
    if GetArtist(Artists[I]) = -1 then
      Artists.Delete(I);

  for I := Playlists.Count-1 downto 0 do
    if GetPlaylist(Playlists[I]) = -1 then
      Playlists.Delete(I);

  // Rebuild list
  BuildMasterList;
end;

procedure TDownloadManager.BuildMasterList;
var
  I, Index: integer;
begin
  MasterList.Clear;

  // Scan all
  for I := 0 to Tracks.Count-1 do
    MasterList.Add( Tracks[I] );

  for I := 0 to Albums.Count-1 do
    begin
      Index := GetAlbum(Albums[I]);
      if Index = -1 then
        Continue;

      AddToMaster( BroadcastAPI.Albums[Index].TracksID );
    end;

  for I := 0 to Artists.Count-1 do
    begin
      Index := GetArtist(Artists[I]);
      if Index = -1 then
        Continue;

      AddToMaster( BroadcastAPI.Artists[Index].TracksID );
    end;

  for I := 0 to Playlists.Count-1 do
    begin
      Index := GetPlaylist(Playlists[I]);
      if Index = -1 then
        Continue;

      AddToMaster( BroadcastAPI.Playlists[Index].TracksID );
    end;
end;

procedure TDownloadManager.AddToMaster(Items: TStringArray);
var
  I, P: integer;
begin
  for I := 0 to High(Items) do
    if not MasterList.Find(Items[I], P) then
      MasterList.Add(Items[I]);
end;

procedure TDownloadManager.CheckDownloads;
begin
  if IsOffline then
    Exit;

  // Wait
  if (Downloader <> nil) then
    begin
      Downloader.Suspended := false;
      case Downloader.Status of
        // Stop thread
        TThreadStatus.Running, TThreadStatus.Queued: begin
          Downloader.Terminate;
          Downloader.WaitFor;
        end;
      end;

      FreeAndNil( Downloader );
    end;

  // Start
  Downloader := TDownloaderClass.Create;
  Downloader.Start;
end;

destructor TDownloadManager.Destroy;
begin
  inherited Destroy;

  // Free
  MasterList.Free;

  Tracks.Free;
  Albums.Free;
  Artists.Free;
  Playlists.Free;
end;

constructor TDownloadManager.Create;
begin
  // Create
  MasterList := TStringList.Create;
  Tracks := TStringList.Create;
  Albums := TStringList.Create;
  Artists := TStringList.Create;
  Playlists := TStringList.Create;

  // Enable binary search
  MasterList.Sorted:=true;
  Tracks.Sorted:=true;
  Albums.Sorted:=true;
  Artists.Sorted:=true;
  Playlists.Sorted:=true;
end;

{ TDialogUpdateRating }

procedure TDialogUpdateRating.DoPrepare;
begin
  TaskExec.Title.Caption:='Updating rating...';
end;

procedure TDialogUpdateRating.DoWork;
var
  Index: integer;
begin
  case ItemSource of
    TDataSource.Tracks: begin
      // API
      Succeeded := UpdateTrackRating(ItemID, NewRating, false);

      // Also update locally (decreases reload requests by 1, better performance)
      Index := GetTrack(ItemID);
      if (Index <> -1) and Succeeded then
        Tracks[Index].Rating := NewRating;

      // Playlist
      TrackRatingToLikedPlaylist(ItemID);

      LoadLibraryAdvanced([TLoad.Track, TLoad.PlayList]);
    end;

    TDataSource.Albums: Succeeded := BroadcastAPI.UpdateAlbumRating(ItemID, NewRating, true);

    else { nothing };
  end;
end;

{ TDialogPlaylistUpdate }

procedure TDialogPlaylistUpdate.DoPrepare;
begin
  TaskExec.Title.Caption:='Updating playlist...';
end;

procedure TDialogPlaylistUpdate.DoWork;
var
  Index: integer;
  N, D: string;
begin
  try
    Index := GetPlaylist(ItemID);
    if Index = -1 then
      Exit;

    // Info
    if ChangeName or ChangeDescription then
      begin
        if ChangeName then
          N := NewName
        else
          N := Playlists[Index].Name;

        if ChangeDescription then
          D := NewDescription
        else
          D := Playlists[Index].Description;

        // Name, Description
        Succeeded := UpdatePlayList(ItemID, N, D, false);
      end;

    // Tracks
    if ChangeTracks then
      Succeeded := ChangePlayList(ItemID, NewTracks)
    else
    if AppendTracks then
      Succeeded := AppentToPlaylist(ItemID, NewTracks)
    else
    if PreAppendTracks then
      Succeeded := PreappendToPlaylist(ItemID, NewTracks)
    else
    if DeleteTracks then
    Succeeded := DeleteFromPlaylist(ItemID, NewTracks);

    // Reload
    if Succeeded then
      LoadLibraryAdvanced([TLoad.PlayList]);
  except
    Succeeded := false;
  end;
end;

{ TInfoLoadArtworkThread }

procedure TInfoLoadArtworkThread.DoFinish;
begin
  if ItemInfo <> nil then
    ItemInfo.Item_Image.Picture.Graphic := ArtPointer;
end;

{ TPlayerLoadArtworkThread }

procedure TPlayerLoadArtworkThread.DoFinish;
begin
  Main.Music_Artwork.Picture.Graphic := ArtPointer;

  // Also update popup
  Main.UpdatePopupPlayMusic;
end;

{ TPushHistoryThread }

procedure TPushHistoryThread.DoWork;
begin
  PushHistory( Items );
end;

{ TDialogRestoreItem }

procedure TDialogRestoreItem.DoPrepare;
begin
  TaskExec.Title.Caption:='Restoring from trash bin...';
end;

procedure TDialogRestoreItem.DoWork;
begin
  // Restore
  try
    case Source of
      TDataSource.Tracks: Succeeded := RestoreTrack(ItemID);
      TDataSource.Albums: Succeeded := RestoreAlbum(ItemID);
      TDataSource.Artists: Succeeded := RestoreArtist(ItemID);
      TDataSource.Playlists: { nothing };
    end;
  except
    Succeeded := false;
  end;
end;

{ TDialogTrashItem }

procedure TDialogTrashItem.DoPrepare;
begin
  TaskExec.Title.Caption:='Sending item to trash bin...';
end;

procedure TDialogTrashItem.DoWork;
begin
  // Delete
  try
    case Source of
      TDataSource.Tracks: Succeeded := DeleteTrack(ItemID);
      TDataSource.Albums: Succeeded := DeleteAlbum(ItemID);
      TDataSource.Artists: Succeeded := DeleteArtist(ItemID);
      TDataSource.Playlists: Succeeded := DeletePlayList(ItemID);
      TDataSource.Genres: Succeeded := DeleteGenre(ItemID);
    end;
  except
    Succeeded := false;
  end;
end;

{ TViewLoadArtworkThread }

procedure TViewLoadArtworkThread.DoFinish;
begin
  Main.Sidebar_Picture.Picture.Graphic := ArtPointer;
end;

{ TDialogTashEmptyTrash }

procedure TDialogTashEmptyTrash.DoPrepare;
begin
  TaskExec.Title.Caption:='Emptying trash...';
end;

procedure TDialogTashEmptyTrash.DoWork;
begin
  try
    Succeeded := CompleteEmptyTrash;
  except
    Succeeded := false;
  end;
end;

{ TDialogedTaskThread }

procedure TDialogedTaskThread.DoFinish;
begin
  if Succeeded then
    TaskExec.ModalResult:=mrOk
  else
    TaskExec.ModalResult:=mrClose;
  TaskExec.CloseQuery;

  Application.ProcessMessages;
  Main.SetFocus;
end;

constructor TDialogedTaskThread.Create;
begin
  inherited Create;
  if TaskExec = nil then
    TaskExec := TTaskExec.Create(Application);

  // Defaults
  TaskExec.Title.Caption:='Doing Something';
  TaskExec.Progress.Style:=pbstMarquee;
end;

{ TFunctionsContainer }

class procedure TFunctionsContainer.SyncStatus;
begin
  if (LoadLib <> nil) and LoadLib.Visible then
      LoadLib.Status_Text.Caption := WORK_STATUS;
end;

class procedure TFunctionsContainer.SyncStatusData;
begin
  if (LoadLib <> nil) and LoadLib.Visible then
      LoadLib.Status_Text.Caption := DATA_WORK_STATUS;
end;

{ TLoginThread }

procedure TLoginThread.DoWork;
begin
  if LOGIN_TOKEN <> '' then
    LoginUser;

  // Status
  if IsAuthenthicated then
    LoadStatus;
end;

procedure TLoginThread.DoFinish;
begin
  if IsAuthenthicated then
    begin
      TLoadLibraryThread.Create;

      // Wait for library
      // LoadLib.ModalResult:=mrOk;
    end
  else
    begin
      LoadLib.ModalResult:=mrClose;
    end;
end;

{ TLoadLibraryThread }

procedure TLoadLibraryThread.DoWork;
begin
  LoadLibrary;
end;

procedure TLoadLibraryThread.DoFinish;
begin
  LoadLib.ModalResult:=mrOk;
  LoadLib.CloseQuery;
end;

{ TInstantTaskThread }

constructor TInstantTaskThread.Create;
begin
  inherited Create;

  // Start
  Start;
end;

{ TTaskThread }

procedure TTaskThread.IncCounter;
begin
  Inc(SimultanousTaskThreads);
end;

procedure TTaskThread.DecCounter;
begin
  Dec(SimultanousTaskThreads);
end;

procedure TTaskThread.Execute;
procedure TerminateExec;
begin
  Status := TThreadStatus.Terminated;
  Synchronize(DoTerminated);
  Synchronize(DecCounter);
end;

begin
  Status := TThreadStatus.Queued;
  // Wait for turn
  while SimultanousTaskThreads >= MAX_THREAD_COUNT do
    Sleep(100);

  // Counter
  Status := TThreadStatus.Running;
  Synchronize(IncCounter);

  // Terminated
  if Terminated then
    begin
      TerminateExec;
      Exit;
    end;

  // Prepare
  Synchronize(DoPrepare);

  // Terminated
  if Terminated then
    begin
      TerminateExec;
      Exit;
    end;

  // Work (threaded)
  try
    DoWork;
    Status := TThreadStatus.Finished;
  except
    // fail
    Status := TThreadStatus.Error;
  end;

  // Terminated
  if Terminated then
    begin
      TerminateExec;
      Exit;
    end;

  // Done
  Synchronize(DoFinish);

  // Counter
  Synchronize(DecCounter);
end;

procedure TTaskThread.DoTerminated;
begin
  // Nothing
end;

procedure TTaskThread.DoPrepare;
begin
  // Nothing
end;

procedure TTaskThread.DoWork;
begin       
  // Nothing
end;

procedure TTaskThread.DoFinish;
begin       
  // Nothing
end;

constructor TTaskThread.Create;
begin
  inherited Create(true);
  FreeOnTerminate:=true;
end;

{ TDownloadArtworkThread }

procedure TDownloadArtworkThread.DoWork;
var
  Cover: TJPEGImage;
begin
  if IsOffline then
    try
      if ExistsInStore(GetItemID(DownloadIndex, DownloadSource), DownloadSource) then
        begin
          Cover := GetArtStoreCache(GetItemID(DownloadIndex, DownloadSource), DownloadSource);
          ArtPointer := Cover;

          // Set
          case DownloadSource of
            TDataSource.Tracks: Tracks[DownloadIndex].CachedImage := Cover;
            TDataSource.Albums: Albums[DownloadIndex].CachedImage := Cover;
            TDataSource.Artists: Artists[DownloadIndex].CachedImage := Cover;
            TDataSource.Playlists: Playlists[DownloadIndex].CachedImage := Cover;
            TDataSource.Genres: Genres[DownloadIndex].CachedImage := Cover;
          end;
        end
      else
        begin
          // Set
          case DownloadSource of
            TDataSource.Tracks: Tracks[DownloadIndex].CachedImage := DefaultPicture;
            TDataSource.Albums: Albums[DownloadIndex].CachedImage := DefaultPicture;
            TDataSource.Artists: Artists[DownloadIndex].CachedImage := DefaultPicture;
            TDataSource.Playlists: Playlists[DownloadIndex].CachedImage := DefaultPicture;
            TDataSource.Genres: Genres[DownloadIndex].CachedImage := DefaultPicture;
          end;
        end;
    except
      // Fail
      Exit;
    end
  else
    case DownloadSource of
      TDataSource.Tracks: ArtPointer := Tracks[DownloadIndex].GetArtwork;
      TDataSource.Albums: ArtPointer := Albums[DownloadIndex].GetArtwork;
      TDataSource.Artists: ArtPointer := Artists[DownloadIndex].GetArtwork;
      TDataSource.Playlists: ArtPointer := Playlists[DownloadIndex].GetArtwork;
      TDataSource.Genres: ArtPointer := Genres[DownloadIndex].GetArtwork;
    end;
end;

procedure TDownloadArtworkThread.DoFinish;
begin
  Main.DrawingBox.Invalidate;
end;

{ TDrawItem }

function TDrawItem.Hidden: boolean;
begin
  Result := HideReasons <> [];
end;

function TDrawItem.Downloaded: boolean;
begin
  Result := IsDownloaded = TDownloadedKind.Direct;
end;

function TDrawItem.Active: boolean;
begin
  Result := (Source = TDataSource.Tracks) and (ItemID = PlayID);
end;

function TDrawItem.IsDownloaded: TDownloadedKind;
var
  I: integer;
begin
  Result := TDownloadedKind.None;
  case Source of
    TDataSource.Tracks: if DownloadManager.Tracks.Find(ItemID, I) then
      Result := TDownloadedKind.Direct
    else
      if DownloadManager.MasterList.Find(ItemID, I) then
        Result := TDownloadedKind.Indirect;

    TDataSource.Albums: if DownloadManager.Albums.Find(ItemID, I) then
      Result := TDownloadedKind.Direct;

    TDataSource.Artists: if DownloadManager.Artists.Find(ItemID, I) then
      Result := TDownloadedKind.Direct;

    TDataSource.Playlists: if DownloadManager.Playlists.Find(ItemID, I) then
      Result := TDownloadedKind.Direct;
  end;
end;

function TDrawItem.ToggleDownloaded: boolean;
begin
  Result := not Downloaded;
  if Result then
    DownloadManager.AddItem(PopupItem.ItemID, PopupItem.Source)
  else
    DownloadManager.RemoveItem(PopupItem.ItemID, PopupItem.Source);

  // Begin update
  DownloadManager.CheckDownloads;
end;

procedure TDrawItem.SetHidden;
begin
  HideReasons := HideReasons + [THiddenReason.Manual];
end;

function TDrawItem.GetPremadeInfoList: string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to High(Information) do
    begin
      Result := Result + Information[I] + #13;
    end;
end;

function TDrawItem.GetPicture: TJPEGImage;
begin
  // Get Type
  case Source of
    TDataSource.Tracks: with Tracks[Index] do
      if ArtworkLoaded then
        Result := CachedImage
          else
            begin
              Result := DefaultPicture;

              if not (TWorkItem.DownloadingImage in Status) then
                StartPictureLoad;
            end;

    TDataSource.Albums: with Albums[Index] do
      if ArtworkLoaded then
        Result := CachedImage
          else
            begin
              Result := DefaultPicture;

              if not (TWorkItem.DownloadingImage in Status) then
                StartPictureLoad;
            end;

    TDataSource.Artists: with Artists[Index] do
      if ArtworkLoaded then
        Result := CachedImage
          else
            begin
              Result := DefaultPicture;

              if not (TWorkItem.DownloadingImage in Status) then
                StartPictureLoad;
            end;

    TDataSource.Playlists: with Playlists[Index] do
      if ArtworkLoaded then
        Result := CachedImage
          else
            begin
              Result := DefaultPicture;

              if not (TWorkItem.DownloadingImage in Status) then
                StartPictureLoad;
            end;

    TDataSource.Genres: with Genres[Index] do
      if ArtworkLoaded then
        Result := CachedImage
          else
            begin
              Result := DefaultPicture;

              if not (TWorkItem.DownloadingImage in Status) then
                StartPictureLoad;
            end;

    else Result := DefaultPicture;
  end;
end;

function TDrawItem.GetLargePicture: TJPEGImage;
begin
  case Source of
    TDataSource.Tracks: Result := Tracks[Index].GetArtwork(true);
    TDataSource.Albums: Result := Albums[Index].GetArtwork;
    TDataSource.Artists: Result := Artists[Index].GetArtwork(true);
    TDataSource.Playlists: Result := Playlists[Index].GetArtwork(true);
    else Result := DefaultPicture;
  end;
end;

procedure TDrawItem.Execute;
begin
  case Source of
    TDataSource.Tracks: Main.StartDrawPlay(ItemID); {Main.PlaySong(ItemID);}
    TDataSource.Albums: Main.SelectPage(TPage.ViewAlbum, ItemID);
    TDataSource.Artists: Main.SelectPage(TPage.ViewArtist, ItemID);
    TDataSource.Playlists: Main.SelectPage(TPage.ViewPlaylist, ItemID);
    TDataSource.Genres: Main.SelectPage(TPage.ViewGenre, ItemID);
  end;
end;

procedure TDrawItem.OpenInformation;
begin
  ItemInfo := TItemInfo.Create(Main);
  with ItemInfo do
    try
      // P
      Item := @Self;

      Item_Image.Picture.Graphic := DefaultPicture;
      with TInfoLoadArtworkThread.Create do
        begin
          DownloadSource := Source;
          DownloadIndex := Index;

          Start;
        end;

      LoadInformation;
      Prepare;

      // Show
      ShowModal;
    finally
      Free;
    end;
end;

function TDrawItem.Invalid: boolean;
begin
  Result := (Index = -1) or (ItemID = '') or (Title = '');
end;

procedure TDrawItem.LoadSourceID(ID: string; From: TDataSource);
var
  Index: integer;
begin
  case From of
    TDataSource.Tracks: Index := GetTrack(ID);
    TDataSource.Albums: Index := GetAlbum(ID);
    TDataSource.Artists: Index := GetArtist(ID);
    TDataSource.Playlists: Index := GetPlaylist(ID);
    else Index := -1;
  end;

  LoadSource( Index, From );
end;

procedure TDrawItem.LoadSource(AIndex: integer; From: TDataSource);
var
  Temp, A, APos: integer;
  Data1: string;
begin
  if AIndex = -1 then
    Exit;

  Index := AIndex;

  HideReasons := [];
  Loaded := true;
  Trashed := false;

  Source := From;

  case From of
    TDataSource.Tracks: begin
      ItemID := Tracks[Index].ID;

      Title := Tracks[Index].Title;
      Rating := Tracks[Index].Rating;

      Trashed := Tracks[Index].IsInTrash;

      // Info
      SetLength(Information, 11);
      Information[0] := 'Track Number: ' + Tracks[Index].TrackNumber.ToString;
      Information[1] := 'Released in: ' + Yearify(Tracks[Index].Year);
      Information[2] := 'Genre: ' + Tracks[Index].Genre;
      Information[3] := 'Length: ' + CalculateLength( Tracks[Index].LengthSeconds );
      A := GetArtist(Tracks[Index].ArtistID);
      if A <> -1 then
        Data1 := Artists[A].ArtistName
      else
        Data1 := 'Unknown';

      Information[4] := 'Artist: ' + Data1;

      A := GetAlbum(Tracks[Index].AlbumID);
      if A <> -1 then
        Information[5] := 'Album: ' + Albums[A].AlbumName;
      Information[6] := 'Uploaded on: ' + DateToStr( Tracks[Index].DayUploaded );
      Information[7] := 'File Size: ' + SizeInString( Tracks[Index].FileSize );
      Information[8] := 'Rating: ' + Tracks[Index].Rating.ToString + '/10';
      Information[9] := 'Media Type: ' + Tracks[Index].AudioType;
      Information[10] := 'Trashed: ' + booleantostring(Trashed);

      // Default Info
      InfoShort := Data1 + ' • ' + Yearify(Tracks[Index].Year);
      InfoLong := Information[1] + ', ' + Information[4] + ', ' +
        Information[3] + ', ' + Information[5] + ', ' + Information[8];
    end;

    TDataSource.Albums: begin
      ItemID := Albums[Index].ID;

      Title := Albums[Index].AlbumName;
      Rating := Albums[Index].Rating;

      Trashed := Albums[Index].IsInTrash;

      // Info
      SetLength(Information, 7);
      Information[0] := 'Total Tracks: ' + Length(Albums[Index].TracksID).ToString;
      Information[1] := 'Released in: ' + Yearify(Albums[Index].Year);
      Temp := GetArtist(Albums[Index].ArtistID);
      if Temp <> -1 then
        Information[2] := 'Artist: ' + Artists[Temp].ArtistName
          else
            Information[2] := 'Artist: Unknown';
      Information[3] := 'Rating: ' + Albums[Index].Rating.ToString;
      Information[4] := 'Disk: ' + Albums[Index].Disk.ToString;
      Temp := 0;
      for A := 0 to High(Albums[Index].TracksID) do
        begin
          APos := GetTrack(Albums[Index].TracksID[A]);
          if APos <> -1 then
            Inc(Temp, Tracks[APos].LengthSeconds );
        end;
      Information[5] := 'Length: ' + CalculateLength(Temp);
      Information[6] := 'Trashed: ' + booleantostring(Trashed);

      // Default Info
      InfoShort := Length(Albums[Index].TracksID).ToString + ' Tracks • ' + Information[2];
      InfoLong := Information[0] + ', ' + Information[2] + ', ' +
        Information[1] + ', ' + Information[3] + ', ' + Information[4];
    end;

    TDataSource.Artists: begin
      ItemID := Artists[Index].ID;

      Title := Artists[Index].ArtistName;
      Rating := Artists[Index].Rating;

      Trashed := Artists[Index].IsInTrash;

      // Info
      SetLength(Information, 4);
      Information[0] := 'Total Tracks: ' + Length(Artists[Index].TracksID).ToString;
      Temp := 0;
      for A := 0 to High(Albums) do
        if Albums[A].ArtistID = ItemID then
          Inc(Temp);
      Information[1] := 'Album count: ' + Temp.ToString;
      Information[2] := 'Rating: ' + Artists[Index].Rating.ToString;
      Information[3] := 'Trashed: ' + booleantostring(Trashed);

      // Default Info
      InfoShort :=  Length(Artists[Index].TracksID).ToString + ' Tracks • ' + Information[1];
      InfoLong := Information[0] + ', ' + Information[1] + ', ' +
        Information[2];
    end;

    TDataSource.Playlists: begin
      ItemID := Playlists[Index].ID;

      Title := Playlists[Index].Name;
      Rating := 0; // Playlists do not have ratings

      Trashed := false; // Playlists cannot be in the trash

      // Info
      SetLength(Information, 3);
      Information[0] := 'Total Tracks: ' + Length(Playlists[Index].TracksID).ToString;
      Temp := 0;
      for A := 0 to High(Playlists[Index].TracksID) do
        begin
          APos := GetTrack(Playlists[Index].TracksID[A]);
          if APos <> -1 then
            Inc(Temp, Tracks[APos].LengthSeconds );
        end;

      Information[1] := 'Length: ' + CalculateLength(Temp);
      Information[2] := 'Description: "' + Playlists[Index].Description + '"';

      // Default Info
      InfoShort := Length(Playlists[Index].TracksID).ToString + ' Tracks • ' + Copy(Information[1], 9, 9);
      InfoLong := Information[0] + ', ' + Information[1] + ', ' + Information[2];
    end;

    TDataSource.Genres: begin
      ItemID := Genres[Index].ID;

      Title := Genres[Index].ID; // same same
      Rating := 0; // Genres do not have ratings

      Trashed := false; // Genres cannot be in the trash

      // Info
      SetLength(Information, 2);
      Information[0] := 'Total Tracks: ' + Length(Genres[Index].TracksID).ToString;
      Temp := 0;
      for A := 0 to High(Genres[Index].TracksID) do
        begin
          APos := GetTrack(Genres[Index].TracksID[A]);
          if APos <> -1 then
            Inc(Temp, Tracks[APos].LengthSeconds );
        end;

      Information[1] := 'Length: ' + CalculateLength(Temp);

      // Default Info
      InfoShort := Length(Genres[Index].TracksID).ToString + ' Tracks • ' + Copy(Information[1], 9, 9);
      InfoLong := Information[0] + ', ' + Information[1];
    end;
  end;

  // Trash Reason
  if Trashed and not TrashInheritance then
    HideReasons := HideReasons + [THiddenReason.Trashed];
end;

procedure TDrawItem.ReloadSource;
begin
  LoadSourceID(ItemID, Source);
end;

procedure TDrawItem.StartPictureLoad;
begin
  if Source in [TDataSource.None] then
    Exit;

  with TDownloadArtworkThread.Create do
    begin
      DownloadSource := Source;
      DownloadIndex := Index;

      Start;
    end;
end;

{ TMain }

procedure TMain.DrawingBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  I: Integer;
begin
  // Set coordinate
  DrawCursorPos := Point(X, Y);

  // Get Press position
  IndexHover := -1;
  for I := 0 to High(SortingList) do
    if DrawItems[SortingList[I]].Bounds.Contains( Point(X, Y) ) then
      begin
        IndexHover := SortingList[I];
        Break;
      end;

  // Dragging
  if DrawPressed and SupportsDragging then
    begin
      if (IndexDown <> IndexHover) and (IndexHover <> -1) and not IsDragging then
        begin
          DraggingIndex := IndexDown;
          IsDragging := true;
        end;

      // Redraw drag
      if IsDragging then
        DrawingBox.Invalidate;
    end;

  // Interact
  if CustomInteract then
    DoCustomInteractMove(X, Y);

  // Cursor
  if IsDragging then
    TPaintBox(Sender).Cursor := crMultiDrag
    else
      if (IndexHover <> -1) then
        TPaintBox(Sender).Cursor := crHandPoint
      else
        TPaintBox(Sender).Cursor := crDefault;
end;

procedure TMain.DrawingBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (Button in [mbLeft, mbRight]) then
    Exit;

  DrawPressed := false;
  PressNow.Enabled:=true;

  if IndexHover = -1 then
    Exit;

  // Draw
  DrawingBox.Invalidate;

  // Click
  if CustomInteract then
    DoCustomInteractUp(Button)
  else
    begin
      if IndexDown = IndexHover then
        case Button of
          TMouseButton.mbLeft: begin
            DrawItems[IndexHover].Execute;
          end;

          TMouseButton.mbRight: begin
            PopupIndex := IndexHover;
            PopupItem := DrawItems[PopupIndex];

            Generic_Menu.Popup;
          end;
        end;
    end;

  // Settings
  if IsDragging then
    begin
      IsDragging := false;
      DrawingBox.Invalidate;
    end;
end;

procedure TMain.MenuItem20Click(Sender: TObject);
begin
  PopupItem.OpenInformation;
end;

procedure TMain.Popup_Queue_BottomClick(Sender: TObject);
begin
  // Move
  QueueMove(PopupIndex, Queue.Count-1);
end;

procedure TMain.Popup_Queue_NowClick(Sender: TObject);
begin
  if PopupIndex < QueueIndex then
    QueueMove(PopupIndex, QueueIndex)
  else
    QueueMove(PopupIndex, QueueIndex+1);
end;

procedure TMain.Popup_Queue_RemoveClick(Sender: TObject);
begin
  // Current
  if (QueueIndex = PopupIndex) then
    begin
      SongFinished;
    end;

  //  Index
  QueueDelete(PopupIndex);
end;

procedure TMain.Queue_ShuffleClick(Sender: TObject);
begin
  ShuffleMode := not ShuffleMode;

  if ShuffleMode then
    begin
      SaveShuffleDefault;

      ShuffleQueue;
    end
  else
    begin
      LoadShuffleDefault;

      QueueFindPlaying;
    end;

  // Update
  QueueUpdated(false);
end;

procedure TMain.Search_BoxButtonClick(Sender: TObject);
begin
  // Beep
  if Search_Box.Text = '' then
    begin
      Beep;
      Exit;
    end;

  // Search (reload)
  CurrentViewReload;
end;

procedure TMain.Search_BoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13: TEditButton(Sender).OnButtonClick(TEditButton(Sender));
  end;
end;

procedure TMain.Filter_BoxChange(Sender: TObject);
begin
  FiltrateSearch(TEdit(Sender).Text, [{TSearchFlag.SearchInfo, better for performance}]);
  DrawingBox.Invalidate;
end;

procedure TMain.Filter_BoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: begin
      Control_ToggleSearch.OnClick(Control_ToggleSearch);
      Control_ToggleSearch.SetFocus;
    end;
  end;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Save Settings
  AppSettings(false);

  // Save downlaods
  DownloadManager.SaveConfig;

  // Save queue
  if Option_savequeue.Checked then
    SaveQueueToFile;

  // Form position
  FormPositionSettings(Main, AppData + 'form.ini', false);

  // Threads
  StopThreads;

  // Free
  Player.Free;
  DownloadManager.Free;
end;

procedure TMain.AudioUpdateTimer(Sender: TObject);
begin
  // Check NOT seeking (normal play)
  if Player.IsFileOpen and not SliderPressed then
    begin
      // Status
      Music_Position.Position:=round(Player.PositionSeconds*10);
      Music_Time.Caption := Format('%S/%S', [CalculateLength(trunc(Player.PositionSeconds)), PlayerTotalLength]);

      // Song done
      if (Player.PlayStatus = TPlayStatus.psStopped) and (LastPlayStatus = TPlayStatus.psPlaying) then
        SongFinished;
    end;

  // UI
  SetPlayIcon(Player.PlayStatus);

  // Status
  LastPlayStatus := Player.PlayStatus;

  // Popup
  UpdatePopupTime;
end;

procedure TMain.BitBtn4Click(Sender: TObject);
var
  A: TPage;
begin
  case TBitBtn(Sender).Tag of
    0: A := TPage.Home;
    1: A := TPage.Search;
    2: A := TPage.Albums;
    3: A := TPage.Songs;
    4: A := TPage.Artists;
    5: A := TPage.Playlists;
    6: A := TPage.Genres;
  end;

  SelectPage(A);
end;

procedure TMain.BitBtn7Click(Sender: TObject);
begin
  SelectPage(TPage.Queue);

  Queue_JumpClick( Queue_Jump );
end;

procedure TMain.Button1Click(Sender: TObject);
begin
  if MessageDlg('Are you sure?', 'Do you want to empty the trash?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      TDialogTashEmptyTrash.Create.Start;

      if TaskExec.ShowModal <> mrOk then
        ShowMessage('Could not empty trash');
    end;
end;

procedure TMain.Button4Click(Sender: TObject);
begin
  if directoryexists(MediaStoreLocation) then
    if MessageDlg('Are you sure?', 'Clear all artwork cache files?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        DeleteDirectory(MediaStoreLocation, true);
        DeleteDirectory(MediaStoreLocation, false);

        // Prep directories
        InitiateDirectories;

        // Storage
        Artwork_Storage.Caption:= GetFolderSizeInStr(MediaStoreLocation);
      end;
end;

procedure TMain.Button5Click(Sender: TObject);
begin
  CreatePlaylist := TCreatePlaylist.Create(Self);
   with CreatePlaylist do
    try
      if ShowModal = mrOk then
        with TDialogCreatePlaylist.Create do
        begin
          Name := Playlist_Name.Text;
          Description := Playlist_Description.Text;
          Tracks := [];

          Start;

          if TaskExec.ShowModal <> mrOk then
            ShowMessage('Could not create playlist');
        end;
    finally
      Free;
    end;
end;

procedure TMain.Button6Click(Sender: TObject);
begin
  OpenURL( 'https://www.ibroadcast.com/premium/' );
end;

procedure TMain.Button_CheckUpdateClick(Sender: TObject);
begin
  with TDialogCheckUpdatesThread.Create do
    begin
      Start;

      TaskExec.ShowModal;
    end;
end;

procedure TMain.Label21Click(Sender: TObject);
begin
  with TLabel(Sender) do
    Caption := Hint;
end;

procedure TMain.MenuItem12Click(Sender: TObject);
begin
  BroadcastAPI.LogOff;
  DeleteCredentials;
end;

procedure TMain.MenuItem13Click(Sender: TObject);
begin
  SelectPage( TPage.Account );
end;

procedure TMain.Music_SpeedKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = 'R') or (Key = 'r') then
    TTrackBar(Sender).Position:=100;
end;

procedure TMain.Music_SpeedMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    TTrackBar(Sender).Position:=100;
end;

procedure TMain.Popup_IDClick(Sender: TObject);
begin
  Clipboard.AsText := PopupItem.ItemID;
end;

procedure TMain.Popup_Playlist_RemoveClick(Sender: TObject);
begin
  // Create thread
  with TDialogPlaylistUpdate.Create do
    begin
      try
        ItemID := PageData;
       except
         Free;
         Exit;
       end;

       DeleteTracks := true;

       SetLength(NewTracks, 1);
       NewTracks[0] := PopupItem.ItemID;

       Start;

       // Exec
       if TaskExec.ShowModal <> mrOk then
        ShowMessage('Could not modify playlist.');
    end;
end;

procedure TMain.Music_Menu1Click(Sender: TObject);
begin
  OpenPopupPlayer;
end;

procedure TMain.Option_artworkClick(Sender: TObject);
begin
  ArtworkStore := TCheckBox(Sender).Checked;
end;

procedure TMain.Option_datasaverClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    DefaultArtSize := TArtSize.Small
  else
    DefaultArtSize := TArtSize.Medium;
end;

procedure TMain.Option_threadsChange(Sender: TObject);
begin
  MAX_THREAD_COUNT := TTrackBar(Sender).Position;
  TTrackBar(Sender).Hint := TTrackBar(Sender).Position.ToString;
end;

procedure TMain.Popup_DownloadClick(Sender: TObject);
begin
  PopupItem.ToggleDownloaded;
  DrawingBox.Invalidate;

  // Reload
  if CurrentPage = TPage.Downloads then
    CurrentViewReload;
end;

procedure TMain.MenuItem2Click(Sender: TObject);
begin
  Help := THelp.Create(Self);
  with Help do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMain.Music_ArtistClick(Sender: TObject);
var
  Index: integer;
  ID: string;
begin
  Index := GetTrack(PlayID);

  if Index = -1 then
    Exit;

  ID := Tracks[Index].ArtistID;

  // Open
  if GetArtist(ID) <> -1 then
    SelectPage(TPage.ViewArtist, ID);
end;

procedure TMain.Music_ArtworkMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (PlayID <> '') then
    begin
      // Close
      Generic_Menu.Close;

      // Data
      PopupItem.LoadSourceID(PlayID, TDataSource.Tracks);

      // Show
      if not PopupItem.Invalid then
        Generic_Menu.PopUp;
    end;
end;

procedure TMain.Music_VolumeChange(Sender: TObject);
begin
  if Player.IsFileOpen then
    begin
      PlayerVolume := TTrackBar(Sender).Position/100;
      Player.Volume := PlayerVolume;

      TTrackBar(Sender).Hint := Format('%D%%', [TTrackBar(Sender).Position]);
    end;
end;

procedure TMain.Music_SpeedChange(Sender: TObject);
begin
    if Player.IsFileOpen then
    begin
      PlayerSpeed := TTrackBar(Sender).Position/100;
      Player.Speed := PlayerSpeed;

      TTrackBar(Sender).Hint := Format('%D%%', [TTrackBar(Sender).Position]);
    end;
end;

procedure TMain.Music_MenuClick(Sender: TObject);
begin
  Panel12.Visible := not Panel12.Visible;

  Panel12.Top := Panel5.Top-1;
end;

procedure TMain.Popup_SaveClick(Sender: TObject);
begin
  SaveDialog1.FileName := PopupItem.Title;
  if SaveDialog1.Execute then
    begin
      with TDialoggedDownloadSongThread.Create do
        begin
          DownloadIndex := PopupItem.Index;
          FilePath := SaveDialog1.FileName;

          Start;

          if TaskExec.ShowModal <> mrOk then
            ShowMessage('Could not download track');
        end;
    end;
end;

procedure TMain.Queue_ClearClick(Sender: TObject);
begin
  QueueClear;
end;

procedure TMain.Queue_JumpClick(Sender: TObject);
begin
  ScrollPosition := (ListHeight + ListSpacing) * QueueIndex - DrawingBox.Height div 3;
end;

procedure TMain.Queue_RepeatClick(Sender: TObject);
begin
  if RepeatMode = High(TRepeat) then
    RepeatMode := TRepeat(0)
  else
    RepeatMode := TRepeat(integer(RepeatMode)+1);

  // Update
  QueueUpdateUI;
end;

procedure TMain.Generic_AddPlaylistClick(Sender: TObject);
var
  Index: integer;
begin
  Index := TMenuItem(Sender).Tag;

  // Create thread
  with TDialogPlaylistUpdate.Create do
    begin
        ItemID := Playlists[Index].ID;

        if TMenuItem(Sender).Checked then
          DeleteTracks := true
        else
          AppendTracks := true;

        SetLength(NewTracks, 1);
        NewTracks[0] := PopupItem.ItemID;

        Start;

        // Exec
        if TaskExec.ShowModal <> mrOk then
          ShowMessage('Could not modify playlist.');
      end;
end;

procedure TMain.Music_NextClick(Sender: TObject);
begin
  // Next
  QueueNext;
end;

procedure TMain.Music_PrevClick(Sender: TObject);
begin
  // Previous
  QueuePrevious;
end;

procedure TMain.Option_ratingChange(Sender: TObject);
begin
  ValueRatingMode := TCheckBox(Sender).Checked;
end;

procedure TMain.CheckBox7Change(Sender: TObject);
begin
  // Search
  CurrentViewReload;
end;

procedure TMain.ComboBox1Change(Sender: TObject);
begin
  case TComboBox(Sender).Text[1] of
    'D': SortType := TSortType.Default;
    'N': SortType := TSortType.Alphabetic;
    'Y': SortType := TSortType.Year;
    'R': SortType := TSortType.Rating;
  end;

  Sort;
  DrawingBox.Invalidate;
end;

procedure TMain.Control_ToggleSearchClick(Sender: TObject);
procedure SetW(Value: integer);
begin
  Filter_Box.Width := Value;
  Application.ProcessMessages;
  Sleep(1);
end;

var
  I: integer;
begin
  if TButton(Sender).Tag = 1 then
    Exit;
  TButton(Sender).Tag := 1;

  if Filter_Box.Visible then
    begin
      for I := Filter_Box.Width downto 0 do
        SetW(I);

      Filter_Box.Hide;

      // Reset
      FiltrateSearch('', []);
      DrawingBox.Invalidate;
    end
  else
    begin
      Filter_Box.Width := 0;
      Filter_Box.Show;

      // Focus
      Filter_Box.SetFocus;

      for I := Filter_Box.Width to 175 do
        SetW(I);
    end;

  TButton(Sender).Tag := 0;
end;

procedure TMain.Control_ViewStyleClick(Sender: TObject);
var
  Style: TViewStyle;
begin
  Style := TViewStyle(integer(ViewStyle) + 1);
  if integer(Style) > Integer(High(TViewStyle)) then
    Style := TViewStyle(0);

  SetViewStyle(Style);
end;

procedure TMain.DrawingBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (Button in [mbLeft, mbRight]) then
    Exit;

  DrawPressed := true;
  PressNow.Enabled:=true;

  IndexDown := IndexHover;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  // Player
  Player := TAudioPlayer.Create;

  // Folder
  InitiateDirectories;

  // Scale
  DoScaling;

  // Form position
  FormPositionSettings(Main, AppData + 'form.ini', true);

  // Create lists
  Queue := TStringList.Create;
  QueueDefault := TStringList.Create;

  // Defaults
  SetViewStyle(TViewStyle.Cover);
  ViewStyles[TPage.Home] := TViewStyle.Cover;
  ViewStyles[TPage.Queue] := TViewStyle.List;
  ViewStyles[TPage.Songs] := TViewStyle.Cover;
  ViewStyles[TPage.Albums] := TViewStyle.Cover;
  ViewStyles[TPage.Artists] := TViewStyle.Cover;
  ViewStyles[TPage.Genres] := TViewStyle.Cover;
  ViewStyles[TPage.Playlists] := TViewStyle.Cover;
  ViewStyles[TPage.Search] := TViewStyle.Cover;

  DrawPageFilters[TPage.Downloads] := TDataSource.None;
  DrawPageFilters[TPage.Trash] := TDataSource.Tracks;

  SearchFilters := [TDataSource.Tracks, TDataSource.Albums, TDataSource.Artists, TDataSource.Playlists];

  // Settings
  AppSettings(true);

  // Page
  SelectPage(TPage.Home);

  // Art
  DefaultPicture := TJPEGImage.Create;
  DefaultPicture.LoadFromFile(RuntimeFolder + 'art.jpeg');

  // Create Spectrum
  Spectrum_Player := TSpectrum.Create(round(Visualisation_Player.Width * ScaleFactor), round(Visualisation_Player.Height * ScaleFactor));
  Spectrum_Player.Height := Visualisation_Player.Height - 20;
  Spectrum_Player.Peak := clMenuText;
  Spectrum_Player.Pen:= RGBToColor(255, 105, 180);
  Spectrum_Player.BackColor := clWindow;

  // Notify
  OnWorkStatusChange := APIStatusChanged;
  OnDataWorkStatusChange := APIStatusDataChanged;
  OnUpdateType := BackendUpdate;

  OnReturnToLogin := DoReturnToLogin;

  // Prepare UI
  QueueUpdateUI;

  // Downlaoder
  DownloadManager := TDownloadManager.Create;

  // Login
  LoginDelay.Enabled:=true;

  // Debug
  DoDebug;
end;

procedure TMain.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  Key: char;
begin
  Key := Char(Msg.CharCode);

  // Control shortcut
  if ssCtrl in KeyDataToShiftState(Msg.KeyData) then
    begin
      Handled := True;
      case Key of
          'S': SelectPage(TPage.Search);
          'P': Music_Play.OnClick( Music_Play );
          'N': Music_Next.OnClick( Music_Next );
          'L': Music_Prev.OnClick( Music_Prev );
          'Q': SelectPage(TPage.Queue);
          'O': if (PopupPlay <> nil) and PopupPlay.Visible then
            PopupPlay.Close
          else
            OpenPopupPlayer;


          'F': if DrawPage and (Toolbar_ViewControls.Parent <> Page_ToolbarContainer) then
            Control_ToggleSearch.OnClick(Control_ToggleSearch);

        else Handled := False;
      end;
    end;
end;

procedure TMain.Generic_MenuPopup(Sender: TObject);
var
  I: integer;
  M: TMenuItem;
  IsQueuePage: boolean;
begin
  // Prepare
  IsQueuePage := CurrentPage = TPage.Queue;

  // Show
  Popup_AddQueue.Visible:=not IsQueuePage;
  Menu_Restore.Visible := PopupItem.Trashed;
  Menu_Permadelete.Visible := PopupItem.Source = TDataSource.Playlists;
  Menu_Trash.Visible := not Menu_Permadelete.Visible and not PopupItem.Trashed;
  MenuItem15.Visible := PopupItem.Source in [TDataSource.Tracks, TDataSource.Albums];
  MenuItem17.Visible := PopupItem.Source = TDataSource.Tracks;
  Add_Playlist.Visible:= (PopupItem.Source = TDataSource.Tracks) and not IsOffline;

  Separator_Browse.Visible:= MenuItem15.Visible or MenuItem17.Visible;

  Popup_Playlist_Remove.Visible := CurrentPage = TPage.ViewPlaylist;
  Separator_Playlist.Visible := Popup_Playlist_Remove.Visible;

  if Popup_Playlist_Remove.Visible then
    try
      I := GetPlaylist(PageData);

      Popup_Playlist_Remove.Enabled:= (I <> -1) and (Playlists[I].PlaylistType = '');
    except
      Popup_Playlist_Remove.Enabled:= false;
    end;

  Popup_Download.Visible:=not PopupItem.Trashed and not IsOffline and not (PopupItem.Source = TDataSource.Genres);
  Separator_Download.Visible:=Popup_Download.Visible;
  Popup_Download.Checked:=PopupItem.Downloaded;
  if Popup_Download.Checked then
    Popup_Download.Caption:='Downloaded'
  else
    Popup_Download.Caption:='Download';

  Popup_Save.Visible:=(PopupItem.Source = TDataSource.Tracks) and not IsOffline;
  Separator_Save.Visible:=Popup_Save.Visible;

  Separator_Queue.Visible:=IsQueuePage;
  Popup_Queue_Now.Visible:=IsQueuePage and (PopupIndex <> QueueIndex);
  Popup_Queue_Bottom.Visible:=IsQueuePage and (PopupIndex <> Queue.Count-1);
  Popup_Queue_Remove.Visible:=IsQueuePage;

  // Populate Playlist
  if Add_Playlist.Visible then
    begin
      for I := Add_Playlist.Count-1 downto 0 do
        Add_Playlist.Delete(I);

      for I := 0 to High(Playlists) do
        if Playlists[I].PlaylistType = '' then
          begin
            M := TMenuItem.Create(Add_Playlist);
            with M do
              begin
                Caption:=Playlists[I].Name;
                Tag:=I;

                ShowAlwaysCheckable:=true;
                Checked := Playlists[I].TracksID.Find(PopupItem.ItemID) <> -1;

                OnClick := Generic_AddPlaylistClick;
              end;

            Add_Playlist.Add(M);
          end;
    end;
end;

procedure TMain.LoginDelayTimer(Sender: TObject);
begin
  { This is not done in Form.Create, to keep the Modal Effect }
  if not Visible then
    Exit;

  // Disable
  TTimer(Sender).Enabled:=false;

  // Load login credentials (if they exist)
  LoadCredentials;

  // Login
  if not IsOffline then
    DoLogin;

  // Offline
  if IsOffline then
    LoadOfflineMode;

  // Load
  DownloadManager.LoadConfig;

  // Remove invalid
  DownloadManager.ValidateDownloads;

  // Start download
  DownloadManager.CheckDownloads;

  // Load queue
  if Option_savequeue.Checked then
    LoadQueueFromFile;

  // Update
  if Option_checkupdates.Checked and (DaysBetween(LastUpdateCheck, Now) > 0) then
    TDialogCheckUpdatesThread.Create.Start;

  // DONE
  LoginScriptDone := true;
end;

procedure TMain.MenuDeleteClick(Sender: TObject);
begin
  // Delete
  if messagedlg('Are you sure?', Format('Do you want to delete "%S"?', [PopupItem.Title]),
    mtWarning, [mbYes, mbNo], 0) = mrYes then
      begin
        // Remove from downloads
        if PopupItem.Downloaded then
          PopupItem.ToggleDownloaded;

        // Send to trash
        with TDialogTrashItem.Create do
          begin
            Source := PopupItem.Source;
            ItemID := PopupItem.ItemID;

            Start;
          end;

        if TaskExec.ShowModal <> mrOk then
          ShowMessage('Could not send to trash');
      end;
end;

procedure TMain.MenuItem11Click(Sender: TObject);
begin
  // Library
  DoReloadlibrary;
end;

procedure TMain.Popup_AddQueueClick(Sender: TObject);
begin
  case PopupItem.Source of
    TDataSource.Tracks: QueueAdd( PopupItem.ItemID );
    TDataSource.Albums: QueueAdd( Albums[PopupItem.Index].TracksID );
    TDataSource.Artists: QueueAdd( Artists[PopupItem.Index].TracksID );
    TDataSource.Playlists: QueueAdd( Playlists[PopupItem.Index].TracksID );
  end;
end;

procedure TMain.MenuItem15Click(Sender: TObject);
begin
  case PopupItem.Source of
    TDataSource.Tracks: SelectPage(TPage.ViewArtist, Tracks[PopupItem.Index].ArtistID);
    TDataSource.Albums: SelectPage(TPage.ViewArtist, Albums[PopupItem.Index].ArtistID);
  end;
end;

procedure TMain.MenuItem17Click(Sender: TObject);
begin
  case PopupItem.Source of
    TDataSource.Tracks: SelectPage(TPage.ViewAlbum, Tracks[PopupItem.Index].AlbumID);
  end;
end;

procedure TMain.MenuItem1Click(Sender: TObject);
begin
  SelectPage( TPage.Settings );
end;

procedure TMain.Menu_RestoreClick(Sender: TObject);
begin
  // Restore
  with TDialogRestoreItem.Create do
    begin
      Source := PopupItem.Source;
      ItemID := PopupItem.ItemID;

      Start;

      if TaskExec.ShowModal <> mrOk then
        ShowMessage('Could not restore from trash');
    end;
end;

procedure TMain.MenuItem4Click(Sender: TObject);
begin
  AboutDialog := TAboutDialog.Create(Self);
  with AboutDialog do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMain.MenuItem5Click(Sender: TObject);
begin
  SelectPage( TPage.Trash );
end;

procedure TMain.MenuItem7Click(Sender: TObject);
begin
  SelectPage( TPage.History );
end;

procedure TMain.MenuItem8Click(Sender: TObject);
begin
  SelectPage( TPage.Downloads );
end;

procedure TMain.Music_PlayClick(Sender: TObject);
begin
  case Player.PlayStatus of
    TPlayStatus.psStopped,
    TPlayStatus.psPaused: Player.Play;
    TPlayStatus.psPlaying: Player.Pause;
  end;
end;

procedure TMain.Music_PositionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SliderPressed := true;
  LastPlayState := Player.PlayStatus;
  Player.Pause;
end;

procedure TMain.Music_PositionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Dest: single;
begin
  if not SliderPressed then Exit;

  SliderPressed := false;

  Dest := TTrackBar(Sender).Position / 10;
  if PlayType = TPlayType.Streaming then
    begin
      SetPlayIcon(psStalled);
      Application.ProcessMessages;
      Player.Volume:=0;

      repeat
        Player.Play;
        Player.PositionSeconds:=Dest;
        Sleep(10);
      until EqualApprox(Player.PositionSeconds, Dest, 1);

      // Reset Settings
      PlayerApplySettings;
    end
  else
    Player.PositionSeconds:=Dest;

  // Status
  Player.PlayStatus := LastPlayState;
end;

procedure TMain.DrawingBoxPaint(Sender: TObject);
var
  AWidth, AHeight, Index, I, X, Y: integer;

  FitX, ExtraSpacing: integer;

  ItemsCount: cardinal;
  ARect: TRect;

  Title: string;
  Style: TTextStyle;
begin
  if CustomDraw then
    begin
      DoCustomDraw(Sender);
      Exit;
    end;

  ItemsCount := Length(SortingList);

  // Common
  AWidth := TPaintBox(Sender).Width;
  AHeight := TPaintBox(Sender).Height;

  X := 0;
  Y := -ScrollPosition;

  // Draw
  if ViewStyle = TViewStyle.Cover then
    begin
      FitX := (AWidth div (CoverWidth + CoverSpacing));
      if FitX = 0 then
        Exit;
      ExtraSpacing := round((AWidth - FitX * (CoverWidth + CoverSpacing)) / FitX);

      for I := 0 to ItemsCount - 1 do
        begin
          // Index
          Index := SortingList[I];

          // Hidden
          if DrawItems[Index].Hidden then
            begin
              DrawItems[Index].Bounds := Rect(0,0,0,0);
              Continue;
            end;

          // Rect
          ARect := Rect( X, Y, X + CoverWidth, Y + CoverHeight);

          DrawItems[Index].Bounds := ARect;
          if (Index = IndexHover) and (Press10Stat <> 0) then
            ARect.Inflate(-Press10Stat, -trunc(ARect.Height/ ARect.Width * Press10Stat));

          // Draw if visible
          if (Y + CoverHeight > 0) and (Y < AHeight) then
            DrawItemCanvas(TPaintBox(Sender).Canvas, ARect, @DrawItems[Index]);

          // Move Line
          Inc(X, CoverWidth + CoverSpacing + ExtraSpacing);

          if X + CoverWidth > AWidth then
            begin
              X := 0;
              Inc(Y, CoverHeight + CoverSpacing);
            end;

          if Y > AHeight then
            Break;
        end;
    end // Style = Cover
  else
    begin
      for I := 0 to ItemsCount - 1 do
        begin
          // Index
          Index := SortingList[I];

          // Hidden
          if DrawItems[Index].Hidden then
            begin
              DrawItems[Index].Bounds := Rect(0,0,0,0);
              Continue;
            end;

          // Rect
          ARect := Rect( X, Y, X + AWidth, Y + ListHeight);

          DrawItems[Index].Bounds := ARect;

          // Draw if visible
          if (Y + CoverHeight > 0) and (Y < AHeight) then
            begin
              if (Index = IndexHover) and (Press10Stat <> 0) then
                ARect.Inflate(-Press10Stat, -Press10Stat);

              // Draw
              DrawItemCanvas(TPaintBox(Sender).Canvas, ARect, @DrawItems[Index]);
            end;

          // Move Line
          Inc(Y, ListHeight + ListSpacing);
                    
          // OOB
          if Y > AHeight then
            Break; Continue
        end;
    end; // Style = List

  // Nothing here
  if (X = 0) and (Y = 0) and (ScrollPosition = 0) then
    begin
      if PageIndexingError then
        Title := 'An indexing error occured. So, this page is empty'
      else
        Title := BLANK_PAGE;
      ARect := TPaintBox(Sender).ClientRect;

      Style.Alignment:=taCenter;
      Style.Layout:=tlCenter;
      Style.Wordbreak := true;

      with TPaintBox(Sender).Canvas do begin
        Font.Assign(Self.Font);
        TextRect(ARect, 0,0, Title, Style);
      end;
    end;

  // Scroll
  UpdateScroll;
end;

procedure TMain.Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with DrawingBox do
  if TRect.Create(ClientToScreen(ClientRect.TopLeft), ClientToScreen(ClientRect.BottomRight)).Contains( Mouse.CursorPos ) then
      ScrollPosition := ScrollPosition - WheelDelta div 2;
end;

procedure TMain.PressNowTimer(Sender: TObject);
var
  S: integer;
begin
  // Status
  if DrawPressed then
    S := sign(10-Press10Stat)
  else
    S := sign(-Press10Stat);

  Inc(Press10Stat, S);

  // Draw
  DrawingBox.Invalidate;

  // Disable
  if S = 0 then
    PressNow.Enabled:=false;
end;

procedure TMain.ScrollBar1Change(Sender: TObject);
begin
  if CurrentPage <> TPage.None then
    DrawingBox.Invalidate;;
end;

procedure TMain.SpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := Point(0, TSpeedButton(Sender).Height);
  P := TSpeedButton(Sender).ClientToScreen(P);

  ExtrasMenu.PopUp(P.X, P.Y);
end;

procedure TMain.Sidebar_ToggleClick(Sender: TObject);
const
  SMALL_SIZE = 50 + 10;
var
  Minimise: boolean;
begin
  Minimise := SubView_Sidebar.Width <> SMALL_SIZE;
  if Minimise then
    SubView_Sidebar.Width := SMALL_SIZE
  else
    SubView_Sidebar.Width := 250;

  if Minimise then
    TSpeedButton(Sender).Caption:=ICON_MENU
  else
    TSpeedButton(Sender).Caption:=ICON_CLOSE;

  Sidebar_Vertical.Visible:=Minimise;
  Sidebar_full.Visible:=not Minimise;
end;

procedure TMain.Debugger32Timer(Sender: TObject);
begin
  Caption :=
    Format('Status: "%S", Threads: %D', [BroadcastAPI.DATA_WORK_STATUS, SimultanousTaskThreads]);
end;

procedure TMain.ToggleBox15Change(Sender: TObject);
begin
  Toolbar_Search1.Visible := TToggleBox(Sender).Checked;
end;

procedure TMain.ToggleBox16Change(Sender: TObject);
begin
  Toolbar_Search2.Visible := TToggleBox(Sender).Checked;
end;

procedure TMain.ToggleBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TWinControl;
  I: integer;
begin
  P := TToggleBox(Sender).Parent;
  for I := 0 to P.ControlCount-1 do
    if P.Controls[I] is TToggleBox then
      begin
        TToggleBox(P.Controls[I]).Checked:=false;
        TToggleBox(P.Controls[I]).Font.Style:=[];
      end;

  TToggleBox(Sender).Checked:=true;
  TToggleBox(Sender).Font.Style:=[fsBold];

  // View
  case TToggleBox(Sender).Tag of
    0: SetDrawPageFilter(TDataSource.Tracks);
    1: SetDrawPageFilter(TDataSource.Albums);
    2: SetDrawPageFilter(TDataSource.Artists);
    3: SetDrawPageFilter(TDataSource.Playlists);
    4: SetDrawPageFilter(TDataSource.None);
  end;

  // Load
  CurrentViewReload;
end;

procedure TMain.ToggleBox6Change(Sender: TObject);
begin
  with TToggleBox(Sender) do
    begin
      // UI
      if Checked then
        Font.Style := [fsBold]
      else
        Font.Style := [];

      // Search (reload)
      CurrentViewReload;
    end;
end;

procedure TMain.ToggleVisual(Sender: TObject);
begin
  Visualisation_Player.Visible := not Visualisation_Player.Visible;
  Equaliser_Indic.Visible:=Visualisation_Player.Visible;
end;

procedure TMain.VisualRenderTimer(Sender: TObject);
begin
  RenderVisualisations;
end;

function TMain.GetScrollMax: integer;
begin
  Result := ScrollBar1.Max;;
end;

function TMain.PageToName(APage: TPage): string;
begin
  case APage of
    TPage.Home: Result := 'Home';
    TPage.Search: Result := 'Search';
    TPage.Queue: Result := 'Play Queue';
    TPage.Albums: Result := 'Albums';
    TPage.Songs: Result := 'Songs';
    TPage.Artists: Result := 'Artists';
    TPage.Playlists: Result := 'Playlists';
    TPage.Genres: Result := 'Genres';
    TPage.Trash: Result := 'Trash';
    TPage.Downloads: Result := 'Downloads';
    TPage.History: Result := 'History';
    TPage.Account: Result := 'Account';
    TPage.Settings: Result := 'Settings';
    TPage.ViewAlbum: Result := 'View Album';
    TPage.ViewArtist: Result := 'View Artist';
    TPage.ViewPlaylist: Result := 'View Playlist';
    TPage.ViewGenre: Result := 'View Genre';

    else Result := 'Page';
  end;
end;

function TMain.GetItemCount: integer;
begin
  Result := Length(DrawItems);
end;

procedure TMain.InitiateDirectories;
var
  AFolder: string;
  D: TDataSource;
begin
  AFolder := GetUserDir + '.config/';

  if not DirectoryExists(AFolder) then
    raise Exception.Create('Config error, app cannot be configured');

  AppData := AFolder + 'cod-ibroadcast/';

  if not DirectoryExists(AppData) then
    MkDir(AppData);

  if not DirectoryExists(AppData) then
    raise Exception.Create('Failed to create application working directory');

  // Prep folders
  TempFolder := AppData + 'temp/';
  if not DirectoryExists(TempFolder) then
    MkDir(TempFolder);

  DownloadsFolder := AppData + 'downloads/';
  if not DirectoryExists(DownloadsFolder) then
    MkDir(DownloadsFolder);

  // Artwork store
  MediaStoreLocation := AppData + 'artwork-store/';

  if ArtworkStore then
    begin
      if not DirectoryExists(MediaStoreLocation) then
        MkDir(MediaStoreLocation);

      for D := Low(TDataSource) to High(TDataSource) do
        if not (D in [TDataSource.None]) then
          begin
            AFolder := GetArtworkStore(D);

            if not DirectoryExists(AFolder) then
              MkDir(AFolder);
          end;
    end;
end;

function TMain.DoLogin: boolean;
begin
  // Create form
  if LoadLib = nil then
    LoadLib := TLoadLib.Create(Application);
  LoadLib.Status_Text.Caption:='Logging in';

  // Login thread
  TLoginThread.Create;

  // Succeeded?
  Result := LoadLib.ShowModal = mrOk;
  if not Result then
    begin
      if (LoginScriptDone or ConnectedToServer) then
        OpenLoginPage
      else
        IsOffline := true;
    end;

  // Page
  SelectPage(TPage.Home);
end;

procedure TMain.DoReloadlibrary;
begin
  // Create form
  if LoadLib = nil then
    LoadLib := TLoadLib.Create(Application);
  LoadLib.Status_Text.Caption:='Contacting iBroadcast...';

  // Login thread
  TLoadLibraryThread.Create;

  LoadLib.ShowModal;
end;

procedure TMain.OpenLoginPage;
begin
  Login := TLogin.Create(Self);
  with Login do
    try
      case ShowModal of
        mrOk: begin
          LOGIN_TOKEN := Login_AppToken.Text;
          APPLICATION_ID := Login_AppID.Text;

          // Retry
          if DoLogin then
            SaveCredentials;
        end;

        mrClose: Application.Terminate;
      end;
    finally
      Free;
    end;
end;

procedure TMain.SaveCredentials;
var
  FilePath: string;
  S: TStringList;
begin
  FilePath := AppData + 'credentials.cred';

  S := TStringList.Create;
  try
    // Save
    S.Add(APPLICATION_ID);
    S.Add(LOGIN_TOKEN);

    S.SaveToFile(FilePath);
  finally
    S.Free;
  end;
end;

procedure TMain.LoadCredentials;
var
  FilePath: string;
  S: TStringList;
begin
  FilePath := AppData + 'credentials.cred';

  if not fileexists(FilePath) then
    Exit;

  S := TStringList.Create;
  try
    S.LoadFromFile(FilePath);

    // Invalid length
    if S.Count < 2 then
      Exit;

    // Read
    APPLICATION_ID := S[0];
    LOGIN_TOKEN := S[1];
  finally
    S.Free;
  end;
end;

procedure TMain.DeleteCredentials;
var
  FilePath: string;
begin
  FilePath := AppData + 'credentials.cred';

  if fileexists(FilePath) then
    deletefile(FilePath);
end;

function TMain.GetScrollPosition: integer;
begin
  Result := ScrollBar1.Position;
end;

procedure TMain.SetScrollMax(AValue: integer);
begin
  ScrollBar1.Max := AValue;
end;

procedure TMain.SetScrollPosition(AValue: integer);
begin
  ScrollBar1.Position:=AValue;
end;

procedure ArrayItemSort(AType: TSortType);
  function GetTitleValue(Index: integer): string;
  begin
    Result := DrawItems[SortingList[Index]].Title;
  end;

  function GetDateValue(Index: integer): cardinal;
  begin
    Result := 0;
    with TDrawItem(DrawItems[SortingList[Index]]) do
      case Source of
        TDataSource.Tracks: Result := Tracks[Index].Year;
        TDataSource.Albums: Result := Albums[Index].Year;
      end;
  end;

  function GetRatingValue(Index: integer): cardinal;
  begin
    Result := DrawItems[SortingList[Index]].Rating;
  end;

  function CompareItems(A, B: integer): boolean;
  begin
    case AType of
      TSortType.Year: Result := GetDateValue(A) < GetDateValue(B);
      TSortType.Rating: Result := GetRatingValue(A) < GetRatingValue(B);

      // Default
      else Result := GetTitleValue(A) > GetTitleValue(B);
    end;
  end;

var
  i, j: Integer;
  temp: integer;
begin
  for i := High(SortingList) downto Low(SortingList) + 1 do
  begin
    for j := Low(SortingList) to i - 1 do
    begin
      if CompareItems(J, J+1) then
      begin
        temp := SortingList[j];
        SortingList[j] := SortingList[j + 1];
        SortingList[j + 1] := temp;
      end;
    end;
  end;
end;

procedure TMain.Sort;
var
  I: integer;
begin
  // Size
  SetLength(SortingList, GetItemCount );

  // Reset Array
  for I := 0 to High(SortingList) do
    SortingList[I] := I;

  // Custom draw (not sorted)
  if CustomDraw then
    Exit;

  // Sort Array
  case SortType of
    TSortType.Default: Exit;
    TSortType.Flipped: for I := 0 to High(SortingList) do
                          SortingList[I] := High(SortingList) - I;
    else ArrayItemSort(SortType);
  end;
end;

function TMain.SearchCompareFound(Data1, Data2: string; Flags: TSearchFlags): boolean;
var
  MashedCompare,
  MashedSource: string;
  (* Mashed Potatoes *)
begin
  // Mash Data
  MashedSource := Data2;
  MashedCompare := Data1;

  if not (TSearchFlag.CaseSensitive in Flags) then
    begin
      MashedSource := AnsiLowerCase(MashedSource);
      MashedCompare := AnsiLowerCase(MashedCompare);
    end;

  if not (TSearchFlag.ExactMatch in Flags) then
    begin
      MashedSource := ClearStringSimbols(MashedSource);
      MashedCompare := ClearStringSimbols(MashedCompare);
    end;

  // Compare
  if TSearchFlag.ExactMatch in Flags then
    Result := MashedCompare = MashedSource
  else
    Result := Pos(MashedCompare, MashedSource) <> 0;
end;

procedure TMain.FiltrateSearch(Term: string; Flags: TSearchFlags);
var
  I: Integer;
  Found: boolean;
  SearchTrashed: boolean;
begin
  SearchTrashed := TSearchFlag.SearchTrashed in Flags;

  for I := 0 to High(DrawItems) do
    begin
      DrawItems[I].HideReasons := DrawItems[I].HideReasons - [THiddenReason.Search];

      if Term = '' then
        Continue;

      if not SearchTrashed and DrawItems[I].Trashed then
        begin
          DrawItems[I].HideReasons := DrawItems[I].HideReasons + [THiddenReason.Search];
          Continue;
        end;

      // Compare
      Found := SearchCompareFound(Term, DrawItems[I].Title, Flags)
        or ((TSearchFlag.SearchInfo in Flags) and SearchCompareFound(Term, DrawItems[I].InfoLong, Flags))
        or (DrawItems[I].ItemID = Term);

      // Visible
      if not Found then
        DrawItems[I].HideReasons := DrawItems[I].HideReasons + [THiddenReason.Search];
    end;

  LastFilterQuery := Term;
end;

procedure TMain.CurrentViewReload;
begin
  LoadDrawItems;
  Sort;
  DrawingBox.Invalidate;
end;

procedure TMain.LoadOfflineMode;
begin
  IsOffline := true;

  // UI
  Button1.Enabled:=false;
  Button5.Enabled:=false;

  MenuItem7.Enabled:=false;
  MenuItem5.Enabled:=false;
  MenuItem13.Enabled:=false;
  MenuItem11.Enabled:=false;
  MenuItem12.Enabled:=false;

  Popup_Download.Enabled:=false;
  Popup_Save.Enabled:=false;
  Menu_Trash.Enabled:=false;
  Menu_Restore.Enabled:=false;
  Menu_Permadelete.Enabled:=false;
  Popup_Playlist_Remove.Enabled:=false;

  if not DebugMode then
    Caption := Caption + ' [OFFLINE]';

  // Download Manager
  with TDialogLibraryLoadThread.Create do
    begin
      Start;

      if TaskExec.ShowModal <> mrOk then
        ShowMessage('Could not load library');
    end;

  // Page
  SelectPage(TPage.Songs);
end;

procedure TMain.DoDebug;
begin
  // Settings
  Debugger32.Enabled:=DebugMode;
  Popup_ID.Visible:=DebugMode;

  // Disable login (UI development)
  //LoginDelay.Enabled:=false;

  // Test code
  //OpenPopupPlayer;
end;

procedure TMain.QueueAdd(Track: string; AutoPlay: boolean);
var
  T: TStringArray;
begin
  T := [];
  T[0] := Track;

  QueueAdd(T, AutoPlay);
end;

procedure TMain.QueueAdd(Tracks: TStringArray; AutoPlay: boolean);
var
  I: integer;
begin
  for I := 0 to High(Tracks) do
    Queue.Add(Tracks[I]);

  // Auto play
  if AutoPlay and (Queue.Count - Length(Tracks) = 0) then
    QueueSetPos(0);

  // Updated
  QueueUpdated;
end;

procedure TMain.QueueClear;
begin
  Queue.Clear;
  QueueIndex := 0;
                
  // Updated
  QueueUpdated;
end;

procedure TMain.QueueMove(IndexFrom, IndexTo: integer);
begin
  if (IndexFrom >= Queue.Count) or (IndexTo >= Queue.Count) or (IndexFrom = IndexTo) then
    Exit;

  Queue.Move(IndexFrom, IndexTo);

  // Comparer
  if IndexFrom = QueueIndex then
    // Equal
    QueueIndex := IndexTo
  else
    case CompareValue(IndexFrom, IndexTo) of
      // Ascending
      LessThanValue : begin
        if InRange(QueueIndex, IndexFrom, IndexTo) then
          Dec(QueueIndex);
      end;

      // Descending
      GreaterThanValue: begin
        if InRange(QueueIndex, IndexTo, IndexFrom) then
          Inc(QueueIndex);
      end;
    end;

  // Updated
  QueueUpdated;
end;

procedure TMain.QueueDelete(Index: integer);
begin
  if Index < Queue.Count then
    Queue.Delete(Index);

  if Index < QueueIndex then
    Dec(QueueIndex);

  // Updated
  QueueUpdated;
end;

procedure TMain.StartDrawPlay(SongID: string);
var
  I, NewIndex: integer;
begin
  QueueClear;
  NewIndex := -1;

  // Add
  for I := 0 to High(DrawItems) do
    if DrawItems[I].Source = TDatasource.Tracks then
      begin
        Queue.Add(DrawItems[I].ItemID);

        if DrawItems[I].ItemID = SongID then
          NewIndex := Queue.Count-1;
      end;

  // Play
  if NewIndex <> -1 then
    QueueSetPos(NewIndex, true);
end;

procedure TMain.QueueSetPos(Index: integer; StartPlay: boolean);
begin
  QueueIndex := Index;

  // Track Changed
  QueueTrackChanged;

  // Play
  QueuePlayCurrent(StartPlay);
end;

procedure TMain.QueuePlayCurrent(StartPlay: boolean);
begin
  if QueueIndex >= Queue.Count then
    Exit;

  // Play
  PlaySong(Queue[QueueIndex], StartPlay);
end;

procedure TMain.QueueNext;
begin
  if QueueIndex < Queue.Count-1 then
    QueueSetPos(QueueIndex+1)
  else
    if RepeatMode = TRepeat.All then
      QueueSetPos(0);
end;

procedure TMain.QueuePrevious;
begin
  if QueueIndex > 0 then
    QueueSetPos(QueueIndex-1);
end;

procedure TMain.QueueUpdated(OverrideShuffleDefault: boolean);
begin
  // Update view
  if CurrentPage = TPage.Queue then
    CurrentViewReload;

  // Save
  if OverrideShuffleDefault then
    SaveShuffleDefault;

  // UI
  QueueUpdateUI;
end;

procedure TMain.QueueTrackChanged;
begin
  // Update view
  if CurrentPage = TPage.Queue then
    CurrentViewReload;

  // UI
  QueueUpdateUI;
end;

procedure TMain.QueueUpdateUI;
begin
  Music_Prev.Enabled:=QueueIndex>0;
  Music_Next.Enabled:=(QueueIndex<Queue.Count-1) and not ((RepeatMode = TRepeat.Off) and (QueueIndex=Queue.Count-1));

  // Repeat
  case RepeatMode of
    TRepeat.All: Queue_Repeat.Caption := 'Repeat All';
    TRepeat.One: Queue_Repeat.Caption := 'Repeat One';
    TRepeat.off: Queue_Repeat.Caption := 'Repeat Off';
  end;

  if ShuffleMode then
    Queue_Shuffle.Caption:='✓ Shuffle'
  else
    Queue_Shuffle.Caption:='Shuffle';

  UpdatePopupPlayIcons;

  // Redraw UI
  DrawingBox.Invalidate;
end;

procedure TMain.QueueFindPlaying;
var
  I: integer;
begin
  for I := 0 to Queue.Count-1 do
    if Queue[I] = PlayID then
      begin
        QueueIndex := I;
        Break;
      end;

  // Updated
  QueueTrackChanged;
end;

procedure TMain.SaveQueueToFile;
var
  FilePath: string;
  F: TextFile;
  I: integer;
begin
  FilePath := AppData + 'last-queue.conf';

  // Delete previous
  if fileexists(FilePath) then
    deletefile(FilePath);

  // Exit
  if Queue.Count = 0 then
    Exit;

  // File
  AssignFile(F, FilePath);
  try
    Rewrite(F);

    // Queue Position
    WriteLn( F, Queue[QueueIndex] );

    // Queue
    for I := 0 to Queue.Count -1 do
      WriteLn(F, Queue[I]);
  finally
    // Close
    CloseFile(F);
  end;
end;

procedure TMain.LoadQueueFromFile;
var
  FilePath: string;
  F: TextFile;
  PlayID: string;
  PlayIndex: integer;
  ID: string;
  S: string;
begin
  FilePath := AppData + 'last-queue.conf';

  // Exit
  if not fileexists(FilePath) then
    Exit;

  Queue.Clear;

  // File
  AssignFile(F, FilePath);
  try
    Reset(F);

    if EOF(F) then
      Exit;

    // Queue position
    ReadLn(F, S);
    PlayID := S;

    while not EOF(F) do
      try
        ReadLn(F, S);
        ID := S;

        if GetTrack(ID) <> -1 then
          Queue.Add(ID);
      except
      end;

    // Play
    PlayIndex := Max(Queue.IndexOf(PlayID), 0);
    QueueSetPos(PlayIndex, false);
  finally
    // Close
    CloseFile(F);
  end;
end;

procedure TMain.LoadShuffleDefault;
var
  I: integer;
begin
  Queue.Clear;

  for I := 0 to QueueDefault.Count-1 do
    Queue.Add( QueueDefault[I] );
end;

procedure TMain.SaveShuffleDefault;
var
  I: integer;
begin
  QueueDefault.Clear;

  for I := 0 to Queue.Count-1 do
    QueueDefault.Add( Queue[I] );
end;

procedure TMain.ShuffleQueue;
var
  RandomQueue: TIntArray;
  I, RandIndex, Start: integer;
begin
  if Queue.Count = 0 then
    Exit;

  // Shuffle previous (simplist random algorithm)
  for I := 0 to QueueIndex-1 do
    begin
      RandIndex := Random(QueueIndex);
      Queue.Move(I, RandIndex);
    end;

  // Shuffle next (Fisher-Yates shuffle algorithm)
  Randomize;
  RandomQueue := GenerateRandomSequence( Queue.Count - QueueIndex - 1 );
  Start := QueueIndex + 1;
  for I := 0 to High(RandomQueue) do
    RandomQueue[I] := RandomQueue[I] + Start - 1;

  // Apply Order
  for I := 0 to High(RandomQueue) do
    Queue.Move(I + Start, RandomQueue[I]);
end;

procedure TMain.WaitForThreads;
var
  T: integer;
begin
  T := 0;
  while (SimultanousTaskThreads <> 0) or (T / 100 >= 30) do
    begin
      Sleep(10);
      Application.ProcessMessages;
      Inc(T);
    end;
end;

procedure TMain.DoCustomDraw(Sender: TObject);
const
  HEAD_SIZE = 20;
var
  AWidth, AHeight, Index, I, J, X, Y: integer;
  FitX, ExtraSpacing: integer;
  ItemsCount: cardinal;
  ARect: TRect;

  LastType: TDataSource;
  LastLine: integer;
  LabelHeight: integer;

  Text: string;
  OutOfBounds: boolean;

  EmptyString: string;
  Style: TTextStyle;
begin
  ItemsCount := Length(DrawItems); // not sorted!

  // Common
  AWidth := TPaintBox(Sender).Width;
  AHeight := TPaintBox(Sender).Height;

  X := 0;
  Y := -ScrollPosition;

  EmptyString := BLANK_PAGE;

  // Label Style (default)
  Style.Alignment:=taLeftJustify;
  Style.Layout:=tlCenter;
  Style.Wordbreak := true;
  with TPaintBox(Sender).Canvas do
    begin
      // Font
      Font.Assign(Self.Font);
      Font.Size:= HEAD_SIZE;
      LabelHeight := TextHeight('ABCabc...');
    end;

  // Scroll Settings (default)
  OutOfBounds := false;

  // Cover Style (defaulkekt)
  FitX := (AWidth div (CoverWidth + CoverSpacing));
  if FitX = 0 then
    Exit;
  ExtraSpacing := round((AWidth - FitX * (CoverWidth + CoverSpacing)) / FitX);

  // Draw
  case CurrentPage of
    TPage.Home: begin
        // Size changed
        if HomeColumns <> FitX then
          LoadDrawItems;

        // Data
        LastLine := -1;
        EmptyString := 'Could not load home page';

        { I = current column (also, FitX) }
        { J = current line }

        for J := 0 to HOME_TOTAL_ROWS-1 do
        for I := 0 to FitX-1 do
        begin
          // Index32 (* Why Index32? Because It sounds cool *)
          Index := J * FitX + I;

          // Limit of Index32
          if Index > Length(DrawItems) then
            Break;

          // Hidden
          if DrawItems[Index].Hidden then
            begin
              DrawItems[Index].Bounds := Rect(0,0,0,0);
              Continue;
            end;

          // Draw labels (if not all items are hidden!!)
          if J <> LastLine then
            begin
              // Set
              LastLine := J;

              // Line
              if X <> 0 then
                Inc(Y, CoverHeight + CoverSpacing);

              X := 0;
              Inc(Y, CoverSpacing);

              if not OutOfBounds then
                with TPaintBox(Sender).Canvas do begin
                  // Font
                  Font.Assign(Self.Font);
                  Font.Size:= HEAD_SIZE;

                  case LastLine of
                    0: Text := 'Recently played albums';
                    1: Text := 'Favorite tracks';
                    2: Text := 'Recently played tracks';
                    3: Text := 'From your playlists';
                    4: Text := 'Recently added tracks';
                  end;

                  // Text
                  ARect := Rect(X, Y, AWidth, Y+LabelHeight);

                  TPaintBox(Sender).Canvas.TextRect(ARect, ARect.Left, 0, Text, Style);
                end;

              // Next
              Inc(Y, LabelHeight + CoverSpacing div 2);
            end;

          // Rect
          ARect := Rect( X, Y, X + CoverWidth, Y + CoverHeight);

          DrawItems[Index].Bounds := ARect;
          if (Index = IndexHover) and (Press10Stat <> 0) then
            ARect.Inflate(-Press10Stat, -trunc(ARect.Height/ ARect.Width * Press10Stat));

          // Draw if visible
          if (Y + CoverHeight > 0) and (Y < AHeight) and not OutOfBounds then
            DrawItemCanvas(TPaintBox(Sender).Canvas, ARect, @DrawItems[Index]);

          // Move Line
          Inc(X, CoverWidth + CoverSpacing + ExtraSpacing);

          if X + CoverWidth > AWidth then
            begin
              X := 0;
              Inc(Y, CoverHeight + CoverSpacing);
            end;
                 
          // OOB
          if Y > AHeight then
            OutOfBounds := true;
        end;

        // Scroll
        ScrollMax:=Y+ScrollPosition;
      end;

    TPage.Queue:
      begin
        // Settings
        EmptyString := 'Queue is empty.';

        for I := 0 to ItemsCount-1 do
        begin
          // Index
          Index := I;

          // Category
          if (Index = 0) or (Index = QueueIndex) or (Index = QueueIndex+1) then
            begin
              // Line
              Inc(Y, CoverSpacing);

              if not OutOfBounds then
                with TPaintBox(Sender).Canvas do begin
                  // Font
                  Font.Assign( Self.Font );
                  Font.Size:= HEAD_SIZE;

                  if Index = QueueIndex then
                    Text := 'Currently playling'
                  else
                  if Index = 0 then
                    Text := 'Previous tracks'
                  else
                  if Index = QueueIndex+1 then
                    Text := 'Next up';

                  // Text
                  ARect := Rect(X, Y, AWidth, Y+LabelHeight);

                  TPaintBox(Sender).Canvas.TextRect(ARect, ARect.Left, 0, Text, Style);
                end;

              // Next
              Inc(Y, LabelHeight + CoverSpacing div 2);
            end;

          // Rect
          ARect := Rect( X, Y, X + AWidth, Y + ListHeight);

          DrawItems[Index].Bounds := ARect;
          if (Index = IndexHover) and (Press10Stat <> 0) then
            ARect.Inflate(-Press10Stat, -trunc(ARect.Height/ ARect.Width * Press10Stat));

          // Draw if visible
          if (Y + CoverHeight > 0) and (Y < AHeight) and not OutOfBounds and not (IsDragging and (DraggingIndex = Index)) then
            DrawItemCanvas(TPaintBox(Sender).Canvas, ARect, @DrawItems[Index]);

          // Move Line
          Inc(Y, ListHeight + ListSpacing);

          // OOB
          if Y > AHeight then
            OutOfBounds := true;
        end;

        // Draw Drag (over other items)
        if IsDragging then
          begin
            ARect := Rect( X, DrawCursorPos.Y - ListHeight div 2, X + AWidth, DrawCursorPos.Y + ListHeight div 2);
            ARect.Inflate(-15, -trunc(ARect.Height/ ARect.Width * 15));

            DrawItemCanvas(TPaintBox(Sender).Canvas, ARect, @DrawItems[DraggingIndex]);
          end;

        // Scroll
        ScrollMax:=Y+ScrollPosition;
      end;

    TPage.Search:
      begin
        // Settings
        LastType := TDataSource.None;
        EmptyString := 'No search results.';

        for I := 0 to ItemsCount - 1 do
        begin
          // Index
          Index := I;

          // Hidden
          if DrawItems[Index].Hidden then
            begin
              DrawItems[Index].Bounds := Rect(0,0,0,0);
              Continue;
            end;

          // Category
          if DrawItems[Index].Source <> LastType then
            begin
              // Set
              LastType := DrawItems[Index].Source;

              // Line
              if X <> 0 then
                Inc(Y, CoverHeight + CoverSpacing);

              X := 0;
              Inc(Y, CoverSpacing);

              if not OutOfBounds then
                with TPaintBox(Sender).Canvas do begin
                  // Font
                  Font.Assign( Self.Font );
                  Font.Size:= HEAD_SIZE;

                  case LastType of
                    TDataSource.Tracks: Text := 'Tracks';
                    TDataSource.Albums: Text := 'Albums';
                    TDataSource.Artists: Text := 'Artists';
                    TDataSource.Playlists: Text := 'Playlists';
                  end;

                  // Text
                  ARect := Rect(X, Y, AWidth, Y+LabelHeight);

                  TPaintBox(Sender).Canvas.TextRect(ARect, ARect.Left, 0, Text, Style);
                end;

              // Next
              Inc(Y, LabelHeight + CoverSpacing div 2);
            end;

          // Rect
          ARect := Rect( X, Y, X + CoverWidth, Y + CoverHeight);

          DrawItems[Index].Bounds := ARect;
          if (Index = IndexHover) and (Press10Stat <> 0) then
                ARect.Inflate(-Press10Stat, -trunc(ARect.Height/ ARect.Width * Press10Stat));

          // Draw if visible
          if (Y + CoverHeight > 0) and (Y < AHeight) and not OutOfBounds then
            DrawItemCanvas(TPaintBox(Sender).Canvas, ARect, @DrawItems[Index]);

          // Move Line
          Inc(X, CoverWidth + CoverSpacing + ExtraSpacing);

          if X + CoverWidth > AWidth then
            begin
              X := 0;
              Inc(Y, CoverHeight + CoverSpacing);
            end;
                   
          // OOB
          if Y > AHeight then
            OutOfBounds := true;
        end;

        // Scroll
        ScrollMax:=Y+ScrollPosition;
      end;
  end;

  // Nothing here
  if (X = 0) and (Y = 0) and (ScrollPosition = 0) then
    begin
      ARect := TPaintBox(Sender).ClientRect;

      Style.Alignment:=taCenter;
      Style.Layout:=tlCenter;
      Style.Wordbreak := true;

      with TPaintBox(Sender).Canvas do begin
        Font.Assign(Self.Font);
        TextRect(ARect, 0,0, EmptyString, Style);
      end;
    end;
end;

procedure TMain.DoCustomInteractUp(Button: TMouseButton);
begin
  case CurrentPage of
    TPage.Queue: begin
      // Drag
      if IsDragging then
        begin
          if IndexHover <> -1 then
            QueueMove(DraggingIndex, IndexHover);

          // Done
          Exit;
        end;

      // Click
      if IndexDown = IndexHover then
        case Button of
          TMouseButton.mbLeft: begin
            QueueSetPos(IndexHover);
          end;

          TMouseButton.mbRight: begin
            PopupIndex := IndexHover;
            PopupItem := DrawItems[PopupIndex];

            Generic_Menu.Popup;
          end;
        end;
    end;

  end;
end;

procedure TMain.DoCustomInteractMove(X, Y: integer);
begin

end;

procedure TMain.LoadDrawItems;
function AddItemID(ID: string; Source: TDataSource): integer;
begin
  Result := Length(DrawItems);
  SetLength(DrawItems, Result+1);
  DrawItems[Result].LoadSourceID(ID, Source);
end;
function AddItem(Item: TDrawItem): integer; overload;
begin
  Result := Length(DrawItems);
  SetLength(DrawItems, Result+1);
  DrawItems[Result] := Item;
end;
function AddItem(Index: integer; Source: TDataSource): integer; overload;
begin
  Result := Length(DrawItems);
  SetLength(DrawItems, Result+1);
  DrawItems[Result].LoadSource(Index, Source);
end;
procedure AddTrackList(List: TStringArray; Source: TDataSource);
var
  I, Index: integer;
begin
  for I := 0 to High(List) do
    begin
      Index := GetData(List[I], Source);

      if Index <> -1 then
        AddItem(Index, Source);
    end;
end;

// Search
var SearchFlags: TSearchFlags;
    SearchTerm: string;
procedure SearchAdder(Item: TDrawItem);
var
  Found: boolean;
begin
  // Trashed
  if Item.Trashed then
    if (TSearchFlag.SearchTrashed in SearchFlags) then
      Item.HideReasons := Item.HideReasons - [THiddenReason.Trashed]
    else
      Exit;

  // Check
  Found := SearchCompareFound(SearchTerm, Item.Title, SearchFlags)
        or ((TSearchFlag.SearchInfo in SearchFlags) and SearchCompareFound(SearchTerm, Item.InfoLong, SearchFlags))
        or (Item.ItemID = SearchTerm);

  if not Found then
    Exit;

  // Add
  AddItem(Item);
end;

// Home
var
NewItems: TStringArray;
procedure AddHomeIDs(Start: integer; Source: TDataSource);
var
  I, Index: integer;
begin
  for I := 0 to HomeColumns-1 do
    begin
      Index := I + Start;

      if I < Length(NewItems) then
        DrawItems[Index].LoadSourceID(NewItems[I], Source);

      if DrawItems[Index].Invalid then
        DrawItems[Index].SetHidden;
    end;
end;

var
  I, Index, IndexTrack: integer;
  Item: TDrawItem;
  ArrayIDs: TStringArray;
begin
  // Length
  SetLength(DrawItems, 0);

  // Reset
  PageIndexingError := false;

  // By Page
  case CurrentPage of
    // Generic Pages
    TPage.Songs: for I := 0 to High(Tracks) do
        AddItem(I, TDataSource.Tracks);
    TPage.Albums: for I := 0 to High(Albums) do
        AddItem(I, TDataSource.Albums);
    TPage.Artists: for I := 0 to High(Artists) do
        AddItem(I, TDataSource.Artists);
    TPage.Playlists: for I := 0 to High(Playlists) do
        AddItem(I, TDataSource.Playlists);
    TPage.Genres: for I := 0 to High(Genres) do
        AddItem(I, TDataSource.Genres);

    // Special Pages
    TPage.Home: begin
      // Settings
      HomeColumns := DrawingBox.Width div (CoverWidth + CoverSpacing);

      SetLength(DrawItems, HomeColumns * HOME_TOTAL_ROWS);

      (* Recently played albums *)
      Index := GetPlaylistOfType('recently-played');

      NewItems := [];
      if Index <> -1 then
        begin
          ArrayIDs := Playlists[Index].TracksID;

          for I := 0 to High(ArrayIDs) do
            begin
              IndexTrack := GetTrack(ArrayIDs[I]);

              if (IndexTrack <> -1) and (NewItems.Find(Tracks[IndexTrack].AlbumID) = -1) and (Tracks[IndexTrack].AlbumID <> '') then
                NewItems.AddValue( Tracks[IndexTrack].AlbumID );

              if NewItems.Count >= HomeColumns then
                Break;
            end;
        end;
      AddHomeIDs(0, TDataSource.Albums);

      (* Recently played tracks *)
      // Use the same index as the last one
      SetLength(NewItems, 0);
      if Index <> -1 then
        begin
          ArrayIDs := Playlists[Index].TracksID;

          for I := 0 to High(ArrayIDs) do
            if GetTrack(ArrayIDs[I]) <> -1 then
              begin
                NewItems.AddValue(ArrayIDs[I]);

                if NewItems.Count >= HomeColumns then
                  Break;
              end;
        end;
      AddHomeIDs(HomeColumns*2, TDataSource.Tracks);

      (* Favorite Tracks *)
      Index := GetPlaylistOfType('thumbsup');

      SetLength(NewItems, 0);
      if Index <> -1 then
        begin
          ArrayIDs := Playlists[Index].TracksID;

          for I := 0 to High(ArrayIDs) do
            if GetTrack(ArrayIDs[I]) <> -1 then
              begin
                NewItems.AddValue(ArrayIDs[I]);

                if NewItems.Count >= HomeColumns then
                  Break;
              end;
        end;
      AddHomeIDs(HomeColumns, TDataSource.Tracks);

      (* From your playlists *)
      SetLength(NewItems, 0);

      for I := 0 to High(Playlists) do
        if Playlists[I].PlaylistType = '' then
          begin
            NewItems.AddValue(Playlists[I].ID);

            if NewItems.Count >= HomeColumns then
              Break;
          end;
      AddHomeIDs(HomeColumns*3, TDataSource.Playlists);

      (* Recently added *)
      Index := GetPlaylistOfType('recently-uploaded');

      SetLength(NewItems, 0);
      if Index <> -1 then
        begin
          ArrayIDs := Playlists[Index].TracksID;

          for I := 0 to High(ArrayIDs) do
            if GetTrack(ArrayIDs[I]) <> -1 then
              begin
                NewItems.AddValue(ArrayIDs[I]);

                if NewItems.Count >= HomeColumns then
                  Break;
              end;
        end;
      AddHomeIDs(HomeColumns*4, TDataSource.Tracks);
    end;

    TPage.Queue: begin
      for I := 0 to Queue.Count-1 do
        begin
          Index := GetTrack(Queue[I]);
          if Index <> -1 then
            AddItem(Index, TDataSource.Tracks);
        end;
    end;

    TPage.Trash: case DrawPageFilter of
      TDataSource.Tracks: for I := 0 to High(Tracks) do
        if Tracks[I].IsInTrash then
          AddItem(I, TDataSource.Tracks);
      TDataSource.Albums: for I := 0 to High(Albums) do
        if Albums[I].IsInTrash then
          AddItem(I, TDataSource.Albums);
      TDataSource.Artists: for I := 0 to High(Artists) do
        if Artists[I].IsInTrash then
          AddItem(I, TDataSource.Artists);
    end;

    TPage.Downloads: case DrawPageFilter of
      TDataSource.None: for I := 0 to DownloadManager.MasterList.Count-1 do
        AddItemID(DownloadManager.MasterList[I], TDataSource.Tracks);
      TDataSource.Tracks: for I := 0 to DownloadManager.Tracks.Count-1 do
        AddItemID(DownloadManager.Tracks[I], TDataSource.Tracks);
      TDataSource.Albums: for I := 0 to DownloadManager.Albums.Count-1 do
        AddItemID(DownloadManager.Albums[I], TDataSource.Albums);
      TDataSource.Artists: for I := 0 to DownloadManager.Artists.Count-1 do
        AddItemID(DownloadManager.Artists[I], TDataSource.Artists);
      TDataSource.Playlists: for I := 0 to DownloadManager.Playlists.Count-1 do
        AddItemID(DownloadManager.Playlists[I], TDataSource.Playlists);
    end;

    TPage.History: begin
      I := GetPlaylistOfType('recently-played');

      if I = -1 then
        PageIndexingError := true
      else
        AddTrackList(Playlists[I].TracksID, TDataSource.Tracks);
    end;

    // Search
    { I use this method of indexing search items because, while It is slower,
      It's faster when you have over 100K tracks alone, not including albums,
      etc. + also prevents in64 overflowing }
    TPage.Search: begin
      SearchFlags := [];
      if CheckBox1.Checked then
        SearchFlags := SearchFlags + [TSearchFlag.ExactMatch];
      if CheckBox2.Checked then
        SearchFlags := SearchFlags + [TSearchFlag.CaseSensitive];
      if CheckBox6.Checked then
        SearchFlags := SearchFlags + [TSearchFlag.SearchInfo];
      if CheckBox7.Checked then
        SearchFlags := SearchFlags + [TSearchFlag.SearchTrashed];

      // Begin
      SearchTerm := Search_Box.Text;
      if SearchTerm <> '' then
        begin
          if ToggleBox6.Checked then
            for I := 0 to High(Tracks) do
              begin
                Item.LoadSource(I, TDataSource.Tracks);

                SearchAdder(Item);
              end;
          if ToggleBox5.Checked then
            for I := 0 to High(Albums) do
              begin
                Item.LoadSource(I, TDataSource.Albums);

                SearchAdder(Item);
              end;
          if ToggleBox4.Checked then
            for I := 0 to High(Artists) do
              begin
                Item.LoadSource(I, TDataSource.Artists);

                SearchAdder(Item);
              end;
          if ToggleBox7.Checked then
            for I := 0 to High(Playlists) do
              begin
                Item.LoadSource(I, TDataSource.Playlists);

                SearchAdder(Item);
              end;
        end;

    end;

    // Sub-views
    TPage.ViewAlbum, TPage.ViewArtist, TPage.ViewPlaylist, TPage.ViewGenre: begin
      // Index
      case CurrentPage of
        TPage.ViewAlbum: I := GetAlbum(PageData);
        TPage.ViewArtist: I := GetArtist(PageData);
        TPage.ViewPlaylist: I := GetPlaylist(PageData);
        TPage.ViewGenre: I := GetGenre(PageData);
      end;

      // Data
      if I = -1 then
        PageIndexingError := true
      else
        begin
          // Tracks / Albums
          case CurrentPage of
            TPage.ViewAlbum: begin
              Item.LoadSource(I, TDataSource.Albums);
              AddTrackList(Albums[I].TracksID, TDataSource.Tracks);
            end;
            TPage.ViewArtist: begin
              Item.LoadSource(I, TDataSource.Artists);
              if DrawPageFilter = TDataSource.Albums then
                begin
                  for I := 0 to High(Albums) do
                    if Albums[I].ArtistID = Item.ItemID then
                      AddItem(I, TDataSource.Albums);
                end
              else
                AddTrackList(Artists[I].TracksID, TDataSource.Tracks);
            end;
            TPage.ViewPlaylist: begin
              Item.LoadSource(I, TDataSource.Playlists);
              AddTrackList(Playlists[I].TracksID, TDataSource.Tracks);
            end;
            TPage.ViewGenre: begin
              Item.LoadSource(I, TDataSource.Genres);
              AddTrackList(Genres[I].TracksID, TDataSource.Tracks);
            end;
          end;

          // Information
          Sidebar_Name.Caption:=Item.Title;
          Sidebar_Vertical.Caption:=Item.Title;
          Sidebar_Description.Caption:=Item.GetPremadeInfoList;

          Sidebar_Picture.Picture.Graphic := DefaultPicture;
          with TViewLoadArtworkThread.Create do
            begin
              DownloadIndex := Item.Index;
              DownloadSource := Item.Source;

              Start;
            end;
        end;
    end;

  end;
end;

procedure TMain.SetViewStyle(Style: TViewStyle);
begin
  ViewStyle := Style;

  case ViewStyle of
    TViewStyle.Cover: Control_ViewStyle.Caption:=ICON_COVER;
    TViewStyle.List: Control_ViewStyle.Caption:=ICON_LIST;
  end;

  // Per page
  ViewStyles[CurrentPage] := Style;

  // Draw
  DrawingBox.Invalidate;
end;

procedure TMain.UpdateScroll;
var
  Column, Lines: integer;
  MaxS: integer;
begin
  if CustomDraw then
    Exit;

  if ViewStyle = TViewStyle.Cover then
    begin
      Column := DrawingBox.Width div (CoverWidth + CoverSpacing);
      Lines := Length(SortingList) div Column;

      MaxS := Lines * (CoverHeight + CoverSpacing);
    end
  else
    begin
      Lines := Length(SortingList);

      MaxS := Lines * (ListHeight + ListSpacing);
    end;

  // Update
  if ScrollMax <> MaxS then
    ScrollMax := MaxS;
end;

procedure TMain.DrawItemCanvas(Canvas: TCanvas; ARect: TRect;
  Item: TDrawItemPointer);
procedure DrawDownloadIcon(ARect: TRect);
var
  TxtStyle: TTextStyle;
begin
  with Canvas do
    begin
        Font.Assign(Self.Font);
        if Item.IsDownloaded = TDownloadedKind.Direct then
          Brush.Color := RGBToColor(255, 69, 0)
        else
          Brush.Color := RGBToColor(30, 144, 255);

      RoundRect(ARect, 10, 10);

      TxtStyle.Alignment:=taCenter;
      TxtStyle.Layout:=tlCenter;

      Brush.Style := bsClear;
      Font.Color := clWhite;

      TextRect(ARect, 0,0, ICON_DOWNLOAD, TxtStyle);
    end;
end;

var
  TempRect: TRect;
  Dist: integer;
  S: string;
  TxtStyle: TTextStyle;
var
  Picture: TJpegImage;
begin
  // Prepare Style
  TxtStyle.SystemFont:= false;
  TxtStyle.Wordbreak:=false;
  TxtStyle.Clipping:=true;

  // Common
  with Canvas do
    begin
      // Fill
      Pen.Style := psClear;
      Brush.Style := bsSolid;

      if Item.Active then
        Brush.Color := ItemActiveColor
      else
        Brush.Color := ItemColor;

      if ViewStyle = TViewStyle.Cover then
        RoundRect( ARect, CoverRadius, CoverRadius )
      else
        RoundRect( ARect, ListRadius, ListRadius );

      // Fix Image Bug
      Picture := Item.GetPicture;
      if Picture <> nil then
        try
          Picture.Empty;
        except
          Picture := nil
        end;
    end;

  with Canvas do
    if ViewStyle = TViewStyle.Cover then
      begin
        // Text
        Brush.Style := bsClear;
        Font.Assign( Self.Font );
        Font.Size:= 16;

        TempRect := ARect;
        Inc(TempRect.Top, ARect.Width);

        TempRect.Height := trunc(60/100 * TempRect.Height);

        S := TrimmifyText(Canvas, Item.Title, TempRect.Width);
        TxtStyle.Alignment:=taCenter;
        TxtStyle.Layout:=tlBottom;
        TextRect(TempRect, 0,0, S, TxtStyle);

        // Subtext
        Brush.Style := bsClear;
        Font.Assign( Self.Font );

        TempRect := ARect;
        Inc(TempRect.Top, ARect.Width + CoverSpacing);

        Inc(TempRect.Top, trunc(60/100 * TempRect.Height));

        S := TrimmifyText(Canvas, Item.InfoShort, TempRect.Width);
        TxtStyle.Alignment:=taCenter;
        TxtStyle.Layout:=tlTop;
        TextRect(TempRect, 0, TempRect.Top, S, TxtStyle);

        // Image
        TempRect := ARect;
        TempRect.Height := ARect.Width;

        if (Picture <> nil) and (not Picture.Empty)  then
          StretchDraw( TempRect, Picture )
        else
          begin
            Brush.Style := bsSolid;
            Brush.Color := ChangeColorSat(ItemColor, 20);

            RoundRect( TempRect, CoverRadius, CoverRadius );
          end;

        // Downloaded
        if Item.IsDownloaded <> TDownloadedKind.None then
          begin
            Font.Assign(Self.Font);
            Font.Size := 26;
            Dist := TextWidth(ICON_FILL);
            TempRect := Rect( ARect.Right - Dist, ARect.Top, ARect.Right, ARect.Top + Dist);
            TempRect.Offset(-5, 5);

            DrawDownloadIcon(TempRect);
          end;
      end
    else
      begin
        // Text
        Brush.Style := bsClear;
        Font.Assign( Self.Font );
        Font.Size:= 11;

        TempRect := ARect;
        Inc(TempRect.Left, ARect.Height + ListSpacing);
        Font.Size := Font.Size + 10;

        TempRect.Height := trunc(40/100 * TempRect.Height);

        S := TrimmifyText(Canvas, Item.Title, TempRect.Width);
        TxtStyle.Alignment:=taLeftJustify;
        TxtStyle.Layout:=tlBottom;
        TextRect(TempRect, TempRect.Left, TempRect.Top, S, TxtStyle);

        // Subtext
        Brush.Style := bsClear;
        Font.Assign( Self.Font );
        Font.Size:= 9;

        TempRect := ARect;
        Inc(TempRect.Left, ARect.Height + ListSpacing);
        Font.Size := Font.Size + 4;

        Inc(TempRect.Top, trunc(40/100 * TempRect.Height));
        TempRect.Bottom := ARect.Bottom;

        //Info := TrimmifyText(Canvas, Info, TempRect.Width);;
        TxtStyle.Alignment:=taLeftJustify;
        TxtStyle.Layout:=tlTop;
        TxtStyle.Wordbreak:=true;
        TextRect(TempRect, TempRect.Left, TempRect.Top, Item.InfoLong, TxtStyle);

        // Image
        TempRect := ARect;
        TempRect.Width := ARect.Height;

        if (Picture <> nil) and (not Picture.Empty) then
          StretchDraw( TempRect, Picture )
        else
          begin
            Brush.Style := bsSolid;
            Brush.Color := ChangeColorSat(ItemColor, 20);

            RoundRect( TempRect, CoverRadius, CoverRadius );
          end;

        // Downloaded
        if Item.IsDownloaded <> TDownloadedKind.None then
          begin
            Font.Assign(Self.Font);
            Font.Size := 26;
            Dist := TextWidth(ICON_FILL);
            TempRect := Rect( ARect.Right - Dist, ARect.Top, ARect.Right, ARect.Top + Dist);
            TempRect.Offset(-5, 5);

            DrawDownloadIcon(TempRect);
          end;
      end;
end;

procedure TMain.RenderVisualisations;
var
  FFTFata: TFFTData;
begin
  if (Player = nil) or (not Player.IsFileOpen) or (Player.PlayStatus <> TPlayStatus.psPlaying) then
    Exit;

  BASS_ChannelGetData(Player.Stream, @FFTFata, BASS_DATA_FFT1024);

  if Visible and Visualisation_Player.Visible then
    Spectrum_Player.Draw(Visualisation_Player.Canvas, FFTFata, 0, -10);

  if (PopupPlay <> nil) and PopupPlay.Visible and PopupPlay.Visualisation_Player.Visible then
    Spectrum_Popup.Draw(PopupPlay.Visualisation_Player.Canvas, FFTFata, 0, -10);
end;

procedure TMain.StopThreads;
begin
  // Check for running threads
  if SimultanousTaskThreads <= 0 then
    Exit;

  // Create
  TDialogStopThreads.Create.Start;

  // Show
  TaskExec.ShowModal;
end;

procedure TMain.DoScaling;
var
  Factor: real;
begin
  Factor := GetMonitorScaling;
  ScaleFactor := Factor;

  // Apply
  CoverSpacing := Round(CoverSpacing * Factor);
  CoverWidth := Round(CoverWidth * Factor);
  CoverHeight := Round(CoverHeight * Factor);
  CoverRadius := Round(CoverRadius * Factor);

  ListSpacing := Round(ListSpacing * Factor);
  ListHeight := Round(ListHeight * Factor);
  ListRadius := Round(ListRadius * Factor);

  // UI
  Panel8.Height:=round(BitBtn3.Height * Factor);
end;

function TMain.GetMonitorScaling: real;
begin
  Result := Monitor.PixelsPerInch / 96;
end;

procedure TMain.UpdateSpecialStat;
begin
  if SPECIAL_STATUS = '' then
    Stat_Viewer.Caption:='Powered by iBroadcastAPI'
  else
    Stat_Viewer.Caption:=SPECIAL_STATUS;
end;

procedure TMain.ResetSpecialStat;
begin
  SetSpecialStat('');
  UpdateSpecialStat;
end;

procedure TMain.SetSpecialStat(Value: string);
begin
  SPECIAL_STATUS := Value;
end;

procedure TMain.SelectPage(APage: TPage; AData: string);
procedure SetPanelM(Panel: TPanel);
var
  I: integer;
begin
  for I := 0 to Panel.ControlCount-1 do
    if Panel.Controls[I] is TBitBtn then
      with (Panel.Controls[I] as TBitBtn) do
        begin
          if not (Byte(Tag) in [1]) then
            Margin := 0;
        end;
end;

procedure ResetToolbars;
begin
  Toolbar_ViewControls.Parent := Page_ToolbarContainer;
  Toolbar_TrashControls.Parent := Page_ToolbarContainer;
  Subview_Sidebar.Parent := Page_ToolbarContainer;

  Toolbar_Search.Parent := Page_ToolbarContainer;
  Toolbar_Search1.Parent := Page_ToolbarContainer;
  Toolbar_Search2.Parent := Page_ToolbarContainer;

  Toolbar_Queue.Parent := Page_ToolbarContainer;

  Toolbar_Downloads.Parent := Page_ToolbarContainer;

  Toolbar_ViewArtist.Parent := Page_ToolbarContainer;

  Toolbar_PlaylistControls.Parent := Page_ToolbarContainer;
end;

var
  B: TBitBtn;
  I: integer;
begin
  // Same
  if (APage = CurrentPage) and (AData = PageData) then
    Exit;

  // Reset Scroll
  CurrentPage := TPage.None;
  ScrollPosition := 0;

  // Set
  CurrentPage := APage;
  PageData := AData;

  // History
  AddToHistory;

  // UI
  SetPanelM(Panel8);
  SetPanelM(Panel9);
  B := Nil;
  case APage of
    TPage.Home: B := BitBtn1;
    TPage.Albums: B := BitBtn3;
    TPage.Songs: B := BitBtn5;
    TPage.Artists: B := BitBtn6;
    TPage.Playlists: B := BitBtn4;
    TPage.Genres: B := BitBtn8;
    TPage.Queue: B := BitBtn7;
  end;
  if B <> nil then
    if not Visible then
      B.Margin := 15
    else
      for I := 1 to 15 do
        begin
          Sleep(2);
          B.Margin := I;
          Application.ProcessMessages;
        end;

  // View style
  SetViewStyle( ViewStyles[CurrentPage] );

  // Close search box
  Filter_Box.Hide;
  Filter_Box.Width:=0;

  // Filter (special pages only)
  SetDrawPageFilter( DrawPageFilters[CurrentPage] );

  // Name
  Page_Title.Caption:= PageToName(CurrentPage);

  // UI
  for I := 0 to Page_Container.ControlCount-1 do
    if Page_Container.Controls[I] is TPanel then
      (Page_Container.Controls[I] as TPanel).Hide;

  // Calculate trash inheritance
  TrashInheritance := CurrentPage in [TPage.Trash, TPage.Queue]; // Always on pages
  if not TrashInheritance then
    for I := High(PageHistory) downto 0 do
      begin
        if PageHistory[I].Page = TPage.Trash then
          begin
            TrashInheritance := true;
            Break;
          end;

        if not (PageHistory[I].Page in [TPage.ViewAlbum, TPage.ViewArtist, TPage.ViewPlaylist, TPage.ViewGenre]) then
          Break;
      end;

  // Page type
  DrawPage := CurrentPage in GENERAL_DRAW_PAGES;
  if DrawPage then
    // Draw Mode
    begin
      Page_Draw.Show;

      CustomDraw := CurrentPage in CUSTOM_DRAW;
      CustomInteract := CurrentPage in CUSTOM_INTERACT;
      SupportsDragging := CurrentPage in DRAG_ABLE_PAGES;

      // Items
      LoadDrawItems;

      // Sort
      Sort;
    end
  else
    // Page Mode
    case CurrentPage of
      TPage.Settings: Page_Settings.Show;
      TPage.Account: Page_Account.Show;
    end;

  // Toolbars
  if DrawPage then
    begin
      ResetToolbars;

      // View controls
      if not CustomDraw then
        Toolbar_ViewControls.Parent := Drawing_Container;

      // Page specific
      case CurrentPage of
        TPage.Trash: Toolbar_TrashControls.Parent := Drawing_Container;
        TPage.Search: begin
          Toolbar_Search.Parent := Page_Draw;
          Toolbar_Search1.Parent := Page_Draw;
          Toolbar_Search2.Parent := Page_Draw;
        end;

        TPage.Downloads: Toolbar_Downloads.Parent := Drawing_Container;

        TPage.Queue: Toolbar_Queue.Parent := Page_Draw;

        TPage.Playlists: Toolbar_PlaylistControls.Parent := Drawing_Container;

        TPage.ViewAlbum, TPage.ViewArtist, TPage.ViewPlaylist, TPage.ViewGenre: begin
          Subview_Sidebar.Parent := Page_Draw;

          if CurrentPage = TPage.ViewArtist then begin
            Toolbar_ViewArtist.Parent := Drawing_Container;

            ToggleBox13.Checked:=true;
            ToggleBox14.Checked:=false;
            SetDrawPageFilter(TDataSource.Tracks);
          end;
        end;
      end;
    end;

  // Custom page executions
  case CurrentPage of
    TPage.Settings: begin
      Option_threads.Position := MAX_THREAD_COUNT;
      Option_threads.Hint := MAX_THREAD_COUNT.ToString;
      Option_datasaver.Checked:=DefaultArtSize = TArtSize.Small;

      Artwork_Storage.Caption:= GetFolderSizeInStr(MediaStoreLocation);
    end;

    TPage.Account: begin
      Label21.Caption := MaskEmailAdress(Account.EmailAdress);
      Label21.Hint := Account.EmailAdress;
      Label24.Caption := DateTimeToString(Account.CreationDate);

      with Label28 do
        if Account.Verified then begin
          Color := clLime;
          Caption := 'Verified';
          Label26.Caption := DateTimeToString(Account.VerificationDate);
        end else begin
          Color := clRed;
          Caption := 'Unverified';
          Label26.Caption:='Not verified';
        end;

      Label31.Caption := LibraryStatus.TotalTracks.ToString;
      Label33.Caption := Length(Albums).ToString;
      Label35.Caption := Length(Artists).ToString;
      Label37.Caption := Length(Playlists).ToString;

      Label39.Caption := LibraryStatus.TotalPlays.ToString;

      Label41.Caption := Account.BitRate;
      Label43.Caption := BooleanToYesNo(Account.OneQueue);
      Label45.Caption := BooleanToYesNo(Account.BetaTester);

      if Account.Premium then
        Label47.Caption := 'You are subscribed to iBroadcast premium!'
      else
        Label47.Caption := 'You are not subscribed to iBroadcast premium';

      Account_Devices.Clear;
      for I := 0 to High(Sessions) do
        Account_Devices.Items.Add(Sessions[I].DeviceName);
    end;

    TPage.Downloads: begin
      Download_StoreUse.Caption:=Format('%S of storage used', [GetFolderSizeInStr(DownloadsFolder)]);
    end;
  end;
end;

function TMain.Playing: boolean;
begin
  Result := Player.PlayStatus in [TPlayStatus.psPlaying, TPlayStatus.psStalled];
end;

procedure TMain.PlaySong(ID: string; StartPlay: boolean);
var
  Index: integer;
  O: string;
begin
  Index := GetTrack(ID);
  if Index = -1 then
    Exit;

  // Cloud downloader
  StopCloudDownloader;

  // Status
  PlayID := ID;

  // Push History
  if StartPlay and not IsOffline then
    AddSongToHistory;

  // Close
  if Player.IsFileOpen then
    Player.CloseFile;

  // Audio timers
  AudioUpdate.Enabled:=false;

  // UI
  SetPlayIcon(psStalled);
  Application.ProcessMessages;

  // Cloud
  PlayType := TPlayType.Streaming;
  if DownloadManager.IsLocal(PlayID, O) then
    PlayType := TPlayType.Local
  else
    if not Option_streaming.Checked then
      PlayType := TPlayType.CloudDownload;

  // Play
  case PlayType of
    TPlayType.Streaming: Player.OpenURL( TTrackItem(Tracks[Index]).GetPlaybackURL );

    TPlayType.Local: Player.OpenFile(O);

    TPlayType.CloudDownload: CloudDownloadPlay(Index, StartPlay);
  end;

  // Settings
  PlayerApplySettings;
  if StartPlay and Player.IsFileOpen then
    Player.Play;

  // Info
  LoadSongInfo;

  // Player
  PlayerUpdate;

  // Audio timers
  AudioUpdate.Enabled:=true;
end;

procedure TMain.AddSongToHistory;
begin
  with TPushHistoryThread.Create do
    begin
      SetLength(Items, 1);

      Items[0].TrackID := PlayID;
      Items[0].TimeStamp := Now;

      Start;
    end;
end;

procedure TMain.SongFinished;
begin
  // Repeat
  if RepeatMode = TRepeat.One then
    QueuePlayCurrent
  else
  // Check for next
  if QueueIndex < Queue.Count-1 then
    QueueNext
  else
  // Queue repeat
    if RepeatMode = TRepeat.All then
      QueueSetPos(0);
end;

procedure TMain.CloudDownloadPlay(TrackIndex: integer; StartPlay: boolean);
begin
  // Stop and free (nicely)
  StopCloudDownloader;

  // Start new thread
  CloudDownloader := TCloudDownloadSongThread.Create;
  with CloudDownloader do
    begin
      DownloadIndex := TrackIndex;
      AutoStartPlayback:= StartPlay;

      Start;
    end;
end;

procedure TMain.StopCloudDownloader;
begin
  if (CloudDownloader <> nil) then
    begin
      if CloudDownloader.Status in [TThreadStatus.Running, TThreadStatus.Queued] then
        begin
          // New destionation

          CloudDownloader.Terminate;
          CloudDownloader.WaitFor;
        end;

      // Free
      FreeAndNil( CloudDownloader );
    end;
end;

procedure TMain.SetPlayIcon(AStatus: TPlayStatus);
begin
  case AStatus of
    TPlayStatus.psPaused,
    TPlayStatus.psStopped: Music_Play.Caption:=ICON_PLAY;
    TPlayStatus.psPlaying: Music_Play.Caption:=ICON_PAUSED;
    TPlayStatus.psStalled: Music_Play.Caption:=ICON_BUFFERING;
  end;

  UpdatePopupPlayIcons;
end;

procedure TMain.LoadSongInfo;
var
  Index: integer;
  Value: integer;
begin
  Index := GetTrack(PlayID);
  if Index = -1 then
    Exit;

  // UI
  Music_Name.Caption:=Tracks[Index].Title;
  Value := GetArtist(TTrackItem(Tracks[Index]).ArtistID);
  if Value <> -1 then
    Music_Artist.Caption := Artists[Value].ArtistName
  else
    Music_Artist.Caption := 'Unknown';

  Main.Music_Artwork.Picture.Graphic := DefaultPicture;
  with TPlayerLoadArtworkThread.Create do
    begin
      DownloadSource := TDataSource.Tracks;
      DownloadIndex := Index;

      Start;
    end;

  Music_Time.Caption:='0:00/0:00';

  // Popup
  UpdatePopupPlayMusic;
end;

procedure TMain.ApplyEqualisation;
begin

end;

procedure TMain.OpenPopupPlayer;
begin
  if PopupPlay <> nil then
    Exit;

  // Show
  PopupPlay := TPopupPlay.Create(Self);

  // Settings
  if Option_playerstaytop.Checked then
    PopupPlay.FormStyle:=fsStayOnTop
  else
    PopupPlay.FormStyle:=fsNormal;
  PopupPlay.AlphaBlendValue:=Option_playeropacity.Position;
  PopupPlay.Visualisation_Player.Visible:=Option_playervisualisation.Checked;
  PopupPlay.OnShortcut:=OnShortcut;

  // Show
  PopupPlay.Show;

  // Hide Self
  Hide;

  // Data and Icons
  UpdatePopupPlayIcons;
  UpdatePopupPlayMusic;
end;

procedure TMain.UpdatePopupPlayIcons;
begin
  if PopupPlay = nil then
    Exit;

  with PopupPlay do begin
    Music_Play.Caption := Self.Music_Play.Caption;

    Music_Play.Caption := Self.Music_Play.Caption;
    Music_Prev.Enabled := Self.Music_Prev.Enabled;
    Music_Next.Enabled := Self.Music_Next.Enabled;

    Music_Artwork.Picture := Self.Music_Artwork.Picture;
  end;
end;

procedure TMain.UpdatePopupPlayMusic;
begin
  if PopupPlay = nil then
    Exit;

  with PopupPlay do begin
    Music_Name.Caption := Self.Music_Name.Caption;
    Music_Artist.Caption := Self.Music_Artist.Caption;

    Music_Artwork.Picture := Self.Music_Artwork.Picture;
  end;
end;

procedure TMain.UpdatePopupTime;
begin
  if PopupPlay = nil then
    Exit;

  with PopupPlay do begin
    Music_Time.Caption := Self.Music_Time.Caption;
  end;
end;

procedure TMain.AddToHistory;
var
  R, I, By: integer;
begin
  // Add
  R := Length(PageHistory);
  SetLength(PageHistory, R+1);
  with PageHistory[R] do
    begin
      Page := CurrentPage;
      ScrollValue := ScrollPosition; { Will be 0 (reason: initiation) }
    end;

  // Size
  R := Length(PageHistory);
  if R > MAX_HISTORY then
    begin
      By := R - MAX_HISTORY;

      for I := 0 to R-1-By do
        PageHistory[I] := PageHistory[I+By];

      // Reset
      SetLength(PageHistory, MAX_HISTORY);
    end;
end;

procedure TMain.SetDrawPageFilter(ATo: TDataSource);
begin
  DrawPageFilter := ATo;
  DrawPageFilters[CurrentPage] := ATo;
end;

procedure TMain.AppSettings(Load: boolean);
const
  SECT_APP = 'Application';
  SECT_GENERAL = 'General';
  SECT_VIEWMODE = 'View modes';
  SECT_ARTWORK = 'Artwork';
  SECT_PLAY = 'Playback';
  SECT_THREAD = 'Threading';
  SECT_POPUP = 'Popup Player';

  ALLOW_PAGE_SET = [TPage.Songs, TPage.Albums, TPage.Artists, TPage.Playlists,
    TPage.ViewAlbum, TPage.ViewArtist, TPage.ViewPlaylist, TPage.ViewGenre, TPage.Genres, TPage.History,
    TPage.Trash, TPage.Downloads];
var
  Ini: TIniFile;
  P: TPage;
begin
  Ini := TIniFile.Create(AppData + 'config.ini');
  with Ini do
    try
      if Load then
        // LOAD
        begin
          Option_checkupdates.Checked := ReadBool(SECT_APP, 'Updates checking', Option_checkupdates.Checked);
          LastUpdateCheck := ReadDateTime(SECT_APP, 'Last update check', LastUpdateCheck);
          if LastUpdateCheck > Now then
            LastUpdateCheck := Now;
          Visualisation_Player.Visible := ReadBool(SECT_GENERAL, 'Visualisations', Visualisation_Player.Visible);
            Equaliser_Indic.Visible:=Visualisation_Player.Visible;
          ValueRatingMode := ReadBool(SECT_GENERAL, 'Value rating', ValueRatingMode);
            Option_Rating.Checked:=ValueRatingMode;
          Option_savequeue.Checked := ReadBool(SECT_GENERAL, 'Save queue', Option_savequeue.Checked);
          ArtworkStore := ReadBool(SECT_ARTWORK, 'Artwork store', ArtworkStore);
            Option_artwork.Checked:=ArtworkStore;
          DefaultArtSize := TArtSize(ReadInteger(SECT_ARTWORK, 'Art quality', integer(DefaultArtSize)));
          Option_streaming.Checked := ReadBool(SECT_PLAY, 'Audio streaming', Option_streaming.Checked);
          RepeatMode := TRepeat(ReadInteger(SECT_PLAY, 'Repeat', integer(RepeatMode)));
          PlayerSpeed := ReadFloat(SECT_PLAY, 'Audio speed', PlayerSpeed);
            Music_Speed.Position:=round(PlayerSpeed*100);
          PlayerVolume := ReadFloat(SECT_PLAY, 'Audio volume', PlayerVolume);
            Music_Volume.Position:=round(PlayerVolume*100);
          MAX_THREAD_COUNT := ReadInteger(SECT_THREAD, 'Max threads', MAX_THREAD_COUNT);
          Option_playervisualisation.Checked := ReadBool(SECT_POPUP, 'Visualisations', Option_playervisualisation.Checked);
          Option_playerstaytop.Checked := ReadBool(SECT_POPUP, 'Stay on top', Option_playerstaytop.Checked);
          Option_playeropacity.Position := ReadInteger(SECT_POPUP, 'Opacity', Option_playeropacity.Position);

          // View modes
          for P := Low(TPage) to High(TPage) do
            if P in ALLOW_PAGE_SET then
                ViewStyles[P] := TViewStyle(ReadInteger(SECT_VIEWMODE, integer(P).ToString, integer(ViewStyles[P])));
        end
      else
        // SAVE
        begin
          WriteBool(SECT_APP, 'Updates checking', Option_checkupdates.Checked);
          WriteDateTime(SECT_APP, 'Last update check', LastUpdateCheck);
          WriteBool(SECT_GENERAL, 'Visualisations', Visualisation_Player.Visible);
          WriteBool(SECT_GENERAL, 'Value rating', ValueRatingMode);
          WriteBool(SECT_GENERAL, 'Save queue', Option_savequeue.Checked);
          WriteBool(SECT_ARTWORK, 'Artwork store', ArtworkStore);
          WriteInteger(SECT_ARTWORK, 'Art quality', integer(DefaultArtSize));
          WriteBool(SECT_PLAY, 'Audio streaming', Option_streaming.Checked);
          WriteInteger(SECT_PLAY, 'Repeat', integer(RepeatMode));
          WriteFloat(SECT_PLAY, 'Audio speed', PlayerSpeed);
          WriteFloat(SECT_PLAY, 'Audio volume', PlayerVolume);
          WriteInteger(SECT_THREAD, 'Max threads', MAX_THREAD_COUNT);
          WriteBool(SECT_POPUP, 'Visualisations', Option_playervisualisation.Checked);
          WriteBool(SECT_POPUP, 'Stay on top', Option_playerstaytop.Checked);
          WriteInteger(SECT_POPUP, 'Opacity', Option_playeropacity.Position);

          // View modes
          for P := Low(TPage) to High(TPage) do
            if P in ALLOW_PAGE_SET then
                WriteInteger(SECT_VIEWMODE, integer(P).ToString, integer(ViewStyles[P]));
        end;

    finally
      Free;
    end;
end;

procedure TMain.BackendUpdate(AUpdate: TDataSource);
begin
  { This function will be called by a thread, so Syncronise is required! }

  TThread.Synchronize(TThread.CurrentThread, Main.CurrentViewReload);
end;

procedure TMain.PlayerUpdate;
begin
  Music_Play.Enabled := Player.IsFileOpen;
  // Music_Prev.Enabled := Player.IsFileOpen; // Check Queue
  // Music_Next.Enabled := Player.IsFileOpen; // Check Queue

  Music_Position.Max:=round(Player.DurationSeconds*10);

  if Player.IsFileOpen then
    PlayerTotalLength := CalculateLength(trunc(Player.DurationSeconds))
  else
    PlayerTotalLength := '00:00';
end;

procedure TMain.PlayerApplySettings;
begin
  Player.Volume:=PlayerVolume;
  Player.Speed:=PlayerSpeed;
end;

end.

