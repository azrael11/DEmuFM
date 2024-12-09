unit main;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.MultiView,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView,
  FMX.Graphics,
  FireDAC.FMXUI.Wait,
  FMX.Edit,
  FMX.ScrollBox,
  FMX.Memo,
  main_info,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.UI,
  FireDAC.Stan.Def,
  FMX.Effects,
  FireDAC.Stan.Intf,
  FireDAC.UI.Intf,
  FMX.Memo.Types,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.DApt,
  FireDAC.Stan.Async,
  FMX.ExtCtrls,
  FMX.Filter.Effects,
  FMX.Dialogs,
  System.Types,
  FMX.ListBox,
  Data.Bind.EngExt,
  FMX.Bind.DBEngExt,
  System.Rtti,
  System.Bindings.Outputs,
  FMX.Bind.Editors,
  Data.Bind.Components,
  FMX.SearchBox,
  // Alcinoe.FMX.VideoPlayer,
  // ksChatList,
  vars_consts,
  System.IOUtils, System.Skia, FMX.Skia, FMX.Ani, FMX.ComboEdit,
  Data.Bind.DBScope, FMX.Grid.Style, FMX.Bind.Grid, Data.Bind.Grid, FMX.Grid,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, System.Bindings.Helper;

type
  Tfrm_main = class(TForm)
    tmr_machine: TTimer;
    rect_actions: TRectangle;
    stb_main: TStatusBar;
    lay_main_grid: TLayout;
    lv_main_list: TListView;
    edt_search: TEdit;
    lbl_search: TLabel;
    rect_search: TRectangle;
    spb_scraper_change: TSpeedButton;
    img_scraper_change: TImage;
    lay_main: TLayout;
    eff_blur_main: TBlurEffect;
    rect_grid: TRectangle;
    vsb_grid: TVertScrollBox;
    layscl_grid: TScaledLayout;
    spb_action_config: TSpeedButton;
    img_action_config: TImage;
    rect_config: TRectangle;
    spb_action_screen: TSpeedButton;
    img_action_screen: TImage;
    spb_action_controllers: TSpeedButton;
    img_action_controllers: TImage;
    spb_action_info: TSpeedButton;
    img_action_info: TImage;
    eff_glow_main: TInnerGlowEffect;
    spb_platform_change: TSpeedButton;
    rect_platform_info: TRectangle;
    img_platform_change: TImage;
    FDCursorWait: TFDGUIxWaitCursor;
    FDPSQLIteDriverLink: TFDPhysSQLiteDriverLink;
    od_main: TOpenDialog;
    stylebook_main: TStyleBook;
    rect_selected_info: TRectangle;
    lbl_selected_info: TLabel;
    lbl_selected_info_value: TLabel;
    txt_stb_main_total: TText;
    pnl_help: TPanel;
    lbl_tag: TLabel;
    lbl_result: TLabel;
    lbl_sum_tag: TLabel;
    lbl_height: TLabel;
    lbl_grid_height: TLabel;
    lay_emu_stats: TLayout;
    spb_emu_working_minor: TSpeedButton;
    spb_emu_working_major: TSpeedButton;
    spb_emu_not_working: TSpeedButton;
    img_emu_working_minor: TImage;
    img_emu_not_working: TImage;
    img_emu_woking_major: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    eff_mono_emu_working_major: TMonochromeEffect;
    eff_mono_emu_working_minor: TMonochromeEffect;
    eff_mono_emu_not_working: TMonochromeEffect;
    tmr_fps: TTimer;
    tmr_pause: TTimer;
    spb_emu_working: TSpeedButton;
    img_emu_working: TImage;
    eff_mono_emu_working: TMonochromeEffect;
    lbl_emu_stats: TLabel;
    Layout1: TLayout;
    sd_main: TSaveDialog;
    lay_game: TLayout;
    rect_game: TRectangle;
    effShadow_game: TShadowEffect;
    rectInfoHeader: TRectangle;
    rectInfo: TRectangle;
    lblInfoCoop: TLabel;
    lblInfoDeveloper: TLabel;
    lblInfoGenre: TLabel;
    lblInfoHiScore: TLabel;
    lblInfoPlayers: TLabel;
    lblInfoPublisher: TLabel;
    lblInfoRom: TLabel;
    lblInfoYear: TLabel;
    rectPlayGame: TRectangle;
    rectScreenshots: TRectangle;
    edtInfoGameName: TEdit;
    eff_blur_grid_info_list: TBlurEffect;
    rectInfoMain: TRectangle;
    dtBoxart: TDropTarget;
    imgGameInfoLogo: TImage;
    lblDescription: TLabel;
    memoDescription: TMemo;
    imgGameInfoBoxart: TImage;
    lblPlay: TLabel;
    ceInfoDeveloper: TComboEdit;
    edtInfoPlayers: TEdit;
    edtInfoCoop: TEdit;
    ceInfoPublisher: TComboEdit;
    edtInfoYear: TEdit;
    edtInfoRomName: TEdit;
    ceInfoGenre: TComboEdit;
    edtInfoHiScore: TEdit;
    rectInfoProgressIconPlayable: TRectangle;
    rectInfoProgressIconMinor: TRectangle;
    rectInfoProgressIconMajor: TRectangle;
    rectInfoProgressIconNonPlayable: TRectangle;
    memoProgress: TMemo;
    imgGameInfoFanart: TImage;
    imgGameInfoBanner: TImage;
    dtBanner: TDropTarget;
    imgGameInfoBoxartFront: TImage;
    imgGameInfoBoxartBack: TImage;
    vsbImg: TVertScrollBox;
    spbScrapeTGDB: TSpeedButton;
    img_grid_info_scrape_tgdb: TImage;
    Label7: TLabel;
    Label8: TLabel;
    spb_grid_info_export_xml: TSpeedButton;
    img_grid_info_export_xml: TImage;
    spb_grid_info_export_json: TSpeedButton;
    img_grid_info_export_json: TImage;
    spb_grid_info_export_html: TSpeedButton;
    img_grid_info_export_html: TImage;
    spb_grid_info_export_markup: TSpeedButton;
    img_grid_info_export_markup: TImage;
    spbInfoEdit: TSpeedButton;
    img_grid_info_edit: TImage;
    eff_fillRGB_grid_info_edit: TFillRGBEffect;
    geInfoProgressIconPlayable: TGlowEffect;
    geInfoProgressIconMinor: TGlowEffect;
    geInfoProgressIconMajor: TGlowEffect;
    geInfoProgressIconNonPlayable: TGlowEffect;
    layTotalGames: TLayout;
    lblTotalGames: TLabel;
    lblTotalGamesValue: TLabel;
    skaiEmuWorking: TSkAnimatedImage;
    ShadowEffect1: TShadowEffect;
    skaiEmuWorkingMinor: TSkAnimatedImage;
    ShadowEffect2: TShadowEffect;
    skaiEmuWorkingMajor: TSkAnimatedImage;
    ShadowEffect3: TShadowEffect;
    skaiEmuNotWorking: TSkAnimatedImage;
    ShadowEffect4: TShadowEffect;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    dtLogo: TDropTarget;
    dtFanart: TDropTarget;
    dtBoxartFront: TDropTarget;
    dtBoxartBack: TDropTarget;
    spbInfoEditClear: TSpeedButton;
    img_grid_info_edit_clear: TImage;
    GlowEffect4: TGlowEffect;
    rectInfoSnapshotRemove: TRectangle;
    rectInfoSnapshotAdd: TRectangle;
    lblInfoSnapshotAdd: TLabel;
    lblInfoSnapshotRemove: TLabel;
    procedure dtBoxartDblClick(Sender: TObject);
    procedure dtBoxartDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure dtBoxartDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure edt_searchTyping(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmr_machineTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure spb_scraper_changeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lv_main_listItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure spb_action_configClick(Sender: TObject);
    procedure spb_action_controllersClick(Sender: TObject);
    procedure spb_action_exitClick(Sender: TObject);
    procedure spb_action_infoClick(Sender: TObject);
    procedure spb_action_playClick(Sender: TObject);
    procedure spb_action_stopClick(Sender: TObject);

    procedure spb_action_mouse_enter(Sender: TObject);
    procedure spb_action_mouse_leave(Sender: TObject);
    procedure spb_emu_working_majorClick(Sender: TObject);
    procedure spb_emu_not_workingClick(Sender: TObject);
    procedure spb_emu_working_minorClick(Sender: TObject);
    procedure spbInfoEditClick(Sender: TObject);
    procedure spbInfoEditClearClick(Sender: TObject);
    procedure spb_platform_changeClick(Sender: TObject);
    procedure tmr_fpsTimer(Sender: TObject);
    procedure tmr_pauseTimer(Sender: TObject);
    procedure vsb_gridViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure Rect_OnMouseEnter(Sender: TObject);
    procedure Rect_OnMouseLeave(Sender: TObject);
    procedure Rect_OnClick(Sender: TObject);
    procedure spb_emu_workingClick(Sender: TObject);
    procedure spb_grid_info_export_htmlClick(Sender: TObject);
    procedure spb_grid_info_export_markupClick(Sender: TObject);
    procedure rectPlayGameMouseEnter(Sender: TObject);
    procedure rectPlayGameMouseLeave(Sender: TObject);
    procedure rectPlayGameClick(Sender: TObject);
    procedure ceInfoDeveloperTyping(Sender: TObject);
    procedure ChangeLanguage(const LangCode: string);
    procedure edtInfoHiScoreChange(Sender: TObject);
    procedure imgGameInfoLogoClick(Sender: TObject);
  private
    { Private declarations }
    procedure run(Sender: TObject);
    // procedure LoadTranslations(const LangFile: string);
  public
    { Public declarations }
    // dsp_video: TALWinVideoPlayer;
    // dsp_video_sur: TALVideoPlayerSurface;
    imgNotFound: FMX.Graphics.TBitmap;

  end;

var
  frm_main: Tfrm_main;
  new_game_type: Word;

implementation

uses
  umain_actions,
  uscraper_tgdb,
  ulang,
  SDL2,
  SDL2_TTF,
  timer_engine,
  main_engine,
  file_engine,
  language,
  init_games,
  scraper_tgdb,
  uarcade_actions,
  umain_config,
  configuration,
  controls_engine,
  udata_controllers,
  emulators,
  config_controls,
  splash,
  front_main,
  files_export,
  uDataModule;

{$R *.fmx}

procedure Tfrm_main.ceInfoDeveloperTyping(Sender: TObject);
var
  vi, TextLength: Integer;
  searchText: string;
  vFilter: string;
begin
  searchText := ceInfoDeveloper.Text;
  vFilter := 'name like ' + QuotedStr(searchText + '%');
  // fdmtDevelopers.Filtered := false;
  // fdmtDevelopers.Filter := vFilter;
  // fdmtDevelopers.Filtered := true;

  if ceInfoDeveloper.DroppedDown = False then
    ceInfoDeveloper.DropDown;
  ceInfoDeveloper.Text := searchText;
  TextLength := length(searchText);
  ceInfoDeveloper.SelStart := TextLength;
end;

procedure Tfrm_main.ChangeLanguage(const LangCode: string);
var
  LangFile: string;
begin
  // LangFile := ExtractFilePath(Application.ExeName) + LangCode + '.json';
  // LoadTranslations(LangFile);
end;

procedure Tfrm_main.dtBoxartDblClick(Sender: TObject);
begin
  main_actions.infoImgDClick;
end;

procedure Tfrm_main.dtBoxartDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  main_actions.infoImgDragOver(Sender, Data, Point, Operation);
end;

procedure Tfrm_main.dtBoxartDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  main_actions.infoImgDropped(Sender, Data, Point);
end;

procedure Tfrm_main.edtInfoHiScoreChange(Sender: TObject);
begin

  if dm.tArcadehiscore.AsInteger = 0 then
    (Sender as TEdit).Text := 'No'
  else if dm.tArcadehiscore.AsInteger = 1 then
    (Sender as TEdit).Text := 'Yes'
end;

procedure Tfrm_main.edt_searchTyping(Sender: TObject);
begin
  main_actions.main_form_search(Sender);
end;

procedure Tfrm_main.run(Sender: TObject);
begin
  main_actions.main_form_run_game;
end;

procedure Tfrm_main.tmr_machineTimer(Sender: TObject);
begin
  main_actions.main_form_step_before_run_game_timer;
end;

procedure Tfrm_main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  main_actions.main_form_close;
end;

procedure Tfrm_main.FormCreate(Sender: TObject);
begin
  self.Visible := False;
  self.Hide;

  main_actions := TMAIN_ACTIONS.Create;
end;

procedure Tfrm_main.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  main_actions.key_down(Key, KeyChar, Shift);
end;

procedure Tfrm_main.FormShow(Sender: TObject);
begin
  main_actions.main_form_show;
  ChangeLanguage('el');
  front_Action.CreateInfoBindings;
end;

procedure Tfrm_main.imgGameInfoLogoClick(Sender: TObject);
begin

end;

// procedure Tfrm_main.LoadTranslations(const LangFile: string);
// var
// JSONValue: TJSONValue;
// JSONObject: TJSONObject;
// JSONString: TStringList;
// begin
// // Διαβάζουμε το JSON αρχείο και το φορτώνουμε ως string
// JSONString := TStringList.Create;
// try
// JSONString.LoadFromFile(LangFile);
//
// // Κάνουμε Parse το JSON string
// JSONValue := TJSONObject.ParseJSONValue(JSONString.Text);
// if JSONValue is TJSONObject then
// begin
// JSONObject := TJSONObject(JSONValue);
//
// // Φορτώνουμε τις μεταφράσεις
// lblInfoRom.Text := JSONObject.GetValue<string>('lblInfoRom', 'Rom Name');
// lblInfoYear.Text := JSONObject.GetValue<string>('lblInfoYear', 'Year');
// lblInfoPlayers.Text := JSONObject.GetValue<string>('lblInfoPlayers', 'Players');
// end;
// finally
// JSONString.Free;
// JSONValue.Free;
// end;
// end;

procedure Tfrm_main.lv_main_listItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  cur_pic: TListItemImage;
  cur_rom: TlistItemText;
  cur_name: TlistItemText;
begin
  cur_name := AItem.Objects.FindObjectT<TlistItemText>('num');
  cur_pic := AItem.Objects.FindObjectT<TListItemImage>('pic');
  cur_rom := AItem.Objects.FindObjectT<TlistItemText>('romname');
end;

procedure Tfrm_main.rectPlayGameClick(Sender: TObject);
begin
  front_Action.selectedGame(true, Sender as TRectangle);
  front_Action.prev_selected := (Sender as TRectangle).Tag;
  main_actions.main_form_play;
  lay_game.Visible := False;
  eff_blur_main.Enabled := False;
end;

procedure Tfrm_main.rectPlayGameMouseEnter(Sender: TObject);
begin
  (Sender as TRectangle).Fill.Color := $FF940101;
end;

procedure Tfrm_main.rectPlayGameMouseLeave(Sender: TObject);
begin
  (Sender as TRectangle).Fill.Color := $FF374B66;
end;

procedure Tfrm_main.Rect_OnClick(Sender: TObject);
begin
  main_actions.infoOnMouseClick(Sender);
end;

procedure Tfrm_main.Rect_OnMouseEnter(Sender: TObject);
begin
  main_actions.infoOnMouseEnter(Sender);
end;

procedure Tfrm_main.Rect_OnMouseLeave(Sender: TObject);
begin
  main_actions.infoOnMouseLeave(Sender);
end;

procedure Tfrm_main.spb_action_configClick(Sender: TObject);
begin
  frm_config.ShowModal;
end;

procedure Tfrm_main.spb_action_controllersClick(Sender: TObject);
begin
  frm_config_controls.ShowModal;
end;

procedure Tfrm_main.spb_action_exitClick(Sender: TObject);
begin
  close;
end;

procedure Tfrm_main.spb_action_infoClick(Sender: TObject);
begin
  frm_info.ShowModal;
end;

procedure Tfrm_main.spb_action_mouse_enter(Sender: TObject);
begin
  main_actions.main_form_sb_mouse_enter(Sender);
end;

procedure Tfrm_main.spb_action_mouse_leave(Sender: TObject);
begin
  main_actions.main_form_sb_mouse_leave(Sender);
end;

procedure Tfrm_main.spb_action_playClick(Sender: TObject);
begin
  main_actions.main_form_play;
end;

procedure Tfrm_main.spb_action_stopClick(Sender: TObject);
begin
  main_actions.main_form_stop;
end;

procedure Tfrm_main.spb_emu_workingClick(Sender: TObject);
begin
  main_actions.main_working_games;
end;

procedure Tfrm_main.spb_emu_working_majorClick(Sender: TObject);
begin
  main_actions.main_working_major_games;
end;

procedure Tfrm_main.spb_emu_not_workingClick(Sender: TObject);
begin
  main_actions.main_not_working_games;
end;

procedure Tfrm_main.spb_emu_working_minorClick(Sender: TObject);
begin
  main_actions.main_working_minor_games;
end;

procedure Tfrm_main.spbInfoEditClick(Sender: TObject);
begin
  main_actions.infoEdit;
end;

procedure Tfrm_main.spbInfoEditClearClick(Sender: TObject);
begin
  front_Action.clearAndRestoreOriginalData;
end;

procedure Tfrm_main.spb_grid_info_export_htmlClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(nil);
  try
    sd.InitialDir := dm.tConfigprj_export.AsString;
    sd.FileName := dm.tArcaderom.AsString + '.html';
    sd.Filter := 'Html Files|*.html';
    if sd.Execute then
      CreateAndSave_Html(sd.FileName, true, nil);
  finally
    sd.Free;
  end;
end;

procedure Tfrm_main.spb_grid_info_export_markupClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(nil);
  try
    sd.InitialDir := vPath;
    sd.FileName := dm.tArcaderom.AsString + '.txt';
    sd.Filter := 'txt Files|*.txt';
    if sd.Execute then
      CreateAndSave_Markup(sd.FileName, true);
  finally
    sd.Free;
  end;
end;

procedure Tfrm_main.spb_platform_changeClick(Sender: TObject);
begin
  emulators.frm_emu.ShowModal;
end;

procedure Tfrm_main.spb_scraper_changeClick(Sender: TObject);
begin
  main_actions.main_form_set_scraper(Sender);
end;

procedure Tfrm_main.tmr_fpsTimer(Sender: TObject);
begin
  // emu_in_game.fps_count := true;
end;

procedure Tfrm_main.tmr_pauseTimer(Sender: TObject);
begin
  // emu_in_game.pause := not emu_in_game.pause;
end;

procedure Tfrm_main.vsb_gridViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  front_Action.gridChange(Sender, OldViewportPosition, NewViewportPosition, ContentSizeChanged);
end;

end.
