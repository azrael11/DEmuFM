unit f_dspfm;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.StrUtils,
  WinApi.ShellAPI,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Filter.Effects,
  FMX.Effects,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.ListBox,
  FMX.Platform,
  FireDAC.Stan.Param,
  controls_engine,
  multi_platform,
  FMX.DialogService,
  WinApi.Windows, System.Skia, FMX.Skia;

type
  Tdspfm = class(TFrame)
    tc_dspfm: TTabControl;
    ti_dspfm_dirs: TTabItem;
    ti_dspfm_languages: TTabItem;
    ShadowEffect1: TShadowEffect;
    effMono_lang_greece: TMonochromeEffect;
    txt_dspfm_gen_qsnap: TText;
    edt_dspfm_gen_qsnap: TEdit;
    spb_dspfm_gen_qsnap: TSpeedButton;
    img_dspfm_gen_qsnap: TImage;
    od_config: TOpenDialog;
    ti_dspfm_general: TTabItem;
    eff_blur_change_button: TBlurEffect;
    eff_glow_change_button: TGlowEffect;
    tmr_config: TTimer;
    ti_dspfm_scrapers: TTabItem;
    rect_scraper_tgdb: TRectangle;
    rect_scraper_screenscaper: TRectangle;
    txt_scraper_tgdb_header: TText;
    sp_scraper_screenscraper_header: TText;
    rect_scraper_tgdb_check: TRectangle;
    rect_scraper_screenscraper_check: TRectangle;
    sp_scraper_tgdb_games: TSpeedButton;
    sp_scraper_tgdb_genres: TSpeedButton;
    sp_scraper_tgdb_publishers: TSpeedButton;
    sp_scraper_tgdb_developers: TSpeedButton;
    prb_scraper_tgdb: TProgressBar;
    txt_scraper_tgdb_progress: TText;
    sp_scraper_tgdb_update: TSpeedButton;
    sp_scraper_tgdb_new: TSpeedButton;
    sp_scraper_screenscraper_message: TText;
    rect_scraper_tgdb_games: TRectangle;
    rect_scraper_tgdb_genres: TRectangle;
    rect_scraper_tgdb_new: TRectangle;
    rect_scraper_tgdb_publishers: TRectangle;
    rect_scraper_tgdb_update: TRectangle;
    rect_scraper_tgdb_developers: TRectangle;
    spb_dspfm_lang_export: TSpeedButton;
    skai_scraper_tgdb_check: TSkAnimatedImage;
    skai_scraper_screenscaper_check: TSkAnimatedImage;
    skai_dspfm_lang_check: TSkAnimatedImage;
    skai_lang_ellinika: TSkAnimatedImage;
    txt_lang_ellinika: TText;
    rect_lang_ellinika: TRectangle;
    rect_lang_russian: TRectangle;
    effMono_lang_russian: TMonochromeEffect;
    skai_lang_russian: TSkAnimatedImage;
    txt_lang_russian: TText;
    rect_lang_francais: TRectangle;
    effMono_lang_francais: TMonochromeEffect;
    skai_lang_francais: TSkAnimatedImage;
    txt_lang_francais: TText;
    rect_lang_brazil: TRectangle;
    effMono_lang_brazil: TMonochromeEffect;
    skai_lang_brazil: TSkAnimatedImage;
    txt_lang_brazil: TText;
    rect_lang_spain: TRectangle;
    effMono_lang_spain: TMonochromeEffect;
    skai_lang_spain: TSkAnimatedImage;
    txt_lang_spain: TText;
    rect_lang_english: TRectangle;
    effMono_lang_english: TMonochromeEffect;
    skai_lang_english: TSkAnimatedImage;
    txt_lang_english: TText;
    rect_lang_german: TRectangle;
    effMono_lang_german: TMonochromeEffect;
    skai_lang_german: TSkAnimatedImage;
    txt_lang_german: TText;
    rect_lang_italian: TRectangle;
    effMono_lang_italian: TMonochromeEffect;
    skai_lang_italian: TSkAnimatedImage;
    txt_lang_italian: TText;
    procedure Rect_OnMouse_Enter(Sender: TObject);
    procedure Rect_OnMouse_Leave(Sender: TObject);
    procedure Rect_OnMouse_Click(Sender: TObject);
    procedure loadLanguage;
    procedure OnShow;
    // scraper
    procedure tmr_configTimer(Sender: TObject);
    procedure rect_scraper_tgdb_checkMouseEnter(Sender: TObject);
    procedure rect_scraper_tgdb_checkMouseLeave(Sender: TObject);
    procedure scraper_tgdb_up_click(Sender: TObject);
    procedure scraper_tgdb_run(Sender: TObject);
    procedure spb_dspfm_gen_qsnapClick(Sender: TObject);
    procedure mouse_enter_flag(Sender: TObject);
    procedure mouse_leave_flag(Sender: TObject);
    // hints
    procedure edt_show_hint(Sender: TObject);
    procedure edt_clear_hint(Sender: TObject);
    procedure rect_scraper_tgdb_checkClick(Sender: TObject);
    procedure rect_scraper_screenscraper_checkClick(Sender: TObject);
    procedure ti_dspfm_languagesClick(Sender: TObject);
    procedure ti_dspfm_scrapersClick(Sender: TObject);
    procedure skai_lang_italianAnimationFinish(Sender: TObject);
  private
    { Private declarations }
    // default
    first_show: boolean;

    // languages area
    // scrapers area
    tgdb_call_num: integer;
    tgdb_time: boolean;

    procedure langMonoEffectState;

  protected
    { Protected declarations }

  public
    { Public declarations }

  end;

implementation

{$R *.fmx}

uses
  configuration,
  umain_config,
  ulang,
  capsdefs,
  prj_functions,
  // scrapers
  uscraper_tgdb,
  scraper_tgdb,
  uTheGamesDatabase,
  //
  udata_controllers, uDataModule;

procedure Tdspfm.edt_clear_hint(Sender: TObject);
begin
  configuration.frm_config.clear_bottom_hint;
end;

procedure Tdspfm.edt_show_hint(Sender: TObject);
begin
  configuration.frm_config.show_bottom_hint((Sender as TEdit).Hint);
end;

procedure Tdspfm.scraper_tgdb_run(Sender: TObject);
var
  num: integer;
begin
  num := ((tgdb_call_num.ToString) + ((Sender as TSpeedButton).Tag).ToString).ToInteger;
  TDialogService.MessageDialog('This will take some time, do you like to proceed?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbCancel, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrOk then
      begin
        case num of
          10:
            // scrape_tgdb.get_tgdb_games(True, LowerCase(scraperTGDB.getPlatformName(emu_active)),
            // prb_scraper_tgdb, txt_scraper_tgdb_progress)
            ;
          11:
            // scrape_tgdb.get_tgdb_games(False, LowerCase(scraperTGDB.getPlatformName(emu_active)),
            // prb_scraper_tgdb, txt_scraper_tgdb_progress)
            ;
          20:
            scrape_tgdb.get_tgdb_genres(True, prb_scraper_tgdb, txt_scraper_tgdb_progress);
          21:
            scrape_tgdb.get_tgdb_genres(False, prb_scraper_tgdb, txt_scraper_tgdb_progress);
          30:
            scrape_tgdb.get_tgdb_publishers(True, prb_scraper_tgdb, txt_scraper_tgdb_progress);
          31:
            scrape_tgdb.get_tgdb_publishers(False, prb_scraper_tgdb, txt_scraper_tgdb_progress);
          40:
            scrape_tgdb.get_tgdb_developers(True, prb_scraper_tgdb, txt_scraper_tgdb_progress);
          41:
            scrape_tgdb.get_tgdb_developers(False, prb_scraper_tgdb, txt_scraper_tgdb_progress);
        end;
        prb_scraper_tgdb.Visible := False;
        sp_scraper_tgdb_update.Visible := False;
        sp_scraper_tgdb_new.Visible := False;
        tgdb_time := True;
        tmr_config.Interval := 300;
        tmr_config.Enabled := True;
      end;
    end);
end;

procedure Tdspfm.scraper_tgdb_up_click(Sender: TObject);
begin
  sp_scraper_tgdb_games.StaysPressed := False;
  sp_scraper_tgdb_genres.StaysPressed := False;
  sp_scraper_tgdb_publishers.StaysPressed := False;
  sp_scraper_tgdb_developers.StaysPressed := False;
  (Sender as TSpeedButton).StaysPressed := True;
  sp_scraper_tgdb_new.Visible := True;
  sp_scraper_tgdb_update.Visible := True;
  tgdb_call_num := (Sender as TSpeedButton).Tag;
end;

procedure Tdspfm.skai_lang_italianAnimationFinish(Sender: TObject);
begin
  case dm.tConfiglang.AsInteger of
    0:
      skai_lang_ellinika.Animation.Loop := True;
    1:
      skai_lang_spain.Animation.Loop := True;
    2:
      skai_lang_russian.Animation.Loop := True;
    3:
      skai_lang_english.Animation.Loop := True;
    4:
      skai_lang_francais.Animation.Loop := True;
    5:
      skai_lang_german.Animation.Loop := True;
    6:
      skai_lang_brazil.Animation.Loop := True;
    7:
      skai_lang_italian.Animation.Loop := True;
  end;
end;

procedure Tdspfm.langMonoEffectState;
begin
  effMono_lang_greece.Enabled := False;
  effMono_lang_spain.Enabled := False;
  effMono_lang_russian.Enabled := False;
  effMono_lang_english.Enabled := False;
  effMono_lang_francais.Enabled := False;
  effMono_lang_german.Enabled := False;
  effMono_lang_brazil.Enabled := False;
  effMono_lang_italian.Enabled := False;

  case dm.tConfiglang.AsInteger of
    0:
      effMono_lang_greece.Enabled := True;
    1:
      effMono_lang_spain.Enabled := True;
    2:
      effMono_lang_russian.Enabled := True;
    3:
      effMono_lang_english.Enabled := True;
    4:
      effMono_lang_francais.Enabled := True;
    5:
      effMono_lang_german.Enabled := True;
    6:
      effMono_lang_brazil.Enabled := True;
    7:
      effMono_lang_italian.Enabled := True;
  end;

end;

procedure Tdspfm.loadLanguage;
begin
  case dm.tConfiglang.AsInteger of
    0:
      Rect_OnMouse_Click(rect_lang_ellinika);
    1:
      Rect_OnMouse_Click(rect_lang_spain);
    2:
      Rect_OnMouse_Click(rect_lang_russian);
    3:
      Rect_OnMouse_Click(rect_lang_english);
    4:
      Rect_OnMouse_Click(rect_lang_francais);
    5:
      Rect_OnMouse_Click(rect_lang_german);
    6:
      Rect_OnMouse_Click(rect_lang_brazil);
    7:
      Rect_OnMouse_Click(rect_lang_italian);
  end;
end;

procedure Tdspfm.mouse_enter_flag(Sender: TObject);
begin
  //
end;

procedure Tdspfm.mouse_leave_flag(Sender: TObject);
begin
  //
end;

procedure Tdspfm.OnShow;
begin
  dm.tConfig.Edit;
  // Translations
  ti_dspfm_general.Text := lang.getTransString(GENERAL, dm.tConfiglang.AsInteger);
  ti_dspfm_dirs.Text := lang.getTransString(DIRECTORIES, dm.tConfiglang.AsInteger);
  ti_dspfm_languages.Text := lang.getTransString(LANGUAGES, dm.tConfiglang.AsInteger);
  spb_dspfm_lang_export.Text := lang.getTransString(EXPORT_SELECTED_LANGUAGE, dm.tConfiglang.AsInteger);

  first_show := True;
  tc_dspfm.TabIndex := 0;

  txt_dspfm_gen_qsnap.Text := 'QSnapshot ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);

  // Languages
  loadLanguage;

  // scrapers
  if dm.tConfigscraper.AsString = 'tgdb' then
  begin
    skai_scraper_tgdb_check.Visible := True;
    skai_scraper_tgdb_check.Animation.Progress := 1;
  end
  else
  begin
    skai_scraper_screenscaper_check.Visible := True;
    skai_scraper_screenscaper_check.Animation.Progress := 1;
  end;
  first_show := False;
end;

procedure Tdspfm.Rect_OnMouse_Click(Sender: TObject);
const
  left_first_row = 259;
  left_second_row = 563;
var
  selected: Byte;

  procedure set_check(Lang_Number: integer);
  var
    Temp_X, Temp_Y: integer;
  begin
    if Lang_Number > 3 then
      Temp_X := left_second_row
    else
      Temp_X := left_first_row;

    case Lang_Number of
      0:
        Temp_Y := rect_lang_ellinika.Position.Y.ToString.ToInteger;
      1:
        Temp_Y := rect_lang_spain.Position.Y.ToString.ToInteger;
      2:
        Temp_Y := rect_lang_russian.Position.Y.ToString.ToInteger;
      3:
        Temp_Y := rect_lang_english.Position.Y.ToString.ToInteger;
      4:
        Temp_Y := rect_lang_francais.Position.Y.ToString.ToInteger;
      5:
        Temp_Y := rect_lang_german.Position.Y.ToString.ToInteger;
      6:
        Temp_Y := rect_lang_brazil.Position.Y.ToString.ToInteger;
      7:
        Temp_Y := rect_lang_italian.Position.Y.ToString.ToInteger;
    end;

    skai_dspfm_lang_check.Position.X := Temp_X;
    skai_dspfm_lang_check.Position.Y := Temp_Y;
  end;

begin
  selected := (Sender as TRectangle).Tag;
  if first_show = False then
  begin
    if dm.tConfiglang.AsInteger <> selected then
    begin
      dm.tConfiglang.Value := selected;
      dm.tConfig.Post;
      dm.tConfig.ApplyUpdates();
      Application.Terminate;
      TThread.Queue(nil,
        procedure
        begin
          ShellExecute(0, nil, PChar(dm.tConfigprj_path.AsString + dm.tConfigprj_name.AsString), nil, nil, SW_SHOWNORMAL);
        end);
    end;
  end
  else
  begin
    set_check(dm.tConfiglang.AsInteger);
  end;
end;

procedure Tdspfm.Rect_OnMouse_Enter(Sender: TObject);
begin
  eff_glow_change_button.Parent := (Sender as TRectangle);
  eff_glow_change_button.Enabled := True;
end;

procedure Tdspfm.Rect_OnMouse_Leave(Sender: TObject);
begin
  eff_glow_change_button.Enabled := False;
end;

procedure Tdspfm.rect_scraper_screenscraper_checkClick(Sender: TObject);
begin
  if dm.tConfigscraper.AsString <> 'ss' then
  begin
    dm.tConfigscraper.Value := 'ss';
    skai_scraper_tgdb_check.Visible := False;
    skai_scraper_screenscaper_check.Visible := True;
    skai_scraper_screenscaper_check.Animation.Start;
    rect_scraper_tgdb.Enabled := False;
    rect_scraper_screenscaper.Enabled := True;
    sp_scraper_screenscraper_message.Visible := True;
  end;
end;

procedure Tdspfm.rect_scraper_tgdb_checkClick(Sender: TObject);
begin
  if dm.tConfigscraper.AsString <> 'tgdb' then
  begin
    dm.tConfigscraper.Value := 'tgdb';
    skai_scraper_tgdb_check.Visible := True;
    skai_scraper_screenscaper_check.Visible := False;
    skai_scraper_tgdb_check.Animation.Start;
    rect_scraper_screenscaper.Enabled := False;
    rect_scraper_tgdb.Enabled := True;
    sp_scraper_screenscraper_message.Visible := False;
  end;
end;

procedure Tdspfm.rect_scraper_tgdb_checkMouseEnter(Sender: TObject);
begin
  eff_glow_change_button.Parent := (Sender as TRectangle);
  eff_glow_change_button.Enabled := True;
end;

procedure Tdspfm.rect_scraper_tgdb_checkMouseLeave(Sender: TObject);
begin
  eff_glow_change_button.Enabled := False;
end;

procedure Tdspfm.spb_dspfm_gen_qsnapClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select QSnapshot Directory...', '', dir) then
  begin
    // config.main.qsnapshot_path := dir;
    dm.tConfigqsnapshot.AsString := dir;
  end;
end;

procedure Tdspfm.ti_dspfm_languagesClick(Sender: TObject);
begin
  skai_dspfm_lang_check.Animation.Start;
  txt_lang_ellinika.Text := lang.getTransString(ELLINIKA, dm.tConfiglang.AsInteger);
  txt_lang_russian.Text := lang.getTransString(RUSSIAN, dm.tConfiglang.AsInteger);
  txt_lang_francais.Text := lang.getTransString(FRANCAIS, dm.tConfiglang.AsInteger);
  txt_lang_brazil.Text := lang.getTransString(BRAZIL, dm.tConfiglang.AsInteger);
  txt_lang_spain.Text := lang.getTransString(SPAIN, dm.tConfiglang.AsInteger);
  txt_lang_english.Text := lang.getTransString(ENGLISH, dm.tConfiglang.AsInteger);
  txt_lang_german.Text := lang.getTransString(GERMAN, dm.tConfiglang.AsInteger);
  txt_lang_italian.Text := lang.getTransString(ITALIAN, dm.tConfiglang.AsInteger);
  skai_lang_ellinika.Animation.Start;
  skai_lang_spain.Animation.Start;
  skai_lang_russian.Animation.Start;
  skai_lang_english.Animation.Start;
  skai_lang_francais.Animation.Start;
  skai_lang_german.Animation.Start;
  skai_lang_brazil.Animation.Start;
  skai_lang_italian.Animation.Start;
end;

procedure Tdspfm.ti_dspfm_scrapersClick(Sender: TObject);
begin
  skai_scraper_tgdb_check.Animation.Start;
  skai_scraper_screenscaper_check.Animation.Start;
end;

procedure Tdspfm.tmr_configTimer(Sender: TObject);
begin
  if tgdb_time then
  begin
    if txt_scraper_tgdb_progress.Opacity <> 0 then
      txt_scraper_tgdb_progress.Opacity := txt_scraper_tgdb_progress.Opacity - 0.01
    else
    begin
      tmr_config.Enabled := False;
      tmr_config.Interval := 100;
    end;
  end
end;

end.
