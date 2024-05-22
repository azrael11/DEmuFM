unit scraper_tgdb;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Effects,
  FMX.Ani,
  FMX.Controls.Presentation,
  Winapi.Windows;

type
  Tfrm_scraper = class(TForm)
    rect_scraper: TRectangle;
    rect_scraper_header: TRectangle;
    lbl_scraper_header: TLabel;
    spb_scraper_start: TSpeedButton;
    lbl_scraper_platform: TLabel;
    lbl_scraper_platform_value: TLabel;
    lbl_scraper_games: TLabel;
    lbl_scraper_games_value: TLabel;
    lbl_scraper_warning: TLabel;
    anim_float_scraper_warning: TFloatAnimation;
    prbar_scraper: TProgressBar;
    lbl_scraper_count: TLabel;
    lbl_scraper_count_value: TLabel;
    eff_shadow_scraper: TShadowEffect;
    eff_glow_scraper: TGlowEffect;
    cb_scraper_only_missing: TCheckBox;
    lbl_scraper_missing: TLabel;
    lbl_scraper_missing_value: TLabel;
    spb_scraper_cancel: TSpeedButton;
    txt_scraper_info: TText;
    txt_scraper_info_game: TText;
    lbl_scraper_rom: TLabel;
    lbl_scraper_rom_value: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure spb_scraper_cancelClick(Sender: TObject);
    procedure spb_scraper_startClick(Sender: TObject);
    procedure spb_scraper_mouse_on_enter(Sender: TObject);
    procedure spb_scraper_mouse_on_leave(Sender: TObject);
  private
    { Private declarations }
  public
    pressed_stop: boolean;
    { Public declarations }
  end;

var
  frm_scraper: Tfrm_scraper;

implementation

{$R *.fmx}

uses
  main,
  uscraper_tgdb,
  umain_config,
  uTheGamesDatabase;

procedure Tfrm_scraper.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Close;
end;

procedure Tfrm_scraper.FormShow(Sender: TObject);
begin
  if Self.StyleBook = nil then
    Self.StyleBook := main.frm_main.stylebook_main;
end;

procedure Tfrm_scraper.spb_scraper_cancelClick(Sender: TObject);
var
  myAction: TCloseAction;
begin
  Self.FormClose(nil, myAction);
end;

procedure Tfrm_scraper.spb_scraper_mouse_on_enter(Sender: TObject);
begin
  eff_glow_scraper.Parent := (Sender as TSpeedButton);
  eff_glow_scraper.Enabled := True;
end;

procedure Tfrm_scraper.spb_scraper_mouse_on_leave(Sender: TObject);
begin
  eff_glow_scraper.Enabled := False;
end;

procedure Tfrm_scraper.spb_scraper_startClick(Sender: TObject);
begin
  if spb_scraper_start.Text = 'Start' then
    scrape_tgdb.start(emu_active)
  else if spb_scraper_start.Text = 'Stop' then
    pressed_stop := True;
end;

end.
