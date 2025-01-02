unit emulators;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Ani,
  FMX.Objects,
  FMX.Controls.Presentation,
  umain_config,
  front_main,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  vars_consts;

type
  Tfrm_emu = class(TForm)
    eff_glow_emu: TGlowEffect;
    rect_emu_header: TRectangle;
    lbl_emu_header: TLabel;
    linEmuArcadeUp: TLine;
    tc_emu: TTabControl;
    ti_emu_arcade: TTabItem;
    ti_emu_computers: TTabItem;
    ti_emu_consoles: TTabItem;
    ti_emu_fantasy: TTabItem;
    ti_emu_handhelds: TTabItem;
    linEmuArcadeDown: TLine;
    spbEmuArcade: TSpeedButton;
    RoundRect1: TRoundRect;
    Text1: TText;
    Line2: TLine;
    Line3: TLine;
    SpeedButton4: TSpeedButton;
    RoundRect2: TRoundRect;
    Text4: TText;
    Line4: TLine;
    Line5: TLine;
    SpeedButton5: TSpeedButton;
    RoundRect3: TRoundRect;
    Text5: TText;
    Line6: TLine;
    Line7: TLine;
    SpeedButton6: TSpeedButton;
    RoundRect4: TRoundRect;
    Text6: TText;
    Line8: TLine;
    Line9: TLine;
    SpeedButton7: TSpeedButton;
    RoundRect5: TRoundRect;
    Text7: TText;
    hzscrlbox_emu_computers: THorzScrollBox;
    spb_emu_computers_spectrum: TSpeedButton;
    Image5: TImage;
    Text10: TText;
    spb_emu_computers_oric: TSpeedButton;
    Image6: TImage;
    Text11: TText;
    spb_emu_computers_amstrad: TSpeedButton;
    Image3: TImage;
    Text8: TText;
    spb_emu_computers_commodore_64: TSpeedButton;
    Image4: TImage;
    Text9: TText;
    hzscrlbox_emu_arcade: THorzScrollBox;
    spb_emu_arcade_all: TSpeedButton;
    Image15: TImage;
    Text20: TText;
    hzscrlbox_emu_consoles: THorzScrollBox;
    spb_emu_consoles_colecovision: TSpeedButton;
    Image17: TImage;
    Text22: TText;
    spb_emu_consoles_nes: TSpeedButton;
    Image18: TImage;
    Text23: TText;
    spb_emu_consoles_mastersystem: TSpeedButton;
    Image19: TImage;
    Text24: TText;
    spb_emu_consoles_megadrive: TSpeedButton;
    Image20: TImage;
    Text25: TText;
    spb_emu_consoles_sg1000: TSpeedButton;
    Image21: TImage;
    Text26: TText;
    spb_emu_consoles_casio_pv1000: TSpeedButton;
    Image22: TImage;
    Text27: TText;
    spb_emu_consoles_casio_pv2000: TSpeedButton;
    Image23: TImage;
    Text28: TText;
    hzscrlbox_emu_handhelds: THorzScrollBox;
    spb_emu_handhelds_gameboy: TSpeedButton;
    Image24: TImage;
    Text29: TText;
    spb_emu_handhelds_gameboy_color: TSpeedButton;
    Image25: TImage;
    Text30: TText;
    spb_emu_handhelds_gameandwatch: TSpeedButton;
    Image29: TImage;
    spb_emu_handhelds_gamegear: TSpeedButton;
    Image30: TImage;
    Text35: TText;
    hzscrlbox_emu_fantasy: THorzScrollBox;
    spb_emu_fantasy_chip8: TSpeedButton;
    Image36: TImage;
    Text41: TText;
    spb_emu_consoles_epoch: TSpeedButton;
    Image7: TImage;
    Text12: TText;
    Text34: TText;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;
    Rectangle6: TRectangle;
    Rectangle7: TRectangle;
    Rectangle8: TRectangle;
    Rectangle9: TRectangle;
    Rectangle10: TRectangle;
    Rectangle11: TRectangle;
    Rectangle12: TRectangle;
    Rectangle13: TRectangle;
    Rectangle14: TRectangle;
    Rectangle15: TRectangle;
    Rectangle16: TRectangle;
    Rectangle17: TRectangle;
    Rectangle18: TRectangle;
    txtEmuArcade: TText;
    memoEmuArcade: TMemo;
    txtEmuComputers: TText;
    memoEmuComputers: TMemo;
    txtEmuConsoles: TText;
    memoEmuConsoles: TMemo;
    txtEmuHandhelds: TText;
    memoEmuHandhelds: TMemo;
    txtEmuFantasy: TText;
    memoEmuFantasy: TMemo;
    sbEmuExit: TSpeedButton;
    imgConfigExit: TImage;
    procedure FormShow(Sender: TObject);
    procedure OnEnter_Image_Glow(Sender: TObject);
    procedure OnLeave_Image_Glow(Sender: TObject);
    procedure showHandPoint(Sender: TObject);
    procedure sbEmuExitClick(Sender: TObject);
    procedure representEmu(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    function show_emulator_selected: Boolean;
  end;

var
  frm_emu: Tfrm_emu;

implementation

uses
  main,
  controls_engine,

  // emulators grid
  uarcade_actions,
  unes_actions,
  emu_functions,
  uDataModule;

{$R *.fmx}

procedure Tfrm_emu.FormShow(Sender: TObject);
begin
  if Self.StyleBook = nil then
    Self.StyleBook := main.frm_main.stylebook_main;
  Self.Width := 957;

  spb_emu_arcade_all.TagString := 'Arcade';
  spb_emu_computers_amstrad.TagString := 'Amstrad CPC 464/664/6128';
  spb_emu_computers_commodore_64.TagString := 'Commodore 64';
  spb_emu_computers_oric.TagString := 'Oric 1/Atmos';
  spb_emu_computers_spectrum.TagString := 'Spectrum 16k/48k/128k/+2/+2A/+3';
  spb_emu_consoles_casio_pv1000.TagString := 'Casio PV-1000';
  spb_emu_consoles_casio_pv2000.TagString := 'Casio PV-2000';
  spb_emu_consoles_colecovision.TagString := 'ColecoVision';
  spb_emu_consoles_epoch.TagString := 'Epoch Super Cassette Vision';
  spb_emu_consoles_mastersystem.TagString := 'Sega Master System';
  spb_emu_consoles_megadrive.TagString := 'Sega Mega Drive';
  spb_emu_consoles_nes.TagString := 'Nintendo Entertainment System NES';
  spb_emu_consoles_sg1000.TagString := 'Sega SG-1000';
  spb_emu_handhelds_gameandwatch.TagString := 'Nintendo Game And Watch';
  spb_emu_handhelds_gameboy.TagString := 'Nintendo GameBoy';
  spb_emu_handhelds_gameboy_color.TagString := 'Nintendo GameBoy Color';
  spb_emu_handhelds_gamegear.TagString := 'Sega GameGear';
  spb_emu_fantasy_chip8.TagString := 'Chip-8';
end;

procedure Tfrm_emu.OnEnter_Image_Glow(Sender: TObject);
var
  comp: TComponent;
begin
  comp := Self.FindComponent('img_emu_' + (Sender As TSpeedButton).TagString);
  eff_glow_emu.Parent := TImage(comp);
  eff_glow_emu.Enabled := True;
  showHandPoint(Sender);
end;

procedure Tfrm_emu.OnLeave_Image_Glow(Sender: TObject);
begin
  eff_glow_emu.Enabled := false;
end;

procedure Tfrm_emu.representEmu(Sender: TObject);
var
  repreEmu, aliasName, platName, overview: string;
  vComp: TComponent;
begin  repreEmu := (Sender as TSpeedButton).Tag.ToString;

  if repreEmu.StartsWith('1') then
  begin
    aliasName := 'arcade';
    platName := 'arcade';
  end
  else if repreEmu.StartsWith('2') then
  begin
    if repreEmu.EndsWith('0') then
      aliasName := 'amstrad-cpc'
    else if repreEmu.EndsWith('1') then
      aliasName := 'commodore-64'
    else if repreEmu.EndsWith('2') then
      aliasName := 'oric-1'
    else if repreEmu.EndsWith('3') then
      aliasName := 'sinclair-zx-spectrum';
    platName := 'computers';
  end
  else if repreEmu.StartsWith('3') then
  begin
    if repreEmu.EndsWith('0') then
      aliasName := 'casio-pv-1000'
    else if repreEmu.EndsWith('1') then
      aliasName := ''
    else if repreEmu.EndsWith('2') then
      aliasName := 'colecovision'
    else if repreEmu.EndsWith('3') then
      aliasName := 'epoch-super-cassette-vision'
    else if repreEmu.EndsWith('4') then
      aliasName := 'sega-master-system'
    else if repreEmu.EndsWith('5') then
      aliasName := 'sega-mega-drive'
    else if repreEmu.EndsWith('6') then
      aliasName := 'nintendo-entertainment-system-nes'
    else if repreEmu.EndsWith('7') then
      aliasName := 'sg1000';
    platName:= 'consoles';
  end
  else if repreEmu.StartsWith('4') then
  begin
    if repreEmu.EndsWith('0') then
      aliasName := 'game-and-watch'
    else if repreEmu.EndsWith('1') then
      aliasName := 'nintendo-gameboy'
    else if repreEmu.EndsWith('2') then
      aliasName := ''
    else if repreEmu.EndsWith('3') then
      aliasName := 'sega-game-gear';
    platName:= 'handhelds';
  end
  else if repreEmu.StartsWith('5') then
  begin
    if repreEmu.EndsWith('0') then
      aliasName := '';
    platName:= 'fantasy';
  end;
  dm.tTGDBPlatforms.Active := true;
  dm.tTGDBPlatforms.Locate('alias', aliasName, []);

  vComp := Self.FindComponent('memoEmu' + platName);
  (vComp as TMemo).Text := dm.tTGDBPlatformsoverview.AsString;

  vComp := Self.FindComponent('txtEmu' + platName);
  (vComp as TText).Text := (Sender as TSpeedButton).TagString;
  dm.tTGDBPlatforms.Active := false;

end;

procedure Tfrm_emu.sbEmuExitClick(Sender: TObject);
begin
  close;
end;

procedure Tfrm_emu.showHandPoint(Sender: TObject);
begin
  if (Sender is TRectangle) then
    (Sender as TRectangle).Cursor := crHandPoint;
  if (Sender is TImage) then
    (Sender as TImage).Cursor := crHandPoint;
  if (Sender is TSpeedButton) then
    (Sender as TSpeedButton).Cursor := crHandPoint;
end;

function Tfrm_emu.show_emulator_selected: Boolean;
var
  last: integer;
begin
  last := High(front_action.grid_rect) - 1;
  main.frm_main.rect_grid.Height := front_action.grid_rect[last].Position.Y + 329;

  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=0';
  dm.tArcade.Filtered := True;
  frm_main.spb_emu_working.Text := '(' + dm.tArcade.RecordCount.ToString + ')';

  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=1';
  dm.tArcade.Filtered := True;
  frm_main.spb_emu_working_minor.Text := '(' + dm.tArcade.RecordCount.ToString + ')';

  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=2';
  dm.tArcade.Filtered := True;
  frm_main.spb_emu_working_major.Text := '(' + dm.tArcade.RecordCount.ToString + ')';

  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=3';
  dm.tArcade.Filtered := True;
  frm_main.spb_emu_not_working.Text := '(' + dm.tArcade.RecordCount.ToString + ')';

  dm.tArcade.Filtered := false;
  frm_main.lblTotalGamesValue.Text := dm.tArcade.RecordCount.ToString;
end;

end.
