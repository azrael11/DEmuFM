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
    lin_emu_1: TLine;
    tc_emu: TTabControl;
    ti_emu_arcade: TTabItem;
    ti_emu_computers: TTabItem;
    ti_emu_consoles: TTabItem;
    ti_emu_fantasy: TTabItem;
    ti_emu_handhelds: TTabItem;
    Line1: TLine;
    SpeedButton3: TSpeedButton;
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
    Text2: TText;
    Memo1: TMemo;
    Image1: TImage;
    Image2: TImage;
    Image8: TImage;
    Image9: TImage;
    Text3: TText;
    Memo2: TMemo;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Text13: TText;
    Memo3: TMemo;
    Image14: TImage;
    Image16: TImage;
    Image26: TImage;
    Image27: TImage;
    Text14: TText;
    Memo4: TMemo;
    Image28: TImage;
    Image31: TImage;
    Image32: TImage;
    Image33: TImage;
    Text15: TText;
    Memo5: TMemo;
    Image34: TImage;
    Image35: TImage;
    Image37: TImage;
    Image38: TImage;
    procedure FormShow(Sender: TObject);
    procedure OnEnter_Image_Glow(Sender: TObject);
    procedure OnLeave_Image_Glow(Sender: TObject);
    procedure spb_emu_arcadeClick(Sender: TObject);
    procedure spb_emu_nesClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    function show_emulator_selected(emu: TEmulatorSelected): Boolean;
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
  emu_functions, uDataModule;

{$R *.fmx}

procedure Tfrm_emu.FormShow(Sender: TObject);
begin
  if Self.StyleBook = nil then
    Self.StyleBook := main.frm_main.stylebook_main;
  Self.Width := 957;
  // spb_emu_arcade.TagString := 'arcade';
  // spb_emu_spectrum.TagString := 'spectrum';
  // spb_emu_amstrad.TagString := 'amstrad';
  // spb_emu_commodore_64.TagString := 'commodore_64';
  // spb_emu_nes.TagString := 'nes';
  // spb_emu_sg_1000.TagString := 'sg_1000';
  // spb_emu_master_system.TagString := 'master_system';
  // spb_emu_mega_drive.TagString := 'mega_drive';
  // spb_emu_colecovision.TagString := 'colecovision';
  // spb_emu_epoch_scv.TagString := 'epoch_scv';
  // spb_emu_gameboy.TagString := 'gameboy';
  // spb_emu_gameboy_color.TagString := 'gameboy_color';
  // spb_emu_gamegear.TagString := 'gamegear';
  // spb_emu_gaw.TagString := 'gaw';
  // spb_emu_chip8.TagString := 'chip8';
end;

procedure Tfrm_emu.OnEnter_Image_Glow(Sender: TObject);
var
  comp: TComponent;
begin
  comp := Self.FindComponent('img_emu_' + (Sender As TSpeedButton).TagString);
  eff_glow_emu.Parent := TImage(comp);
  eff_glow_emu.Enabled := True;
end;

procedure Tfrm_emu.OnLeave_Image_Glow(Sender: TObject);
begin
  eff_glow_emu.Enabled := false;
end;

function Tfrm_emu.show_emulator_selected(emu: TEmulatorSelected): Boolean;
var
  last: integer;
begin
  last := High(front_action.grid_rect) - 1;
  main.frm_main.rect_grid.Height := front_action.grid_rect[last].Position.Y + 329;

  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=0';
  dm.tArcade.Filtered := true;
  frm_main.spb_emu_working.Text := '(' + dm.tArcade.RecordCount.ToString + ')';
  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=1';
  dm.tArcade.Filtered := true;
  frm_main.spb_emu_working_minor.Text := '(' + dm.tArcade.RecordCount.ToString + ')';
  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=2';
  dm.tArcade.Filtered := true;
  frm_main.spb_emu_working_major.Text := '(' + dm.tArcade.RecordCount.ToString + ')';
  dm.tArcade.Filtered := false;
  dm.tArcade.Filter := 'state_icon=3';
  dm.tArcade.Filtered := true;
  frm_main.spb_emu_not_working.Text := '(' + dm.tArcade.RecordCount.ToString + ')';
  dm.tArcade.Filtered := false;
end;

procedure Tfrm_emu.spb_emu_arcadeClick(Sender: TObject);
begin
  if emu_active <> emus_Arcade then
  begin
    emu_active := emus_Arcade;
    front_action.destroy_grid;
    front_action.create_grid('arcade');
    // frm_main.img_platform_change.Bitmap := img_emu_arcade.Bitmap;
    show_emulator_selected(emu_active);
    Self.Close;
  end;
end;

procedure Tfrm_emu.spb_emu_nesClick(Sender: TObject);
begin
  if emu_active <> emus_Nes then
  begin
    emu_active := emus_Nes;
    front_action.destroy_grid;
    front_action.create_grid('nes');
    show_emulator_selected(emu_active);
    // frm_main.img_platform_change.Bitmap := img_emu_nes.Bitmap;
    Self.Close;
  end;
end;

end.
