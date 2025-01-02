unit configuration;

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
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  SubjectStand,
  FrameStand,
  f_dspfm,
  f_arcade,
  f_spectrum,
  f_amstrad,
  f_commodore64,
  f_nes,
  f_gameboy,
  f_gameboy_color,
  f_colecovision,
  f_master_system,
  f_sg1000,
  f_gamegear,
  f_epoch_scv,
  f_mega_drive,
  f_chip8,
  f_game_and_watch;

type
  Tfrm_config = class(TForm)
    rect_emulators: TRectangle;
    rect_dspfm: TRectangle;
    spb_dspfm: TSpeedButton;
    txt_dspfm: TText;
    img_dspfm: TImage;
    rect_emu_nes: TRectangle;
    spb_emu_nes: TSpeedButton;
    txt_emu_nes: TText;
    img_emu_nes: TImage;
    rect_emu_gb: TRectangle;
    spb_emu_gb: TSpeedButton;
    txt_emu_gb: TText;
    img_emu_gb: TImage;
    rect_config_main: TRectangle;
    vertsb_emulators: TVertScrollBox;
    rect_config_header: TRectangle;
    lbl_config_header: TLabel;
    rect_emu_spectrum: TRectangle;
    spb_emu_spectrum: TSpeedButton;
    txt_emu_spectrum: TText;
    img_emu_spectrum: TImage;
    rect_emu_amstrad: TRectangle;
    spb_emu_amstrad: TSpeedButton;
    txt_emu_amstrad: TText;
    img_emu_amstrad: TImage;
    rect_emu_commodore: TRectangle;
    spb_emu_commodore: TSpeedButton;
    txt_emu_commodore: TText;
    img_emu_commodore: TImage;
    rect_emu_gb_color: TRectangle;
    spb_emu_gb_color: TSpeedButton;
    txt_emu_gb_color: TText;
    img_emu_gb_color: TImage;
    rect_emu_colecovision: TRectangle;
    spb_emu_colecovision: TSpeedButton;
    txt_emu_colecovision: TText;
    img_emu_colecovision: TImage;
    rect_emu_master_system: TRectangle;
    spb_emu_master_system: TSpeedButton;
    txt_emu_master_system: TText;
    img_emu_master_system: TImage;
    rect_emu_sg1000: TRectangle;
    spb_emu_sg1000: TSpeedButton;
    txt_emu_sg1000: TText;
    img_emu_sg1000: TImage;
    rect_emu_gamegear: TRectangle;
    spb_emu_gamegear: TSpeedButton;
    txt_emu_gamegear: TText;
    img_emu_gamegear: TImage;
    rect_emu_escv: TRectangle;
    spb_emu_escv: TSpeedButton;
    txt_emu_escv: TText;
    img_emu_escv: TImage;
    rect_emu_megadrive: TRectangle;
    spb_emu_megadrive: TSpeedButton;
    txt_emu_megadrive: TText;
    img_emu_megadrive: TImage;
    rect_emu_chip8: TRectangle;
    spb_emu_chip8: TSpeedButton;
    txt_emu_chip8: TText;
    img_emu_chip8: TImage;
    lay_config_dspfm: TLayout;
    framestand_dspfm: TFrameStand;
    rect_emu_arcade: TRectangle;
    spb_emu_arcade: TSpeedButton;
    txt_emu_arcade: TText;
    img_emu_arcade: TImage;
    rect_emu_gaw: TRectangle;
    spb_emu_gaw: TSpeedButton;
    txt_emu_gaw: TText;
    img_emu_gaw: TImage;
    framestand_arcade: TFrameStand;
    framestand_spectrum: TFrameStand;
    framestand_amstrad: TFrameStand;
    framestand_commodore64: TFrameStand;
    framestand_nes: TFrameStand;
    framestand_gameboy: TFrameStand;
    framestand_gameboy_color: TFrameStand;
    framestand_colecovision: TFrameStand;
    framestand_master_system: TFrameStand;
    framestand_gamegear: TFrameStand;
    framestand_sg1000: TFrameStand;
    framestand_epoch_scv: TFrameStand;
    framestand_mega_drive: TFrameStand;
    framestand_chip8: TFrameStand;
    framestand_gaw: TFrameStand;
    lay_config_arcade: TLayout;
    lay_config_spectrum: TLayout;
    lay_config_amstrad: TLayout;
    lay_config_commodore64: TLayout;
    lay_config_nes: TLayout;
    lay_config_gameboy: TLayout;
    lay_config_gameboy_color: TLayout;
    lay_config_colecovision: TLayout;
    lay_config_master_system: TLayout;
    lay_config_sg1000: TLayout;
    lay_config_gamegear: TLayout;
    lay_config_epoch_scv: TLayout;
    lay_config_mega_drive: TLayout;
    lay_config_chip8: TLayout;
    lay_config_gaw: TLayout;
    rect_config_footer: TRectangle;
    txt_config_info: TText;
    sbConfigExit: TSpeedButton;
    imgConfigExit: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure spb_onmouse_click(Sender: TObject);
    procedure spb_onmouse_enter(Sender: TObject);
    procedure spb_onmouse_leave(Sender: TObject);
    procedure sbConfigExitClick(Sender: TObject);
    procedure sbConfigExitMouseEnter(Sender: TObject);
  private
    { Private declarations }
    selected_platform: byte;
    selected_platform_name: string;

    procedure show_frame(frame_number: byte);

  public
    { Public declarations }
    frame_dspfm: TFrameInfo<Tdspfm>;
    frame_arcade: TFrameInfo<Tarcade>;
    frame_spectrum: TFrameInfo<Tspectrum>;
    frame_amstrad: TFrameInfo<Tamstrad>;
    frame_commodore64: TFrameInfo<Tcommodore_64>;
    frame_nes: TFrameInfo<Tn_e_s>;
    frame_gameboy: TFrameInfo<Tgameboy>;
    frame_gameboy_color: TFrameInfo<Tgameboy_color>;
    frame_colecovision: TFrameInfo<Tcolecovision>;
    frame_master_system: TFrameInfo<Tmaster_system>;
    frame_sg1000: TFrameInfo<Tsg_1000>;
    frame_gamegear: TFrameInfo<Tgamegear>;
    frame_epoch_scv: TFrameInfo<Tepoch_scv>;
    frame_mega_drive: TFrameInfo<Tmega_drive>;
    frame_chip8: TFrameInfo<Tchip8>;
    frame_game_and_watch: TFrameInfo<Tgame_and_watch>;
    procedure set_lang_strings;
    procedure close_frames_stands;
    procedure show_bottom_hint(text: string);
    procedure clear_bottom_hint;

  end;

var
  frm_config: Tfrm_config;

implementation

{$R *.fmx}

uses
  main,
  umain_config,
  ulang,
  uDataModule;

procedure Tfrm_config.clear_bottom_hint;
begin
  txt_config_info.text := '';
end;

procedure Tfrm_config.close_frames_stands;
begin
  frame_dspfm.Close;
  frame_arcade.Close;
  frame_spectrum.Close;
  frame_amstrad.Close;
  frame_commodore64.Close;
  frame_nes.Close;
  frame_gameboy.Close;
  frame_gameboy_color.Close;
  frame_colecovision.Close;
  frame_master_system.Close;
  frame_sg1000.Close;
  frame_gamegear.Close;
  frame_epoch_scv.Close;
  frame_mega_drive.Close;
  frame_chip8.Close;
  frame_game_and_watch.Close;
end;

procedure Tfrm_config.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  close_frames_stands;
end;

procedure Tfrm_config.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if not frame_dspfm.Hiding then
    frame_dspfm.Frame.OnKeyDown(Sender, Key, KeyChar, Shift);
end;

procedure Tfrm_config.FormShow(Sender: TObject);
begin
  dm.tLanguage.Active := true;
  dm.tLanguagePop.Active := true;
  if Self.StyleBook = nil then
    Self.StyleBook := main.frm_main.stylebook_main;
  frame_dspfm := framestand_dspfm.New<Tdspfm>(lay_config_dspfm);
  frame_arcade := framestand_arcade.New<Tarcade>(lay_config_arcade);
  frame_spectrum := framestand_spectrum.New<Tspectrum>(lay_config_spectrum);
  frame_amstrad := framestand_amstrad.New<Tamstrad>(lay_config_amstrad);
  frame_commodore64 := framestand_commodore64.New<Tcommodore_64>(lay_config_commodore64);
  frame_nes := framestand_nes.New<Tn_e_s>(lay_config_nes);
  frame_gameboy := framestand_gameboy.New<Tgameboy>(lay_config_gameboy);
  frame_gameboy_color := framestand_gameboy_color.New<Tgameboy_color>(lay_config_gameboy_color);
  frame_colecovision := framestand_colecovision.New<Tcolecovision>(lay_config_colecovision);
  frame_master_system := framestand_master_system.New<Tmaster_system>(lay_config_master_system);
  frame_sg1000 := framestand_sg1000.New<Tsg_1000>(lay_config_sg1000);
  frame_gamegear := framestand_gamegear.New<Tgamegear>(lay_config_gamegear);
  frame_epoch_scv := framestand_epoch_scv.New<Tepoch_scv>(lay_config_epoch_scv);
  frame_mega_drive := framestand_mega_drive.New<Tmega_drive>(lay_config_mega_drive);
  frame_chip8 := framestand_chip8.New<Tchip8>(lay_config_chip8);
  frame_game_and_watch := framestand_gaw.New<Tgame_and_watch>(lay_config_gaw);
  frame_dspfm.Show;
  frame_dspfm.Frame.OnShow;
  frame_arcade.Frame.OnShow;
  frame_nes.Frame.OnShow;
  set_lang_strings;
  selected_platform := 0;
  selected_platform_name := 'rect_dspfm';
  lbl_config_header.text := 'DEmuFM';
  rect_dspfm.Fill.Color := TAlphaColorRec.Lightgray;
  dm.tLanguage.Active := false;
  dm.tLanguagePop.Active := false;
  frm_Main.eff_blur_main.Enabled := true;
end;

{ Tfrm_config }

procedure Tfrm_config.sbConfigExitClick(Sender: TObject);
begin
  frm_Main.eff_blur_main.Enabled := false;
  close;
end;

procedure Tfrm_config.sbConfigExitMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).Cursor := crHandPoint;
end;

procedure Tfrm_config.set_lang_strings;
begin
//  Self.Caption := lang.load_lang_string('Configuration');
end;

procedure Tfrm_config.show_bottom_hint(text: string);
begin
  txt_config_info.text := text;
end;

procedure Tfrm_config.show_frame(frame_number: byte);
begin
  if selected_platform <> frame_number then
  begin
    case selected_platform of
      0:
        frame_dspfm.Hide();
      1:
        begin
          frame_arcade.Frame.onClose;
          frame_arcade.Hide();
        end;
      2:
        frame_spectrum.Hide();
      3:
        frame_amstrad.Hide();
      4:
        frame_commodore64.Hide();
      5:
        frame_nes.Hide();
      6:
        frame_gameboy.Hide();
      7:
        frame_gameboy_color.Hide();
      8:
        frame_colecovision.Hide();
      9:
        frame_master_system.Hide();
      10:
        frame_sg1000.Hide();
      11:
        frame_gamegear.Hide();
      12:
        frame_epoch_scv.Hide();
      13:
        frame_mega_drive.Hide();
      14:
        frame_chip8.Hide();
      15:
        frame_game_and_watch.Hide();
    end;

    selected_platform := frame_number;

    case selected_platform of
      0:
        frame_dspfm.Show;
      1:
        frame_arcade.Show;
      2:
        frame_spectrum.Show;
      3:
        frame_amstrad.Show;
      4:
        frame_commodore64.Show;
      5:
        frame_nes.Show;
      6:
        frame_gameboy.Show;
      7:
        frame_gameboy_color.Show;
      8:
        frame_colecovision.Show;
      9:
        frame_master_system.Show;
      10:
        frame_sg1000.Show;
      11:
        frame_gamegear.Show;
      12:
        frame_epoch_scv.Show;
      13:
        frame_mega_drive.Show;
      14:
        frame_chip8.Show;
      15:
        frame_game_and_watch.Show;
    end;

  end;
end;

procedure Tfrm_config.spb_onmouse_click(Sender: TObject);
var
  vComp: TComponent;
  comp_name, text_name: string;
begin
  if selected_platform <> (Sender as TSpeedButton).Tag then
  begin
    vComp := Self.FindComponent(selected_platform_name);
    (vComp as TRectangle).Fill.Color := $FF375278;
    case (Sender as TSpeedButton).Tag of
      0:
        show_frame(0);
      1:
        show_frame(1);
      2:
        show_frame(2);
      3:
        show_frame(3);
      4:
        show_frame(4);
      5:
        show_frame(5);
      6:
        show_frame(6);
      7:
        show_frame(7);
      8:
        show_frame(8);
      9:
        show_frame(9);
      10:
        show_frame(10);
      11:
        show_frame(11);
      12:
        show_frame(12);
      13:
        show_frame(13);
      14:
        show_frame(14);
      15:
        show_frame(15);
    end;
    comp_name := (Sender as TSpeedButton).Name;
    Delete(comp_name, 1, 3);
    text_name := 'txt' + comp_name;
    vComp := Self.FindComponent(text_name);
    lbl_config_header.text := (vComp as TText).text;
    comp_name := 'rect' + comp_name;
    selected_platform_name := comp_name;
    vComp := Self.FindComponent(comp_name);
    (vComp as TRectangle).Fill.Color := TAlphaColorRec.Lightgray;
  end;
end;

procedure Tfrm_config.spb_onmouse_enter(Sender: TObject);
begin
  //
end;

procedure Tfrm_config.spb_onmouse_leave(Sender: TObject);
begin
  //
end;

end.
