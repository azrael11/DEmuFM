unit f_controls_keyboard;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Filter.Effects,
  FMX.Objects,
  FMX.Controls.Presentation;

type
  Tcontrol_keyboard = class(TFrame)
    lbl_ck_but1: TLabel;
    lbl_ck_but2: TLabel;
    lbl_ck_but3: TLabel;
    lbl_ck_but4: TLabel;
    lbl_ck_but5: TLabel;
    lbl_ck_but6: TLabel;
    lbl_ck_coin: TLabel;
    lbl_ck_down: TLabel;
    lbl_ck_left: TLabel;
    lbl_ck_right: TLabel;
    lbl_ck_start: TLabel;
    lbl_ck_up: TLabel;
    rect_ck_but1: TRectangle;
    txt_ck_but1: TText;
    rect_ck_but2: TRectangle;
    txt_ck_but2: TText;
    rect_ck_but3: TRectangle;
    txt_ck_but3: TText;
    rect_ck_but4: TRectangle;
    txt_ck_but4: TText;
    rect_ck_but5: TRectangle;
    txt_ck_but5: TText;
    rect_ck_but6: TRectangle;
    txt_ck_but6: TText;
    rect_ck_coin: TRectangle;
    txt_ck_coin: TText;
    rect_ck_down: TRectangle;
    txt_ck_down: TText;
    rect_ck_left: TRectangle;
    txt_ck_left: TText;
    rect_ck_right: TRectangle;
    txt_ck_right: TText;
    rect_ck_start: TRectangle;
    txt_ck_start: TText;
    rect_ck_up: TRectangle;
    txt_ck_up: TText;
    spb_ck_edit: TSpeedButton;
    img_ck_edit: TImage;
    eff_frgb_edit: TFillRGBEffect;
    tmr_ck: TTimer;

    procedure FrameKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure spb_ck_editClick(Sender: TObject);
    procedure rect_edit_click(Sender: TObject);
    procedure tmr_ckTimer(Sender: TObject);

  private
    { Private declarations }
    edit_mode: boolean;
    passes: byte;
    dots: byte;
    before_char: string;
    edit_txt: TText;

    procedure save_player_keymap_data(player, update_key: string);
    procedure test_keyboard(player: byte);
  public
    { Public declarations }
    procedure show_cur_frame;
  end;

implementation

uses
  config_controls,
  prj_functions,
  ulang,
  capsdefs,
  controls_engine,
  uDataModule;

{$R *.fmx}

procedure Tcontrol_keyboard.FrameKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if edit_mode then
  begin
    tmr_ck.Enabled := False;
    if Key <> 0 then
      edit_txt.Text := prj_functions.key_to_string(Key)
    else
      edit_txt.Text := UpperCase(KeyChar);
    save_player_keymap_data(frm_config_controls.player, edit_txt.Text);
    before_char := '';
  end;
end;

procedure Tcontrol_keyboard.rect_edit_click(Sender: TObject);
var
  vComp: TComponent;
  name: string;
begin
  if edit_mode then
  begin
    if before_char <> '' then
    begin
      edit_txt.Text := before_char;
      before_char := '';
    end;
    name := (Sender as TRectangle).name;
    Delete(name, 1, 4);
    name := 'txt' + name;
    vComp := Self.FindComponent(name);
    edit_txt := TText(vComp);
    before_char := edit_txt.Text;
    edit_txt.Text := '';
    passes := 0;
    dots := 0;
    tmr_ck.Enabled := True;
  end;
end;

procedure Tcontrol_keyboard.save_player_keymap_data(player, update_key: string);
var
  col_name: string;
  new_key: Word;
begin
  // if check_key_in_use(Key) = False then
  // begin
  col_name := edit_txt.TagString;
  new_key := key_num(update_key);
  // end;
end;

procedure Tcontrol_keyboard.show_cur_frame;
begin
   lbl_ck_up.Text := lang.getTransString(UP, dm.tConfiglang.AsInteger);
   lbl_ck_down.Text := lang.getTransString(DOWN, dm.tConfiglang.AsInteger);
   lbl_ck_left.Text := lang.getTransString(LEFT, dm.tConfiglang.AsInteger);
   lbl_ck_right.Text := lang.getTransString(RIGHT, dm.tConfiglang.AsInteger);
   lbl_ck_but1.Text := lang.getTransString(BUTTON, dm.tConfiglang.AsInteger) + ' 1';
   lbl_ck_but2.Text := lang.getTransString(BUTTON, dm.tConfiglang.AsInteger) + ' 2';
   lbl_ck_but3.Text := lang.getTransString(BUTTON, dm.tConfiglang.AsInteger) + ' 3';
   lbl_ck_but4.Text := lang.getTransString(BUTTON, dm.tConfiglang.AsInteger) + ' 4';
   lbl_ck_but5.Text := lang.getTransString(BUTTON, dm.tConfiglang.AsInteger) + ' 5';
   lbl_ck_but6.Text := lang.getTransString(BUTTON, dm.tConfiglang.AsInteger) + ' 6';
   lbl_ck_coin.Text := lang.getTransString(COIN, dm.tConfiglang.AsInteger);
   lbl_ck_start.Text := lang.getTransString(START, dm.tConfiglang.AsInteger);
   txt_ck_up.TagString := 'key_up';
   txt_ck_down.TagString := 'key_down';
   txt_ck_left.TagString := 'key_left';
   txt_ck_right.TagString := 'key_right';
   txt_ck_but1.TagString := 'key_b0';
   txt_ck_but2.TagString := 'key_b1';
   txt_ck_but3.TagString := 'key_b2';
   txt_ck_but4.TagString := 'key_b3';
   txt_ck_but5.TagString := 'key_b4';
   txt_ck_but6.TagString := 'key_b5';
   txt_ck_coin.TagString := 'key_coin';
   txt_ck_start.TagString := 'key_start';
   test_keyboard(frm_config_controls.player.ToInteger);
end;

procedure Tcontrol_keyboard.spb_ck_editClick(Sender: TObject);
  procedure set_mode(rect_color, txt_color: TColor; stroke: integer; rect_cursor: TCursor);
  begin
    rect_ck_up.Fill.color := rect_color;
    txt_ck_up.TextSettings.FontColor := txt_color;
    rect_ck_down.Fill.color := rect_color;
    txt_ck_down.TextSettings.FontColor := txt_color;
    rect_ck_left.Fill.color := rect_color;
    txt_ck_left.TextSettings.FontColor := txt_color;
    rect_ck_right.Fill.color := rect_color;
    txt_ck_right.TextSettings.FontColor := txt_color;
    rect_ck_but1.Fill.color := rect_color;
    txt_ck_but1.TextSettings.FontColor := txt_color;
    rect_ck_but2.Fill.color := rect_color;
    txt_ck_but2.TextSettings.FontColor := txt_color;
    rect_ck_but3.Fill.color := rect_color;
    txt_ck_but3.TextSettings.FontColor := txt_color;
    rect_ck_but4.Fill.color := rect_color;
    txt_ck_but4.TextSettings.FontColor := txt_color;
    rect_ck_but5.Fill.color := rect_color;
    txt_ck_but5.TextSettings.FontColor := txt_color;
    rect_ck_but6.Fill.color := rect_color;
    txt_ck_but6.TextSettings.FontColor := txt_color;
    rect_ck_coin.Fill.color := rect_color;
    txt_ck_coin.TextSettings.FontColor := txt_color;
    rect_ck_start.Fill.color := rect_color;
    txt_ck_start.TextSettings.FontColor := txt_color;
    rect_ck_up.cursor := cursor;
    rect_ck_down.cursor := cursor;
    rect_ck_left.cursor := cursor;
    rect_ck_right.cursor := cursor;
    rect_ck_but1.cursor := cursor;
    rect_ck_but2.cursor := cursor;
    rect_ck_but3.cursor := cursor;
    rect_ck_but4.cursor := cursor;
    rect_ck_but5.cursor := cursor;
    rect_ck_but6.cursor := cursor;
    rect_ck_coin.cursor := cursor;
    rect_ck_start.cursor := cursor;
    rect_ck_up.stroke.Thickness := stroke;
    rect_ck_down.stroke.Thickness := stroke;
    rect_ck_left.stroke.Thickness := stroke;
    rect_ck_right.stroke.Thickness := stroke;
    rect_ck_but1.stroke.Thickness := stroke;
    rect_ck_but2.stroke.Thickness := stroke;
    rect_ck_but3.stroke.Thickness := stroke;
    rect_ck_but4.stroke.Thickness := stroke;
    rect_ck_but5.stroke.Thickness := stroke;
    rect_ck_but6.stroke.Thickness := stroke;
    rect_ck_coin.stroke.Thickness := stroke;
    rect_ck_start.stroke.Thickness := stroke;
  end;

begin
  edit_mode := not edit_mode;

  if edit_mode then
  begin
    set_mode($FF44BEB0, TAlphaColorRec.White, 1, crHandPoint);
  end
  else
  begin
    if before_char <> '' then
    begin
      edit_txt.Text := before_char;
      tmr_ck.Enabled := False;
    end;
    set_mode(TAlphaColorRec.White, TAlphaColorRec.Black, 0, crDefault);
  end;
  eff_frgb_edit.Enabled := edit_mode;
end;

{ Tcontrol_keyboard }

procedure Tcontrol_keyboard.test_keyboard(player: byte);
begin
  if active_controllers[0, player] then
  begin
    txt_ck_up.Text := key_name(temp_key[player].Key.up);
    txt_ck_down.Text := key_name(temp_key[player].Key.down);
    txt_ck_left.Text := key_name(temp_key[player].Key.left);
    txt_ck_right.Text := key_name(temp_key[player].Key.right);
    txt_ck_but1.Text := key_name(temp_key[player].Key.b0);
    txt_ck_but2.Text := key_name(temp_key[player].Key.b1);
    txt_ck_but3.Text := key_name(temp_key[player].Key.b2);
    txt_ck_but4.Text := key_name(temp_key[player].Key.b3);
    txt_ck_but5.Text := key_name(temp_key[player].Key.b4);
    txt_ck_but6.Text := key_name(temp_key[player].Key.b5);
    txt_ck_coin.Text := key_name(temp_key[player].Key.coin);
    txt_ck_start.Text := key_name(temp_key[player].Key.start);

    edit_mode := False;
    before_char := '';
  end;
end;

procedure Tcontrol_keyboard.tmr_ckTimer(Sender: TObject);
begin
  if passes > 3 then
  begin
    edit_txt.Text := before_char;
    tmr_ck.Enabled := False;
  end
  else
  begin
    Inc(dots);
    edit_txt.Text := edit_txt.Text + '.';
    if dots = 4 then
    begin
      edit_txt.Text := '';
      Inc(passes);
      dots := 0;
    end;
  end;
end;

end.
