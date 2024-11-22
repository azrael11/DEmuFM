unit unes_actions;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Objects,
  FMX.Filter.Effects,
  FMX.Types,
  FMX.Graphics,
  FMX.Forms,
  System.UITypes,
  vars_consts;

type
  TNES_MOUSE = class
    procedure OnMouseClick(Sender: TObject);
    procedure OnMouseEnter(Sender: TObject);
    procedure OnMouseLeave(Sender: TObject);
  end;

type
  TNES_ACTIONS = class
  private

  public
    grid_nes_selected: integer;
    grid_nes_main: TRectangle;
    grid_nes_rect: array of TRectangle;
    grid_nes_img_gray: array of TMonochromeEffect;
    grid_nes_img: array of TImage;
    grid_nes_text: array of TText;
    grid_nes_state: array of TRectangle;
    rom_nes_selected: string;
    // game_to_run: integer;
    // game_run_name: string;
    // roms_to_run: array of boolean;

    constructor create;
    destructor destroy;

    procedure show_games_in_grid;
    procedure create_nes_game_info(rom: String);
    procedure restore_before_stop_game;
  end;

var
  Nes_Action: TNES_ACTIONS;
  Nes_Mouse: TNES_MOUSE;

implementation

uses
  umain_config,
  main,
  emu_functions,
  splash,
  uDataModule;
{ TNES_ACTIONS }

constructor TNES_ACTIONS.create;
begin
//  emu_active := emus_Nes;
end;

procedure TNES_ACTIONS.create_nes_game_info(rom: String);
begin
  //
end;

destructor TNES_ACTIONS.destroy;
begin
  //
end;

procedure TNES_ACTIONS.restore_before_stop_game;
begin
  grid_nes_rect[grid_nes_selected].Fill.Color := $FF2B3A4F;
  grid_nes_rect[grid_nes_selected].Stroke.Thickness := 1;
  grid_nes_selected := -1;
end;

procedure TNES_ACTIONS.show_games_in_grid;
var
  count, vi, temp_x, temp_y: integer;
  temp_state: integer;
  is_grey: Boolean;
begin

  grid_nes_main := TRectangle.create(main.frm_main.rect_grid);
  grid_nes_main.Name := 'Nes_grid';
  grid_nes_main.Parent := main.frm_main.rect_grid;
  grid_nes_main.Align := TAlignLayout.Client;

  if count <> 0 then
  begin
    frm_splash.pb_splash_progress.Min := 0;
    frm_splash.pb_splash_progress.Max := count;

    SetLength(grid_nes_rect, count + 1);
    SetLength(grid_nes_img, count + 1);
    SetLength(grid_nes_img_gray, count + 1);
    SetLength(grid_nes_text, count + 1);
    SetLength(grid_nes_state, count + 1);

    grid_nes_selected := -1;
    rom_nes_selected := '';

    vi := 0;
    temp_x := 0;
    temp_y := 0;

    dm.tNes.First;

    while not dm.tNes.Eof do
    begin
      is_grey := False;

      dm.tNesMedia.Locate('rom', dm.tNesrom.AsString);

//      if dm.tNesMediabox_art.AsString = '' then
//        dm.tNesMediabox_art.AsString := config.emu_path[0].box_art + 'not_found.png';

      if not Assigned(grid_nes_rect[vi]) then
      begin
        grid_nes_rect[vi] := TRectangle.create(grid_nes_main);
        grid_nes_rect[vi].Name := 'Grid_Rect_Nes_Game_' + vi.ToString;
        grid_nes_rect[vi].Parent := grid_nes_main;
        grid_nes_rect[vi].SetBounds(10 + (temp_x * 204), 20 + (temp_y * 309), 202, 307);
        grid_nes_rect[vi].Fill.Color := $FF2B3A4F;
        grid_nes_rect[vi].Fill.Kind := TBrushKind.Solid;
        grid_nes_rect[vi].OnClick := Nes_Mouse.OnMouseClick;
        grid_nes_rect[vi].OnMouseEnter := Nes_Mouse.OnMouseEnter;
        grid_nes_rect[vi].OnMouseLeave := Nes_Mouse.OnMouseLeave;
        grid_nes_rect[vi].Tag := vi;
        grid_nes_rect[vi].TagString := dm.tNesrom.AsString;
      end;

      if not Assigned(grid_nes_img[vi]) then
      begin
        grid_nes_img[vi] := TImage.create(grid_nes_rect[vi]);
        grid_nes_img[vi].Name := 'Grid_Img_Nes_Game_' + vi.ToString;
        grid_nes_img[vi].Parent := grid_nes_rect[vi];
        grid_nes_img[vi].SetBounds(6, 8, 186, 245);
        grid_nes_img[vi].HitTest := False;
        grid_nes_img[vi].Bitmap.LoadFromFile(dm.tNesMediabox_art.AsString);
        grid_nes_img_gray[vi] := TMonochromeEffect.create(grid_nes_img[vi]);
        grid_nes_img_gray[vi].Name := 'Grid_Img_Gray_Game_' + vi.ToString;
        grid_nes_img_gray[vi].Parent := grid_nes_img[vi];
        grid_nes_img_gray[vi].Enabled := is_grey;
      end;

      if not Assigned(grid_nes_state[vi]) then
      begin
        grid_nes_state[vi] := TRectangle.create(grid_nes_rect[vi]);
        grid_nes_state[vi].Name := 'Grid_Rect_State_' + vi.ToString;
        grid_nes_state[vi].Parent := grid_nes_rect[vi];
        grid_nes_state[vi].SetBounds(grid_nes_rect[vi].Width - 30, 10, 20, 20);
        grid_nes_state[vi].Stroke.Thickness := 2;
        grid_nes_state[vi].XRadius := 10;
        grid_nes_state[vi].YRadius := 10;
        case temp_state of
          1:
            grid_nes_state[vi].Fill.Color := $CC5C93ED;
          2:
            grid_nes_state[vi].Fill.Color := $CCF2D624;
          3:
            grid_nes_state[vi].Fill.Color := $CCFF0000;
        end;
        grid_nes_state[vi].HitTest := False;
      end;

      if not Assigned(grid_nes_text[vi]) then
      begin
        grid_nes_text[vi] := TText.create(grid_nes_rect[vi]);
        grid_nes_text[vi].Name := 'Grid_Txt_Nes_Game_' + vi.ToString;
        grid_nes_text[vi].Parent := grid_nes_rect[vi];
        grid_nes_text[vi].Height := 50;
        grid_nes_text[vi].Align := TAlignLayout.Bottom;
        grid_nes_text[vi].TextSettings.Font.Family := 'BigPartyO2Green';
        grid_nes_text[vi].TextSettings.Font.Size := 24;
        grid_nes_text[vi].TextSettings.WordWrap := True;
        grid_nes_text[vi].TextSettings.Trimming := TTextTrimming.Character;
        grid_nes_text[vi].Text := dm.tNesrom.AsString;
        grid_nes_text[vi].HitTest := False;
      end;

      if temp_x = 8 then
      begin
        temp_x := 0;
        inc(temp_y);
      end
      else
        inc(temp_x);

      inc(vi);
      frm_splash.lbl_splash_progress.Text := 'Game : "' + dm.tNesrom.AsString + '" loaded.';
      frm_splash.pb_splash_progress.Value := vi;
      Application.ProcessMessages;
      dm.tNes.Next;
    end;
  end;
end;

{ TNES_MOUSE }

procedure TNES_MOUSE.OnMouseClick(Sender: TObject);
begin
  if Nes_Action.grid_nes_selected <> -1 then
  begin
    Nes_Action.grid_nes_rect[Nes_Action.grid_nes_selected].Fill.Color := $FF2B3A4F;
    Nes_Action.grid_nes_rect[Nes_Action.grid_nes_selected].Stroke.Thickness := 1;
    Nes_Action.grid_nes_rect[Nes_Action.grid_nes_selected].Stroke.Color := TAlphaColorRec.Black;
  end;

  (Sender as TRectangle).Fill.Color := $FF375278;
  Nes_Action.grid_nes_selected := (Sender as TRectangle).Tag;
  Nes_Action.rom_nes_selected := (Sender as TRectangle).TagString;
  Nes_Action.create_nes_game_info(Nes_Action.rom_nes_selected);
  main.frm_main.lbl_selected_info.Text := 'Selected : ';
  main.frm_main.lbl_selected_info_value.Text := Nes_Action.grid_nes_text[Nes_Action.grid_nes_selected].Text;
end;

procedure TNES_MOUSE.OnMouseEnter(Sender: TObject);
begin
  if (Sender as TRectangle).Tag <> Nes_Action.grid_nes_selected then
  begin
    (Sender as TRectangle).Stroke.Thickness := 3;
    (Sender as TRectangle).Stroke.Color := $FF44BEB0;
    (Sender as TRectangle).Cursor := crHandPoint;
  end;
end;

procedure TNES_MOUSE.OnMouseLeave(Sender: TObject);
begin
  if (Sender as TRectangle).Tag <> Nes_Action.grid_nes_selected then
  begin
    (Sender as TRectangle).Stroke.Color := TAlphaColorRec.Black;
    (Sender as TRectangle).Stroke.Thickness := 1;
  end;
end;

end.
