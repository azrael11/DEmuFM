unit uarcade_actions;

interface

uses
  FMX.ListView.Appearances,
  FMX.ListView.Types,
  FMX.Objects,
  System.SysUtils,
  System.UITypes,
  FMX.Types,
  FMX.Filter.Effects,
  FMX.Dialogs,
  FMX.Ani,
  SDL2,
  SDL2_Image,
  System.IniFiles,
  vars_consts;

type
  TLOCAL_BEZEL_VARS = record
    res: string;
    image_w: integer;
    image_h: integer;
    visible_area_x: integer;
    visible_area_y: integer;
    overlays: integer;
    overlay_num: array of string;
    overlay_num_full: array of boolean;
    overlay_num_descs: array of integer;
  end;

type
  TARCADE_ACTIONS = class
  private
    procedure createFistConfig;
  protected

  public
    old_line, vis_up, vis_down, grid_selected: integer;
    grid_arcade_main: TRectangle;
    grid_rect: array of TRectangle;
    grid_img_gray: array of TMonochromeEffect;
    grid_img: array of TImage;
    grid_img_ani: array of TFloatAnimation;
    grid_text: array of TText;
    grid_state: array of TRectangle;
    roms_to_run: array of boolean;

    constructor Create;
    destructor Destroy;

    procedure loadBezel;
    procedure setFullscreen;

    procedure Show_games_in_ListView; // deprecated must move to frontend actions
  end;

var
  arcadeAction: TARCADE_ACTIONS;
  bezel_ini: TIniFile;
  bezel_rec: TLOCAL_BEZEL_VARS;

implementation

uses
  main,
  umain_config,
  main_engine,
  uDataModule;

{ TARCADE_ACTIONS }

constructor TARCADE_ACTIONS.Create;
begin
  if dm.tArcadeConfig.RecordCount = 0 then
    createFistConfig;
end;

procedure TARCADE_ACTIONS.createFistConfig;
begin
  dm.tArcadeConfigfullscreen.AsInteger := 0;
  dm.tArcadeConfigfull_x.AsInteger := 1920;
  dm.tArcadeConfigfull_y.AsInteger := 1080;
  dm.tArcadeConfigbezels.AsInteger := 0;
  dm.tArcadeConfigsound.AsInteger := 1;
  dm.tArcadeConfig.Post;
  dm.tArcadeConfig.ApplyUpdates();
end;

destructor TARCADE_ACTIONS.Destroy;
begin
  // emu_active := emus_Arcade;
end;

procedure TARCADE_ACTIONS.loadBezel;
var
  vi: integer;
  fullCfgPath, fullImgPath: string;
  screenWidth, screenHeight: integer;
  scaledSurface: PSDL_Surface;
begin
  // Check bezel configuration and image paths
  fullCfgPath := dm.tConfigprj_media.AsString + 'arcade' + PathDelim + 'bezels' + PathDelim + dm.tArcaderom.AsString + '.cfg';
  fullImgPath := dm.tConfigprj_media.AsString + 'arcade' + PathDelim + 'bezels' + PathDelim + dm.tArcaderom.AsString + '.png';
  // fullCfgPath := dm.tArcadeConfigbezels_path.AsString + dm.tArcaderom.AsString + '.cfg';
  // fullImgPath := dm.tArcadeConfigbezels_path.AsString + dm.tArcaderom.AsString + '.png';

  if not FileExists(fullCfgPath) or not FileExists(fullImgPath) then
  begin
    main_engine.bezel_loading := False;
    Exit;
  end;

  bezel_ini := TIniFile.Create(fullCfgPath);

  bezel_rec.res := bezel_ini.ReadString('dimensions', 'res', bezel_rec.res);
  bezel_rec.image_w := bezel_ini.ReadInteger('dimensions', 'image_w', bezel_rec.image_w);
  bezel_rec.image_h := bezel_ini.ReadInteger('dimensions', 'image_h', bezel_rec.image_h);
  bezel_rec.visible_area_x := bezel_ini.ReadInteger('dimensions', 'visible_area_x', bezel_rec.visible_area_x);
  bezel_rec.visible_area_y := bezel_ini.ReadInteger('dimensions', 'visible_area_y', bezel_rec.visible_area_y);

  bezel_surface := SDL_GetWindowSurface(window_render);
  if not Assigned(bezel_surface) then
  begin
    // ShowMessage('Failed to get bezel surface: ' + SDL_GetError);
    main_engine.bezel_loading := False;
    Exit;
  end;

  bezel_img_surface := IMG_Load(PAnsiChar(AnsiString(fullImgPath)));
  if not Assigned(bezel_img_surface) then
  begin
    ShowMessage('Failed to load bezel image: ' + fullImgPath + ' - ' + SDL_GetError);
    main_engine.bezel_loading := False;
    Exit;
  end;

  screenWidth := bezel_surface^.w;
  screenHeight := bezel_surface^.h;

  if (bezel_rec.image_w <> screenWidth) or (bezel_rec.image_h <> screenHeight) then
  begin
    scaledSurface := SDL_CreateRGBSurface(0, screenWidth, screenHeight, bezel_img_surface^.format^.BitsPerPixel, bezel_img_surface^.format^.Rmask, bezel_img_surface^.format^.Gmask, bezel_img_surface^.format^.Bmask, bezel_img_surface^.format^.Amask);

    if not Assigned(scaledSurface) then
    begin
      // ShowMessage('Failed to create scaled surface: ' + SDL_GetError);
      SDL_FreeSurface(bezel_img_surface);
      main_engine.bezel_loading := False;
      Exit;
    end;

    SDL_UpperBlitScaled(bezel_img_surface, nil, scaledSurface, nil);
    SDL_FreeSurface(bezel_img_surface);
    bezel_img_surface := scaledSurface;
  end;

  bezel_img_rect.x := 0;
  bezel_img_rect.y := 0;
  bezel_img_rect.w := bezel_rec.image_w;
  bezel_img_rect.h := bezel_rec.image_h;

  main_engine.bezel_loading := True;
end;

procedure TARCADE_ACTIONS.setFullscreen;
begin
  if dm.tArcadeConfigfullscreen.AsInteger = 1 then
    dm.tArcadeConfigfullscreen.AsInteger := 0
  else
    dm.tArcadeConfigfullscreen.AsInteger := 1;
  dm.tArcadeConfig.Post;
end;

procedure TARCADE_ACTIONS.Show_games_in_ListView;
var
  item, lheader: TListViewItem;
  lnum, lrom, lyear, lmanufactor, ldescription: TlistItemText;
  lpic: TListItemImage;
begin
  main.frm_main.lv_main_list.Items.Clear;
  dm.tArcade.First;
  while not dm.tArcade.Eof do
  begin

    dm.tArcadeTGDB.Locate('rom', dm.tArcaderom.AsString);

    lheader := main.frm_main.lv_main_list.Items.Add;
    lheader.Purpose := TListItemPurpose.Header;
    item := main.frm_main.lv_main_list.Items.Add;
    lnum := item.Objects.FindObjectT<TlistItemText>('num');
    lnum.TagString := dm.tArcaderom.AsString;
    lnum.Font.Family := 'Showcard Gothic';
    lnum.Font.size := 16;
    lpic := item.Objects.FindObjectT<TListItemImage>('pic');
    lrom := item.Objects.FindObjectT<TlistItemText>('romname');
    lrom.Font.Family := 'Showcard Gothic';
    lrom.Font.size := 16;
    lrom.TagString := dm.tArcadeexe_num.AsString;
    lyear := item.Objects.FindObjectT<TlistItemText>('year');
    lyear.Font.Family := 'Showcard Gothic';
    lyear.Font.size := 16;
    lmanufactor := item.Objects.FindObjectT<TlistItemText>('manufactor');
    lmanufactor.Font.Family := 'Showcard Gothic';
    lmanufactor.Font.size := 16;
    ldescription := item.Objects.FindObjectT<TlistItemText>('description');
    ldescription.Font.Family := 'Showcard Gothic';
    ldescription.Font.size := 16;
    ldescription.TextVertAlign := TTextAlign.Leading;
    ldescription.WordWrap := True;
    ldescription.Trimming := TTextTrimming.Character;
    ldescription.SelectedTextColor := TAlphaColorRec.Red;
    lheader.Text := dm.tArcadename.AsString;
    lnum.Text := dm.tArcadenum.AsString;
    // if temp_path <> '' then
    // begin
    // temp_Bitmap := TBitmap.Create;
    // // if FileExists(temp_path) then
    // // temp_Bitmap.LoadFromFile(temp_path)
    // // else
    // // temp_Bitmap.LoadFromFile(config.emu_path[0].images + 'not_found.png');
    // if temp_rom_path <> '' then
    // begin
    // lpic.Bitmap := temp_Bitmap;
    // lpic.TagString := 'run';
    // end
    // else if (temp_rom_path = '') and (FileExists(temp_path)) then
    // begin
    // temp_Bitmap := prj_functions.ConvertToGrayscale(temp_Bitmap, algluminosity);
    // lpic.Bitmap := temp_Bitmap;
    // lpic.TagString := 'no';
    // end;
    // end;
    lrom.Text := dm.tArcaderom.AsString;
    lyear.Text := dm.tArcadeyear.AsString;;
    lmanufactor.Text := dm.tArcademanufactor.AsString;;
    ldescription.Text := dm.tArcadeTGDBoverview.AsString;;

    dm.tArcade.Next;
  end;
end;

end.
