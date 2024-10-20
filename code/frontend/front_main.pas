unit front_main;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  FMX.Objects,
  FMX.Filter.Effects,
  FMX.Ani,
  FMX.Types,
  FMX.Graphics,
  FMX.Forms,
  System.Types,
  System.UITypes,
  vars_consts,
  FireDAC.Comp.Client,
  Data.DB,
  FMX.Skia, FMX.Layouts;

const
  cs_icon: array [0 .. 3] of string = ('Working just fine', 'Working with minor errors', 'Working with major errors', 'Not working at all');

type
  TFILTER = (fil_All, fil_Working, fil_Working_With_Minor, fil_Working_With_Major, fil_Not_Working, fil_None);

type
  TSEARCH_TYPE = (st_search, st_filter);

type
  TMOVE_TYPE = (MT_UP, MT_DOWN, MT_LEFT, MT_RIGHT);

type
  TFRONEND_MOUSE = record
    procedure Click(Sender: TObject);
    procedure DoubleClick(Sender: TObject);
    procedure OnEnter(Sender: TObject);
    procedure OnLeave(Sender: TObject);
  end;

type
  TFRONTEND = class
  private
    old_line, vis_up, vis_down: integer;
    grid_img_gray: array of TMonochromeEffect;
    grid_state: array of TRectangle;
    roms_to_run: array of boolean;
    filters: array [0 .. 3] of TFILTER;
    new_filter: string;
    filter_order: string;
    new_pic_path: string;
    is_edited: boolean;

    procedure move_scrollbar(rect: TRectangle; move_type: TMOVE_TYPE);

    procedure elements_edit_info(edit: boolean);
    procedure keep_start_data_info;
    procedure save_into_database_info;

  public
    mouse: TFRONEND_MOUSE;
    // arcade_game: TArcadeGameInfo;
    console_game: TConsoleGameInfo;

    temp_arcade_data: TArcadeGameInfo;
    temp_console_data: TConsoleGameInfo;

    splash: boolean;
    arcade_game_run: integer;

    grid_selected: integer;
    prev_selected: integer;
    current_emu: string;
    // rom_selected: string;
    gamename: string;
    grid_img: array of TImage;
    grid_text: array of TText;

    grid_rect: array of TRectangle;

    old_search: string;

    constructor Create;
    destructor Destroy;

    // grid actions
    procedure create_grid(emu: string);
    procedure destroy_grid;

    procedure selected_game_in_grid(new_rect: TRectangle);
    procedure stop_game_playing;
    procedure grid_show_covers_first_time;
    procedure grid_view_change(Sender: TObject; const OldViewportPos, NewViewportPos: TPointF; const ContentSizeChanged: boolean);
    procedure key_down(var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure search_grid(search_type: TSEARCH_TYPE);
    procedure clear_grid;

    // Info
    procedure clearInfo;
    procedure createInfo(game: string);
    procedure edit_info(edit: boolean);
    procedure edit_img_doubleclick_info;
    procedure edit_dt_info_DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure edit_dt_info_Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure edit_clear_info;
    procedure Rect_OnMouseEnter(Sender: TObject);
    procedure Rect_OnMouseLeave(Sender: TObject);
    procedure Rect_OnMouseClick(Sender: TObject);

    // Filters
    procedure Filter(Filter: TFILTER);
  end;

var
  front_Action: TFRONTEND;
  // Keep_data: TGRID_INFO_DATA;

implementation

uses
  main,
  emu_functions,
  umain_config,
  splash,
  uarcade_actions,
  uTheGamesDatabase,
  umain_actions,
  multi_platform,
  prj_functions,
  controls_engine,
  configuration,
  config_controls,
  main_info,
  emulators,
  main_engine,
  uDataModule;

{ TFRONTEND }

procedure TFRONTEND.clearInfo;
begin
  with frm_main do
  begin
    lblInfoRomValue.Text := '';
//    lblInfoDeveloperValue.Text := '';
    lblInfoPublisherValue.Text := '';
    lblInfoYearValue.Text := '';
    lblInfoPlayersValue.Text := '';
    lblInfoCoopValue.Text := '';
    lblInfoGenreValue.Text := '';
    lblInfoHiScoreValue.Text := '';
    memoDescription.Lines.Clear;
    memoProgress.Lines.Clear;
    imgLogo.Bitmap := nil;
    img_game_sc_1.Bitmap := nil;
    img_game_sc_2.Bitmap := nil;
    img_game_sc_3.Bitmap := nil;
    img_game_sc_4.Bitmap := nil;
    img_game_sc_5.Bitmap := nil;
  end;
end;

procedure TFRONTEND.clear_grid;
var
  vi: integer;
begin
  for vi := 0 to High(grid_rect) - 1 do
  begin
    grid_rect[vi].Visible := False;
    grid_rect[vi].Fill.Color := $FF2B3A4F;
  end;
  main.frm_main.lbl_selected_info_value.Text := 'None';
end;

constructor TFRONTEND.Create;
begin
  old_search := '';
  prev_selected := -1;
end;

procedure TFRONTEND.create_grid(emu: string);
var
  count, count_tgdb, vi: integer;
  temp_rom, temp_path, temp_name: string;
  temp_state: integer;
  temp_x, temp_y: integer;
  tmpTable: TFDTable;
begin
  current_emu := emu;
  frm_main.vsb_grid.BeginUpdate;
  grid_selected := -1;
  with dm do
  begin
    if tConfigcurrent_emu.AsString = 'arcade' then
      tmpTable := tArcade;
    tmpTable.IndexFieldNames := 'rom';
  end;

  if splash then
  begin
    frm_splash.pb_splash_progress.Min := 0;
    frm_splash.pb_splash_progress.Max := count;
  end;

  SetLength(grid_rect, tmpTable.RecordCount + 1);
  SetLength(grid_img, tmpTable.RecordCount + 1);
  SetLength(grid_img_gray, tmpTable.RecordCount + 1);
  SetLength(grid_text, tmpTable.RecordCount + 1);
  SetLength(grid_state, tmpTable.RecordCount + 1);
  SetLength(roms_to_run, tmpTable.RecordCount + 1);

  grid_selected := -1;
  vi := 0;
  temp_x := 0;
  temp_y := 0;

  tmpTable.First;
  while not tmpTable.Eof do
  begin
    grid_rect[vi] := TRectangle.Create(frm_main.rect_grid);
    grid_rect[vi].name := 'grid_game_' + vi.ToString;
    grid_rect[vi].Parent := frm_main.rect_grid;
    grid_rect[vi].SetBounds(6 + (temp_x * 189), 6 + (temp_y * 290), 186, 286);
    grid_rect[vi].Fill.Color := $FF2B3A4F;
    grid_rect[vi].Fill.Kind := TBrushKind.Solid;
    grid_rect[vi].OnClick := front_Action.mouse.Click;
    grid_rect[vi].OnDblClick := front_Action.mouse.DoubleClick;
    grid_rect[vi].OnMouseEnter := front_Action.mouse.OnEnter;
    grid_rect[vi].OnMouseLeave := front_Action.mouse.OnLeave;
    grid_rect[vi].Tag := vi;
    grid_rect[vi].TagString := tmpTable.FieldByName('rom').AsString; // temp_rom;

    grid_img[vi] := TImage.Create(grid_rect[vi]);
    grid_img[vi].name := 'Grid_Img_Game_' + vi.ToString;
    grid_img[vi].Parent := grid_rect[vi];
    grid_img[vi].SetBounds(6, 6, 174, 230);
    grid_img[vi].HitTest := False;
    // Grey image not found
    grid_img_gray[vi] := TMonochromeEffect.Create(grid_img[vi]);
    grid_img_gray[vi].name := 'Grid_Img_Gray_Game_' + vi.ToString;
    grid_img_gray[vi].Parent := grid_img[vi];
    grid_img_gray[vi].Enabled := True;

    grid_state[vi] := TRectangle.Create(grid_rect[vi]);
    grid_state[vi].name := 'Grid_Rect_State_' + vi.ToString;
    grid_state[vi].Parent := grid_rect[vi];
    grid_state[vi].SetBounds(grid_rect[vi].Width - 30, 10, 20, 20);
    grid_state[vi].Stroke.Thickness := 2;
    grid_state[vi].XRadius := 10;
    grid_state[vi].YRadius := 10;
    case tmpTable.FieldByName('state_icon').AsInteger { temp_state } of
      0:
        grid_state[vi].Fill.Color := $FF43A22C;
      1:
        grid_state[vi].Fill.Color := $FF5C93ED;
      2:
        grid_state[vi].Fill.Color := $FFF2D624;
      3:
        grid_state[vi].Fill.Color := $FF940101;
    end;
    grid_state[vi].HitTest := False;

    grid_text[vi] := TText.Create(grid_rect[vi]);
    grid_text[vi].name := 'Grid_Txt_Game_' + vi.ToString;
    grid_text[vi].Parent := grid_rect[vi];
    grid_text[vi].Height := 50;
    grid_text[vi].Align := TAlignLayout.Bottom;
    grid_text[vi].TextSettings.Font.Family := 'BigPartyO2Green';
    grid_text[vi].TextSettings.Font.Size := 24;
    grid_text[vi].TextSettings.WordWrap := True;
    grid_text[vi].TextSettings.Trimming := TTextTrimming.Character;
    grid_text[vi].Text := tmpTable.FieldByName('name').AsString; // temp_name;
    grid_text[vi].HitTest := False;

    if temp_x = 9 then
    begin
      temp_x := 0;
      inc(temp_y);
    end
    else
      inc(temp_x);

    if splash then
    begin
      frm_splash.lbl_splash_progress.Text := 'Game : "' + tmpTable.FieldByName('name').AsString { temp_name }
        + '" loaded.';
      frm_splash.pb_splash_progress.Value := vi;
    end;

    if tmpTable.FieldByName('rom_path').AsString <> '' then
      if FileExists(tmpTable.FieldByName('rom_path').AsString) then
        grid_img_gray[vi].Enabled := False;

    Application.ProcessMessages;
    inc(vi);
    tmpTable.Next;
  end;

  old_line := -100;
  vis_up := 0;
  vis_down := 4;
  grid_show_covers_first_time;
  filters[0] := fil_All;
  filters[1] := fil_None;
  filters[2] := fil_None;
  filters[3] := fil_None;
  splash := False;
  new_filter := '';
  filter_order := '';
  frm_main.vsb_grid.EndUpdate;
  frm_main.vsb_grid.Height := (temp_y * 290) + 12;
end;

procedure TFRONTEND.createInfo(game: string);
var
  Cur_Bitmap, iBitmap: TBitmap;
  time: string;
  vi: integer;
  screenImg: string;
  getFromTGDB, getFromMedia: boolean;
begin
  getFromTGDB := False;
  clearInfo;

  with frm_main do
  begin
    dm.tArcade.Locate('rom', game);
    if dm.tArcadeTGDB.Locate('rom', game) then
      getFromTGDB := True;
    if dm.tArcadeMedia.Locate('rom', game) then
      getFromMedia := True;

    if (getFromMedia) and (dm.tArcadeMedia.FieldByName('box_art').AsString <> '') then
      imgMain.Bitmap.LoadFromFile(dm.tArcadeMedia.FieldByName('box_art').AsString)
    else
      imgMain.Bitmap.LoadFromFile(dm.tConfigprj_images_main.AsString + 'not_found.png');

    dm.tArcadeTGDBImages.Filtered := False;
    dm.tArcadeTGDBImages.Filter := 'rom = ' + game.QuotedString;
    dm.tArcadeTGDBImages.Filtered := True;
    if dm.tArcadeTGDBImages.RecordCount > 0 then
    begin
      while not dm.tArcadeTGDBImages.Eof do
      begin
        if dm.tArcadeTGDBImagesimg_type.AsString = 'clearlogo' then
          imgLogo.Bitmap.LoadFromFile(dm.tArcadeConfigtgdb_images.AsString + dm.tArcadeTGDBImagesfilename.AsString);
        dm.tArcadeTGDBImages.Next;
      end;
    end
    else
      imgLogo.Bitmap.LoadFromFile(dm.tConfigprj_images_main.AsString + 'not_found.png');

    dm.tArcadeTGDBImages.Filtered := False;
    dm.tArcadeTGDBImages.Filter := 'rom = ' + game.QuotedString + ' AND img_type = ' + QuotedStr('screenshot');
    dm.tArcadeTGDBImages.Filtered := True;

    vi := 0;
    if dm.tArcadeTGDBImages.RecordCount > 0 then
    begin
      dm.tArcadeTGDBImages.First;
      while not dm.tArcadeTGDBImages.Eof do
      begin
        screenImg := dm.tArcadeConfigtgdb_images.AsString + dm.tArcadeTGDBImagesfilename.AsString;
        case vi of
          0:
            if FileExists(screenImg) then
            begin
              img_game_sc_1.Bitmap.LoadFromFile(screenImg);
              inc(vi);
            end;
          1:
            if FileExists(screenImg) then
            begin
              img_game_sc_2.Bitmap.LoadFromFile(screenImg);
              inc(vi);
            end;
          2:
            if FileExists(screenImg) then
            begin
              img_game_sc_3.Bitmap.LoadFromFile(screenImg);
              inc(vi);
            end;
          3:
            if FileExists(screenImg) then
            begin
              img_game_sc_4.Bitmap.LoadFromFile(screenImg);
              inc(vi);
            end;
          4:
            if FileExists(screenImg) then
            begin
              img_game_sc_5.Bitmap.LoadFromFile(screenImg);
              inc(vi);
            end;
        end;
        dm.tArcadeTGDBImages.Next;
      end;
    end;

    dm.tArcadeTGDBImages.Filtered := False;
    dm.tArcadeTGDBImages.Filter := '';
    dm.tArcadeTGDBImages.Filtered := True;

    if getFromTGDB then
      lblInfoHeader.Text := dm.tArcadeTGDBtitle.AsString
    else
      lblInfoHeader.Text := dm.tArcadename.AsString;

    lblInfoRomValue.Text := dm.tArcaderom.AsString;
    if getFromTGDB then
    begin
      lblInfoPlayersValue.Text := dm.tArcadeTGDBplayers.AsString;
      lblInfoCoopValue.Text := dm.tArcadeTGDBcoop.AsString;
      lblInfoYearValue.Text := dm.tArcadeTGDBrelease_date.AsString;
      memoDescription.Text := dm.tArcadeTGDBoverview.AsString;
    end
    else
    begin
      lblInfoPlayersValue.Text := '';
      lblInfoCoopValue.Text := '';
      lblInfoYearValue.Text := '';
      memoDescription.Text := '';
    end;

    if getFromTGDB then
    begin
      if dm.tArcadeTGDBgenres.AsString <> '' then
      begin
        dm.tTGDBGenres.Locate('id', dm.tArcadeTGDBgenres.AsString);
        lblInfoGenreValue.Text := dm.tTGDBGenresname.AsString;
      end
      else
        lblInfoGenreValue.Text := '';
    end
    else
      lblInfoGenreValue.Text := '';

    if getFromTGDB then
    begin
      if dm.tArcadeTGDBdevelopers.AsString <> '' then
      begin
        dm.tTGDBDevelopers.Locate('id', dm.tArcadeTGDBdevelopers.AsString);
//        lblInfoDeveloperValue.Text := dm.tTGDBDevelopersname.AsString;
        ceInfoDeveloper.Text := dm.tTGDBDevelopersname.AsString;
      end
      else
//        lblInfoDeveloperValue.Text := '';
    end
    else
    begin
//      lblInfoDeveloperValue.Text := '';
    end;

    if getFromTGDB then
    begin
      if dm.tArcadeTGDBpublishers.AsString <> '' then
      begin
        dm.tTGDBPublishers.Locate('id', dm.tArcadeTGDBpublishers.AsString);
        lblInfoPublisherValue.Text := dm.tTGDBPublishersname.AsString;
      end
      else
        lblInfoPublisherValue.Text := '';
    end
    else
      lblInfoPublisherValue.Text := '';

    case dm.tArcadehiscore.AsInteger of
      1:
        lblInfoHiScoreValue.Text := 'Yes';
      0:
        lblInfoHiScoreValue.Text := 'No';
    end;
    lblProgress.Text := dm.tArcadestate.AsString;
    memoProgress.Text := dm.tArcadestate_desc.AsString;

    case dm.tArcadestate_icon.AsInteger of
      0:
        rect_grid_info_progress_1.Fill.Color := $FF43A22B;
      1:
        rect_grid_info_progress_1.Fill.Color := $FF5C93ED;
      2:
        rect_grid_info_progress_1.Fill.Color := $FF5C93ED;
      3:
        rect_grid_info_progress_1.Fill.Color := $FF940101;
    end;
    frm_main.lblProgress.Text := cs_icon[dm.tArcadestate_icon.AsInteger];

    frm_main.rect_grid_info_progress_2.Visible := False;
    frm_main.rect_grid_info_progress_3.Visible := False;
    frm_main.rect_grid_info_progress_4.Visible := False;
    frm_main.rect_grid_info_progress_select.Visible := False;
  end;

  pause_offgt_stopped := True;
  main.frm_main.txt_stb_main_total.Text := 'Total play time : ' + multi_platform.int_to_time(dm.tArcadetotal_time.AsInteger);
end;

destructor TFRONTEND.Destroy;
begin
  //
end;

procedure TFRONTEND.destroy_grid;
var
  vi: integer;
begin
  for vi := 0 to High(grid_rect) do
    FreeAndNil(grid_rect[vi]);
end;

procedure TFRONTEND.edit_clear_info;
begin
  if emu_active = emus_Arcade then
  begin
    dm.tArcadeMedia.Locate('rom', dm.tArcadename.AsString);
    dm.tArcadeTGDB.Locate('rom', dm.tArcadename.AsString);

    with frm_main do
    begin
      // main.frm_main.lbl_grid_info_header.Text := dm.tArcadename.AsString;
      // main.frm_main.img_grid_info.Bitmap.LoadFromFile(dm.tArcadeMediabox_art.AsString);
      new_pic_path := dm.tArcadeMediabox_art.AsString;
      ceInfoDeveloper.Text := dm.tArcadeTGDBdevelopers.AsString;
      edtInfoPublisher.Text := dm.tArcadeTGDBpublishers.AsString;
      edtInfoYear.Text := dm.tArcadeyear.AsString;
      edtInfoPlayers.Text := dm.tArcadeTGDBplayers.AsString;
      edtInfoGenre.Text := dm.tArcadeTGDBgenres.AsString;
      memoDescription.Text := dm.tArcadestate_desc.AsString;
    end;
  end;
end;

procedure TFRONTEND.edit_dt_info_DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if ((ExtractFileExt(Data.Files[0]) <> '.png') and (ExtractFileExt(Data.Files[0]) <> '.jpg')) then
    Operation := TDragOperation.None
  else
    Operation := TDragOperation.Copy;
end;

procedure TFRONTEND.edit_dt_info_Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  // main.frm_main.img_grid_info.Bitmap.LoadFromFile(Data.Files[0]);
  new_pic_path := Data.Files[0];
end;

procedure TFRONTEND.edit_img_doubleclick_info;
begin
  main.frm_main.od_main.Filter := 'Image Files (png, jpg)|*.png; *jpg| PNG Image |*.png| JPG Image |*jpg';
  if main.frm_main.od_main.Execute then
  begin
    // if main.frm_main.od_main.FileName <> '' then
    // main.frm_main.img_grid_info.Bitmap.LoadFromFile(main.frm_main.od_main.FileName);
  end;
end;

procedure TFRONTEND.edit_info(edit: boolean);
begin
  if edit then
    keep_start_data_info
  else
    save_into_database_info;
  elements_edit_info(edit);
  is_edited := edit;
end;

procedure TFRONTEND.elements_edit_info(edit: boolean);
begin
  with frm_main do
  begin
    edtInfoHeader.Visible := edit;
    dt_grid_info.Visible := edit;
    spbInfoEditClear.Visible := edit;
//    edtInfoDeveloper.Visible := edit;
    ceInfoDeveloper.Enabled := true;
    edtInfoPublisher.Visible := edit;
    edtInfoYear.Visible := edit;
    edtInfoPlayers.Visible := edit;
    edtInfoCoop.Visible := edit;
    edtInfoGenre.Visible := edit;
    if edit then
    begin
      edtInfoHeader.Text := lblInfoHeader.Text;
//      edtInfoDeveloper.Text := lblInfoDeveloperValue.Text;
      edtInfoPublisher.Text := lblInfoPublisherValue.Text;
      edtInfoYear.Text := lblInfoYearValue.Text;
      edtInfoPlayers.Text := lblInfoPlayersValue.Text;
      edtInfoCoop.Text := lblInfoCoopValue.Text;
      edtInfoGenre.Text := lblInfoGenreValue.Text;
    end
    else
    begin
      lblInfoHeader.Text := edtInfoHeader.Text;
//      lblInfoDeveloperValue.Text := edtInfoDeveloper.Text;
//      lblInfoPublisherValue.Text := edtInfoPublisher.Text;
      lblInfoYearValue.Text := edtInfoYear.Text;
      lblInfoPlayersValue.Text := edtInfoPlayers.Text;
      lblInfoCoopValue.Text := edtInfoCoop.Text;
      lblInfoGenreValue.Text := edtInfoGenre.Text;
    end;

    rect_grid_info_progress_2.Visible := edit;
    rect_grid_info_progress_3.Visible := edit;
    rect_grid_info_progress_4.Visible := edit;
    rect_grid_info_progress_select.Visible := edit;
    case edit of
      True:
        begin
          rect_grid_info_progress_1.Fill.Color := $FF43A22B;
          rect_grid_info_progress_2.Fill.Color := $FF5C93ED;
          rect_grid_info_progress_3.Fill.Color := $FFF2D624;
          rect_grid_info_progress_4.Fill.Color := $FF940101;
          case dm.tArcadestate_icon.AsInteger of
            0:
              rect_grid_info_progress_select.Position.X := 1;
            1:
              rect_grid_info_progress_select.Position.X := 36;
            2:
              rect_grid_info_progress_select.Position.X := 71;
            3:
              rect_grid_info_progress_select.Position.X := 106
          end;
        end;
      False:
        begin
          case dm.tArcadestate_icon.AsInteger of
            0:
              rect_grid_info_progress_1.Fill.Color := $FF43A22B;
            1:
              rect_grid_info_progress_1.Fill.Color := $FF5C93ED;
            2:
              rect_grid_info_progress_1.Fill.Color := $FFF2D624;
            3:
              rect_grid_info_progress_1.Fill.Color := $FF940101;
          end;
        end;
    end;

    memoDescription.ReadOnly := not edit;
    memoProgress.ReadOnly := not edit;
  end;
end;

procedure TFRONTEND.Filter(Filter: TFILTER);
var
  splitted_string: TStringDynArray;
  vi: integer;
begin
  filter_order := '';
  if Filter = fil_Working then
    if filters[0] = fil_Working then
      filters[0] := fil_None
    else
      filters[0] := Filter;
  if Filter = fil_Working_With_Minor then
    if filters[1] = fil_Working_With_Minor then
      filters[1] := fil_None
    else
      filters[1] := Filter;
  if Filter = fil_Working_With_Major then
    if filters[2] = fil_Working_With_Major then
      filters[2] := fil_None
    else
      filters[2] := Filter;
  if Filter = fil_Not_Working then
    if filters[3] = fil_Not_Working then
      filters[3] := fil_None
    else
      filters[3] := Filter;

  if ((filters[0] = fil_None) and (filters[1] = fil_None) and (filters[2] = fil_None) and (filters[3] = fil_None)) or
    ((filters[0] = fil_Working) and (filters[1] = fil_Working_With_Minor) and (filters[2] = fil_Working_With_Major) and (filters[3] = fil_Not_Working)) then
  begin
    filters[0] := fil_All;
    filters[1] := fil_None;
    filters[2] := fil_None;
    filters[3] := fil_None;
    clear_grid;
    frm_main.edt_search.Text := '';
    search_grid(st_search);
    new_filter := '';
    filter_order := '';
    frm_main.eff_mono_emu_working.Enabled := False;
    frm_main.eff_mono_emu_working_minor.Enabled := False;
    frm_main.eff_mono_emu_working_major.Enabled := False;
    frm_main.eff_mono_emu_not_working.Enabled := False;
  end
  else
  begin
    case Filter of
      fil_Working:
        new_filter := new_filter + ' 0 ';
      fil_Working_With_Minor:
        new_filter := new_filter + ' 1 ';
      fil_Working_With_Major:
        new_filter := new_filter + ' 2 ';
      fil_Not_Working:
        new_filter := new_filter + ' 3 ';
    end;

    if filters[0] = fil_Working then
      frm_main.eff_mono_emu_working.Enabled := False
    else
      frm_main.eff_mono_emu_working.Enabled := True;

    if filters[1] = fil_Working_With_Minor then
      frm_main.eff_mono_emu_working_minor.Enabled := False
    else
      frm_main.eff_mono_emu_working_minor.Enabled := True;

    if filters[2] = fil_Working_With_Major then
      frm_main.eff_mono_emu_working_major.Enabled := False
    else
      frm_main.eff_mono_emu_working_major.Enabled := True;

    if filters[3] = fil_Not_Working then
      frm_main.eff_mono_emu_not_working.Enabled := False
    else
      frm_main.eff_mono_emu_not_working.Enabled := True;

    splitted_string := SplitString(new_filter, ' ');
    for vi := 0 to High(splitted_string) do
      if splitted_string[vi] <> '' then
        filter_order := filter_order + splitted_string[vi] + ',';

    delete(filter_order, length(filter_order), 1);
    search_grid(st_filter);
  end;
end;

procedure TFRONTEND.grid_show_covers_first_time;
var
  vi: integer;
  img_path: string;
begin
  vi := 0;
  with dm.tArcade do
  begin
    First;
    while not Eof do
    begin
      if dm.tArcadeMedia.Locate('rom', dm.tArcade.FieldByName('rom').AsString) then
      begin
        if dm.tArcadeMediabox_art.AsString = '' then
          grid_img[vi].Bitmap.LoadFromFile(dm.tConfigprj_images_main.AsString + 'not_found.png')
        else
          grid_img[vi].Bitmap.LoadFromFile(dm.tArcadeMediabox_art.AsString);
      end
      else
        grid_img[vi].Bitmap.LoadFromFile(dm.tConfigprj_images_main.AsString + 'not_found.png');
      inc(vi);
      Next;
    end;
  end;
end;

procedure TFRONTEND.grid_view_change(Sender: TObject; const OldViewportPos, NewViewportPos: TPointF; const ContentSizeChanged: boolean);
var
  move_up, move_down, take_action: boolean;
  vi, now_line, remove_line: integer;
  rom_name, img_path, media_table: string;
  height_port: single;
begin
  move_up := False;
  move_down := False;
  now_line := Trunc((NewViewportPos.Y) / grid_rect[0].Height);

  if OldViewportPos.Y > NewViewportPos.Y then
    move_up := True
  else if NewViewportPos.Y > OldViewportPos.Y then
    move_down := True;

  if move_up then
  begin
    if now_line < vis_down then
    begin
      take_action := True;
      remove_line := vis_down + 5;
      if vis_up <> 0 then
        Dec(vis_up);
      if vis_down <> 4 then
        Dec(vis_down);
    end;
  end
  else if move_down then
  begin
    if now_line > vis_up then
    begin
      take_action := True;
      remove_line := vis_up;
      inc(vis_up);
      inc(vis_down);
    end;
  end;

  // media_table := current_emu + '_media';
  // if (old_line <> now_line) and take_action then
  // begin
  // for vi := 0 to covers do
  // if ((now_line * 9) + vi) < High(grid_rect) then
  // begin
  // dm.tArcadeMedia.Locate('rom', grid_rect[(now_line * 9) + vi].TagString);
  // if dm.tArcadeMediabox_art.AsString = '' then
  // img_path := dm.tConfigprj_images_main.AsString + 'not_found.png'
  // else
  // img_path := dm.tArcadeMediabox_art.AsString;
  // if grid_img[(now_line * 9) + vi].Bitmap.IsEmpty then
  // grid_img[(now_line * 9) + vi].Bitmap.LoadFromFile(img_path);
  // grid_img_ani[(now_line * 9) + vi].Enabled := True;
  // end;
  //
  // for vi := 0 to 8 do
  // begin
  // if ((remove_line * 9) + vi) < High(grid_rect) then
  // begin
  // grid_img_ani[(remove_line * 9) + vi].Enabled := False;
  // grid_img[(remove_line * 9) + vi].Bitmap := nil;
  // end;
  // end;
  //
  // old_line := now_line;
  // end;
end;

procedure TFRONTEND.keep_start_data_info;
begin
  // temp_arcade_data := arcade_game;
end;

procedure TFRONTEND.key_down(var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  set_key: string;
  vComp: TComponent;
  temp_selected: integer;
begin
  if Key <> 0 then
    set_key := prj_functions.key_to_string(Key)
  else
    set_key := uppercase(KeyChar);

  if set_key = 'P' then
    main_actions.main_form_play;

  if set_key = 'Y' then
    main_actions.main_form_grid_show;

  if set_key = dm.tKeyboardFrontendquit_dspfm.AsString then
    frm_main.Close
  else if set_key = dm.tKeyboardFrontendplay.AsString then
  begin

  end
  else if set_key = dm.tKeyboardFrontendmove_up.AsString then
  begin
    case grid_selected of
      - 1:
        temp_selected := 0;
      0 .. 8:
        temp_selected := grid_selected
    else
      temp_selected := grid_selected - 9;
    end;
    vComp := frm_main.rect_grid.FindComponent('grid_game_' + temp_selected.ToString);
    selected_game_in_grid(vComp as TRectangle);
    move_scrollbar(vComp as TRectangle, MT_UP);
  end
  else if set_key = dm.tKeyboardFrontendmove_down.AsString then
  begin
    if grid_selected > High(grid_rect) - 9 then
      temp_selected := grid_selected
    else
    begin
      case grid_selected of
        - 1:
          temp_selected := 0;
      else
        temp_selected := grid_selected + 9;
      end;
    end;
    vComp := frm_main.rect_grid.FindComponent('grid_game_' + temp_selected.ToString);
    selected_game_in_grid(vComp as TRectangle);
    if grid_selected > 8 then
      move_scrollbar(vComp as TRectangle, MT_DOWN);
  end
  else if set_key = dm.tKeyboardFrontendmove_left.AsString then
  begin
    case grid_selected of
      - 1, 0:
        temp_selected := 0;
    else
      temp_selected := grid_selected - 1;
    end;
    vComp := frm_main.rect_grid.FindComponent('grid_game_' + temp_selected.ToString);
    selected_game_in_grid(vComp as TRectangle);
    if grid_selected > -1 then
      move_scrollbar(vComp as TRectangle, MT_LEFT);
  end
  else if set_key = dm.tKeyboardFrontendmove_right.AsString then
  begin
    if grid_selected >= High(grid_rect) - 1 then
      temp_selected := grid_selected
    else
    begin
      case grid_selected of
        - 1:
          temp_selected := 0
      else
        temp_selected := grid_selected + 1;
      end;
    end;
    vComp := frm_main.rect_grid.FindComponent('grid_game_' + temp_selected.ToString);
    selected_game_in_grid(vComp as TRectangle);
    if grid_selected < High(grid_rect) - 1 then
      move_scrollbar(vComp as TRectangle, MT_RIGHT);
  end
  else if set_key = dm.tKeyboardFrontendshow_configuration.AsString then
    frm_config.ShowModal
  else if set_key = dm.tKeyboardFrontendshow_controls.AsString then
    frm_config_controls.ShowModal
  else if set_key = dm.tKeyboardFrontendshow_display.AsString then
  begin

  end
  else if set_key = dm.tKeyboardFrontendshow_information.AsString then
    frm_info.ShowModal
  else if set_key = dm.tKeyboardFrontendshow_hide_time_game.AsString then
    main.frm_main.txt_stb_main_total.Visible := not main.frm_main.txt_stb_main_total.Visible
  else if set_key = dm.tKeyboardFrontendplatform_emulators.AsString then
    frm_emu.ShowModal;
end;

procedure TFRONTEND.move_scrollbar(rect: TRectangle; move_type: TMOVE_TYPE);
var
  scrollbar_y: single;
  times: single;
begin
  scrollbar_y := rect.Height;
  case move_type of
    MT_UP:
      frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.Position.X, scrollbar_y);
    MT_DOWN:
      frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.Position.X, -scrollbar_y);
    MT_LEFT:
      begin
        times := (rect.Tag + 1) / 9;
        if Frac(times) = 0 then
          frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.Position.X, scrollbar_y);
      end;
    MT_RIGHT:
      begin
        times := rect.Tag / 9;
        if (Frac(times) = 0) and (times <> 0) then
          frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.Position.X, -scrollbar_y);
      end;
  end;
end;

procedure TFRONTEND.Rect_OnMouseClick(Sender: TObject);
begin
  if is_edited then
  begin
    case (Sender as TRectangle).Tag of
      0:
        frm_main.memoProgress.Text := 'Everything seems ok.';
      1:
        frm_main.memoProgress.Text := 'Graphics: ' + #13#10 + 'Sound:';
      2:
        frm_main.memoProgress.Text := 'Graphics: ' + #13#10 + 'Sound:' + #13#10 + 'Core:' + #13#10 + 'Hang:';
      3:
        frm_main.memoProgress.Text := 'Nothing seems working ok';
    end;
  end;
end;

procedure TFRONTEND.Rect_OnMouseEnter(Sender: TObject);
begin
  if is_edited then
  begin
    (Sender as TRectangle).Cursor := crHandPoint;
    frm_main.rect_grid_info_progress_select.Position.X := (Sender as TRectangle).Position.X - 4;
    frm_main.lblProgress.Text := cs_icon[(Sender as TRectangle).Tag];
  end;
end;

procedure TFRONTEND.Rect_OnMouseLeave(Sender: TObject);
begin
  case dm.tArcadestate_icon.AsInteger of
    0:
      frm_main.rect_grid_info_progress_select.Position.X := 1;
    1:
      frm_main.rect_grid_info_progress_select.Position.X := 36;
    2:
      frm_main.rect_grid_info_progress_select.Position.X := 71;
    3:
      frm_main.rect_grid_info_progress_select.Position.X := 106;
  end;
  frm_main.lblProgress.Text := cs_icon[dm.tArcadestate_icon.AsInteger];
end;

procedure TFRONTEND.save_into_database_info;
var
  now_time: TDateTime;
  dev, pub, genr: string;
  emu, emu_tgdb, emu_media: string;
  name: string;
begin
  with frm_main do
  begin
    emu := Emulation_Name(emu_active);
    emu_tgdb := emu + '_tgdb';
    emu_media := emu + '_media';

//    dev := vScraper_TGDB.get_id_developer_by_name(edtInfoDeveloper.Text);
    pub := vScraper_TGDB.get_id_publisher_by_name(edtInfoPublisher.Text);
    genr := vScraper_TGDB.get_id_genre_by_name(edtInfoGenre.Text);

    dm.tArcadeTGDB.Locate('rom', dm.tArcaderom.AsString);

    dm.tArcadeTGDBrelease_date.AsString := lblInfoYearValue.Text;
    dm.tArcadeTGDBplayers.AsString := lblInfoPlayersValue.Text;
    dm.tArcadeTGDBoverview.AsString := memoDescription.Text;
    dm.tArcadeTGDBcoop.AsString := lblInfoCoopValue.Text;
    dm.tArcadeTGDBdevelopers.AsString := dev;
    dm.tArcadeTGDBpublishers.AsString := pub;
    dm.tArcadeTGDBgenres.AsString := genr;
    dm.tArcadeTGDBlast_updated.AsString := DateTimeToStr(now);
    dm.tArcadeTGDB.Post;
    dm.tArcadeTGDB.ApplyUpdates();

    // if main.frm_main.lbl_grid_info_header.Text = '' then
    // name := gamename
    // else
    // name := main.frm_main.lbl_grid_info_header.Text;

    dm.tArcadename.AsString := name;
    dm.tArcadestate.AsString := lblProgress.Text;
    dm.tArcadestate_desc.AsString := memoProgress.Text;
    dm.tArcade.Post;
    dm.tArcade.ApplyUpdates();

    dm.tArcadeMedia.Locate('rom', dm.tArcaderom.AsString);
    dm.tArcadeMediabox_art.AsString := new_pic_path;
    dm.tArcadeMedia.Post;
    dm.tArcadeMedia.ApplyUpdates();

    if new_pic_path <> '' then
      grid_img[grid_selected].Bitmap.LoadFromFile(new_pic_path);
    grid_text[grid_selected].Text := name;
  end;
end;

procedure TFRONTEND.search_grid(search_type: TSEARCH_TYPE);
var
  vi: integer;
  name: string;
begin;
  if search_type = st_search then
  begin
    name := frm_main.edt_search.Text + '%';
    dm.tArcade.Filtered := False;
    dm.tArcade.FilterOptions := dm.tArcade.FilterOptions + [foCaseInsensitive];
    dm.tArcade.Filter := 'name LIKE ' + QuotedStr(name);
    dm.tArcade.Filtered := True;
    dm.tArcade.First;
  end
  else if search_type = st_filter then
  begin
    filter_order := '(' + filter_order + ')';
    dm.tArcade.Filtered := False;
    dm.tArcade.FilterOptions := dm.tArcade.FilterOptions + [foCaseInsensitive];
    dm.tArcade.Filter := 'state_icon IN ' + filter_order;
    dm.tArcade.Filtered := True;
  end;

  if dm.tArcade.RecordCount > 0 then
  begin
    for vi := 0 to High(grid_rect) - 1 do
      if grid_rect[vi].TagString = dm.tArcaderom.AsString then
      begin
        selected_game_in_grid(grid_rect[vi]);
        frm_main.vsb_grid.ViewportPosition := PointF(frm_main.vsb_grid.ViewportPosition.X, grid_rect[vi].Position.Y);
        break;
      end;

    for vi := 0 to High(grid_rect) - 1 do
    begin
      grid_rect[vi].Visible := True;
      grid_img_gray[vi].Enabled := True;
    end;

    while not dm.tArcade.Eof do
    begin
      for vi := 0 to High(grid_rect) - 1 do
        if grid_rect[vi].TagString = dm.tArcaderom.AsString then
        begin
          grid_rect[vi].Visible := False;
          grid_img_gray[vi].Enabled := False;
          break;
        end;
      dm.tArcade.Next;
    end;
  end
  else
  begin
    if search_type = st_search then
    begin
      name := frm_main.edt_search.Text;
      delete(name, length(name), 1);
      frm_main.edt_search.Text := name;
      frm_main.edt_search.CaretPosition := length(frm_main.edt_search.Text);
    end;
  end;

end;

procedure TFRONTEND.selected_game_in_grid(new_rect: TRectangle);
begin
  if grid_selected <> -1 then
  begin
    grid_rect[grid_selected].Fill.Color := $FF2B3A4F;
    grid_rect[grid_selected].Stroke.Thickness := 1;
    grid_rect[grid_selected].Stroke.Color := TAlphaColorRec.Black;
  end;

  new_rect.Fill.Color := $FF375278;
  grid_selected := new_rect.Tag;
  dm.tArcade.Locate('rom', new_rect.TagString);
  // create_info(new_rect.TagString);
  frm_main.lbl_selected_info.Text := 'Selected : ';
  frm_main.lbl_selected_info_value.Text := grid_text[grid_selected].Text;
end;

procedure TFRONTEND.stop_game_playing;
begin
  grid_rect[grid_selected].Fill.Color := $FF2B3A4F;
  grid_rect[grid_selected].Stroke.Thickness := 1;
  grid_selected := -1;
end;

{ TFRONEND_MOUSE }

procedure TFRONEND_MOUSE.Click(Sender: TObject);
begin
  front_Action.selected_game_in_grid(Sender as TRectangle);
  front_Action.createInfo((Sender as TRectangle).TagString);
  front_Action.prev_selected := (Sender as TRectangle).Tag;
end;

procedure TFRONEND_MOUSE.DoubleClick(Sender: TObject);
begin
  front_Action.selected_game_in_grid(Sender as TRectangle);
  front_Action.prev_selected := (Sender as TRectangle).Tag;
  main_actions.main_form_play;
end;

procedure TFRONEND_MOUSE.OnEnter(Sender: TObject);
begin
  if (Sender as TRectangle).Tag <> front_Action.grid_selected then
  begin
    (Sender as TRectangle).Stroke.Thickness := 3;
    (Sender as TRectangle).Stroke.Color := $FF44BEB0;
    (Sender as TRectangle).Cursor := crHandPoint;
    frm_main.lbl_selected_info_value.Text := front_Action.grid_text[(Sender as TRectangle).Tag].Text;
  end
end;

procedure TFRONEND_MOUSE.OnLeave(Sender: TObject);
begin
  if (Sender as TRectangle).Tag <> front_Action.grid_selected then
  begin
    (Sender as TRectangle).Stroke.Color := TAlphaColorRec.Black;
    (Sender as TRectangle).Stroke.Thickness := 1;
  end
end;

end.
