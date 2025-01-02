unit front_main;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.DateUtils,
  FMX.Objects,
  FMX.Effects,
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
  FMX.Skia,
  FMX.Layouts,
  Data.Bind.Components;

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
  TORIGINALINFODATA = record
    gName: string;
    gYear: string;
    gDeveloper: string;
    gPublisher: string;
    gGenre: string;
    gPlayers: string;
    gDescription: string;
    gProgress: string;
    gStateIcon: string;
  end;

type
  TFRONTEND = class
  private
    old_line, vis_up, vis_down: integer;
    grid_img_gray: array of TMonochromeEffect;
    roms_to_run: array of boolean;
    new_pic_path: string;
    actFilter: string;
    actSearch: string;

    procedure move_scrollbar(rect: TRectangle; move_type: TMOVE_TYPE);

    procedure editInfoElements(edit: boolean);

    procedure keepInfoOriginalData;

  public
    tmpTable, tmpTableConfig: TFDTable;
    arcadeGameInfo: array of TGAMEINFO;

    mouse: TFRONEND_MOUSE;

    splash: boolean;

    grid_selected: integer;
    prev_selected: integer;

    grid_img: array of TImage;
    grid_text: array of TText;
    grid_rect: array of TRectangle;
    grid_state: array of TRectangle;

    is_edited, is_contentlocked: boolean;

    imgSnap: array of TImage;
    imgSnapGlow: array of TGlowEffect;

    constructor Create;
    destructor Destroy;

    // grid actions
    procedure createGrid(emu: string);
    procedure destroy_grid;

    procedure selectedGame(DoubleClick: boolean; new_rect: TRectangle);
    procedure gridChange(Sender: TObject; const OldViewportPos, NewViewportPos: TPointF; const ContentSizeChanged: boolean);
    procedure key_down(var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure clear_grid;

    // Info
    procedure CreateInfoBindings;

    procedure createInfo(game: string);
    procedure editInfo(edit: boolean);
    procedure edit_img_doubleclick_info;
    procedure imgDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure imgDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure Rect_OnMouseEnter(Sender: TObject);
    procedure Rect_OnMouseLeave(Sender: TObject);
    procedure Rect_OnMouseClick(Sender: TObject);
    procedure ImgOnMouseClick(Sender: TObject);

    procedure clearAndRestoreOriginalData;

    procedure editInfoFree;
    procedure editInfoSave;

    // Filters
    procedure Filter(Filter: string);
    procedure searchGame;
    procedure updateGridAfterFilter;

  end;

var
  front_Action: TFRONTEND;
  oriInfoData: TORIGINALINFODATA;

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
  prev_selected := -1;
end;

procedure TFRONTEND.createGrid(emu: string);
var
  count, count_tgdb, vi: integer;
  temp_rom, temp_path, temp_name: string;
  temp_state: integer;
  temp_x, temp_y: integer;

  procedure addCover(img: TImage);
  var
    isLocated: boolean;
  begin
    dm.tArcadeTGDBImages.Active := true;
    if tmpTableConfig.FieldByName('select_cover').AsString = 'boxart' then
      isLocated := dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([tmpTable.FieldByName('rom').AsString, 'boxart', '']), [])
    else if tmpTableConfig.FieldByName('select_cover').AsString = 'boxart_front' then
      isLocated := dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([tmpTable.FieldByName('rom').AsString, 'boxart', 'front']), [])
    else if tmpTableConfig.FieldByName('select_cover').AsString = 'boxart_back' then
      isLocated := dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([tmpTable.FieldByName('rom').AsString, 'boxart', 'back']), [])
    else if tmpTableConfig.FieldByName('select_cover').AsString = 'banner' then
      isLocated := dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([tmpTable.FieldByName('rom').AsString, 'banner', '']), [])
    else if tmpTableConfig.FieldByName('select_cover').AsString = 'fanart' then
      isLocated := dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([tmpTable.FieldByName('rom').AsString, 'fanart', '']), [])
    else if tmpTableConfig.FieldByName('select_cover').AsString = 'clearlogo' then
      isLocated := dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([tmpTable.FieldByName('rom').AsString, 'clearlogo', '']), [])
    else if tmpTableConfig.FieldByName('select_cover').AsString = 'screenshot' then
      isLocated := dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([tmpTable.FieldByName('rom').AsString, 'screenshot', '']), []);

    if isLocated then
    begin
      if fileexists(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString) then
        img.Bitmap.LoadFromFile(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString)
      else
        img.Bitmap := frm_main.imgNotFound;
    end
    else
      img.Bitmap := frm_main.imgNotFound;

    dm.tArcadeTGDBImages.Active := False;
  end;

begin
  frm_main.vsb_grid.BeginUpdate;
  with dm do
  begin
    if tConfigcurrent_emu.AsString = 'arcade' then
    begin
      tmpTable := tArcade;
      tmpTable.Active := true;
      tmpTable.IndexFieldNames := 'name';
      tmpTableConfig := tArcadeConfig;
      tmpTableConfig.Active := true;
    end;
  end;

  SetLength(grid_rect, tmpTable.RecordCount + 1);
  SetLength(grid_img, tmpTable.RecordCount + 1);
  SetLength(grid_img_gray, tmpTable.RecordCount + 1);
  SetLength(grid_text, tmpTable.RecordCount + 1);
  SetLength(grid_state, tmpTable.RecordCount + 1);
  SetLength(roms_to_run, tmpTable.RecordCount + 1);
  SetLength(arcadeGameInfo, tmpTable.RecordCount + 1);

  grid_selected := -1;
  vi := 0;
  temp_x := 0;
  temp_y := 0;

  tmpTable.First;

  if splash then
  begin
    frm_splash.pb_splash_progress.Min := 0;
    frm_splash.pb_splash_progress.Max := tmpTable.RecordCount;
    frm_splash.pb_splash_progress.Value := 0;
  end;

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

    grid_img[vi] := TImage.Create(grid_rect[vi]);
    grid_img[vi].name := 'Grid_Img_Game_' + vi.ToString;
    grid_img[vi].Parent := grid_rect[vi];
    grid_img[vi].SetBounds(6, 6, 174, 230);
    addCover(grid_img[vi]);
    grid_img[vi].HitTest := False;

    grid_img_gray[vi] := TMonochromeEffect.Create(grid_img[vi]);
    grid_img_gray[vi].name := 'Grid_Img_Gray_Game_' + vi.ToString;
    grid_img_gray[vi].Parent := grid_img[vi];
    grid_img_gray[vi].Enabled := true;

    grid_state[vi] := TRectangle.Create(grid_rect[vi]);
    grid_state[vi].name := 'Grid_Rect_State_' + vi.ToString;
    grid_state[vi].Parent := grid_rect[vi];
    grid_state[vi].SetBounds(grid_rect[vi].Width - 30, 10, 20, 20);
    grid_state[vi].Stroke.Thickness := 1;
    grid_state[vi].XRadius := 10;
    grid_state[vi].YRadius := 10;
    case tmpTable.FieldByName('state_icon').AsInteger of
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
    grid_text[vi].TextSettings.WordWrap := true;
    grid_text[vi].TextSettings.Trimming := TTextTrimming.Character;
    grid_text[vi].Text := tmpTable.FieldByName('name').AsString;
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
      frm_splash.lbl_splash_progress.Text := 'Game : "' + tmpTable.FieldByName('name').AsString + '" loaded.';
      frm_splash.pb_splash_progress.Value := vi;
    end;

    if tmpTable.FieldByName('rom_path').AsString <> '' then
      if fileexists(tmpTable.FieldByName('rom_path').AsString) then
        grid_img_gray[vi].Enabled := False;

    arcadeGameInfo[vi].position := vi;
    arcadeGameInfo[vi].Arcade_canIRun := not grid_img_gray[vi].Enabled;
    arcadeGameInfo[vi].Arcade_runNumber := tmpTable.FieldByName('exe_num').AsInteger;
    arcadeGameInfo[vi].Arcade_RomName := tmpTable.FieldByName('rom').AsString;
    arcadeGameInfo[vi].Arcade_GameName := tmpTable.FieldByName('name').AsString;

    Application.ProcessMessages;
    inc(vi);
    tmpTable.Next;
  end;

  old_line := -100;
  vis_up := 0;
  vis_down := 4;
  actFilter := '';
  splash := False;
  frm_main.vsb_grid.EndUpdate;
  frm_main.rect_grid.Height := (temp_y * 290) + 12;
end;

procedure TFRONTEND.createInfo(game: string);
var
  devID, publID, genID: string;
  vi: integer;
  currDateTime: TDateTime;
begin
  is_contentlocked := true;
  frm_main.imgInfoUnlockContent.Bitmap := frm_main.imgLock;
  frm_main.spbInfoRemoveContent.Enabled := False;
  frm_main.spbInfoRemoveImages.Enabled := False;
  dm.tArcade.Locate('rom', game, []);
  if dm.tConfigscraper.AsString = 'tgdb' then
  begin
    dm.tArcadeTGDB.Active := true;
    dm.tTGDBDevelopers.Active := true;
    dm.tTGDBPublishers.Active := true;
    dm.tTGDBGenres.Active := true;
    dm.tArcadeTGDBImages.Active := true;
  end;
  dm.tArcadeTGDB.Locate('rom', game, []);
  devID := dm.tArcadeTGDB.FieldByName('developers').AsString;
  publID := dm.tArcadeTGDB.FieldByName('publishers').AsString;
  genID := dm.tArcadeTGDB.FieldByName('genres').AsString;
  if devID <> '' then
    dm.tTGDBDevelopers.Locate('id', devID, []);
  if publID <> '' then
    dm.tTGDBPublishers.Locate('id', publID, []);
  if genID <> '' then
    dm.tTGDBGenres.Locate('id', genID, []);

  frm_main.rectInfoProgressIconPlayable.Enabled := False;
  frm_main.geInfoProgressIconPlayable.Enabled := False;
  frm_main.rectInfoProgressIconMinor.Enabled := False;
  frm_main.geInfoProgressIconMinor.Enabled := False;
  frm_main.rectInfoProgressIconMajor.Enabled := False;
  frm_main.geInfoProgressIconMajor.Enabled := False;
  frm_main.rectInfoProgressIconNonPlayable.Enabled := False;
  frm_main.geInfoProgressIconNonPlayable.Enabled := False;

  case dm.tArcadestate_icon.AsInteger of
    0:
      begin
        frm_main.rectInfoProgressIconPlayable.Enabled := true;
        frm_main.geInfoProgressIconPlayable.Enabled := true;
      end;
    1:
      begin
        frm_main.rectInfoProgressIconMinor.Enabled := true;
        frm_main.geInfoProgressIconMinor.Enabled := true;
      end;
    2:
      begin
        frm_main.rectInfoProgressIconMajor.Enabled := true;
        frm_main.geInfoProgressIconMajor.Enabled := true;
      end;
    3:
      begin
        frm_main.rectInfoProgressIconNonPlayable.Enabled := true;
        frm_main.geInfoProgressIconNonPlayable.Enabled := true;
      end;
  end;

  if dm.tArcadeTGDBImages.Locate('rom; img_type; side', VarArrayOf([game, 'boxart', '']), []) then
    frm_main.imgGameInfoBoxart.Bitmap.LoadFromFile(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString)
  else
    frm_main.imgGameInfoBoxart.Bitmap := frm_main.imgNotFound;

  if dm.tArcadeTGDBImages.Locate('rom; img_type; side', VarArrayOf([game, 'clearlogo', '']), []) then
    frm_main.imgGameInfoLogo.Bitmap.LoadFromFile(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString)
  else
    frm_main.imgGameInfoLogo.Bitmap := frm_main.imgNotFound;

  if dm.tArcadeTGDBImages.Locate('rom; img_type; side', VarArrayOf([game, 'fanart', '']), []) then
    frm_main.imgGameInfoFanart.Bitmap.LoadFromFile(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString)
  else
    frm_main.imgGameInfoFanart.Bitmap := frm_main.imgNotFound;

  if dm.tArcadeTGDBImages.Locate('rom; img_type; side', VarArrayOf([game, 'banner', '']), []) then
    frm_main.imgGameInfoBanner.Bitmap.LoadFromFile(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString)
  else
    frm_main.imgGameInfoBanner.Bitmap := frm_main.imgNotFound;

  if dm.tArcadeTGDBImages.Locate('rom; img_type; side', VarArrayOf([game, 'boxart', 'front']), []) then
    frm_main.imgGameInfoBoxartFront.Bitmap.LoadFromFile(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString)
  else
    frm_main.imgGameInfoBoxartFront.Bitmap := frm_main.imgNotFound;

  if dm.tArcadeTGDBImages.Locate('rom; img_type; side', VarArrayOf([game, 'boxart', 'back']), []) then
    frm_main.imgGameInfoBoxartBack.Bitmap.LoadFromFile(dm.tArcadeTGDBImagespath.AsString + dm.tArcadeTGDBImagesfilename.AsString)
  else
    frm_main.imgGameInfoBoxartBack.Bitmap := frm_main.imgNotFound;

  dm.query.SQL.Clear;
  dm.query.SQL.Text := 'SELECT path,filename FROM arcade_tgdb_images WHERE rom=:rom AND img_type=:img_type';
  dm.query.ParamByName('rom').AsString := tmpTable.FieldByName('rom').AsString;
  dm.query.ParamByName('img_type').AsString := 'screenshot';
  dm.query.Open;

  vi := 0;
  if dm.query.RecordCount > 0 then
  begin
    SetLength(imgSnap, dm.query.RecordCount);
    SetLength(imgSnapGlow, dm.query.RecordCount);
    with dm.query do
    begin
      First;
      while not Eof do
      begin
        imgSnap[vi] := TImage.Create(frm_main.vsbImg);
        imgSnap[vi].name := 'imgSnap' + vi.ToString;
        imgSnap[vi].Parent := frm_main.vsbImg;
        imgSnap[vi].SetBounds(6, 20 + (vi * 237), 241, 217);
        imgSnap[vi].Bitmap.LoadFromFile(dm.query.FieldByName('path').AsString + dm.query.FieldByName('filename').AsString);
        imgSnap[vi].Tag := 20 + vi;
        imgSnap[vi].OnClick := ImgOnMouseClick;

        imgSnapGlow[vi] := TGlowEffect.Create(imgSnap[vi]);
        imgSnapGlow[vi].name := 'imgSnapGlow' + vi.ToString;
        imgSnapGlow[vi].Parent := imgSnap[vi];
        imgSnapGlow[vi].GlowColor := TAlphaColorRec.Red;
        imgSnapGlow[vi].Enabled := False;

        inc(vi);
        Next;
      end;
    end;
  end;

  currDateTime := StrToDateTime(dm.tArcadestate_date.AsString);
  frm_main.lblInfoLastUpdate.Text := 'Last UpDate: ' + FormatDateTime('dd/mm/yyyy - hh:nn:ss AM/PM', currDateTime);
end;

procedure TFRONTEND.CreateInfoBindings;
var
  linkControlGameName: TLinkFillControlToField;
  linkControlRomName: TLinkFillControlToField;
  linkControlYear: TLinkFillControlToField;
  linkControlDevelopers: TLinkFillControlToField;
  linkControlPublishers: TLinkFillControlToField;
  linkControlGenres: TLinkFillControlToField;
  linkControlPlayers: TLinkFillControlToField;
  linkControlCoop: TLinkFillControlToField;
  linkControlHiScore: TLinkFillControlToField;

  linkControlDescription: TLinkFillControlToField;
  linkControlDescState: TLinkFillControlToField;
begin
  linkControlGameName := TLinkFillControlToField.Create(frm_main);
  linkControlGameName.Control := frm_main.edtInfoGameName;
  linkControlGameName.DataSource := dm.bsDBArcade;
  linkControlGameName.FieldName := 'name';

  linkControlRomName := TLinkFillControlToField.Create(frm_main);
  linkControlRomName.Control := frm_main.edtInfoRomName;
  linkControlRomName.DataSource := dm.bsDBArcade;
  linkControlRomName.FieldName := 'rom';

  linkControlYear := TLinkFillControlToField.Create(frm_main);
  linkControlYear.Control := frm_main.edtInfoYear;
  linkControlYear.DataSource := dm.bsDBArcade;
  linkControlYear.FieldName := 'year';

  linkControlDevelopers := TLinkFillControlToField.Create(frm_main);
  linkControlDevelopers.Control := frm_main.ceInfoDeveloper;
  linkControlDevelopers.DataSource := dm.bsDBTGDBDevelopers;
  linkControlDevelopers.FieldName := 'name';

  linkControlPublishers := TLinkFillControlToField.Create(frm_main);
  linkControlPublishers.Control := frm_main.ceInfoPublisher;
  linkControlPublishers.DataSource := dm.bsDBTGDBPublishers;
  linkControlPublishers.FieldName := 'name';

  linkControlGenres := TLinkFillControlToField.Create(frm_main);
  linkControlGenres.Control := frm_main.ceInfoGenre;
  linkControlGenres.DataSource := dm.bsDBTGDBGenres;
  linkControlGenres.FieldName := 'name';

  linkControlPlayers := TLinkFillControlToField.Create(frm_main);
  linkControlPlayers.Control := frm_main.edtInfoPlayers;
  linkControlPlayers.DataSource := dm.bsDBArcadeTGDB;
  linkControlPlayers.FieldName := 'players';

  linkControlCoop := TLinkFillControlToField.Create(frm_main);
  linkControlCoop.Control := frm_main.edtInfoCoop;
  linkControlCoop.DataSource := dm.bsDBArcadeTGDB;
  linkControlCoop.FieldName := 'coop';

  linkControlHiScore := TLinkFillControlToField.Create(frm_main);
  linkControlHiScore.Control := frm_main.edtInfoHiScore;
  linkControlHiScore.DataSource := dm.bsDBArcade;
  linkControlHiScore.FieldName := 'hiscore';

  linkControlDescription := TLinkFillControlToField.Create(frm_main);
  linkControlDescription.Control := frm_main.memoDescription;
  linkControlDescription.DataSource := dm.bsDBArcadeTGDB;
  linkControlDescription.FieldName := 'overview';

  linkControlDescState := TLinkFillControlToField.Create(frm_main);
  linkControlDescState.Control := frm_main.memoProgress;
  linkControlDescState.DataSource := dm.bsDBArcade;
  linkControlDescState.FieldName := 'state_desc';
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
  begin
    grid_rect[vi].Visible := true;
    FreeAndNil(grid_rect[vi]);
  end;
end;

procedure TFRONTEND.clearAndRestoreOriginalData;
begin
  with frm_main do
  begin
    tmpTable.edit;
    edtInfoGameName.Text := oriInfoData.gName;
    edtInfoYear.Text := oriInfoData.gYear;
    ceInfoDeveloper.Text := oriInfoData.gDeveloper;
    ceInfoPublisher.Text := oriInfoData.gPublisher;
    ceInfoGenre.Text := oriInfoData.gGenre;
    edtInfoPlayers.Text := oriInfoData.gPlayers;
    memoDescription.Text := oriInfoData.gDescription;
    memoProgress.Text := oriInfoData.gProgress;
    tmpTable.FieldByName('state_icon').AsString := oriInfoData.gStateIcon;
  end;
end;

procedure TFRONTEND.imgDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if ((ExtractFileExt(Data.Files[0]) <> '.png') and (ExtractFileExt(Data.Files[0]) <> '.jpg')) then
    Operation := TDragOperation.None
  else
    Operation := TDragOperation.Copy;
end;

procedure TFRONTEND.imgDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  // main.frm_main.img_grid_info.Bitmap.LoadFromFile(Data.Files[0]);
  new_pic_path := Data.Files[0];
end;

procedure TFRONTEND.ImgOnMouseClick(Sender: TObject);
var
  vi, vou: integer;
begin
  if ((Sender as TImage).Tag >= 20) and ((Sender as TImage).Tag <= 30) then
  begin
    if is_edited then
    begin
      for vi := 0 to High(imgSnap) do
        imgSnapGlow[vi].Enabled := False;
      vou := (Sender as TImage).Tag;
      imgSnapGlow[vou - 20].Enabled := true;
    end;
  end;
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

procedure TFRONTEND.editInfo(edit: boolean);
begin
  if edit then
    keepInfoOriginalData
  else
    editInfoSave;
  editInfoElements(edit);
  is_edited := edit;
end;

procedure TFRONTEND.editInfoFree;
var
  vi: integer;
begin
  for vi := High(imgSnap) downto 0 do
  begin
    imgSnap[vi].Bitmap := nil;
    FreeAndNil(imgSnap[vi]);
  end;
  SetLength(imgSnap, 0);

  if dm.tConfigscraper.AsString = 'tgdb' then
  begin
    dm.tArcadeTGDB.Active := False;
    dm.tTGDBDevelopers.Active := False;
    dm.tTGDBPublishers.Active := False;
    dm.tTGDBGenres.Active := False;
    dm.tArcadeTGDBImages.Active := False;
  end;
end;

procedure TFRONTEND.editInfoElements(edit: boolean);
begin
  with frm_main do
  begin
    edtInfoGameName.ReadOnly := not edit;
    edtInfoGameName.Enabled := edit;

    edtInfoYear.Enabled := edit;
    edtInfoYear.ReadOnly := not edit;
    ceInfoDeveloper.Enabled := edit;
    ceInfoDeveloper.ReadOnly := not edit;
    ceInfoPublisher.Enabled := edit;
    ceInfoPublisher.ReadOnly := not edit;
    ceInfoGenre.Enabled := edit;
    ceInfoGenre.ReadOnly := not edit;
    edtInfoPlayers.Enabled := edit;
    edtInfoPlayers.ReadOnly := not edit;
    memoDescription.ReadOnly := not edit;
    memoProgress.ReadOnly := not edit;
    rectInfoProgressIconPlayable.Enabled := edit;
    rectInfoProgressIconMinor.Enabled := edit;
    rectInfoProgressIconMajor.Enabled := edit;
    rectInfoProgressIconNonPlayable.Enabled := edit;
    dtBoxart.Visible := edit;
    dtBanner.Visible := edit;
    dtLogo.Visible := edit;
    dtFanart.Visible := edit;
    dtBoxartFront.Visible := edit;
    dtBoxartBack.Visible := edit;
    rectInfoSnapshotAdd.Visible := edit;
    rectInfoSnapshotRemove.Visible := edit;

    spbInfoEditClear.Visible := edit;
    effMCInfoExit.Enabled := edit;
  end;
end;

procedure TFRONTEND.Filter(Filter: string);
var
  vi: integer;
  temp_x, temp_y: integer;

  function RemoveCondition(const Input: string; const Condition: string): string;
  var
    ResultString: string;
  begin
    ResultString := StringReplace(Input, Condition + ' or ', '', [rfReplaceAll, rfIgnoreCase]);
    ResultString := StringReplace(ResultString, ' or ' + Condition, '', [rfReplaceAll, rfIgnoreCase]);
    ResultString := StringReplace(ResultString, Condition, '', [rfReplaceAll, rfIgnoreCase]);
    Result := Trim(ResultString);
  end;

begin
  if ContainsText(actFilter, Filter) then
  begin
    if actFilter = 'state_icon=' + Filter then
      actFilter := ''
    else
      actFilter := RemoveCondition(actFilter, 'state_icon=' + Filter);
  end
  else
  begin
    if actFilter = '' then
      actFilter := 'state_icon=' + Filter
    else
      actFilter := actFilter + ' OR state_icon=' + Filter;
  end;

  tmpTable.Filtered := False;
  tmpTable.Filter := actFilter;
  tmpTable.Filtered := true;

  updateGridAfterFilter;
end;

procedure TFRONTEND.gridChange(Sender: TObject; const OldViewportPos, NewViewportPos: TPointF; const ContentSizeChanged: boolean);
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
    move_up := true
  else if NewViewportPos.Y > OldViewportPos.Y then
    move_down := true;

  if move_up then
  begin
    if now_line < vis_down then
    begin
      take_action := true;
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
      take_action := true;
      remove_line := vis_up;
      inc(vis_up);
      inc(vis_down);
    end;
  end;
end;

procedure TFRONTEND.keepInfoOriginalData;
begin
  dm.tArcadeTGDB.Locate('rom', tmpTable.FieldByName('rom').AsString, []);
  dm.tTGDBDevelopers.Locate('id', dm.tArcadeTGDBdevelopers.AsString, []);
  dm.tTGDBPublishers.Locate('id', dm.tArcadeTGDBpublishers.AsString, []);
  dm.tTGDBGenres.Locate('id', dm.tArcadeTGDBgenres.AsString, []);
  oriInfoData.gName := tmpTable.FieldByName('name').AsString;
  oriInfoData.gYear := tmpTable.FieldByName('year').AsString;
  oriInfoData.gDeveloper := dm.tTGDBDevelopersname.AsString;
  oriInfoData.gPublisher := dm.tTGDBPublishersname.AsString;
  oriInfoData.gGenre := dm.tTGDBGenresname.AsString;
  oriInfoData.gPlayers := dm.tArcadeTGDBplayers.AsString;
  oriInfoData.gDescription := dm.tArcadeTGDBoverview.AsString;
  oriInfoData.gProgress := dm.tArcadestate_desc.AsString;
  oriInfoData.gStateIcon := tmpTable.FieldByName('state_icon').AsString;
end;

procedure TFRONTEND.key_down(var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  set_key: string;
  vComp: TComponent;
  temp_selected: integer;
begin
  dm.tKeyboardFrontend.Active := true;
  if Key <> 0 then
    set_key := prj_functions.key_to_string(Key)
  else
    set_key := uppercase(KeyChar);

  if set_key = 'P' then
    main_actions.main_form_play;

  if set_key = 'Y' then
  begin
    if frm_main.lay_game.Visible then
      editInfoFree
    else
      front_Action.createInfo(front_Action.arcadeGameInfo[grid_rect[grid_selected].Tag].Arcade_RomName);
    main_actions.infoShow;
  end;

  if (set_key = dm.tKeyboardFrontendquit_dspfm.AsString) and (not(ssShift in Shift)) then
  begin
    if frm_main.lay_game.Visible then
    begin
      editInfoFree;
      main_actions.infoShow;
    end
    else
      frm_main.Close;
  end
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
      temp_selected := grid_selected - 10;
    end;
    vComp := frm_main.rect_grid.FindComponent('grid_game_' + temp_selected.ToString);
    selectedGame(False, vComp as TRectangle);
    move_scrollbar(vComp as TRectangle, MT_UP);
  end
  else if set_key = dm.tKeyboardFrontendmove_down.AsString then
  begin
    if grid_selected > High(grid_rect) - 10 then
      temp_selected := grid_selected
    else
    begin
      case grid_selected of
        - 1:
          temp_selected := 0;
      else
        temp_selected := grid_selected + 10;
      end;
    end;
    vComp := frm_main.rect_grid.FindComponent('grid_game_' + temp_selected.ToString);
    selectedGame(False, vComp as TRectangle);
    if grid_selected > 9 then
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
    selectedGame(False, vComp as TRectangle);
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
    selectedGame(False, vComp as TRectangle);
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
  begin
    frm_main.txtSTBLastPlayed.Visible := not frm_main.txtSTBLastPlayed.Visible;
    frm_main.txtSTBPlayTime.Visible := not frm_main.txtSTBPlayTime.Visible;
    frm_main.txtSTBPlayCounts.Visible := not frm_main.txtSTBPlayCounts.Visible;
  end
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
      frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.position.X, scrollbar_y);
    MT_DOWN:
      frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.position.X, -scrollbar_y);
    MT_LEFT:
      begin
        times := (rect.Tag + 1) / 9;
        if Frac(times) = 0 then
          frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.position.X, scrollbar_y);
      end;
    MT_RIGHT:
      begin
        times := rect.Tag / 9;
        if (Frac(times) = 0) and (times <> 0) then
          frm_main.vsb_grid.ScrollTo(frm_main.vsb_grid.position.X, -scrollbar_y);
      end;
  end;
end;

procedure TFRONTEND.Rect_OnMouseClick(Sender: TObject);
begin
  if (Sender as TRectangle).Tag in [0 .. 3] then
  begin
    tmpTable.edit;
    with frm_main do
    begin
      geInfoProgressIconPlayable.Enabled := False;
      geInfoProgressIconMinor.Enabled := False;
      geInfoProgressIconMajor.Enabled := False;
      geInfoProgressIconNonPlayable.Enabled := False;

      if is_edited then
      begin
        case (Sender as TRectangle).Tag of
          0:
            begin
              tmpTable.FieldByName('state_icon').AsString := '0';
              geInfoProgressIconPlayable.Enabled := true;
              memoProgress.Text := 'Everything seems ok.';
            end;
          1:
            begin
              tmpTable.FieldByName('state_icon').AsString := '1';
              geInfoProgressIconMinor.Enabled := true;
              memoProgress.Text := 'Graphics: ' + #13#10 + 'Sound:' + #13#10 + 'Controls:';
            end;
          2:
            begin
              tmpTable.FieldByName('state_icon').AsString := '2';
              geInfoProgressIconMajor.Enabled := true;
              memoProgress.Text := 'Graphics: ' + #13#10 + 'Sound:' + #13#10 + 'Controls:' + #13#10 + 'Core:' + #13#10 + 'Hang:';
            end;
          3:
            begin
              tmpTable.FieldByName('state_icon').AsString := '3';
              geInfoProgressIconNonPlayable.Enabled := true;
              memoProgress.Text := 'Not working ' + #13#10 + 'Issues: ';
            end;
        end;
      end;
    end;
  end;
  if (Sender as TRectangle).Tag in [10 .. 11] then
  begin
    case (Sender as TRectangle).Tag of
      10:
        begin
          frm_main.od_main.Filter := 'jpg (*.jpg)|*.jpg|png (*.png)|*.png|All Supported image files (*.jpg, *.png)|*.jpg; *.png';
          frm_main.od_main.Title := 'Select image';
          frm_main.od_main.Execute;
        end;
      11:
        ;
    end;
  end;
end;

procedure TFRONTEND.Rect_OnMouseEnter(Sender: TObject);
begin
  if is_edited then
  begin
    (Sender as TRectangle).Cursor := crHandPoint;
  end;
end;

procedure TFRONTEND.Rect_OnMouseLeave(Sender: TObject);
begin
  //
end;

procedure TFRONTEND.editInfoSave;
begin
  dm.tArcadestate_date.AsString := DateTimeToStr(now);
  tmpTable.ApplyUpdates();
  tmpTableConfig.ApplyUpdates();
  dm.tArcadeTGDB.ApplyUpdates();
  dm.tArcadeTGDBImages.ApplyUpdates();
end;

procedure TFRONTEND.searchGame;
var
  gameSearch: string;
  vi: integer;
  temp_x, temp_y: integer;
begin;
  gameSearch := uppercase(frm_main.edt_search.Text + '%');
  tmpTable.Filtered := False;
  tmpTable.Filter := 'UPPER(name) LIKE ' + QuotedStr(gameSearch);
  tmpTable.Filtered := true;

  updateGridAfterFilter;
end;

procedure TFRONTEND.selectedGame(DoubleClick: boolean; new_rect: TRectangle);
var
  currDateTime : TDateTime;
begin
  if grid_selected <> -1 then
  begin
    grid_rect[grid_selected].Fill.Color := $FF2B3A4F;
    grid_rect[grid_selected].Stroke.Thickness := 1;
    grid_rect[grid_selected].Stroke.Color := TAlphaColorRec.Black;
  end;

  new_rect.Fill.Color := $FF375278;
  grid_selected := new_rect.Tag;
  tmpTable.Locate('rom', arcadeGameInfo[new_rect.Tag].Arcade_RomName);
  frm_main.lbl_selected_info.Text := 'Selected : ';
  frm_main.lbl_selected_info_value.Text := grid_text[grid_selected].Text;
  if tmpTable.FieldByName('last_played').AsString = '' then
    frm_main.txtSTBLastPlayed.Text := 'Last Played: Never'
  else
  begin
    currDateTime := StrToDateTime(tmpTable.FieldByName('last_played').AsString);
    frm_main.txtSTBLastPlayed.Text := 'Last Played: ' + FormatDateTime('dd/mm/yyyy - hh:nn:ss AM/PM', currDateTime);
  end;
  frm_main.txtSTBPlayCounts.Text := 'Game Played: ' + tmpTable.FieldByName('play_times').AsString + ' times';

  frm_main.txtSTBPlayTime.Text := 'Total Time Played: ' + int_to_time(tmpTable.FieldByName('total_time').AsInteger);
  frm_main.edt_search.ResetFocus;
end;

procedure TFRONTEND.updateGridAfterFilter;
var
  vi, temp_x, temp_y: integer;
begin
  temp_x := 0;
  temp_y := 0;

  frm_main.vsb_grid.BeginUpdate;
  for vi := 0 to High(grid_rect) - 1 do
  begin
    if tmpTable.Locate('rom', arcadeGameInfo[vi].Arcade_RomName, []) then
    begin
      grid_rect[vi].Visible := true;
      grid_rect[vi].position.X := temp_x * 189;
      grid_rect[vi].position.Y := temp_y * 290;
      if temp_x = 9 then
      begin
        temp_x := 0;
        inc(temp_y);
      end
      else
        inc(temp_x);
    end
    else
      grid_rect[vi].Visible := False;
  end;
  inc(temp_y);
  frm_main.vsb_grid.EndUpdate;
  frm_main.rect_grid.Height := (temp_y * 290) + 12;
end;

{ TFRONEND_MOUSE }

procedure TFRONEND_MOUSE.Click(Sender: TObject);
begin
  front_Action.selectedGame(False, Sender as TRectangle);
  front_Action.prev_selected := (Sender as TRectangle).Tag;
end;

procedure TFRONEND_MOUSE.DoubleClick(Sender: TObject);
begin
  front_Action.selectedGame(true, Sender as TRectangle);
  front_Action.prev_selected := (Sender as TRectangle).Tag;
  if front_Action.arcadeGameInfo[(Sender as TRectangle).Tag].Arcade_canIRun then
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
