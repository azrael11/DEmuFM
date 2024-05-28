unit scraper_tgdb_opt;

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
  FMX.Layouts,
  FMX.Objects,
  FMX.Effects,
  FMX.StdCtrls,
  FMX.Ani,
  FMX.Controls.Presentation,
  uTheGamesDatabase,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FireDAC.Stan.Param,
  uInternet_files,
  System.IOUtils, System.Skia, FMX.Skia, System.Threading;

type
  TLIST_MOUSE = class
    procedure OnClick(Sender: TObject);
    procedure OnMouseEnter(Sender: TObject);
    procedure OnMouseLeave(Sender: TObject);
  end;

type
  TLIST_GAMES = record
    back: TRectangle;
    text: TText;
  end;

type
  Tfrm_scraper_tgdb_opt = class(TForm)
    rect_scraper_tgdb_opt: TRectangle;
    rect_scraper_tgdb_opt_header: TRectangle;
    lbl_scraper_tgdb_opt_header: TLabel;
    lbl_scraper_tgdb_opt_platform: TLabel;
    lbl_scraper_tgdb_opt_platform_value: TLabel;
    eff_shadow_scraper_tgdb_opt: TShadowEffect;
    rect_scraper_tgdb_opt_game_info: TRectangle;
    img_scraper_tgdb_opt_game_info: TImage;
    lbl_scraper_tgdb_opt_game_info: TLabel;
    vertsb_scraper_tgdb_opt_game_list: TVertScrollBox;
    eff_glow_scraper_tgdb_opt: TGlowEffect;
    rect_scraper_tgdb_opt_game_list: TRectangle;
    spb_scraper_apply: TSpeedButton;
    spb_scraper_cancel: TSpeedButton;
    lbl_scraper_tgdb_opt_year: TLabel;
    lbl_scraper_tgdb_opt_year_value: TLabel;
    lbl_scraper_tgdb_opt_publisher: TLabel;
    lbl_scraper_tgdb_opt_publisher_value: TLabel;
    memo_scraper_tgdb_opt_description: TMemo;
    arrowRight: TSkSvg;
    arrowLeft: TSkSvg;
    skai_scraper_tgdb_opt_wait: TSkAnimatedImage;
    procedure button_on_mouse_enter(Sender: TObject);
    procedure button_on_mouse_leave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure spb_scraper_cancelClick(Sender: TObject);
    procedure spb_scraper_applyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure arrowLeftMouseEnter(Sender: TObject);
    procedure arrowRightMouseEnter(Sender: TObject);
  private
    { Private declarations }
    result_found: integer;
    list_selected_item: integer;
    procedure save_click_to_temp(num: integer);
    procedure clear_temp;
  public
    { Public declarations }
    show_game_num: integer;
    store_in_temp: array [0 .. 20] of boolean;
    games_data: T_TGDB_SCRAPER_GAME;
    list_mouse: TLIST_MOUSE;
    lgames: array [0 .. 20] of TLIST_GAMES;
    procedure show_temp_data(num: integer);
    procedure show_results_for_gamename(game_data: T_TGDB_SCRAPER_GAME);
    procedure show_info_for_selected_gamename(game_data_s: T_TGDB_SCRAPER_GAME);
  end;

var
  frm_scraper_tgdb_opt: Tfrm_scraper_tgdb_opt;

implementation

uses
  umain_config,
  scraper_tgdb,
  uarcade_actions,
  main, front_main, uDataModule;

{$R *.fmx}
{ Tfrm_scraper_tgdb_opt }

procedure Tfrm_scraper_tgdb_opt.arrowLeftMouseEnter(Sender: TObject);
begin
  if (Sender as TSkSvg).Enabled then
    (Sender as TSkSvg).Cursor := crHandPoint;
end;

procedure Tfrm_scraper_tgdb_opt.arrowRightMouseEnter(Sender: TObject);
begin
  if (Sender as TSkSvg).Enabled then
    (Sender as TSkSvg).Cursor := crHandPoint;
end;

procedure Tfrm_scraper_tgdb_opt.button_on_mouse_enter(Sender: TObject);
begin
  eff_glow_scraper_tgdb_opt.Parent := (Sender as TSpeedButton);
  eff_glow_scraper_tgdb_opt.Enabled := True;
end;

procedure Tfrm_scraper_tgdb_opt.button_on_mouse_leave(Sender: TObject);
begin
  eff_glow_scraper_tgdb_opt.Enabled := False;
end;

procedure Tfrm_scraper_tgdb_opt.clear_temp;
var
  vi: integer;
begin
  for vi := 0 to High(lgames) do
  begin
    FreeAndNil(lgames[vi].text);
    FreeAndNil(lgames[vi].back);
  end;

  lbl_scraper_tgdb_opt_game_info.text := 'Game : ';
  lbl_scraper_tgdb_opt_platform_value.text := '';
  lbl_scraper_tgdb_opt_year_value.text := '';
  lbl_scraper_tgdb_opt_publisher_value.text := '';
  img_scraper_tgdb_opt_game_info.Bitmap := nil;
  memo_scraper_tgdb_opt_description.text := '';

  for vi := 0 to 20 do
    store_in_temp[vi] := False;
  // TDirectory.Delete(config.main.prj_temp, True);
  // TDirectory.CreateDirectory(config.main.prj_temp);
end;

procedure Tfrm_scraper_tgdb_opt.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  clear_temp;
  frm_scraper.spb_scraper_cancel.Enabled := True;
  frm_scraper.txt_scraper_info_game.Visible := False;
  frm_scraper.txt_scraper_info.Visible := False;
  frm_scraper.prbar_scraper.Visible := False;
  frm_scraper.spb_scraper_start.text := 'Start';
end;

procedure Tfrm_scraper_tgdb_opt.FormShow(Sender: TObject);
begin
  if Self.StyleBook = nil then
    Self.StyleBook := main.frm_main.stylebook_main;

  if result_found <> 0 then
    lgames[0].back.OnClick(lgames[0].back);
end;

procedure Tfrm_scraper_tgdb_opt.save_click_to_temp(num: integer);
var
  temp_file: TStringList;
begin
  store_in_temp[num] := True;
  TDirectory.CreateDirectory(dm.tConfigprj_temp.AsString + num.ToString);
  temp_file := TStringList.Create;
  temp_file.Add(lbl_scraper_tgdb_opt_game_info.text);
  temp_file.Add(lbl_scraper_tgdb_opt_platform_value.text);
  temp_file.Add(lbl_scraper_tgdb_opt_year_value.text);
  temp_file.Add(lbl_scraper_tgdb_opt_publisher_value.text);
  temp_file.Add('*');
  temp_file.Add(memo_scraper_tgdb_opt_description.text);
  temp_file.SaveToFile(dm.tConfigprj_temp.AsString + num.ToString + PathDelim + num.ToString + '_temp.txt');
  img_scraper_tgdb_opt_game_info.Bitmap.SaveToFile(dm.tConfigprj_temp.AsString + num.ToString + PathDelim +
    num.ToString + '.png');
end;

procedure Tfrm_scraper_tgdb_opt.show_info_for_selected_gamename(game_data_s: T_TGDB_SCRAPER_GAME);
var
  temp_bit: TBitmap;
begin
  temp_bit := nil;
  lbl_scraper_tgdb_opt_game_info.text := 'Game : ' + game_data_s.games[0].title;
  lbl_scraper_tgdb_opt_platform_value.text := vScraper_TGDB.get_platform_string_by_id
    (game_data_s.games[0].platform_id.ToInteger);
  if game_data_s.box_art.base_url.original <> '' then
  begin
    try
      temp_bit := uInternet_files.Get_Image_new(game_data_s.box_art.base_url.original +
        game_data_s.box_art.game[0].data[0].filename);
    finally
      img_scraper_tgdb_opt_game_info.Bitmap := temp_bit;
    end;
  end
  else
  begin
    // img_scraper_tgdb_opt_game_info.Bitmap.LoadFromFile(config.main.prj_images_path.main + 'not_found.png');
  end;
  lbl_scraper_tgdb_opt_year_value.text := game_data_s.games[0].release_date;
  if length(game_data_s.games[0].publishers) > 0 then
  begin
    lbl_scraper_tgdb_opt_publisher_value.text := vScraper_TGDB.get_publisher_by_id
      (game_data_s.games[0].publishers[0]);
  end
  else
  begin
    lbl_scraper_tgdb_opt_publisher_value.text := ' ';
  end;
  memo_scraper_tgdb_opt_description.text := game_data_s.games[0].overview;
  save_click_to_temp(show_game_num);
end;

procedure Tfrm_scraper_tgdb_opt.show_results_for_gamename(game_data: T_TGDB_SCRAPER_GAME);
var
  vi: integer;
  rom: string;

  procedure add_in_list(num: integer; name: string; not_found: boolean);
  begin
    lgames[num].back := TRectangle.Create(vertsb_scraper_tgdb_opt_game_list);
    lgames[num].back.name := 'Game_no_' + num.ToString;
    lgames[num].back.Parent := vertsb_scraper_tgdb_opt_game_list;
    lgames[num].back.SetBounds(0, 0 + (num * 35), vertsb_scraper_tgdb_opt_game_list.Width, 33);
    lgames[num].back.Fill.Color := $FF375278;
    lgames[num].back.Stroke.Thickness := 1;
    lgames[num].back.Tag := vi;
    if not_found = False then
    begin
      lgames[num].back.OnClick := list_mouse.OnClick;
      lgames[num].back.OnMouseEnter := list_mouse.OnMouseEnter;
      lgames[num].back.OnMouseLeave := list_mouse.OnMouseLeave;
    end;

    lgames[num].text := TText.Create(lgames[num].back);
    lgames[num].text.name := 'Name_Game_no_' + num.ToString;
    lgames[num].text.Parent := lgames[num].back;
    lgames[num].text.Align := TAlignLayout.Client;
    lgames[num].text.TextSettings.HorzAlign := TTextAlign.Center;
    lgames[num].text.TextSettings.Font.Size := 16;
    lgames[num].text.Font.Style := lgames[num].text.Font.Style + [TFontStyle.fsBold];
    lgames[num].text.TextSettings.FontColor := TAlphaColorRec.White;
    lgames[num].text.text := name;
    lgames[num].text.HitTest := False;
  end;

begin
  spb_scraper_apply.Enabled := True;
  rom := frm_main.lblInfoRomValue.text;
  lbl_scraper_tgdb_opt_header.text := 'Results for " ' + rom + ' "';
  show_game_num := -1;
  games_data := game_data;
  if game_data.count = '0' then
  begin
    add_in_list(0, 'Sorry, not results found.', True);
    spb_scraper_apply.Enabled := False;
  end
  else
  begin
    for vi := 0 to High(game_data.games) - 1 do
      add_in_list(vi, game_data.games[vi].title, False);
  end;
  result_found := game_data.count.ToInteger;
end;

procedure Tfrm_scraper_tgdb_opt.show_temp_data(num: integer);
var
  temp_file: TStringList;
  vi: integer;
begin
  // temp_dir := config.main.prj_temp + num.ToString + PathDelim;
  temp_file := TStringList.Create;
  temp_file.LoadFromFile(dm.tConfigprj_temp.AsString + num.ToString + PathDelim + num.ToString + '_temp.txt');
  for vi := 0 to temp_file.count - 1 do
  begin
    case vi of
      0:
        lbl_scraper_tgdb_opt_game_info.text := temp_file.Strings[vi];
      1:
        lbl_scraper_tgdb_opt_platform_value.text := temp_file.Strings[vi];
      2:
        lbl_scraper_tgdb_opt_year_value.text := temp_file.Strings[vi];
      3:
        lbl_scraper_tgdb_opt_publisher_value.text := temp_file.Strings[vi];
      5:
        memo_scraper_tgdb_opt_description.text := temp_file.Strings[vi];
    end;
  end;
  img_scraper_tgdb_opt_game_info.Bitmap.LoadFromFile(dm.tConfigprj_temp.AsString + num.ToString + PathDelim +
    num.ToString + '.png');
end;

procedure Tfrm_scraper_tgdb_opt.spb_scraper_cancelClick(Sender: TObject);
begin
  Close;
end;

procedure Tfrm_scraper_tgdb_opt.spb_scraper_applyClick(Sender: TObject);
var
  mTime: TDateTime;
  vId: string;
  t_games_data: T_TGDB_SCRAPER_GAME;
  t_game_images: T_TGDB_SCRAPER_GAME_IMAGES;
  time_now: string;
  vi: integer;
  temp_bit: TBitmap;
  imgBoxArtPath: String;

  vtype, side, filename, resolution: string;

  procedure saveBitmapToPath(path, side, imgfilename: string);
  var
    imgPath: string;
  begin
    temp_bit := uInternet_files.Get_Image_new(path);
    imgPath := dm.tArcadeConfigtgdb_images.AsString + imgfilename;
    if side = 'front' then
      imgBoxArtPath := imgPath;
    temp_bit.SaveToFile(imgPath);
  end;

  procedure addTotArcadeTGDB;
  begin
    dm.tArcadeTGDBid.Value := t_games_data.games[0].id.ToInteger;
    dm.tArcadeTGDBtitle.Value := t_games_data.games[0].title;
    dm.tArcadeTGDBrelease_date.Value := t_games_data.games[0].release_date;
    dm.tArcadeTGDBplatform_id.Value := t_games_data.games[0].platform_id;
    dm.tArcadeTGDBplayers.Value := t_games_data.games[0].players;
    dm.tArcadeTGDBoverview.Value := t_games_data.games[0].overview;
    dm.tArcadeTGDBlast_updated.Value := t_games_data.games[0].last_updated;
    dm.tArcadeTGDBrating.Value := t_games_data.games[0].rating;
    dm.tArcadeTGDBcoop.Value := t_games_data.games[0].coop;
    dm.tArcadeTGDByoutube.Value := t_games_data.games[0].youtube;
    dm.tArcadeTGDBos.Value := t_games_data.games[0].os;
    dm.tArcadeTGDBprocessor.Value := t_games_data.games[0].processor;
    dm.tArcadeTGDBram.Value := t_games_data.games[0].ram;
    dm.tArcadeTGDBvideo.Value := t_games_data.games[0].video;
    dm.tArcadeTGDBhdd.Value := t_games_data.games[0].hdd;
    dm.tArcadeTGDBsound.Value := t_games_data.games[0].sound;
    dm.tArcadeTGDBdevelopers.Value := t_games_data.games[0].developers[0];
    dm.tArcadeTGDBgenres.Value := t_games_data.games[0].genres[0];
    if t_games_data.games[0].publishers <> nil then
      dm.tArcadeTGDBpublishers.Value := t_games_data.games[0].publishers[0]
    else
      dm.tArcadeTGDBpublishers.Value := '';
    if t_games_data.games[0].alternates <> nil then
      dm.tArcadeTGDBalternates.Value := t_games_data.games[0].alternates[0]
    else
      dm.tArcadeTGDBalternates.Value := '';
  end;

begin
  mTime := TDateTime(now);
  vId := frm_scraper_tgdb_opt.games_data.games[list_selected_item].id;
  t_games_data := vScraper_TGDB.get_games_by_game_id(vId);
  time_now := DateTimeToStr(mTime);

  dm.tArcade.Edit;
  dm.tArcadeTGDBImages.Edit;

  dm.tArcadename.AsString := t_games_data.games[0].title;
  dm.tArcade.Post;
  dm.tArcade.ApplyUpdates();

  with dm.tArcadeTGDB do
  begin
    Locate('rom', dm.tArcaderom.AsString);
    if RecordCount <> 0 then
    begin
      Edit;
      addTotArcadeTGDB;
      Post;
      ApplyUpdates();
    end
    else
    begin
      Open;
      Insert;
      addTotArcadeTGDB;
      Post;
      ApplyUpdates();
      CommitUpdates;
    end;
  end;

  t_game_images := vScraper_TGDB.get_games_images(t_games_data.games[0].id);

  for vi := 0 to High(t_game_images.images) do
  begin
    dm.tArcadeTGDBImages.Edit;
    vtype := t_game_images.images[vi].vtype;
    side := t_game_images.images[vi].side;
    filename := t_game_images.images[vi].filename;
    resolution := t_game_images.images[vi].resolution;
    vId := t_games_data.games[0].id;
    if dm.tArcadeTGDBImages.Locate('id;game_id;rom;img_type;side;filename;resolution',
      VarArrayOf([vId.ToInteger, dm.tArcadeTGDBid.Value, dm.tArcaderom.Value, vtype, side, filename,
      resolution]), []) = False then
    begin
      dm.tArcadeTGDBImages.Open;
      dm.tArcadeTGDBImages.Insert;
      dm.tArcadeTGDBImagesid.Value := t_game_images.images[vi].id.ToInteger;
      dm.tArcadeTGDBImagesgame_id.AsString := dm.tArcadeTGDBid.Value.ToString;
      dm.tArcadeTGDBImagesrom.Value := dm.tArcaderom.AsString;
      dm.tArcadeTGDBImagesimg_type.Value := t_game_images.images[vi].vtype;
      dm.tArcadeTGDBImagesside.Value := t_game_images.images[vi].side;
      dm.tArcadeTGDBImagesfilename.Value := t_game_images.images[vi].filename;
      dm.tArcadeTGDBImagesresolution.Value := t_game_images.images[vi].resolution;
      saveBitmapToPath(t_games_data.box_art.base_url.large + t_game_images.images[vi].filename,
        t_game_images.images[vi].side, t_game_images.images[vi].filename);
      try
        dm.tArcadeTGDBImages.Post;
        dm.tArcadeTGDBImages.ApplyUpdates();
        dm.tArcadeTGDBImages.CommitUpdates;
      except
        on E: Exception do
        begin
          ShowMessage('Error ApplyUpdates : ' + E.Message);
        end
      end;
    end;
  end;

  TFile.Copy(dm.tConfigprj_temp.AsString + list_selected_item.ToString + PathDelim +
    list_selected_item.ToString + '.png', config.emu_path[0].box_art + dm.tArcaderom.AsString +
    '_original.png', True);

  if dm.tArcadeMedia.Locate('rom', dm.tArcaderom.AsString) then
  begin
    dm.tArcadeMedia.Edit;
    dm.tArcadeMediabox_art.AsString := imgBoxArtPath;
    dm.tArcadeMedia.Post;
    dm.tArcadeMedia.ApplyUpdates();
  end
  else
  begin
    dm.tArcadeMedia.Open;
    dm.tArcadeMedia.Insert;
    dm.tArcadeMediarom.AsString := dm.tArcaderom.AsString;
    dm.tArcadeMediabox_art.AsString := imgBoxArtPath;
    dm.tArcadeMedia.Post;
    dm.tArcadeMedia.ApplyUpdates();
    dm.tArcadeMedia.CommitUpdates;
  end;
  front_action.grid_img[front_action.grid_selected].Bitmap := nil;
  front_action.grid_img[front_action.grid_selected].Bitmap.LoadFromFile(imgBoxArtPath);
  front_action.grid_text[front_action.grid_selected].text := t_games_data.games[0].title;

  front_action.createInfo(dm.tArcaderom.AsString);

  spb_scraper_cancelClick(nil);
  scraper_tgdb.frm_scraper.spb_scraper_cancelClick(nil);
end;

{ TLIST_MOUSE }

procedure TLIST_MOUSE.OnClick(Sender: TObject);
var
  vId: string;
  t_games_data: T_TGDB_SCRAPER_GAME;
begin
  frm_scraper_tgdb_opt.skai_scraper_tgdb_opt_wait.Animation.Start;
  frm_scraper_tgdb_opt.img_scraper_tgdb_opt_game_info.Visible := False;
  TTask.Run(
    procedure
    begin
      { Some calculation that takes time }
      if frm_scraper_tgdb_opt.show_game_num <> (Sender as TRectangle).Tag then
      begin
        frm_scraper_tgdb_opt.show_game_num := (Sender as TRectangle).Tag;
        if frm_scraper_tgdb_opt.store_in_temp[frm_scraper_tgdb_opt.show_game_num] then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              frm_scraper_tgdb_opt.skai_scraper_tgdb_opt_wait.Animation.Stop;
              frm_scraper_tgdb_opt.img_scraper_tgdb_opt_game_info.Visible := True;
              frm_scraper_tgdb_opt.show_temp_data(frm_scraper_tgdb_opt.show_game_num);
              frm_scraper_tgdb_opt.list_selected_item := (Sender as TRectangle).Tag;
            end);
        end
        else
        begin
          vId := frm_scraper_tgdb_opt.games_data.games[(Sender as TRectangle).Tag].id;
          t_games_data := vScraper_TGDB.get_games_by_game_id(vId);
          TThread.Synchronize(nil,
            procedure
            begin
              frm_scraper_tgdb_opt.skai_scraper_tgdb_opt_wait.Animation.Stop;
              frm_scraper_tgdb_opt.img_scraper_tgdb_opt_game_info.Visible := True;
              frm_scraper_tgdb_opt.list_selected_item := (Sender as TRectangle).Tag;
              frm_scraper_tgdb_opt.show_info_for_selected_gamename(t_games_data);
            end);
        end;
      end;
    end);
end;

procedure TLIST_MOUSE.OnMouseEnter(Sender: TObject);
begin
  (Sender as TRectangle).Stroke.Thickness := 2;
  (Sender as TRectangle).Fill.Color := TAlphaColorRec.Darkgray;
  (Sender as TRectangle).Cursor := crHandPoint;
  frm_scraper_tgdb_opt.lgames[(Sender as TRectangle).Tag].text.TextSettings.FontColor := TAlphaColorRec.White;
end;

procedure TLIST_MOUSE.OnMouseLeave(Sender: TObject);
begin
  (Sender as TRectangle).Stroke.Thickness := 1;
  (Sender as TRectangle).Fill.Color := $FF375278;
  frm_scraper_tgdb_opt.lgames[(Sender as TRectangle).Tag].text.TextSettings.FontColor := TAlphaColorRec.Black;
end;

end.
