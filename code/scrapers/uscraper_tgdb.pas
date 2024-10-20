unit uscraper_tgdb;

interface

uses
  System.Classes,
  System.SysUtils,
  uTheGamesDatabase,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Forms,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FireDAC.Stan.Param,
  uInternet_files,
  FMX.DialogService,
  System.UITypes,
  umain_config,
  vars_consts,
  FireDAC.Comp.Client;

type
  TTypeList = (TTL_Developers, TTL_Publishers, TTL_Genres);

type
  TScraperGameInfo = record
    name: string;
    rom: string;
  end;

type
  TSCRAPER_TGDB = class
  private
    count_roms: integer;

    function get_num_of_roms(Platform_Type: TEmulatorSelected): integer;
    procedure reload_platform;

  public

    constructor create(AOwner: TComponent; Platform_Type: TEmulatorSelected; GameName, rom: String;
      only_one: boolean);
    destructor destroy; override;

    procedure prepear_start(Platform_Type: TEmulatorSelected; GameName, RomName: String; only_one: boolean);
    procedure start(Platform_Type: TEmulatorSelected);

    procedure scrape_by_platform(Platform_Name: String; Platform_ID, Platform_Const, Num: integer);
    procedure scrape_by_platform_one_game(Platform_Name: String; Platform_ID, Platform_Const: integer;
      GameInfo: TScraperGameInfo);

    // Grid list info
    function get_tgdb_list_of(list: TTypeList): TStringList;
    procedure replace_list_item(list: TTypeList);

    // Config
    procedure get_tgdb_games(update: boolean; Platform_Name: string; pbar: TProgressBar; ptxt: TText);
    procedure get_tgdb_genres(update: boolean; pbar: TProgressBar; ptxt: TText);
    procedure get_tgdb_publishers(update: boolean; pbar: TProgressBar; ptxt: TText);
    procedure get_tgdb_developers(update: boolean; pbar: TProgressBar; ptxt: TText);
    //
  end;

var
  scrape_tgdb: TSCRAPER_TGDB;
  name_s: string;
  tgdb_list_developers, tgdb_list_publishers, tgdb_list_genres: TStringList;

implementation

uses
  scraper_tgdb,
  main,
  uarcade_actions,
  scraper_tgdb_opt,
  emu_functions,
  front_main, uDataModule;

{ TSCREENSCAPER_ARCADE }

constructor TSCRAPER_TGDB.create(AOwner: TComponent; Platform_Type: TEmulatorSelected; GameName, rom: String;
  only_one: boolean);
begin
  count_roms := get_num_of_roms(Platform_Type);

  vScraper_TGDB := TTGDB_SCRAPER.create;

  tgdb_list_developers := TStringList.create;
  tgdb_list_developers.Add('none');
  tgdb_list_publishers := TStringList.create;
  tgdb_list_publishers.Add('none');
  tgdb_list_genres := TStringList.create;
  tgdb_list_genres.Add('none');

  if vScraper_TGDB.Check_Platforms_id_in_Database then
    prepear_start(Platform_Type, GameName, rom, only_one);

end;

destructor TSCRAPER_TGDB.destroy;
begin
  FreeAndNil(scrape_tgdb);
  inherited;
end;

function TSCRAPER_TGDB.get_num_of_roms(Platform_Type: TEmulatorSelected): integer;
begin
  case Platform_Type of
    emus_Arcade:
      result := dm.tArcade.RecordCount;
    emus_Nes:
      result := dm.tNes.RecordCount;
  end;
end;

procedure TSCRAPER_TGDB.get_tgdb_developers(update: boolean; pbar: TProgressBar; ptxt: TText);
var
  vi, count_db: integer;
  developers: T_TGDB_SCRAPER_DEVELOPERS;
  dev_id, dev_name: string;
  count_new, count_old: integer;
begin
  count_db := dm.tTGDBDevelopers.RecordCount;
  developers := vScraper_TGDB.get_developers;
  pbar.Visible := True;
  ptxt.Visible := True;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (developers.count).ToInteger;
  if update = false then
  begin
    if (count_db > 0) and (update = false) then
      dm.tTGDBDevelopers.ExecSQL('Delete From ' + dm.tTGDBDevelopers.TableName);

    for vi := 0 to (developers.count).ToInteger - 1 do
    begin
      dm.tTGDBDevelopersid.AsString := developers.developers[vi].id;
      dm.tTGDBDevelopersname.AsString := developers.developers[vi].name;
      dm.tTGDBDevelopers.Post;

      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Developer "' + developers.developers[vi].name + '"';

      application.ProcessMessages;
    end;

    name_s := 'Database has " ' + vi.ToString + '" new Developers ';
    dm.tTGDBDevelopers.ApplyUpdates();
  end
  else
  begin
    count_new := 0;
    count_old := 0;
    for vi := 0 to (developers.count).ToInteger - 1 do
    begin
      dm.tTGDBDevelopers.Locate('id', developers.developers[vi].id);

      if dm.tTGDBDevelopersid.AsString <> '' then
      begin
        dm.tTGDBDevelopers.Edit;
        dm.tTGDBDevelopersname.AsString := developers.developers[vi].name;
        dm.tTGDBDevelopers.Post;
        dm.tTGDBDevelopers.ApplyUpdates();

        ptxt.Text := 'Updating Developer "' + developers.developers[vi].name + '"';
        Inc(count_old);
      end
      else
      begin
        dm.tTGDBDevelopers.Open;
        dm.tTGDBDevelopers.InsertRecord([developers.developers[vi].id, developers.developers[vi].name]);
        dm.tTGDBDevelopers.Post;
        ptxt.Text := 'Add new Developer "' + developers.developers[vi].name + '"';
        Inc(count_new);
      end;

      pbar.Value := vi + 1;
      application.ProcessMessages;
    end;
    name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString +
      ' " Developers ';
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER_TGDB.get_tgdb_games(update: boolean; Platform_Name: string; pbar: TProgressBar;
  ptxt: TText);
var
  count_db: integer;
  game_name, rom_name, plat_name, game_name_db, name_s: string;
  count_new, count_old, plat_id, plat_const: integer;
  emu_state: TEmulatorSelected;
  game_t: T_TGDB_SCRAPER_GAME;
  bitmap_t: TBitmap;
begin
  // plat_name := Platform_Name + '_tgdb';
  // count_db := DSPFM_Data.table_count_db(plat_name);
  //
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'SELECT COUNT(*) FROM ' + plat_name;
  // DSPFM_Data.Query.Open;
  //
  // pbar.Visible := True;
  // ptxt.Visible := True;
  // pbar.Value := 0;
  // pbar.Min := 0;
  // pbar.Max := DSPFM_Data.Query.Fields[0].AsInteger;
  //
  // plat_id := vScraper_TGDB.get_platform_id(emu_state);
  // plat_const := vScraper_TGDB.get_platform_const_num(emu_state);
  //
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'SELECT name,rom FROM ' + plat_name + ' ORDER BY name ASC;';
  // DSPFM_Data.Query.Open;
  //
  // if update then
  // begin
  // while not DSPFM_Data.Query.Eof do
  // begin
  // count_old := 0;
  // count_new := 0;
  // game_name := DSPFM_Data.Query.FieldByName('name').AsString;
  // rom_name := DSPFM_Data.Query.FieldByName('rom').AsString;
  // game_t := vScraper_TGDB.Scrape_With_GameName(plat_id, game_name);
  //
  // DSPFM_Data.Query_2.Close;
  // DSPFM_Data.Query_2.SQL.Clear;
  // DSPFM_Data.Query_2.SQL.Text := 'SELECT rom FROM ' + plat_name + ' WHERE rom=''' +
  // rom_name + '''';
  // DSPFM_Data.Query_2.Open;
  //
  // game_name_db := DSPFM_Data.Query_2.Fields[0].AsString;
  //
  // if game_name_db <> '' then
  // begin
  // DSPFM_Data.Query_2.Close;
  // DSPFM_Data.Query_2.SQL.Clear;
  // DSPFM_Data.Query_2.SQL.Text := 'UPDATE ' + plat_name + ' SET id=:id, title=:title, ' +
  // ' release_date=:release_date, platform_id=:platform_id, players=:players, ' +
  // ' overview=:overview, last_updated=:last_updated, rating=:rating, coop=:coop, ' +
  // ' youtube=:youtube, os=:os, processor=:processor, ram=:ram, hdd=:hdd, video=:video, ' +
  // ' sound=:sound, developers=:developers, genres=:genres, publishers=:publishers, ' +
  // ' alternates=:alternates, box_art_original=:box_art_original, ' +
  // ' box_art_small=:box_art_small, box_art_thumb=:box_art_thumb, ' +
  // ' box_art_cropped=:box_art_cropped, box_art_medium=:box_art_medium, ' +
  // ' box_art_large=:box_art_large, pic_1=:pic_1, pic_1_ext=:pic_1_ext, rom=:rom ' +
  // ' WHERE rom=''' + rom_name + '''';
  //
  // DSPFM_Data.Query_2.ParamByName('id').AsString := game_t.games[0].id;
  // DSPFM_Data.Query_2.ParamByName('title').AsString := game_t.games[0].title;
  // DSPFM_Data.Query_2.ParamByName('release_date').AsString := game_t.games[0].release_date;
  // DSPFM_Data.Query_2.ParamByName('platform_id').AsString := game_t.games[0].Platform_ID;
  // DSPFM_Data.Query_2.ParamByName('players').AsString := game_t.games[0].players;
  // DSPFM_Data.Query_2.ParamByName('overview').AsString := game_t.games[0].overview;
  // DSPFM_Data.Query_2.ParamByName('last_updated').AsString := game_t.games[0].last_updated;
  // DSPFM_Data.Query_2.ParamByName('rating').AsString := game_t.games[0].rating;
  // DSPFM_Data.Query_2.ParamByName('coop').AsString := game_t.games[0].coop;
  // DSPFM_Data.Query_2.ParamByName('youtube').AsString := game_t.games[0].youtube;
  // DSPFM_Data.Query_2.ParamByName('os').AsString := game_t.games[0].os;
  // DSPFM_Data.Query_2.ParamByName('processor').AsString := game_t.games[0].processor;
  // DSPFM_Data.Query_2.ParamByName('ram').AsString := game_t.games[0].ram;
  // DSPFM_Data.Query_2.ParamByName('hdd').AsString := game_t.games[0].hdd;
  // DSPFM_Data.Query_2.ParamByName('video').AsString := game_t.games[0].video;
  // DSPFM_Data.Query_2.ParamByName('sound').AsString := game_t.games[0].sound;
  // DSPFM_Data.Query_2.ParamByName('developers').AsString := game_t.games[0].developers[0];
  // DSPFM_Data.Query_2.ParamByName('genres').AsString := game_t.games[0].genres[0];
  // DSPFM_Data.Query_2.ParamByName('publishers').AsString := game_t.games[0].publishers[0];
  // if game_t.games[0].alternates <> nil then
  // DSPFM_Data.Query_2.ParamByName('alternates').AsString := game_t.games[0].alternates[0]
  // else
  // DSPFM_Data.Query_2.ParamByName('alternates').AsString := '';
  // DSPFM_Data.Query_2.ParamByName('box_art_original').AsString :=
  // game_t.box_art.base_url.original;
  // DSPFM_Data.Query_2.ParamByName('box_art_small').AsString := game_t.box_art.base_url.small;
  // DSPFM_Data.Query_2.ParamByName('box_art_thumb').AsString := game_t.box_art.base_url.thumb;
  // DSPFM_Data.Query_2.ParamByName('box_art_cropped').AsString :=
  // game_t.box_art.base_url.cropped;
  // DSPFM_Data.Query_2.ParamByName('box_art_medium').AsString := game_t.box_art.base_url.medium;
  // DSPFM_Data.Query_2.ParamByName('box_art_large').AsString := game_t.box_art.base_url.large;
  // DSPFM_Data.Query_2.ParamByName('pic_1').AsString := game_t.box_art.game[0].data[0].filename;
  // DSPFM_Data.Query_2.ParamByName('pic_1_ext').AsString :=
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename);
  // DSPFM_Data.Query_2.ParamByName('rom').AsString := rom_name;
  // DSPFM_Data.Query_2.ExecSQL;
  //
  // ptxt.Text := 'Downloading Images for " ' + game_name + ' "';
  // application.ProcessMessages;
  //
  // if game_t.box_art.base_url.original <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.original +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_original' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // if Temp_Game.box_art.base_url.small <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.small);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_small' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // if game_t.box_art.base_url.thumb <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.thumb +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_thumb' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // if Temp_Game.box_art.base_url.cropped <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.cropped);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_cropped' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // // if Temp_Game.box_art.base_url.medium <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.medium);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_medium' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // if game_t.box_art.base_url.large <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.large +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_large' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // Getting Video
  // //
  // //
  // // Getting other stuff
  // Inc(count_old);
  // pbar.Value := pbar.Value + 1;
  // DSPFM_Data.Query.Next;
  // end
  // else
  // begin
  // DSPFM_Data.Query_2.Close;
  // DSPFM_Data.Query_2.SQL.Clear;
  // DSPFM_Data.Query_2.SQL.Text :=
  // 'INSERT INTO arcade_tgdb (id, title, release_date, platform_id, players,' +
  // 'overview, last_updated, rating, coop, youtube, os, processor, ram, hdd, video, sound, developers,'
  // + 'genres, publishers, alternates, box_art_original, box_art_small, box_art_thumb, box_art_cropped, '
  // + 'box_art_medium, box_art_large, pic_1, pic_1_ext, rom) VALUES (:id, :title, :release_date, :platform_id, :players,'
  // + ':overview, :last_updated, :rating, :coop, :youtube, :os, :processor, :ram, :hdd, :video, :sound, :developers,'
  // + ':genres, :publishers, :alternates, :box_art_original, :box_art_small, :box_art_thumb, :box_art_cropped, '
  // + ':box_art_medium, :box_art_large, :pic_1, :pic_1_ext, :rom)';
  //
  // DSPFM_Data.Query_2.ParamByName('id').AsString := game_t.games[0].id;
  // DSPFM_Data.Query_2.ParamByName('title').AsString := game_t.games[0].title;
  // DSPFM_Data.Query_2.ParamByName('release_date').AsString := game_t.games[0].release_date;
  // DSPFM_Data.Query_2.ParamByName('platform_id').AsString := game_t.games[0].Platform_ID;
  // DSPFM_Data.Query_2.ParamByName('players').AsString := game_t.games[0].players;
  // DSPFM_Data.Query_2.ParamByName('overview').AsString := game_t.games[0].overview;
  // DSPFM_Data.Query_2.ParamByName('last_updated').AsString := game_t.games[0].last_updated;
  // DSPFM_Data.Query_2.ParamByName('rating').AsString := game_t.games[0].rating;
  // DSPFM_Data.Query_2.ParamByName('coop').AsString := game_t.games[0].coop;
  // DSPFM_Data.Query_2.ParamByName('youtube').AsString := game_t.games[0].youtube;
  // DSPFM_Data.Query_2.ParamByName('os').AsString := game_t.games[0].os;
  // DSPFM_Data.Query_2.ParamByName('processor').AsString := game_t.games[0].processor;
  // DSPFM_Data.Query_2.ParamByName('ram').AsString := game_t.games[0].ram;
  // DSPFM_Data.Query_2.ParamByName('hdd').AsString := game_t.games[0].hdd;
  // DSPFM_Data.Query_2.ParamByName('video').AsString := game_t.games[0].video;
  // DSPFM_Data.Query_2.ParamByName('sound').AsString := game_t.games[0].sound;
  // DSPFM_Data.Query_2.ParamByName('developers').AsString := game_t.games[0].developers[0];
  // DSPFM_Data.Query_2.ParamByName('genres').AsString := game_t.games[0].genres[0];
  // DSPFM_Data.Query_2.ParamByName('publishers').AsString := game_t.games[0].publishers[0];
  // if game_t.games[0].alternates <> nil then
  // DSPFM_Data.Query_2.ParamByName('alternates').AsString := game_t.games[0].alternates[0]
  // else
  // DSPFM_Data.Query_2.ParamByName('alternates').AsString := '';
  // DSPFM_Data.Query_2.ParamByName('box_art_original').AsString :=
  // game_t.box_art.base_url.original;
  // DSPFM_Data.Query_2.ParamByName('box_art_small').AsString := game_t.box_art.base_url.small;
  // DSPFM_Data.Query_2.ParamByName('box_art_thumb').AsString := game_t.box_art.base_url.thumb;
  // DSPFM_Data.Query_2.ParamByName('box_art_cropped').AsString :=
  // game_t.box_art.base_url.cropped;
  // DSPFM_Data.Query_2.ParamByName('box_art_medium').AsString := game_t.box_art.base_url.medium;
  // DSPFM_Data.Query_2.ParamByName('box_art_large').AsString := game_t.box_art.base_url.large;
  // DSPFM_Data.Query_2.ParamByName('pic_1').AsString := game_t.box_art.game[0].data[0].filename;
  // DSPFM_Data.Query_2.ParamByName('pic_1_ext').AsString :=
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename);
  // DSPFM_Data.Query_2.ParamByName('rom').AsString := rom_name;
  // DSPFM_Data.Query_2.ExecSQL;
  //
  // ptxt.Text := 'Downloading Images for " ' + game_name + ' "';
  // application.ProcessMessages;
  //
  // if game_t.box_art.base_url.original <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.original +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_original' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // if Temp_Game.box_art.base_url.small <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.small);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_small' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // if game_t.box_art.base_url.thumb <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.thumb +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_thumb' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // if Temp_Game.box_art.base_url.cropped <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.cropped);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_cropped' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // // if Temp_Game.box_art.base_url.medium <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.medium);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_medium' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // if game_t.box_art.base_url.large <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.large +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_large' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // Getting Video
  // //
  // //
  // // Getting other stuff
  //
  // Inc(count_new);
  // pbar.Value := pbar.Value + 1;
  // DSPFM_Data.Query.Next;
  // end;
  // name_s := 'Update " ' + count_old.ToString + ' " and Insert new " ' + count_new.ToString +
  // '  " games in database'
  // end;
  // end
  // else
  // begin
  // count_new := 0;
  // if (count_db > 0) and (update = false) then
  // begin
  // // delete all files in images folder
  // // delete all files in videos folder
  // // delete all files in manuals folder
  // DSPFM_Data.table_delete(plat_name);
  // end;
  //
  // while not DSPFM_Data.Query.Eof do
  // begin
  // game_name := DSPFM_Data.Query.FieldByName('name').AsString;
  // rom_name := DSPFM_Data.Query.FieldByName('rom').AsString;
  // ptxt.Text := 'Downloading Data for " ' + game_name + ' "';
  // application.ProcessMessages;
  //
  // game_t := vScraper_TGDB.Scrape_With_GameName(plat_id, game_name);
  //
  // DSPFM_Data.Query_2.Close;
  // DSPFM_Data.Query_2.SQL.Clear;
  // DSPFM_Data.Query_2.SQL.Text :=
  // 'INSERT INTO arcade_tgdb (id, title, release_date, platform_id, players,' +
  // 'overview, last_updated, rating, coop, youtube, os, processor, ram, hdd, video, sound, developers,'
  // + 'genres, publishers, alternates, box_art_original, box_art_small, box_art_thumb, box_art_cropped, '
  // + 'box_art_medium, box_art_large, pic_1, pic_1_ext, rom) VALUES (:id, :title, :release_date, :platform_id, :players,'
  // + ':overview, :last_updated, :rating, :coop, :youtube, :os, :processor, :ram, :hdd, :video, :sound, :developers,'
  // + ':genres, :publishers, :alternates, :box_art_original, :box_art_small, :box_art_thumb, :box_art_cropped, '
  // + ':box_art_medium, :box_art_large, :pic_1, :pic_1_ext, :rom)';
  //
  // DSPFM_Data.Query_2.ParamByName('id').AsString := game_t.games[0].id;
  // DSPFM_Data.Query_2.ParamByName('title').AsString := game_t.games[0].title;
  // DSPFM_Data.Query_2.ParamByName('release_date').AsString := game_t.games[0].release_date;
  // DSPFM_Data.Query_2.ParamByName('platform_id').AsString := game_t.games[0].Platform_ID;
  // DSPFM_Data.Query_2.ParamByName('players').AsString := game_t.games[0].players;
  // DSPFM_Data.Query_2.ParamByName('overview').AsString := game_t.games[0].overview;
  // DSPFM_Data.Query_2.ParamByName('last_updated').AsString := game_t.games[0].last_updated;
  // DSPFM_Data.Query_2.ParamByName('rating').AsString := game_t.games[0].rating;
  // DSPFM_Data.Query_2.ParamByName('coop').AsString := game_t.games[0].coop;
  // DSPFM_Data.Query_2.ParamByName('youtube').AsString := game_t.games[0].youtube;
  // DSPFM_Data.Query_2.ParamByName('os').AsString := game_t.games[0].os;
  // DSPFM_Data.Query_2.ParamByName('processor').AsString := game_t.games[0].processor;
  // DSPFM_Data.Query_2.ParamByName('ram').AsString := game_t.games[0].ram;
  // DSPFM_Data.Query_2.ParamByName('hdd').AsString := game_t.games[0].hdd;
  // DSPFM_Data.Query_2.ParamByName('video').AsString := game_t.games[0].video;
  // DSPFM_Data.Query_2.ParamByName('sound').AsString := game_t.games[0].sound;
  // DSPFM_Data.Query_2.ParamByName('developers').AsString := game_t.games[0].developers[0];
  // DSPFM_Data.Query_2.ParamByName('genres').AsString := game_t.games[0].genres[0];
  // DSPFM_Data.Query_2.ParamByName('publishers').AsString := game_t.games[0].publishers[0];
  // if game_t.games[0].alternates <> nil then
  // DSPFM_Data.Query_2.ParamByName('alternates').AsString := game_t.games[0].alternates[0]
  // else
  // DSPFM_Data.Query_2.ParamByName('alternates').AsString := '';
  // DSPFM_Data.Query_2.ParamByName('box_art_original').AsString :=
  // game_t.box_art.base_url.original;
  // DSPFM_Data.Query_2.ParamByName('box_art_small').AsString := game_t.box_art.base_url.small;
  // DSPFM_Data.Query_2.ParamByName('box_art_thumb').AsString := game_t.box_art.base_url.thumb;
  // DSPFM_Data.Query_2.ParamByName('box_art_cropped').AsString := game_t.box_art.base_url.cropped;
  // DSPFM_Data.Query_2.ParamByName('box_art_medium').AsString := game_t.box_art.base_url.medium;
  // DSPFM_Data.Query_2.ParamByName('box_art_large').AsString := game_t.box_art.base_url.large;
  // DSPFM_Data.Query_2.ParamByName('pic_1').AsString := game_t.box_art.game[0].data[0].filename;
  // DSPFM_Data.Query_2.ParamByName('pic_1_ext').AsString :=
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename);
  // DSPFM_Data.Query_2.ParamByName('rom').AsString := rom_name;
  // DSPFM_Data.Query_2.ExecSQL;
  //
  // ptxt.Text := 'Downloading Images for " ' + game_name + ' "';
  // application.ProcessMessages;
  //
  // if game_t.box_art.base_url.original <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.original +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_original' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // if Temp_Game.box_art.base_url.small <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.small);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_small' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // if game_t.box_art.base_url.thumb <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.thumb +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_thumb' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // if Temp_Game.box_art.base_url.cropped <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.cropped);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_cropped' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // // if Temp_Game.box_art.base_url.medium <> '' then
  // // begin
  // // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.medium);
  // // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_medium' +
  // // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
  // // FreeAndNil(Temp_Bitmap);
  // // end;
  // if game_t.box_art.base_url.large <> '' then
  // begin
  // bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.large +
  // game_t.box_art.game[0].data[0].filename);
  // bitmap_t.SaveToFile(config.emu_path[plat_const].box_art + rom_name + '_large' +
  // ExtractFileExt(game_t.box_art.game[0].data[0].filename));
  // FreeAndNil(bitmap_t);
  // end;
  // // Getting Video
  // //
  // //
  // // Getting other stuff
  // Inc(count_new);
  // pbar.Value := pbar.Value + 1;
  // DSPFM_Data.Query.Next;
  // end;
  // name_s := 'Insert new " ' + count_new.ToString + ' games in database"';
  // end;
  // ptxt.Text := name_s;
end;

procedure TSCRAPER_TGDB.get_tgdb_genres(update: boolean; pbar: TProgressBar; ptxt: TText);
var
  vi, count_db: integer;
  genres: T_TGDB_SCRAPER_GENRES;
  gen_id, gen_name, plat_name: string;
  count_new, count_old: integer;
begin
  plat_name := 'scraper_tgdb_genres';
  // count_db := DSPFM_Data.table_count_db(plat_name);
  //
  // genres := vScraper_TGDB.get_genres;
  // pbar.Visible := True;
  // ptxt.Visible := True;
  // pbar.Value := 0;
  // pbar.Min := 0;
  // pbar.Max := (genres.count).ToInteger;
  // if update = false then
  // begin
  // if (count_db > 0) and (update = false) then
  // DSPFM_Data.table_delete(plat_name);
  //
  // for vi := 0 to (genres.count).ToInteger - 1 do
  // begin
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';
  //
  // DSPFM_Data.Query.ParamByName('id').AsString := genres.genres[vi].id;
  // DSPFM_Data.Query.ParamByName('name').AsString := genres.genres[vi].name;
  //
  // DSPFM_Data.Query.ExecSQL;
  // pbar.Value := vi + 1;
  // ptxt.Text := 'Adding Gerne "' + genres.genres[vi].name + '"';
  //
  // application.ProcessMessages;
  // end;
  // name_s := 'Database has " ' + vi.ToString + '" new Genres';
  // end
  // else
  // begin
  // count_old := 0;
  // count_new := 0;
  // for vi := 0 to (genres.count).ToInteger - 1 do
  // begin
  // gen_id := genres.genres[vi].id;
  //
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'SELECT name FROM ' + plat_name + ' WHERE id=''' + gen_id + '''';
  // DSPFM_Data.Query.Open;
  //
  // gen_name := DSPFM_Data.Query.Fields[0].AsString;
  //
  // if gen_name <> '' then
  // begin
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'UPDATE ' + plat_name + ' SET id=:id, name=:name WHERE id=:id';
  //
  // DSPFM_Data.Query.ParamByName('id').AsString := gen_id;
  // DSPFM_Data.Query.ParamByName('name').AsString := gen_name;
  //
  // DSPFM_Data.Query.ExecSQL;
  //
  // ptxt.Text := 'Updating Gerne "' + gen_name + '"';
  // Inc(count_old);
  // end
  // else
  // begin
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';
  //
  // DSPFM_Data.Query.ParamByName('id').AsString := genres.genres[vi].id;
  // DSPFM_Data.Query.ParamByName('name').AsString := genres.genres[vi].name;
  //
  // DSPFM_Data.Query.ExecSQL;
  // ptxt.Text := 'Add new Genre "' + genres.genres[vi].name + '"';
  // Inc(count_new);
  // end;
  // pbar.Value := vi + 1;
  // application.ProcessMessages;
  // end;
  // name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString +
  // ' " Gernes ';
  // end;
  // ptxt.Text := name_s;
end;

function TSCRAPER_TGDB.get_tgdb_list_of(list: TTypeList): TStringList;
begin
  result := TStringList.create;
  case list of
    TTL_Developers:
      begin
        with dm do
        begin
          while not tTGDBDevelopers.eof do
          begin
            result.Add(tTGDBDevelopersname.AsString);
            tTGDBDevelopers.next;
          end;
        end;
        main.frm_main.spbInfoListApply.Tag := 10;
      end;
    TTL_Publishers:
      begin
        with dm do
        begin
          while not tTGDBPublishers.eof do
          begin
            result.Add(tTGDBPublishersname.AsString);
            tTGDBPublishers.next;
          end;
        end;
        main.frm_main.spbInfoListApply.Tag := 20;
      end;
    TTL_Genres:
      begin
        with dm do
        begin
          while not tTGDBGenres.eof do
          begin
            result.Add(tTGDBGenresname.AsString);
            tTGDBGenres.next;
          end;
        end;
        main.frm_main.spbInfoListApply.Tag := 30;
      end;
  end;
end;

procedure TSCRAPER_TGDB.get_tgdb_publishers(update: boolean; pbar: TProgressBar; ptxt: TText);
var
  vi, count_db: integer;
  publishers: T_TGDB_SCRAPER_PUBLISHERS;
  pub_id, pub_name, plat_name: string;
  count_new, count_old: integer;
begin
  publishers := vScraper_TGDB.get_publishers;
  pbar.Visible := True;
  ptxt.Visible := True;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (publishers.count).ToInteger;
  if update = false then
  begin
    if (count_db > 0) and (update = false) then
      dm.tTGDBPublishers.ExecSQL('DELETE FROM ' + dm.tTGDBPublishers.TableName);
    for vi := 0 to (publishers.count).ToInteger - 1 do
    begin
      dm.tTGDBPublishers.Edit;
      dm.tTGDBPublishersid.AsString := publishers.publishers[vi].id;
      dm.tTGDBPublishersname.AsString := publishers.publishers[vi].name;
      dm.tTGDBPublishers.Post;
      dm.tTGDBPublishers.ApplyUpdates();

      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Publisher "' + publishers.publishers[vi].name + '"';
      application.ProcessMessages;
    end;
  end
  else
  begin
    // count_old := 0;
    // count_new := 0;
    // for vi := 0 to (publishers.count).ToInteger - 1 do
    // begin
    // pub_id := publishers.publishers[vi].id;
    //
    // DSPFM_Data.Query.Close;
    // DSPFM_Data.Query.SQL.Clear;
    // DSPFM_Data.Query.SQL.Text := 'SELECT name FROM ' + plat_name + ' WHERE id=''' + pub_id + '''';
    // DSPFM_Data.Query.Open;
    //
    // pub_name := DSPFM_Data.Query.Fields[0].AsString;
    //
    // if pub_name <> '' then
    // begin
    // DSPFM_Data.Query.Close;
    // DSPFM_Data.Query.SQL.Clear;
    // DSPFM_Data.Query.SQL.Text := 'UPDATE ' + plat_name + ' SET id=:id, name=:name WHERE id=:id';
    //
    // DSPFM_Data.Query.ParamByName('id').AsString := pub_id;
    // DSPFM_Data.Query.ParamByName('name').AsString := pub_name;
    //
    // DSPFM_Data.Query.ExecSQL;
    //
    // ptxt.Text := 'Updating Publisher "' + pub_name + '"';
    // Inc(count_old);
    // end
    // else
    // begin
    // DSPFM_Data.Query.Close;
    // DSPFM_Data.Query.SQL.Clear;
    // DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';
    //
    // DSPFM_Data.Query.ParamByName('id').AsString := publishers.publishers[vi].id;
    // DSPFM_Data.Query.ParamByName('name').AsString := publishers.publishers[vi].name;
    //
    // DSPFM_Data.Query.ExecSQL;
    // ptxt.Text := 'Add new Genre "' + publishers.publishers[vi].name + '"';
    // Inc(count_new);
    // end;
    pbar.Value := vi + 1;
    application.ProcessMessages;
    // end;
    name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString +
      ' " Publishers ';
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER_TGDB.prepear_start(Platform_Type: TEmulatorSelected; GameName, RomName: String;
  only_one: boolean);
begin
  frm_scraper.lbl_scraper_platform_value.Text := vScraper_TGDB.get_platform_string(Platform_Type);
  if (GameName = '') and (only_one = false) then
  begin
    frm_scraper.lbl_scraper_count_value.Text := count_roms.ToString;
    frm_scraper.lbl_scraper_games_value.Text := 'All';
    frm_scraper.lbl_scraper_missing_value.Text := vScraper_TGDB.Get_Missing(Platform_Type).ToString;
    frm_scraper.prbar_scraper.Max := count_roms;
  end
  else
  begin
    frm_scraper.lbl_scraper_games_value.Text := GameName;
    frm_scraper.lbl_scraper_rom_value.Text := RomName;
    frm_scraper.prbar_scraper.Max := 1;
    frm_scraper.lbl_scraper_count_value.Text := '1';
  end;
  frm_scraper.prbar_scraper.Min := 0;
  frm_scraper.prbar_scraper.Value := 0;
  frm_scraper.txt_scraper_info_game.Text := '';
  frm_scraper.txt_scraper_info.Text := '';
  frm_scraper.lbl_scraper_warning.Visible := false;
  main.frm_main.eff_blur_main.Enabled := True;
  frm_scraper.rect_scraper.Visible := True;
end;

procedure TSCRAPER_TGDB.reload_platform;
begin
  front_action.destroy_grid;
  front_action.create_grid(front_action.current_emu);
end;

procedure TSCRAPER_TGDB.replace_list_item(list: TTypeList);
begin
  with frm_main do
  begin
    case list of
//      TTL_Developers:
//        edtInfoDeveloper.Text := lbInfoList.Items.Strings[lbInfoList.ItemIndex];
      TTL_Publishers:
        edtInfoPublisher.Text := lbInfoList.Items.Strings[lbInfoList.ItemIndex];
      TTL_Genres:
        edtInfoGenre.Text := lbInfoList.Items.Strings[lbInfoList.ItemIndex];
    end;
    eff_blur_grid_info_list.Enabled := false;
    layInfoList.Visible := false;
    lbInfoList.Items.Clear;
  end;
end;

procedure TSCRAPER_TGDB.scrape_by_platform(Platform_Name: String; Platform_ID, Platform_Const, Num: integer);
var
  tempInfo: TScraperGameInfo;
  tempGame: T_TGDB_SCRAPER_GAME;
  tempGameImages: T_TGDB_SCRAPER_GAME_IMAGES;
  Temp_Bitmap: TBitmap;
  vi: integer;
  exists_allready: boolean;
  emu, Query: string;

  function get_exist_from_database(rom: string): boolean;
  begin
    dm.tArcadeTGDB.Locate('rom', rom);
    if dm.tArcadeTGDBrom.AsString <> '' then
      result := True
    else
      result := false;
  end;

  procedure update_game_media_to_database;
  begin
    dm.tArcadeTGDB.Locate('rom', dm.tArcaderom.AsString);
    dm.tArcadeTGDB.Edit;
    dm.tArcadeTGDBid.AsString := tempGame.games[0].id;
    dm.tArcadeTGDBtitle.AsString := tempGame.games[0].title;
    dm.tArcadeTGDBrelease_date.AsString := tempGame.games[0].release_date;
    dm.tArcadeTGDBplatform_id.AsString := tempGame.games[0].Platform_ID;
    dm.tArcadeTGDBplayers.AsString := tempGame.games[0].players;
    dm.tArcadeTGDBoverview.AsString := tempGame.games[0].overview;
    dm.tArcadeTGDBlast_updated.AsString := tempGame.games[0].last_updated;
    dm.tArcadeTGDBrating.AsString := tempGame.games[0].rating;
    dm.tArcadeTGDBcoop.AsString := tempGame.games[0].coop;
    dm.tArcadeTGDByoutube.AsString := tempGame.games[0].youtube;
    dm.tArcadeTGDBos.AsString := tempGame.games[0].os;
    dm.tArcadeTGDBprocessor.AsString := tempGame.games[0].processor;
    dm.tArcadeTGDBram.AsString := tempGame.games[0].ram;
    dm.tArcadeTGDBhdd.AsString := tempGame.games[0].hdd;
    dm.tArcadeTGDBvideo.AsString := tempGame.games[0].video;
    dm.tArcadeTGDBsound.AsString := tempGame.games[0].sound;
    dm.tArcadeTGDBdevelopers.AsString := tempGame.games[0].developers[0];
    dm.tArcadeTGDBgenres.AsString := tempGame.games[0].genres[Num];
    dm.tArcadeTGDBpublishers.AsString := tempGame.games[0].publishers[0];
    if tempGame.games[0].alternates <> nil then
      dm.tArcadeTGDBalternates.AsString := tempGame.games[Num].alternates[0]
    else
      dm.tArcadeTGDBalternates.AsString := '';

    dm.tTGDB.Edit;
    if dm.tTGDBbox_art_original.AsString <> tempGame.box_art.base_url.original then

    dm.tTGDBbox_art_original.AsString := tempGame.box_art.base_url.original;
    dm.tTGDBbox_art_small.AsString := tempGame.box_art.base_url.small;
    dm.tTGDBbox_art_thumb.AsString := tempGame.box_art.base_url.thumb;
    dm.tTGDBbox_art_cropped.AsString := tempGame.box_art.base_url.cropped;
    dm.tTGDBbox_art_medium.AsString := tempGame.box_art.base_url.medium;
    dm.tTGDBbox_art_large.AsString := tempGame.box_art.base_url.large;

    dm.tTGDB.Post;
    dm.tArcadeTGDB.Post;

    dm.tTGDB.ApplyUpdates();
    dm.tArcadeTGDB.ApplyUpdates();
  end;

  procedure add_new_to_database;
  var
    media, image_data_path, tgdb: string;
  begin
    dm.tArcadeTGDB.Edit;
    dm.tArcadeTGDBid.AsString := tempGame.games[0].id;
    dm.tArcadeTGDBtitle.AsString := tempGame.games[0].title;
    dm.tArcadeTGDBrelease_date.AsString := tempGame.games[0].release_date;
    dm.tArcadeTGDBplatform_id.AsString := tempGame.games[0].Platform_ID;
    dm.tArcadeTGDBplayers.AsString := tempGame.games[0].players;
    dm.tArcadeTGDBoverview.AsString := tempGame.games[0].overview;
    dm.tArcadeTGDBlast_updated.AsString := tempGame.games[0].last_updated;
    dm.tArcadeTGDBrating.AsString := tempGame.games[0].rating;
    dm.tArcadeTGDBcoop.AsString := tempGame.games[0].coop;
    dm.tArcadeTGDByoutube.AsString := tempGame.games[0].youtube;
    dm.tArcadeTGDBos.AsString := tempGame.games[0].os;
    dm.tArcadeTGDBprocessor.AsString := tempGame.games[0].processor;
    dm.tArcadeTGDBram.AsString := tempGame.games[0].ram;
    dm.tArcadeTGDBhdd.AsString := tempGame.games[0].hdd;
    dm.tArcadeTGDBvideo.AsString := tempGame.games[0].video;
    dm.tArcadeTGDBsound.AsString := tempGame.games[0].sound;
    dm.tArcadeTGDBdevelopers.AsString := tempGame.games[0].developers[0];
    dm.tArcadeTGDBgenres.AsString := tempGame.games[0].genres[0];
    dm.tArcadeTGDBpublishers.AsString := tempGame.games[0].publishers[0];
    if tempGame.games[0].alternates <> nil then
      dm.tArcadeTGDBalternates.AsString := tempGame.games[0].alternates[0]
    else
      dm.tArcadeTGDBalternates.AsString := '';
    dm.tTGDBbox_art_original.AsString := tempGame.box_art.base_url.original;
    dm.tTGDBbox_art_small.AsString := tempGame.box_art.base_url.small;
    dm.tTGDBbox_art_thumb.AsString := tempGame.box_art.base_url.thumb;
    dm.tTGDBbox_art_cropped.AsString := tempGame.box_art.base_url.cropped;
    dm.tTGDBbox_art_medium.AsString := tempGame.box_art.base_url.medium;
    dm.tTGDBbox_art_large.AsString := tempGame.box_art.base_url.large;

    // Getting Images
    if tempGame.box_art.base_url.original <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image_new(tempGame.box_art.base_url.original + tempGame.box_art.game
        [0].data[0].filename);
      image_data_path := config.emu_path[Platform_Const].box_art + tempInfo.rom + '_original' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename);
      Temp_Bitmap.SaveToFile(image_data_path);
      FreeAndNil(Temp_Bitmap);

      media := emu + '_media';

      dm.tArcadeMedia.Locate('rom', tempInfo.rom);
      dm.tArcadeMediabox_art.AsString := image_data_path;
      dm.tArcadeMedia.Post;
    end;
    if tempGame.box_art.base_url.small <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image(tempGame.box_art.base_url.small);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.name + '_small' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.thumb <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image_new(tempGame.box_art.base_url.thumb + tempGame.box_art.game[0]
        .data[0].filename);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.rom + '_thumb' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.cropped <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image(tempGame.box_art.base_url.cropped);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.name + '_cropped' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.medium <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image(tempGame.box_art.base_url.medium);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.name + '_medium' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.large <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image_new(tempGame.box_art.base_url.large + tempGame.box_art.game[0]
        .data[0].filename);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.rom + '_large' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    // Getting video
    // Getting other stuff
    // Getting images
    if tempGame.box_art.base_url.original <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image_new(tempGame.box_art.base_url.original + tempGame.box_art.game
        [0].data[0].filename);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.rom + '_original' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.small <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image(tempGame.box_art.base_url.small);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.name + '_small' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.thumb <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image_new(tempGame.box_art.base_url.thumb + tempGame.box_art.game[0]
        .data[0].filename);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.rom + '_thumb' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.cropped <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image(tempGame.box_art.base_url.cropped);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.name + '_cropped' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.medium <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image(tempGame.box_art.base_url.medium);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.name + '_medium' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    if tempGame.box_art.base_url.large <> '' then
    begin
      Temp_Bitmap := uInternet_files.Get_Image_new(tempGame.box_art.base_url.large + tempGame.box_art.game[0]
        .data[0].filename);
      Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].box_art + tempInfo.rom + '_large' +
        ExtractFileExt(tempGame.box_art.game[0].data[0].filename));
      FreeAndNil(Temp_Bitmap);
    end;
    // Getting Video
    // Getting other stuff
  end;

begin
  scraper_tgdb.frm_scraper.pressed_stop := false;
  frm_scraper.spb_scraper_start.Text := 'Stop';
  frm_scraper.anim_float_scraper_warning.Enabled := True;
  frm_scraper.spb_scraper_cancel.Enabled := false;
  frm_scraper.cb_scraper_only_missing.Enabled := false;
  frm_scraper.txt_scraper_info_game.Visible := True;
  frm_scraper.txt_scraper_info.Visible := True;
  frm_scraper.prbar_scraper.Visible := True;
  frm_scraper.prbar_scraper.Value := 0;
  emu := emu_functions.Emulation_Name(emu_active);

  vScraper_TGDB.Check_Platforms_id_in_Database;

  frm_scraper.txt_scraper_info_game.Text := 'Scraping for Developers (This happend only one time)';
  if dm.tTGDBDevelopers.RecordCount = 0 then
    get_tgdb_developers(false, frm_scraper.prbar_scraper, frm_scraper.txt_scraper_info);

  frm_scraper.txt_scraper_info_game.Text := 'Scraping for Publishers (This happend only one time)';
  if dm.tTGDBPublishers.RecordCount = 0 then
    get_tgdb_publishers(false, frm_scraper.prbar_scraper, frm_scraper.txt_scraper_info);

  frm_scraper.txt_scraper_info_game.Text := 'Scraping for Genres (This happend only one time)';
  if dm.tTGDBGenres.RecordCount = 0 then
    get_tgdb_genres(false, frm_scraper.prbar_scraper, frm_scraper.txt_scraper_info);

  if frm_scraper.lbl_scraper_missing_value.Text = '0' then
  begin
    if frm_scraper.cb_scraper_only_missing.isChecked then
    begin
      TDialogService.MessageDialog('There is nothing to do everething is ok.', TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
        procedure(const AResult: TModalResult)
        begin
          if AResult = mrOk then
          begin
            frm_scraper.spb_scraper_start.Text := 'Start';
            frm_scraper.anim_float_scraper_warning.Enabled := false;
            frm_scraper.spb_scraper_cancel.Enabled := false;
            frm_scraper.cb_scraper_only_missing.Enabled := True;
            exit;
          end;
        end);
    end
    else
    begin
      update_game_media_to_database;
    end;
  end
  else
  begin
    vi := 1;
    with dm.tArcade do
    begin
      first;
      while not eof do
      begin
        tempGame.count := '0';

        tempInfo.name := FieldByName('name').AsString;
        tempInfo.rom := FieldByName('rom').AsString;
        tempGame := vScraper_TGDB.Scrape_With_GameName(Platform_ID, tempInfo.name);
        tempGameImages := vScraper_TGDB.get_games_images(tempGame.games[0].id);

        frm_scraper.txt_scraper_info_game.Text := 'Scraping for "' + tempInfo.name + '"';
        frm_scraper.txt_scraper_info.Text := 'Download data (' + vi.ToString + ' from ' + Num.ToString + ')';

        application.ProcessMessages;

        exists_allready := get_exist_from_database(tempInfo.rom);

        if tempGame.count <> '0' then
        begin
          if frm_scraper.cb_scraper_only_missing.isChecked then
          begin
            if exists_allready = false then
              add_new_to_database;
          end
          else
          begin
            if exists_allready then
              update_game_media_to_database
            else
              add_new_to_database;
          end;
        end;
        Inc(vi);
        frm_scraper.prbar_scraper.Value := vi;
        if scraper_tgdb.frm_scraper.pressed_stop then
        begin
          scrape_tgdb.prepear_start(emu_active, '', '', false);
          break;
        end;
        next;
      end;
    end;
  end;

  reload_platform;
  frm_scraper.spb_scraper_start.Text := 'Start';
  frm_scraper.anim_float_scraper_warning.Enabled := false;
  frm_scraper.txt_scraper_info_game.Visible := false;
  frm_scraper.txt_scraper_info.Visible := false;
  frm_scraper.prbar_scraper.Visible := false;
  frm_scraper.cb_scraper_only_missing.Enabled := True;
  frm_scraper.spb_scraper_cancel.Enabled := True;
end;

procedure TSCRAPER_TGDB.scrape_by_platform_one_game(Platform_Name: String;
Platform_ID, Platform_Const: integer; GameInfo: TScraperGameInfo);
var
  scrapedGame: T_TGDB_SCRAPER_GAME;
begin
  scraper_tgdb.frm_scraper.pressed_stop := false;
  frm_scraper.spb_scraper_start.Text := 'Stop';
  frm_scraper.anim_float_scraper_warning.Enabled := True;
  frm_scraper.spb_scraper_cancel.Enabled := false;
  frm_scraper.cb_scraper_only_missing.Enabled := false;
  frm_scraper.txt_scraper_info_game.Visible := True;
  frm_scraper.txt_scraper_info.Visible := True;
  frm_scraper.prbar_scraper.Visible := True;
  frm_scraper.prbar_scraper.Value := 0;
  //
  if frm_scraper.lbl_scraper_missing_value.Text = '0' then
  begin
    if frm_scraper.cb_scraper_only_missing.isChecked then
    begin
      TDialogService.MessageDialog('There is nothing to do everething is ok.', TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
        procedure(const AResult: TModalResult)
        begin
          if AResult = mrOk then
          begin
            frm_scraper.spb_scraper_start.Text := 'Start';
            frm_scraper.anim_float_scraper_warning.Enabled := false;
            frm_scraper.spb_scraper_cancel.Enabled := false;
            frm_scraper.cb_scraper_only_missing.Enabled := True;
            exit;
          end;
        end);
    end
    else
    begin
      frm_scraper.txt_scraper_info_game.Text := 'Scraping for "' + GameInfo.name + '"';
      frm_scraper.txt_scraper_info.Text := 'Download data';
      application.ProcessMessages;
      scrapedGame := vScraper_TGDB.Scrape_With_GameName(Platform_ID, GameInfo.rom);
      if scrapedGame.count = '0' then
        scrapedGame := vScraper_TGDB.Scrape_With_GameName(Platform_ID, GameInfo.name);
      frm_scraper_tgdb_opt.show_results_for_gamename(scrapedGame);
      frm_scraper_tgdb_opt.ShowModal;
    end;
  end
  else
  begin
    frm_scraper.txt_scraper_info_game.Text := 'Scraping for "' + GameInfo.name + '"';
    frm_scraper.txt_scraper_info.Text := 'Download data';
    application.ProcessMessages;
    scrapedGame := vScraper_TGDB.Scrape_With_GameName(Platform_ID, GameInfo.name);
    if scrapedGame.count = '0' then
      scrapedGame := vScraper_TGDB.Scrape_With_GameName(Platform_ID, GameInfo.rom);
    frm_scraper_tgdb_opt.show_results_for_gamename(scrapedGame);
    frm_scraper_tgdb_opt.ShowModal;
  end;
end;

procedure TSCRAPER_TGDB.start(Platform_Type: TEmulatorSelected);
var
  Num: integer;
  GameInfo: TScraperGameInfo;
begin
  Num := front_action.grid_selected;
  if frm_scraper.lbl_scraper_count_value.Text = '1' then
  begin
    GameInfo.name := dm.tArcadename.AsString;
    GameInfo.rom := dm.tArcaderom.AsString;

    scrape_by_platform_one_game(vScraper_TGDB.get_platform_string(Platform_Type),
      vScraper_TGDB.get_platform_id(Platform_Type), vScraper_TGDB.get_platform_const_num(Platform_Type),
      GameInfo);
  end
  else
  begin
    scrape_by_platform(vScraper_TGDB.get_platform_string(Platform_Type),
      vScraper_TGDB.get_platform_id(Platform_Type), vScraper_TGDB.get_platform_const_num(Platform_Type),
      count_roms);
  end;
end;

end.
