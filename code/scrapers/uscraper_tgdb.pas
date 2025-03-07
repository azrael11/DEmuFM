unit uscraper_tgdb;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
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
  TGameScraperThread = class(TThread)
  private
    FPlatformName: String;
    FPlatformID, FNum: Integer;
    FStopRequested: Boolean;
    FProgressValue: Integer;
    FProgressText: String;

    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(const PlatformName: String; PlatformID, Num: Integer);
    procedure RequestStop;
  end;

type
  TSCRAPER_TGDB = class
  private
    count_roms: Integer;

    function getCountRoms(Platform_Type: String): Integer;
    procedure reload_platform;

  public

    constructor Create(AOwner: TComponent; Platform_Type: String; GameName, rom: String; only_one: Boolean);
    destructor destroy; override;

    procedure initStartProces(platformType: string; GameName, RomName: String; only_one: Boolean);
    procedure startScaping(platformType: string);

    procedure scrapeGamesByPlatform(Platform_Name: String; Platform_ID, Num: Integer);
    procedure scrapeGameByPlatform(Platform_Name: String; Platform_ID: Integer; GameInfo: TScraperGameInfo);

    // Config
    procedure getTGDBGames(update: Boolean; Platform_Name: string; pbar: TProgressBar; ptxt: TText);
    procedure getTGDBGenres(update: Boolean; pbar: TProgressBar; ptxt: TText);
    procedure getTGDBPublishers(update: Boolean; pbar: TProgressBar; ptxt: TText);
    procedure getTGDBDevelopers(update: Boolean; pbar: TProgressBar; ptxt: TText);
    //
  end;

var
  scrape_tgdb: TSCRAPER_TGDB;
  GameScraperThread: TGameScraperThread;
  name_s: string;

implementation

uses
  scraper_tgdb,
  main,
  uarcade_actions,
  scraper_tgdb_opt,
  emu_functions,
  front_main, uDataModule;

{ TSCREENSCAPER_ARCADE }

constructor TSCRAPER_TGDB.Create(AOwner: TComponent; Platform_Type: String; GameName, rom: String; only_one: Boolean);
begin
  count_roms := getCountRoms(Platform_Type);
  scraperTGDB := TTGDB_SCRAPER.Create;
  if scraperTGDB.checkPlatformID then
    initStartProces(Platform_Type, GameName, rom, only_one);
end;

destructor TSCRAPER_TGDB.destroy;
begin
  FreeAndNil(scrape_tgdb);
  inherited;
end;

function TSCRAPER_TGDB.getCountRoms(Platform_Type: string): Integer;
begin
  if Platform_Type = 'arcade' then
    result := dm.tArcade.RecordCount
  else if Platform_Type = 'nes' then
    result := dm.tNes.RecordCount;
end;

procedure TSCRAPER_TGDB.getTGDBDevelopers(update: Boolean; pbar: TProgressBar; ptxt: TText);
var
  vi: Integer;
  developers: T_TGDB_SCRAPER_DEVELOPERS;
  count_new, count_old: Integer;
begin
  developers := scraperTGDB.getDevelopers;
  pbar.Visible := true;
  ptxt.Visible := true;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (developers.count).ToInteger;
  if update = false then
  begin
    if (dm.tTGDBDevelopers.RecordCount > 0) and (update = false) then
      dm.tTGDBDevelopers.ExecSQL('Delete From ' + dm.tTGDBDevelopers.TableName);

    for vi := 0 to (developers.count).ToInteger - 1 do
    begin
      dm.query.SQL.Text := 'INSERT INTO ' + dm.tTGDBDevelopers.TableName + ' (id, name) VALUES (:id, :name)';
      dm.query.ParamByName('id').AsString := developers.developers[vi].id;
      dm.query.ParamByName('name').AsString := developers.developers[vi].name;
      dm.query.ExecSQL;
      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Developer "' + developers.developers[vi].name + '"';
      application.ProcessMessages;
    end;

    name_s := 'Database has " ' + vi.ToString + '" new Developers ';
    application.ProcessMessages;
  end
  else
  begin
    dm.tTGDBDevelopers.Active := true;
    count_new := 0;
    count_old := 0;
    for vi := 0 to (developers.count).ToInteger - 1 do
    begin
      if dm.tTGDBDevelopers.Locate('id', developers.developers[vi].id) then
      begin
        dm.query.SQL.Text := 'UPDATE ' + dm.tTGDBDevelopers.TableName + ' SET name=:name  WHERE id=:id';
        dm.query.ParamByName('id').AsString := developers.developers[vi].id;
        dm.query.ParamByName('name').AsString := developers.developers[vi].name;
        dm.query.ExecSQL;
        ptxt.Text := 'Updating Developer "' + developers.developers[vi].name + '"';
        Inc(count_old);
        application.ProcessMessages;
      end
      else
      begin
        dm.query.SQL.Text := 'INSERT INTO ' + dm.tTGDBDevelopers.TableName + ' (id, name) VALUES (:id, :name)';
        dm.query.ParamByName('id').AsString := developers.developers[vi].id;
        dm.query.ParamByName('name').AsString := developers.developers[vi].name;
        dm.query.ExecSQL;
        ptxt.Text := 'Add new Developer "' + developers.developers[vi].name + '"';
        Inc(count_new);
        application.ProcessMessages;
      end;
      pbar.Value := vi + 1;
    end;
    name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString + ' " Developers ';
    application.ProcessMessages;
    dm.tTGDBDevelopers.Active := false;
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER_TGDB.getTGDBGames(update: Boolean; Platform_Name: string; pbar: TProgressBar; ptxt: TText);
var
  count_db: Integer;
  game_name, rom_name, plat_name, game_name_db, name_s: string;
  count_new, count_old, plat_id, plat_const: Integer;
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

procedure TSCRAPER_TGDB.getTGDBGenres(update: Boolean; pbar: TProgressBar; ptxt: TText);
var
  vi, count_db: Integer;
  genres: T_TGDB_SCRAPER_GENRES;
  count_new, count_old: Integer;
begin
  genres := scraperTGDB.getGenres;
  pbar.Visible := true;
  ptxt.Visible := true;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (genres.count).ToInteger;
  if update = false then
  begin
    if (dm.tTGDBGenres.RecordCount > 0) and (update = false) then
      dm.tTGDBGenres.ExecSQL('Delete From ' + dm.tTGDBGenres.TableName);
    for vi := 0 to (genres.count).ToInteger - 1 do
    begin
      dm.query.SQL.Text := 'INSERT INTO ' + dm.tTGDBGenres.TableName + ' (id, name) VALUES (:id, :name)';
      dm.query.ParamByName('id').AsString := genres.genres[vi].id;
      dm.query.ParamByName('name').AsString := genres.genres[vi].name;
      dm.query.ExecSQL;
      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Genre "' + genres.genres[vi].name + '"';
      application.ProcessMessages;
    end;
    name_s := 'Database has " ' + vi.ToString + '" new Genres ';
    application.ProcessMessages;
  end
  else
  begin
    dm.tTGDBGenres.Active := true;
    count_new := 0;
    count_old := 0;
    for vi := 0 to (genres.count).ToInteger - 1 do
    begin
      if dm.tTGDBGenres.Locate('id', genres.genres[vi].id) then
      begin
        dm.query.SQL.Text := 'UPDATE ' + dm.tTGDBGenres.TableName + ' SET name=:name  WHERE id=:id';
        dm.query.ParamByName('id').AsString := genres.genres[vi].id;
        dm.query.ParamByName('name').AsString := genres.genres[vi].name;
        dm.query.ExecSQL;
        ptxt.Text := 'Updating Genre "' + genres.genres[vi].name + '"';
        Inc(count_old);
      end
      else
      begin
        dm.query.SQL.Text := 'INSERT INTO ' + dm.tTGDBGenres.TableName + ' (id, name) VALUES (:id, :name)';
        dm.query.ParamByName('id').AsString := genres.genres[vi].id;
        dm.query.ParamByName('name').AsString := genres.genres[vi].name;
        dm.query.ExecSQL;
        ptxt.Text := 'Add new Genre "' + genres.genres[vi].name + '"';
        Inc(count_new);
      end;
      pbar.Value := vi + 1;
      application.ProcessMessages;
    end;
    name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString + ' " Genres ';
    application.ProcessMessages;
    dm.tTGDBGenres.Active := false;
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER_TGDB.getTGDBPublishers(update: Boolean; pbar: TProgressBar; ptxt: TText);
var
  vi: Integer;
  publishers: T_TGDB_SCRAPER_PUBLISHERS;
  count_new, count_old: Integer;
begin
  publishers := scraperTGDB.getPublishers;
  pbar.Visible := true;
  ptxt.Visible := true;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (publishers.count).ToInteger;
  if update = false then
  begin
    if (dm.tTGDBPublishers.RecordCount > 0) and (update = false) then
      dm.tTGDBDevelopers.ExecSQL('Delete From ' + dm.tTGDBPublishers.TableName);

    for vi := 0 to (publishers.count).ToInteger - 1 do
    begin
      dm.query.SQL.Text := 'INSERT INTO ' + dm.tTGDBPublishers.TableName + ' (id, name) VALUES (:id, :name)';
      dm.query.ParamByName('id').AsString := publishers.publishers[vi].id;
      dm.query.ParamByName('name').AsString := publishers.publishers[vi].name;
      dm.query.ExecSQL;
      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Publisher "' + publishers.publishers[vi].name + '"';
      application.ProcessMessages;
    end;
    name_s := 'Database has " ' + vi.ToString + '" new Publishers ';
    application.ProcessMessages;
  end
  else
  begin
    dm.tTGDBPublishers.Active := true;
    count_new := 0;
    count_old := 0;
    for vi := 0 to (publishers.count).ToInteger - 1 do
    begin
      if dm.tTGDBPublishers.Locate('id', publishers.publishers[vi].id) then
      begin
        dm.query.SQL.Text := 'UPDATE ' + dm.tTGDBPublishers.TableName + ' SET name=:name  WHERE id=:id';
        dm.query.ParamByName('id').AsString := publishers.publishers[vi].id;
        dm.query.ParamByName('name').AsString := publishers.publishers[vi].name;
        dm.query.ExecSQL;
        ptxt.Text := 'Updating Publisher "' + publishers.publishers[vi].name + '"';
        Inc(count_old);
      end
      else
      begin
        dm.query.SQL.Text := 'INSERT INTO ' + dm.tTGDBPublishers.TableName + ' (id, name) VALUES (:id, :name)';
        dm.query.ParamByName('id').AsString := publishers.publishers[vi].id;
        dm.query.ParamByName('name').AsString := publishers.publishers[vi].name;
        dm.query.ExecSQL;
        ptxt.Text := 'Add new Publisher "' + publishers.publishers[vi].name + '"';
        Inc(count_new);
        application.ProcessMessages;
      end;

      pbar.Value := vi + 1;
      application.ProcessMessages;
      dm.tTGDBPublishers.Active := false;
    end;
    name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString + ' " Publishers ';
    application.ProcessMessages;
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER_TGDB.initStartProces(platformType: string; GameName, RomName: String; only_one: Boolean);
begin
  frm_scraper.lbl_scraper_platform_value.Text := platformType;
  if (GameName = '') and (only_one = false) then
  begin
    frm_scraper.lbl_scraper_count_value.Text := count_roms.ToString;
    frm_scraper.lbl_scraper_games_value.Text := 'All';
    frm_scraper.lbl_scraper_missing_value.Text := scraperTGDB.getMissingRoms(platformType).ToString;
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
  main.frm_main.eff_blur_main.Enabled := true;
  frm_scraper.rect_scraper.Visible := true;
end;

procedure TSCRAPER_TGDB.reload_platform;
begin
  front_action.destroy_grid;
  front_action.createGrid(dm.tConfigcurrent_emu.AsString);
end;

procedure TSCRAPER_TGDB.scrapeGamesByPlatform(Platform_Name: String; Platform_ID, Num: Integer);
var
  tempInfo: TScraperGameInfo;
  tempGame: T_TGDB_SCRAPER_GAME;
  tempGameImages: T_TGDB_SCRAPER_GAME_IMAGES;
  vi: Integer;
  exists_allready: Boolean;

  procedure imagesAddOrUpdate(Add: Boolean; tGame: T_TGDB_SCRAPER_GAME; tImages: T_TGDB_SCRAPER_GAME_IMAGES);
  var
    vk: Integer;
    iBitmap: FMX.Graphics.TBitmap;
    savePath: string;
    boxartPath: string;
    finalFilename: string;
  begin
    if Add then
    begin
      dm.query.SQL.Clear;
      dm.query.SQL.Text := 'INSERT INTO ' + dm.tArcadeTGDBImages.TableName + ' (id, rom, img_type, side, filename, resolution, path, url_path) VALUES (:id, :rom, :img_type, :side, :filename, :resolution, :path, :url_path)';
      dm.query.ParamByName('id').AsString := tGame.box_art.game[0].data[0].id;
      dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;
      dm.query.ParamByName('img_type').AsString := tGame.box_art.game[0].data[0].vtype;
      dm.query.ParamByName('side').AsString := '';
      finalFilename := ExtractFileName(ExtractFileName(StringReplace(tGame.box_art.game[0].data[0].filename, '/', '\', [rfReplaceAll])));
      dm.query.ParamByName('filename').AsString := finalFilename;
      dm.query.ParamByName('resolution').AsString := tGame.box_art.game[0].data[0].resolution;
      savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tGame.box_art.game[0].data[0].vtype + PathDelim;
      dm.query.ParamByName('path').AsString := savePath;
      try
        iBitmap := uInternet_files.Get_Image_new(tGame.box_art.base_url.thumb + tGame.box_art.game[0].data[0].filename);
        iBitmap.SaveToFile(savePath + finalFilename);
        dm.query.ParamByName('url_path').AsString := (tGame.box_art.base_url.thumb + tGame.box_art.game[0].data[0].filename);
        dm.query.ExecSQL;
      finally
        FreeAndNil(iBitmap);
      end;

      for vk := 0 to High(tImages.images) - 1 do
      begin
        dm.query.SQL.Clear;
        dm.query.SQL.Text := 'INSERT INTO ' + dm.tArcadeTGDBImages.TableName + ' (id, rom, img_type, side, filename, resolution, path, url_path) VALUES (:id, :rom, :img_type, :side, :filename, :resolution, :path, :url_path)';
        dm.query.ParamByName('id').AsString := tImages.images[vk].id;
        dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;
        dm.query.ParamByName('img_type').AsString := tImages.images[vk].vtype;
        dm.query.ParamByName('side').AsString := tImages.images[vk].side;
        finalFilename := ExtractFileName(ExtractFileName(StringReplace(tImages.images[vk].filename, '/', '\', [rfReplaceAll])));
        dm.query.ParamByName('filename').AsString := finalFilename;
        dm.query.ParamByName('resolution').AsString := tImages.images[vk].resolution;

        if tImages.images[vk].vtype = 'boxart' then
        begin
          if ContainsText(tImages.images[vk].filename, 'front') then
            savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tImages.images[vk].vtype + PathDelim + 'front' + PathDelim
          else if ContainsText(tImages.images[vk].filename, 'back') then
            savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tImages.images[vk].vtype + PathDelim + 'back' + PathDelim
        end
        else
          savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tImages.images[vk].vtype + PathDelim;
        dm.query.ParamByName('path').AsString := savePath;

        if tImages.images[vk].vtype = 'boxart' then
        begin
          try
            iBitmap := uInternet_files.Get_Image_new(tImages.base_url.original + tImages.images[vk].filename);
            iBitmap.SaveToFile(savePath + finalFilename);
            dm.query.ParamByName('url_path').AsString := tImages.base_url.original + tImages.images[vk].filename;
            dm.query.ExecSQL;
            iBitmap := nil;
            iBitmap := uInternet_files.Get_Image_new(tImages.base_url.thumb + tImages.images[vk].filename);
            iBitmap.SaveToFile(savePath + 'thumb_' + finalFilename);
            dm.query.ParamByName('url_path').AsString := tImages.base_url.thumb + tImages.images[vk].filename;
            dm.query.ExecSQL;
          finally
            FreeAndNil(iBitmap);
          end;
        end
        else
        begin
          try
            iBitmap := uInternet_files.Get_Image_new(tImages.base_url.original + tImages.images[vk].filename);
            iBitmap.SaveToFile(savePath + finalFilename);
            dm.query.ParamByName('url_path').AsString := tImages.base_url.original + tImages.images[vk].filename;
            dm.query.ExecSQL;
          finally
            FreeAndNil(iBitmap);
          end;
        end;
      end;
    end;
  end;

  procedure updateRecordInDatabase(tGame: T_TGDB_SCRAPER_GAME; tImages: T_TGDB_SCRAPER_GAME_IMAGES);
  begin
    dm.query.SQL.Clear;
    dm.query.SQL.Text := 'UPDATE ' + dm.tArcadeTGDB.TableName +
      ' SET  id=:id, title=:title, release_date=:release_date, platform_id=:platform_id, players=:players, overview=:overview, last_updated=:last_updated, rating=:rating, coop=:coop, youtube=:youtube, os=:os, processor=:processor, ram=:ram, hdd=:hdd, video=:video, sound=:sound, developers=:developers, genres=:genres, publishers=:publishers, alternates=:alternates WHERE rom=:rom';
    dm.query.ParamByName('id').AsString := tGame.games[0].id;
    dm.query.ParamByName('title').AsString := tGame.games[0].title;
    dm.query.ParamByName('release_date').AsString := tGame.games[0].release_date;
    dm.query.ParamByName('platform_id').AsString := tGame.games[0].Platform_ID;
    dm.query.ParamByName('players').AsString := tGame.games[0].players;
    dm.query.ParamByName('overview').AsString := tGame.games[0].overview;
    dm.query.ParamByName('last_updated').AsString := tGame.games[0].last_updated;
    dm.query.ParamByName('rating').AsString := tGame.games[0].rating;
    dm.query.ParamByName('coop').AsString := tGame.games[0].coop;
    dm.query.ParamByName('youtube').AsString := tGame.games[0].youtube;
    dm.query.ParamByName('os').AsString := tGame.games[0].os;
    dm.query.ParamByName('processor').AsString := tGame.games[0].processor;
    dm.query.ParamByName('ram').AsString := tGame.games[0].ram;
    dm.query.ParamByName('hdd').AsString := tGame.games[0].hdd;
    dm.query.ParamByName('video').AsString := tGame.games[0].video;
    dm.query.ParamByName('sound').AsString := tGame.games[0].sound;
    dm.query.ParamByName('developers').AsString := tGame.games[0].developers[0];
    dm.query.ParamByName('genres').AsString := tGame.games[0].genres[0];
    dm.query.ParamByName('publishers').AsString := tGame.games[0].publishers[0];
    if tGame.games[0].alternates <> nil then
      dm.query.ParamByName('alternates').AsString := tGame.games[0].id
    else
      dm.query.ParamByName('alternates').AsString := '';
    dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;

    imagesAddOrUpdate(true, tGame, tImages);
  end;

  procedure addNewRecordToDatabase(tGame: T_TGDB_SCRAPER_GAME; tImages: T_TGDB_SCRAPER_GAME_IMAGES);
  var
    media, image_data_path, tgdb: string;
  begin
    dm.query.SQL.Clear;
    dm.query.SQL.Text := 'INSERT INTO ' + dm.tArcadeTGDB.TableName +
      ' (id, title, release_date, platform_id, players, overview, last_updated, rating, coop, youtube, os, processor, ram, hdd, video, sound, developers, genres, publishers, alternates, rom) VALUES (:id, :title, :release_date, :platform_id, :players, :overview, :last_updated, :rating, :coop, :youtube, :os, :processor, :ram, :hdd, :video, :sound, :developers, :genres, :publishers, :alternates, :rom)';
    dm.query.ParamByName('id').AsString := tempGame.games[0].id;
    dm.query.ParamByName('title').AsString := tempGame.games[0].title;
    dm.query.ParamByName('release_date').AsString := tempGame.games[0].release_date;
    dm.query.ParamByName('platform_id').AsString := tempGame.games[0].Platform_ID;
    dm.query.ParamByName('players').AsString := tempGame.games[0].players;
    dm.query.ParamByName('overview').AsString := tempGame.games[0].overview;
    dm.query.ParamByName('last_updated').AsString := tempGame.games[0].last_updated;
    dm.query.ParamByName('rating').AsString := tempGame.games[0].rating;
    dm.query.ParamByName('coop').AsString := tempGame.games[0].coop;
    dm.query.ParamByName('youtube').AsString := tempGame.games[0].youtube;
    dm.query.ParamByName('os').AsString := tempGame.games[0].os;
    dm.query.ParamByName('processor').AsString := tempGame.games[0].processor;
    dm.query.ParamByName('ram').AsString := tempGame.games[0].ram;
    dm.query.ParamByName('hdd').AsString := tempGame.games[0].hdd;
    dm.query.ParamByName('video').AsString := tempGame.games[0].video;
    dm.query.ParamByName('sound').AsString := tempGame.games[0].sound;
    dm.query.ParamByName('developers').AsString := tempGame.games[0].developers[0];
    dm.query.ParamByName('genres').AsString := tempGame.games[0].genres[0];
    dm.query.ParamByName('publishers').AsString := tempGame.games[0].publishers[0];
    if tempGame.games[0].alternates <> nil then
      dm.query.ParamByName('alternates').AsString := tempGame.games[0].id
    else
      dm.query.ParamByName('alternates').AsString := '';
    dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;
    dm.query.ExecSQL;

    imagesAddOrUpdate(true, tGame, tImages);
  end;

  procedure setStateForm(atStart: Boolean);
  begin
    if atStart then
    begin
      scraper_tgdb.frm_scraper.pressed_stop := false;
      frm_scraper.spb_scraper_start.Text := 'Stop';
      frm_scraper.anim_float_scraper_warning.Enabled := true;
      frm_scraper.spb_scraper_cancel.Enabled := false;
      frm_scraper.cb_scraper_only_missing.Enabled := false;
      frm_scraper.txt_scraper_info_game.Visible := true;
      frm_scraper.txt_scraper_info.Visible := true;
      frm_scraper.prbar_scraper.Visible := true;
      frm_scraper.prbar_scraper.Value := 0;
    end
    else
    begin
      frm_scraper.spb_scraper_start.Text := 'Start';
      frm_scraper.anim_float_scraper_warning.Enabled := false;
      frm_scraper.txt_scraper_info_game.Visible := false;
      frm_scraper.txt_scraper_info.Visible := false;
      frm_scraper.prbar_scraper.Visible := false;
      frm_scraper.cb_scraper_only_missing.Enabled := true;
      frm_scraper.spb_scraper_cancel.Enabled := true;
    end;
  end;

  procedure checkDevelopersPublishersGenres;
  begin
    dm.tTGDBDevelopers.Active := true;
    dm.tTGDBPublishers.Active := true;
    dm.tTGDBGenres.Active := true;
    if dm.tTGDBDevelopers.RecordCount = 0 then
    begin
      frm_scraper.txt_scraper_info_game.Text := 'Scraping for Developers (This happend only one time)';
      application.ProcessMessages;
      getTGDBDevelopers(false, frm_scraper.prbar_scraper, frm_scraper.txt_scraper_info);
    end;

    if dm.tTGDBPublishers.RecordCount = 0 then
    begin
      frm_scraper.txt_scraper_info_game.Text := 'Scraping for Publishers (This happend only one time)';
      application.ProcessMessages;
      getTGDBPublishers(false, frm_scraper.prbar_scraper, frm_scraper.txt_scraper_info);
    end;

    if dm.tTGDBGenres.RecordCount = 0 then
    begin
      frm_scraper.txt_scraper_info_game.Text := 'Scraping for Genres (This happend only one time)';
      application.ProcessMessages;
      getTGDBGenres(false, frm_scraper.prbar_scraper, frm_scraper.txt_scraper_info);
    end;
    dm.tTGDBDevelopers.Active := false;
    dm.tTGDBPublishers.Active := false;
    dm.tTGDBGenres.Active := false;
  end;

begin
  setStateForm(true);
  scraperTGDB.checkPlatformID;

  if frm_scraper.lbl_scraper_missing_value.Text = '0' then
  begin
    if frm_scraper.cb_scraper_only_missing.isChecked then
    begin
      TDialogService.MessageDialog('There is nothing to do everething is ok.', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
        procedure(const AResult: TModalResult)
        begin
          if AResult = mrOk then
          begin
            frm_scraper.spb_scraper_start.Text := 'Start';
            frm_scraper.anim_float_scraper_warning.Enabled := false;
            frm_scraper.spb_scraper_cancel.Enabled := false;
            frm_scraper.cb_scraper_only_missing.Enabled := true;
            exit;
          end;
        end);
    end
    else
      // updateRecordInDatabase;
  end
  else
  begin
    checkDevelopersPublishersGenres;
    vi := 1;
    with front_action.tmpTable do
    begin
      first;
      while not eof do
      begin
        tempInfo.name := FieldByName('name').AsString;
        tempInfo.rom := FieldByName('rom').AsString;

        tempGame := scraperTGDB.getScrapeRom(Platform_ID, tempInfo.name);
        tempGameImages := scraperTGDB.getScrapeRomImages(tempGame.games[0].id);

        frm_scraper.txt_scraper_info_game.Text := 'Scraping for "' + tempInfo.name + '"';
        frm_scraper.txt_scraper_info.Text := 'Download data (' + vi.ToString + ' from ' + Num.ToString + ')';

        application.ProcessMessages;

        dm.tArcadeTGDB.Active := true;
        exists_allready := dm.tArcadeTGDB.Locate('rom', tempInfo.rom);
        dm.tArcadeTGDB.Active := false;
        if exists_allready then
          updateRecordInDatabase(tempGame, tempGameImages)
        else
          addNewRecordToDatabase(tempGame, tempGameImages);

        Inc(vi);
        frm_scraper.prbar_scraper.Value := vi;
        if scraper_tgdb.frm_scraper.pressed_stop then
        begin
          // scrape_tgdb.prepear_start(emu_active, '', '', false);
          break;
        end;
        next;
      end;
    end;
  end;

  reload_platform;
  setStateForm(false);
end;

procedure TSCRAPER_TGDB.scrapeGameByPlatform(Platform_Name: String; Platform_ID: Integer; GameInfo: TScraperGameInfo);
var
  scrapedGame: T_TGDB_SCRAPER_GAME;
begin
  scraper_tgdb.frm_scraper.pressed_stop := false;
  frm_scraper.spb_scraper_start.Text := 'Stop';
  frm_scraper.anim_float_scraper_warning.Enabled := true;
  frm_scraper.spb_scraper_cancel.Enabled := false;
  frm_scraper.cb_scraper_only_missing.Enabled := false;
  frm_scraper.txt_scraper_info_game.Visible := true;
  frm_scraper.txt_scraper_info.Visible := true;
  frm_scraper.prbar_scraper.Visible := true;
  frm_scraper.prbar_scraper.Value := 0;
  //
  if frm_scraper.lbl_scraper_missing_value.Text = '0' then
  begin
    if frm_scraper.cb_scraper_only_missing.isChecked then
    begin
      TDialogService.MessageDialog('There is nothing to do everething is ok.', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
        procedure(const AResult: TModalResult)
        begin
          if AResult = mrOk then
          begin
            frm_scraper.spb_scraper_start.Text := 'Start';
            frm_scraper.anim_float_scraper_warning.Enabled := false;
            frm_scraper.spb_scraper_cancel.Enabled := false;
            frm_scraper.cb_scraper_only_missing.Enabled := true;
            exit;
          end;
        end);
    end
    else
    begin
      frm_scraper.txt_scraper_info_game.Text := 'Scraping for "' + GameInfo.name + '"';
      frm_scraper.txt_scraper_info.Text := 'Download data';
      application.ProcessMessages;
      scrapedGame := scraperTGDB.getScrapeRom(Platform_ID, GameInfo.rom);
      if scrapedGame.count = '0' then
        scrapedGame := scraperTGDB.getScrapeRom(Platform_ID, GameInfo.name);
      frm_scraper_tgdb_opt.show_results_for_gamename(scrapedGame);
      frm_scraper_tgdb_opt.ShowModal;
    end;
  end
  else
  begin
    frm_scraper.txt_scraper_info_game.Text := 'Scraping for "' + GameInfo.name + '"';
    frm_scraper.txt_scraper_info.Text := 'Download data';
    application.ProcessMessages;
    scrapedGame := scraperTGDB.getScrapeRom(Platform_ID, GameInfo.name);
    if scrapedGame.count = '0' then
      scrapedGame := scraperTGDB.getScrapeRom(Platform_ID, GameInfo.rom);
    frm_scraper_tgdb_opt.show_results_for_gamename(scrapedGame);
    frm_scraper_tgdb_opt.ShowModal;
  end;
end;

procedure TSCRAPER_TGDB.startScaping(platformType: string);
var
  Num: Integer;
  GameInfo: TScraperGameInfo;
begin
  Num := front_action.grid_selected;
  if frm_main.lay_game.Visible then
  begin
    GameInfo.name := dm.tArcadename.AsString;
    GameInfo.rom := dm.tArcaderom.AsString;
    scrapeGameByPlatform(dm.tConfigcurrent_emu.AsString, scraperTGDB.getPlatformID(platformType), GameInfo);
  end
  else
  begin
//    scrapeGamesByPlatform(dm.tConfigcurrent_emu.AsString, scraperTGDB.getPlatformID(platformType), count_roms);
    GameScraperThread := TGameScraperThread.Create('PlatformName', scraperTGDB.getPlatformID(platformType), count_roms);
  end;
end;

{ TGameScraperThread }

constructor TGameScraperThread.Create(const PlatformName: String; PlatformID, Num: Integer);
begin
  inherited Create(false);
  FreeOnTerminate := true;
  FPlatformName := PlatformName;
  FPlatformID := PlatformID;
  FNum := Num;
  FStopRequested := false;
end;

procedure TGameScraperThread.Execute;
var
  vi: Integer;
  tempInfo: TScraperGameInfo;
  tempGame: T_TGDB_SCRAPER_GAME;
  tempGameImages: T_TGDB_SCRAPER_GAME_IMAGES;
  exists_allready: Boolean;
  procedure imagesAddOrUpdate(Add: Boolean; tGame: T_TGDB_SCRAPER_GAME; tImages: T_TGDB_SCRAPER_GAME_IMAGES);
  var
    vk: Integer;
    iBitmap: FMX.Graphics.TBitmap;
    savePath: string;
    boxartPath: string;
    finalFilename: string;
  begin
    if Add then
    begin
      dm.query.SQL.Clear;
      dm.query.SQL.Text := 'INSERT INTO ' + dm.tArcadeTGDBImages.TableName + ' (id, rom, img_type, side, filename, resolution, path, url_path) VALUES (:id, :rom, :img_type, :side, :filename, :resolution, :path, :url_path)';
      dm.query.ParamByName('id').AsString := tGame.box_art.game[0].data[0].id;
      dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;
      dm.query.ParamByName('img_type').AsString := tGame.box_art.game[0].data[0].vtype;
      dm.query.ParamByName('side').AsString := '';
      finalFilename := ExtractFileName(ExtractFileName(StringReplace(tGame.box_art.game[0].data[0].filename, '/', '\', [rfReplaceAll])));
      dm.query.ParamByName('filename').AsString := finalFilename;
      dm.query.ParamByName('resolution').AsString := tGame.box_art.game[0].data[0].resolution;
      savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tGame.box_art.game[0].data[0].vtype + PathDelim;
      dm.query.ParamByName('path').AsString := savePath;
      try
        iBitmap := uInternet_files.Get_Image_new(tGame.box_art.base_url.thumb + tGame.box_art.game[0].data[0].filename);
        iBitmap.SaveToFile(savePath + finalFilename);
        dm.query.ParamByName('url_path').AsString := (tGame.box_art.base_url.thumb + tGame.box_art.game[0].data[0].filename);
        dm.query.ExecSQL;
      finally
        FreeAndNil(iBitmap);
      end;

      for vk := 0 to High(tImages.images) - 1 do
      begin
        dm.query.SQL.Clear;
        dm.query.SQL.Text := 'INSERT INTO ' + dm.tArcadeTGDBImages.TableName + ' (id, rom, img_type, side, filename, resolution, path, url_path) VALUES (:id, :rom, :img_type, :side, :filename, :resolution, :path, :url_path)';
        dm.query.ParamByName('id').AsString := tImages.images[vk].id;
        dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;
        dm.query.ParamByName('img_type').AsString := tImages.images[vk].vtype;
        dm.query.ParamByName('side').AsString := tImages.images[vk].side;
        finalFilename := ExtractFileName(ExtractFileName(StringReplace(tImages.images[vk].filename, '/', '\', [rfReplaceAll])));
        dm.query.ParamByName('filename').AsString := finalFilename;
        dm.query.ParamByName('resolution').AsString := tImages.images[vk].resolution;

        if tImages.images[vk].vtype = 'boxart' then
        begin
          if ContainsText(tImages.images[vk].filename, 'front') then
            savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tImages.images[vk].vtype + PathDelim + 'front' + PathDelim
          else if ContainsText(tImages.images[vk].filename, 'back') then
            savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tImages.images[vk].vtype + PathDelim + 'back' + PathDelim
        end
        else
          savePath := dm.tConfigprj_media.AsString + dm.tConfigcurrent_emu.AsString + PathDelim + 'tgdb_images' + PathDelim + tImages.images[vk].vtype + PathDelim;
        dm.query.ParamByName('path').AsString := savePath;

        if tImages.images[vk].vtype = 'boxart' then
        begin
          try
            iBitmap := uInternet_files.Get_Image_new(tImages.base_url.original + tImages.images[vk].filename);
            iBitmap.SaveToFile(savePath + finalFilename);
            dm.query.ParamByName('url_path').AsString := tImages.base_url.original + tImages.images[vk].filename;
            dm.query.ExecSQL;
            iBitmap := nil;
            iBitmap := uInternet_files.Get_Image_new(tImages.base_url.thumb + tImages.images[vk].filename);
            iBitmap.SaveToFile(savePath + 'thumb_' + finalFilename);
            dm.query.ParamByName('url_path').AsString := tImages.base_url.thumb + tImages.images[vk].filename;
            dm.query.ExecSQL;
          finally
            FreeAndNil(iBitmap);
          end;
        end
        else
        begin
          try
            iBitmap := uInternet_files.Get_Image_new(tImages.base_url.original + tImages.images[vk].filename);
            iBitmap.SaveToFile(savePath + finalFilename);
            dm.query.ParamByName('url_path').AsString := tImages.base_url.original + tImages.images[vk].filename;
            dm.query.ExecSQL;
          finally
            FreeAndNil(iBitmap);
          end;
        end;
      end;
    end;
  end;

  procedure updateRecordInDatabase(tGame: T_TGDB_SCRAPER_GAME; tImages: T_TGDB_SCRAPER_GAME_IMAGES);
  begin
    dm.query.SQL.Clear;
    dm.query.SQL.Text := 'UPDATE ' + dm.tArcadeTGDB.TableName +
      ' SET  id=:id, title=:title, release_date=:release_date, platform_id=:platform_id, players=:players, overview=:overview, last_updated=:last_updated, rating=:rating, coop=:coop, youtube=:youtube, os=:os, processor=:processor, ram=:ram, hdd=:hdd, video=:video, sound=:sound, developers=:developers, genres=:genres, publishers=:publishers, alternates=:alternates WHERE rom=:rom';
    dm.query.ParamByName('id').AsString := tGame.games[0].id;
    dm.query.ParamByName('title').AsString := tGame.games[0].title;
    dm.query.ParamByName('release_date').AsString := tGame.games[0].release_date;
    dm.query.ParamByName('platform_id').AsString := tGame.games[0].Platform_ID;
    dm.query.ParamByName('players').AsString := tGame.games[0].players;
    dm.query.ParamByName('overview').AsString := tGame.games[0].overview;
    dm.query.ParamByName('last_updated').AsString := tGame.games[0].last_updated;
    dm.query.ParamByName('rating').AsString := tGame.games[0].rating;
    dm.query.ParamByName('coop').AsString := tGame.games[0].coop;
    dm.query.ParamByName('youtube').AsString := tGame.games[0].youtube;
    dm.query.ParamByName('os').AsString := tGame.games[0].os;
    dm.query.ParamByName('processor').AsString := tGame.games[0].processor;
    dm.query.ParamByName('ram').AsString := tGame.games[0].ram;
    dm.query.ParamByName('hdd').AsString := tGame.games[0].hdd;
    dm.query.ParamByName('video').AsString := tGame.games[0].video;
    dm.query.ParamByName('sound').AsString := tGame.games[0].sound;
    dm.query.ParamByName('developers').AsString := tGame.games[0].developers[0];
    dm.query.ParamByName('genres').AsString := tGame.games[0].genres[0];
    dm.query.ParamByName('publishers').AsString := tGame.games[0].publishers[0];
    if tGame.games[0].alternates <> nil then
      dm.query.ParamByName('alternates').AsString := tGame.games[0].id
    else
      dm.query.ParamByName('alternates').AsString := '';
    dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;

    imagesAddOrUpdate(true, tGame, tImages);
  end;

  procedure addNewRecordToDatabase(tGame: T_TGDB_SCRAPER_GAME; tImages: T_TGDB_SCRAPER_GAME_IMAGES);
  var
    media, image_data_path, tgdb: string;
  begin
    dm.query.SQL.Clear;
    dm.query.SQL.Text := 'INSERT INTO ' + dm.tArcadeTGDB.TableName +
      ' (id, title, release_date, platform_id, players, overview, last_updated, rating, coop, youtube, os, processor, ram, hdd, video, sound, developers, genres, publishers, alternates, rom) VALUES (:id, :title, :release_date, :platform_id, :players, :overview, :last_updated, :rating, :coop, :youtube, :os, :processor, :ram, :hdd, :video, :sound, :developers, :genres, :publishers, :alternates, :rom)';
    dm.query.ParamByName('id').AsString := tempGame.games[0].id;
    dm.query.ParamByName('title').AsString := tempGame.games[0].title;
    dm.query.ParamByName('release_date').AsString := tempGame.games[0].release_date;
    dm.query.ParamByName('platform_id').AsString := tempGame.games[0].Platform_ID;
    dm.query.ParamByName('players').AsString := tempGame.games[0].players;
    dm.query.ParamByName('overview').AsString := tempGame.games[0].overview;
    dm.query.ParamByName('last_updated').AsString := tempGame.games[0].last_updated;
    dm.query.ParamByName('rating').AsString := tempGame.games[0].rating;
    dm.query.ParamByName('coop').AsString := tempGame.games[0].coop;
    dm.query.ParamByName('youtube').AsString := tempGame.games[0].youtube;
    dm.query.ParamByName('os').AsString := tempGame.games[0].os;
    dm.query.ParamByName('processor').AsString := tempGame.games[0].processor;
    dm.query.ParamByName('ram').AsString := tempGame.games[0].ram;
    dm.query.ParamByName('hdd').AsString := tempGame.games[0].hdd;
    dm.query.ParamByName('video').AsString := tempGame.games[0].video;
    dm.query.ParamByName('sound').AsString := tempGame.games[0].sound;
    dm.query.ParamByName('developers').AsString := tempGame.games[0].developers[0];
    dm.query.ParamByName('genres').AsString := tempGame.games[0].genres[0];
    dm.query.ParamByName('publishers').AsString := tempGame.games[0].publishers[0];
    if tempGame.games[0].alternates <> nil then
      dm.query.ParamByName('alternates').AsString := tempGame.games[0].id
    else
      dm.query.ParamByName('alternates').AsString := '';
    dm.query.ParamByName('rom').AsString := front_action.tmpTable.FieldByName('rom').AsString;
    dm.query.ExecSQL;

    imagesAddOrUpdate(true, tGame, tImages);
  end;

begin
  try
    vi := 1;
    with front_action.tmpTable do
    begin
      first;
      while not eof and not FStopRequested do
      begin
        tempInfo.name := FieldByName('name').AsString;
        tempInfo.rom := FieldByName('rom').AsString;

        tempGame := scraperTGDB.getScrapeRom(FPlatformID, tempInfo.name);
        tempGameImages := scraperTGDB.getScrapeRomImages(tempGame.games[0].id);

        dm.tArcadeTGDB.Active := true;
        exists_allready := dm.tArcadeTGDB.Locate('rom', tempInfo.rom);
        dm.tArcadeTGDB.Active := false;

        if exists_allready then
          updateRecordInDatabase(tempGame, tempGameImages)
        else
          addNewRecordToDatabase(tempGame, tempGameImages);

        Inc(vi);
        FProgressValue := vi;
        FProgressText := 'Scraping for "' + tempInfo.name + '"';
        Synchronize(UpdateUI);

        next;
      end;
    end;
  except
    on E: Exception do
      Synchronize(
        procedure
        begin
          ShowMessage('Error in thread: ' + E.Message);
        end);
  end;
end;

procedure TGameScraperThread.RequestStop;
begin
  FStopRequested := true;
end;

procedure TGameScraperThread.UpdateUI;
begin
  frm_scraper.prbar_scraper.Value := FProgressValue;
  frm_scraper.txt_scraper_info_game.Text := FProgressText;
  frm_scraper.txt_scraper_info.Text := Format('Download data (%d of %d)', [FProgressValue, FNum]);
end;

end.
