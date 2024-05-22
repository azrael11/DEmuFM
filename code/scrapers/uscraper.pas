unit uscraper;

interface

uses
  System.Classes,
  System.SysUtils,
  uTheGamesDatabase,
  FMX.Dialogs,
  umain_config,
  FMX.StdCtrls,
  FMX.Forms,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FireDAC.Stan.Param,
  uInternet_files;

type
  TSCRAPER = class
  private
    count_roms: integer;

    function get_num_of_roms(Platform_Type: TEMULATION_STATE): integer;

  public

    constructor create(AOwner: TComponent; Platform_Type: TEMULATION_STATE; GameName, Rom: String);
    destructor destroy; override;

    procedure prepear_start(Platform_Type: TEMULATION_STATE; GameName: String);

    procedure start(Platform_Type: TEMULATION_STATE);

    procedure get_tgdb_games(update: boolean; platform_name: string; pbar: TProgressBar;
      ptxt: TText);
    procedure get_tgdb_genres(update: boolean; platform_name: string; pbar: TProgressBar;
      ptxt: TText);
    procedure get_tgdb_publishers(update: boolean; platform_name: string; pbar: TProgressBar;
      ptxt: TText);
    procedure get_tgdb_developers(update: boolean; platform_name: string; pbar: TProgressBar;
      ptxt: TText);
  end;

var
  Scrape: TSCRAPER;
  name_s: string;

implementation

uses
  udata,
  udata_emulation,
  scraper_tgdb,
  main;

var
  Data_Emu: TEMULATION_DATA_QUERIES;

  { TSCREENSCAPER_ARCADE }

constructor TSCRAPER.create(AOwner: TComponent; Platform_Type: TEMULATION_STATE;
  GameName, Rom: String);
begin
  Data_Emu := TEMULATION_DATA_QUERIES.create;
  count_roms := get_num_of_roms(Platform_Type);

  vScraper_TGDB := TTGDB_SCRAPER.create;

  if vScraper_TGDB.Check_Platforms_id_in_Database then
    prepear_start(Platform_Type, GameName);

end;

destructor TSCRAPER.destroy;
begin
  FreeAndNil(Scrape);
  inherited;
end;

function TSCRAPER.get_num_of_roms(Platform_Type: TEMULATION_STATE): integer;
begin
  result := Data_Emu.get_number_or_roms(Platform_Type);
end;

procedure TSCRAPER.get_tgdb_developers(update: boolean; platform_name: string; pbar: TProgressBar;
  ptxt: TText);
var
  vi, count_db: integer;
  developers: T_TGDB_SCRAPER_DEVELOPERS;
  dev_id, dev_name, plat_name: string;
  count_new, count_old: integer;
begin
  plat_name := platform_name + '_tgdb_developers';
  count_db := DSPFM_Data.table_count_db(plat_name);

  developers := vScraper_TGDB.get_developers;
  pbar.Visible := True;
  ptxt.Visible := True;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (developers.count).ToInteger;
  if update = false then
  begin
    if (count_db > 0) and (update = false) then
      DSPFM_Data.table_delete(plat_name);

    for vi := 0 to (developers.count).ToInteger - 1 do
    begin
      DSPFM_Data.Query.Close;
      DSPFM_Data.Query.SQL.Clear;
      DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';

      DSPFM_Data.Query.ParamByName('id').AsString := developers.developers[vi].id;
      DSPFM_Data.Query.ParamByName('name').AsString := developers.developers[vi].name;

      DSPFM_Data.Query.ExecSQL;

      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Developer "' + developers.developers[vi].name + '"';

      application.ProcessMessages;
    end;

    name_s := 'Database has " ' + vi.ToString + '" new Developers ';
  end
  else
  begin
    count_new := 0;
    count_old := 0;
    for vi := 0 to (developers.count).ToInteger - 1 do
    begin
      dev_id := developers.developers[vi].id;

      DSPFM_Data.Query.Close;
      DSPFM_Data.Query.SQL.Clear;
      DSPFM_Data.Query.SQL.Text := 'SELECT name FROM ' + plat_name + ' WHERE id=''' + dev_id + '''';
      DSPFM_Data.Query.Open;

      dev_name := DSPFM_Data.Query.Fields[0].AsString;

      if dev_name <> '' then
      begin
        DSPFM_Data.Query.Close;
        DSPFM_Data.Query.SQL.Clear;
        DSPFM_Data.Query.SQL.Text := 'UPDATE ' + plat_name + ' SET id=:id, name=:name WHERE id=:id';

        DSPFM_Data.Query.ParamByName('id').AsString := dev_id;
        DSPFM_Data.Query.ParamByName('name').AsString := dev_name;

        DSPFM_Data.Query.ExecSQL;

        ptxt.Text := 'Updating Developer "' + dev_name + '"';
        Inc(count_old);
      end
      else
      begin
        DSPFM_Data.Query.Close;
        DSPFM_Data.Query.SQL.Clear;
        DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';

        DSPFM_Data.Query.ParamByName('id').AsString := developers.developers[vi].id;
        DSPFM_Data.Query.ParamByName('name').AsString := developers.developers[vi].name;

        DSPFM_Data.Query.ExecSQL;

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

procedure TSCRAPER.get_tgdb_games(update: boolean; platform_name: string; pbar: TProgressBar;
  ptxt: TText);
var
  count_db: integer;
  game_name, rom_name, plat_name, game_name_db, name_s: string;
  count_new, count_old, plat_id, plat_const: integer;
  emu_state: TEMULATION_STATE;
  game_t: T_TGDB_SCRAPER_GAME;
  bitmap_t: TBitmap;
begin
  plat_name := platform_name + '_tgdb';
  count_db := DSPFM_Data.table_count_db(plat_name);

  DSPFM_Data.Query.Close;
  DSPFM_Data.Query.SQL.Clear;
  DSPFM_Data.Query.SQL.Text := 'SELECT COUNT(*) FROM ' + plat_name;
  DSPFM_Data.Query.Open;

  pbar.Visible := True;
  ptxt.Visible := True;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := DSPFM_Data.Query.Fields[0].AsInteger;

  plat_id := vScraper_TGDB.get_platform_id(emu_state);
  plat_const := vScraper_TGDB.get_platform_const_num(emu_state);

  DSPFM_Data.Query.Close;
  DSPFM_Data.Query.SQL.Clear;
  DSPFM_Data.Query.SQL.Text := 'SELECT name,rom FROM ' + plat_name + ' ORDER BY name ASC;';
  DSPFM_Data.Query.Open;

  if update then
  begin
    while not DSPFM_Data.Query.Eof do
    begin
      count_old := 0;
      count_new := 0;
      game_name := DSPFM_Data.Query.FieldByName('name').AsString;
      rom_name := DSPFM_Data.Query.FieldByName('rom').AsString;
      game_t := vScraper_TGDB.Scrape_With_GameName(plat_id, game_name);

      DSPFM_Data.Query_2.Close;
      DSPFM_Data.Query_2.SQL.Clear;
      DSPFM_Data.Query_2.SQL.Text := 'SELECT rom FROM ' + plat_name + ' WHERE rom=''' +
        rom_name + '''';
      DSPFM_Data.Query_2.Open;

      game_name_db := DSPFM_Data.Query_2.Fields[0].AsString;

      if game_name_db <> '' then
      begin
        DSPFM_Data.Query_2.Close;
        DSPFM_Data.Query_2.SQL.Clear;
        DSPFM_Data.Query_2.SQL.Text := 'UPDATE ' + plat_name + ' SET id=:id, title=:title, ' +
          ' release_date=:release_date, platform_id=:platform_id, players=:players, ' +
          ' overview=:overview, last_updated=:last_updated, rating=:rating, coop=:coop, ' +
          ' youtube=:youtube, os=:os, processor=:processor, ram=:ram, hdd=:hdd, video=:video, ' +
          ' sound=:sound, developers=:developers, genres=:genres, publishers=:publishers, ' +
          ' alternates=:alternates, box_art_original=:box_art_original, ' +
          ' box_art_small=:box_art_small, box_art_thumb=:box_art_thumb, ' +
          ' box_art_cropped=:box_art_cropped, box_art_medium=:box_art_medium, ' +
          ' box_art_large=:box_art_large, pic_1=:pic_1, pic_1_ext=:pic_1_ext, rom=:rom ' +
          ' WHERE rom=''' + rom_name + '''';

        DSPFM_Data.Query_2.ParamByName('id').AsString := game_t.games[0].id;
        DSPFM_Data.Query_2.ParamByName('title').AsString := game_t.games[0].title;
        DSPFM_Data.Query_2.ParamByName('release_date').AsString := game_t.games[0].release_date;
        DSPFM_Data.Query_2.ParamByName('platform_id').AsString := game_t.games[0].Platform_ID;
        DSPFM_Data.Query_2.ParamByName('players').AsString := game_t.games[0].players;
        DSPFM_Data.Query_2.ParamByName('overview').AsString := game_t.games[0].overview;
        DSPFM_Data.Query_2.ParamByName('last_updated').AsString := game_t.games[0].last_updated;
        DSPFM_Data.Query_2.ParamByName('rating').AsString := game_t.games[0].rating;
        DSPFM_Data.Query_2.ParamByName('coop').AsString := game_t.games[0].coop;
        DSPFM_Data.Query_2.ParamByName('youtube').AsString := game_t.games[0].youtube;
        DSPFM_Data.Query_2.ParamByName('os').AsString := game_t.games[0].os;
        DSPFM_Data.Query_2.ParamByName('processor').AsString := game_t.games[0].processor;
        DSPFM_Data.Query_2.ParamByName('ram').AsString := game_t.games[0].ram;
        DSPFM_Data.Query_2.ParamByName('hdd').AsString := game_t.games[0].hdd;
        DSPFM_Data.Query_2.ParamByName('video').AsString := game_t.games[0].video;
        DSPFM_Data.Query_2.ParamByName('sound').AsString := game_t.games[0].sound;
        DSPFM_Data.Query_2.ParamByName('developers').AsString := game_t.games[0].developers[0];
        DSPFM_Data.Query_2.ParamByName('genres').AsString := game_t.games[0].genres[0];
        DSPFM_Data.Query_2.ParamByName('publishers').AsString := game_t.games[0].publishers[0];
        if game_t.games[0].alternates <> nil then
          DSPFM_Data.Query_2.ParamByName('alternates').AsString := game_t.games[0].alternates[0]
        else
          DSPFM_Data.Query_2.ParamByName('alternates').AsString := '';
        DSPFM_Data.Query_2.ParamByName('box_art_original').AsString :=
          game_t.box_art.base_url.original;
        DSPFM_Data.Query_2.ParamByName('box_art_small').AsString := game_t.box_art.base_url.small;
        DSPFM_Data.Query_2.ParamByName('box_art_thumb').AsString := game_t.box_art.base_url.thumb;
        DSPFM_Data.Query_2.ParamByName('box_art_cropped').AsString :=
          game_t.box_art.base_url.cropped;
        DSPFM_Data.Query_2.ParamByName('box_art_medium').AsString := game_t.box_art.base_url.medium;
        DSPFM_Data.Query_2.ParamByName('box_art_large').AsString := game_t.box_art.base_url.large;
        DSPFM_Data.Query_2.ParamByName('pic_1').AsString := game_t.box_art.game[0].data[0].filename;
        DSPFM_Data.Query_2.ParamByName('pic_1_ext').AsString :=
          ExtractFileExt(game_t.box_art.game[0].data[0].filename);
        DSPFM_Data.Query_2.ParamByName('rom').AsString := rom_name;
        DSPFM_Data.Query_2.ExecSQL;

        ptxt.Text := 'Downloading Images for " ' + game_name + ' "';
        application.ProcessMessages;

        if game_t.box_art.base_url.original <> '' then
        begin
          bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.original +
            game_t.box_art.game[0].data[0].filename);
          bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_original' +
            ExtractFileExt(game_t.box_art.game[0].data[0].filename));
          FreeAndNil(bitmap_t);
        end;
        // if Temp_Game.box_art.base_url.small <> '' then
        // begin
        // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.small);
        // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_small' +
        // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
        // FreeAndNil(Temp_Bitmap);
        // end;
        if game_t.box_art.base_url.thumb <> '' then
        begin
          bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.thumb +
            game_t.box_art.game[0].data[0].filename);
          bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_thumb' +
            ExtractFileExt(game_t.box_art.game[0].data[0].filename));
          FreeAndNil(bitmap_t);
        end;
        // if Temp_Game.box_art.base_url.cropped <> '' then
        // begin
        // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.cropped);
        // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_cropped' +
        // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
        // FreeAndNil(Temp_Bitmap);
        // end;
        // if Temp_Game.box_art.base_url.medium <> '' then
        // begin
        // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.medium);
        // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_medium' +
        // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
        // FreeAndNil(Temp_Bitmap);
        // end;
        if game_t.box_art.base_url.large <> '' then
        begin
          bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.large +
            game_t.box_art.game[0].data[0].filename);
          bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_large' +
            ExtractFileExt(game_t.box_art.game[0].data[0].filename));
          FreeAndNil(bitmap_t);
        end;
        // Getting Video
        //
        //
        // Getting other stuff
        Inc(count_old);
        pbar.Value := pbar.Value + 1;
        DSPFM_Data.Query.Next;
      end
      else
      begin
        DSPFM_Data.Query_2.Close;
        DSPFM_Data.Query_2.SQL.Clear;
        DSPFM_Data.Query_2.SQL.Text :=
          'INSERT INTO arcade_tgdb (id, title, release_date, platform_id, players,' +
          'overview, last_updated, rating, coop, youtube, os, processor, ram, hdd, video, sound, developers,'
          + 'genres, publishers, alternates, box_art_original, box_art_small, box_art_thumb, box_art_cropped, '
          + 'box_art_medium, box_art_large, pic_1, pic_1_ext, rom) VALUES (:id, :title, :release_date, :platform_id, :players,'
          + ':overview, :last_updated, :rating, :coop, :youtube, :os, :processor, :ram, :hdd, :video, :sound, :developers,'
          + ':genres, :publishers, :alternates, :box_art_original, :box_art_small, :box_art_thumb, :box_art_cropped, '
          + ':box_art_medium, :box_art_large, :pic_1, :pic_1_ext, :rom)';

        DSPFM_Data.Query_2.ParamByName('id').AsString := game_t.games[0].id;
        DSPFM_Data.Query_2.ParamByName('title').AsString := game_t.games[0].title;
        DSPFM_Data.Query_2.ParamByName('release_date').AsString := game_t.games[0].release_date;
        DSPFM_Data.Query_2.ParamByName('platform_id').AsString := game_t.games[0].Platform_ID;
        DSPFM_Data.Query_2.ParamByName('players').AsString := game_t.games[0].players;
        DSPFM_Data.Query_2.ParamByName('overview').AsString := game_t.games[0].overview;
        DSPFM_Data.Query_2.ParamByName('last_updated').AsString := game_t.games[0].last_updated;
        DSPFM_Data.Query_2.ParamByName('rating').AsString := game_t.games[0].rating;
        DSPFM_Data.Query_2.ParamByName('coop').AsString := game_t.games[0].coop;
        DSPFM_Data.Query_2.ParamByName('youtube').AsString := game_t.games[0].youtube;
        DSPFM_Data.Query_2.ParamByName('os').AsString := game_t.games[0].os;
        DSPFM_Data.Query_2.ParamByName('processor').AsString := game_t.games[0].processor;
        DSPFM_Data.Query_2.ParamByName('ram').AsString := game_t.games[0].ram;
        DSPFM_Data.Query_2.ParamByName('hdd').AsString := game_t.games[0].hdd;
        DSPFM_Data.Query_2.ParamByName('video').AsString := game_t.games[0].video;
        DSPFM_Data.Query_2.ParamByName('sound').AsString := game_t.games[0].sound;
        DSPFM_Data.Query_2.ParamByName('developers').AsString := game_t.games[0].developers[0];
        DSPFM_Data.Query_2.ParamByName('genres').AsString := game_t.games[0].genres[0];
        DSPFM_Data.Query_2.ParamByName('publishers').AsString := game_t.games[0].publishers[0];
        if game_t.games[0].alternates <> nil then
          DSPFM_Data.Query_2.ParamByName('alternates').AsString := game_t.games[0].alternates[0]
        else
          DSPFM_Data.Query_2.ParamByName('alternates').AsString := '';
        DSPFM_Data.Query_2.ParamByName('box_art_original').AsString :=
          game_t.box_art.base_url.original;
        DSPFM_Data.Query_2.ParamByName('box_art_small').AsString := game_t.box_art.base_url.small;
        DSPFM_Data.Query_2.ParamByName('box_art_thumb').AsString := game_t.box_art.base_url.thumb;
        DSPFM_Data.Query_2.ParamByName('box_art_cropped').AsString :=
          game_t.box_art.base_url.cropped;
        DSPFM_Data.Query_2.ParamByName('box_art_medium').AsString := game_t.box_art.base_url.medium;
        DSPFM_Data.Query_2.ParamByName('box_art_large').AsString := game_t.box_art.base_url.large;
        DSPFM_Data.Query_2.ParamByName('pic_1').AsString := game_t.box_art.game[0].data[0].filename;
        DSPFM_Data.Query_2.ParamByName('pic_1_ext').AsString :=
          ExtractFileExt(game_t.box_art.game[0].data[0].filename);
        DSPFM_Data.Query_2.ParamByName('rom').AsString := rom_name;
        DSPFM_Data.Query_2.ExecSQL;

        ptxt.Text := 'Downloading Images for " ' + game_name + ' "';
        application.ProcessMessages;

        if game_t.box_art.base_url.original <> '' then
        begin
          bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.original +
            game_t.box_art.game[0].data[0].filename);
          bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_original' +
            ExtractFileExt(game_t.box_art.game[0].data[0].filename));
          FreeAndNil(bitmap_t);
        end;
        // if Temp_Game.box_art.base_url.small <> '' then
        // begin
        // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.small);
        // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_small' +
        // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
        // FreeAndNil(Temp_Bitmap);
        // end;
        if game_t.box_art.base_url.thumb <> '' then
        begin
          bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.thumb +
            game_t.box_art.game[0].data[0].filename);
          bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_thumb' +
            ExtractFileExt(game_t.box_art.game[0].data[0].filename));
          FreeAndNil(bitmap_t);
        end;
        // if Temp_Game.box_art.base_url.cropped <> '' then
        // begin
        // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.cropped);
        // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_cropped' +
        // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
        // FreeAndNil(Temp_Bitmap);
        // end;
        // if Temp_Game.box_art.base_url.medium <> '' then
        // begin
        // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.medium);
        // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_medium' +
        // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
        // FreeAndNil(Temp_Bitmap);
        // end;
        if game_t.box_art.base_url.large <> '' then
        begin
          bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.large +
            game_t.box_art.game[0].data[0].filename);
          bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_large' +
            ExtractFileExt(game_t.box_art.game[0].data[0].filename));
          FreeAndNil(bitmap_t);
        end;
        // Getting Video
        //
        //
        // Getting other stuff

        Inc(count_new);
        pbar.Value := pbar.Value + 1;
        DSPFM_Data.Query.Next;
      end;
      name_s := 'Update " ' + count_old.ToString + ' " and Insert new " ' + count_new.ToString +
        '  " games in database'
    end;
  end
  else
  begin
    count_new := 0;
    if (count_db > 0) and (update = false) then
    begin
      // delete all files in images folder
      // delete all files in videos folder
      // delete all files in manuals folder
      DSPFM_Data.table_delete(plat_name);
    end;

    while not DSPFM_Data.Query.Eof do
    begin
      game_name := DSPFM_Data.Query.FieldByName('name').AsString;
      rom_name := DSPFM_Data.Query.FieldByName('rom').AsString;
      ptxt.Text := 'Downloading Data for " ' + game_name + ' "';
      application.ProcessMessages;

      game_t := vScraper_TGDB.Scrape_With_GameName(plat_id, game_name);

      DSPFM_Data.Query_2.Close;
      DSPFM_Data.Query_2.SQL.Clear;
      DSPFM_Data.Query_2.SQL.Text :=
        'INSERT INTO arcade_tgdb (id, title, release_date, platform_id, players,' +
        'overview, last_updated, rating, coop, youtube, os, processor, ram, hdd, video, sound, developers,'
        + 'genres, publishers, alternates, box_art_original, box_art_small, box_art_thumb, box_art_cropped, '
        + 'box_art_medium, box_art_large, pic_1, pic_1_ext, rom) VALUES (:id, :title, :release_date, :platform_id, :players,'
        + ':overview, :last_updated, :rating, :coop, :youtube, :os, :processor, :ram, :hdd, :video, :sound, :developers,'
        + ':genres, :publishers, :alternates, :box_art_original, :box_art_small, :box_art_thumb, :box_art_cropped, '
        + ':box_art_medium, :box_art_large, :pic_1, :pic_1_ext, :rom)';

      DSPFM_Data.Query_2.ParamByName('id').AsString := game_t.games[0].id;
      DSPFM_Data.Query_2.ParamByName('title').AsString := game_t.games[0].title;
      DSPFM_Data.Query_2.ParamByName('release_date').AsString := game_t.games[0].release_date;
      DSPFM_Data.Query_2.ParamByName('platform_id').AsString := game_t.games[0].Platform_ID;
      DSPFM_Data.Query_2.ParamByName('players').AsString := game_t.games[0].players;
      DSPFM_Data.Query_2.ParamByName('overview').AsString := game_t.games[0].overview;
      DSPFM_Data.Query_2.ParamByName('last_updated').AsString := game_t.games[0].last_updated;
      DSPFM_Data.Query_2.ParamByName('rating').AsString := game_t.games[0].rating;
      DSPFM_Data.Query_2.ParamByName('coop').AsString := game_t.games[0].coop;
      DSPFM_Data.Query_2.ParamByName('youtube').AsString := game_t.games[0].youtube;
      DSPFM_Data.Query_2.ParamByName('os').AsString := game_t.games[0].os;
      DSPFM_Data.Query_2.ParamByName('processor').AsString := game_t.games[0].processor;
      DSPFM_Data.Query_2.ParamByName('ram').AsString := game_t.games[0].ram;
      DSPFM_Data.Query_2.ParamByName('hdd').AsString := game_t.games[0].hdd;
      DSPFM_Data.Query_2.ParamByName('video').AsString := game_t.games[0].video;
      DSPFM_Data.Query_2.ParamByName('sound').AsString := game_t.games[0].sound;
      DSPFM_Data.Query_2.ParamByName('developers').AsString := game_t.games[0].developers[0];
      DSPFM_Data.Query_2.ParamByName('genres').AsString := game_t.games[0].genres[0];
      DSPFM_Data.Query_2.ParamByName('publishers').AsString := game_t.games[0].publishers[0];
      if game_t.games[0].alternates <> nil then
        DSPFM_Data.Query_2.ParamByName('alternates').AsString := game_t.games[0].alternates[0]
      else
        DSPFM_Data.Query_2.ParamByName('alternates').AsString := '';
      DSPFM_Data.Query_2.ParamByName('box_art_original').AsString :=
        game_t.box_art.base_url.original;
      DSPFM_Data.Query_2.ParamByName('box_art_small').AsString := game_t.box_art.base_url.small;
      DSPFM_Data.Query_2.ParamByName('box_art_thumb').AsString := game_t.box_art.base_url.thumb;
      DSPFM_Data.Query_2.ParamByName('box_art_cropped').AsString := game_t.box_art.base_url.cropped;
      DSPFM_Data.Query_2.ParamByName('box_art_medium').AsString := game_t.box_art.base_url.medium;
      DSPFM_Data.Query_2.ParamByName('box_art_large').AsString := game_t.box_art.base_url.large;
      DSPFM_Data.Query_2.ParamByName('pic_1').AsString := game_t.box_art.game[0].data[0].filename;
      DSPFM_Data.Query_2.ParamByName('pic_1_ext').AsString :=
        ExtractFileExt(game_t.box_art.game[0].data[0].filename);
      DSPFM_Data.Query_2.ParamByName('rom').AsString := rom_name;
      DSPFM_Data.Query_2.ExecSQL;

      ptxt.Text := 'Downloading Images for " ' + game_name + ' "';
      application.ProcessMessages;

      if game_t.box_art.base_url.original <> '' then
      begin
        bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.original +
          game_t.box_art.game[0].data[0].filename);
        bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_original' +
          ExtractFileExt(game_t.box_art.game[0].data[0].filename));
        FreeAndNil(bitmap_t);
      end;
      // if Temp_Game.box_art.base_url.small <> '' then
      // begin
      // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.small);
      // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_small' +
      // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
      // FreeAndNil(Temp_Bitmap);
      // end;
      if game_t.box_art.base_url.thumb <> '' then
      begin
        bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.thumb +
          game_t.box_art.game[0].data[0].filename);
        bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_thumb' +
          ExtractFileExt(game_t.box_art.game[0].data[0].filename));
        FreeAndNil(bitmap_t);
      end;
      // if Temp_Game.box_art.base_url.cropped <> '' then
      // begin
      // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.cropped);
      // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_cropped' +
      // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
      // FreeAndNil(Temp_Bitmap);
      // end;
      // if Temp_Game.box_art.base_url.medium <> '' then
      // begin
      // Temp_Bitmap := uInternet_files.Get_Image(Temp_Game.box_art.base_url.medium);
      // Temp_Bitmap.SaveToFile(config.emu_path[Platform_Const].images + Temp_GameName + '_medium' +
      // ExtractFileExt(Temp_Game.box_art.game[0].data[0].filename));
      // FreeAndNil(Temp_Bitmap);
      // end;
      if game_t.box_art.base_url.large <> '' then
      begin
        bitmap_t := uInternet_files.Get_Image_new(game_t.box_art.base_url.large +
          game_t.box_art.game[0].data[0].filename);
        bitmap_t.SaveToFile(config.emu_path[plat_const].images + rom_name + '_large' +
          ExtractFileExt(game_t.box_art.game[0].data[0].filename));
        FreeAndNil(bitmap_t);
      end;
      // Getting Video
      //
      //
      // Getting other stuff
      Inc(count_new);
      pbar.Value := pbar.Value + 1;
      DSPFM_Data.Query.Next;
    end;
    name_s := 'Insert new " ' + count_new.ToString + ' games in database"';
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER.get_tgdb_genres(update: boolean; platform_name: string; pbar: TProgressBar;
  ptxt: TText);
var
  vi, count_db: integer;
  genres: T_TGDB_SCRAPER_GENRES;
  gen_id, gen_name, plat_name: string;
  count_new, count_old: integer;
begin
  plat_name := platform_name + '_tgdb_genres';
  count_db := DSPFM_Data.table_count_db(plat_name);

  genres := vScraper_TGDB.get_genres;
  pbar.Visible := True;
  ptxt.Visible := True;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (genres.count).ToInteger;
  if update = false then
  begin
    if (count_db > 0) and (update = false) then
      DSPFM_Data.table_delete(plat_name);

    for vi := 0 to (genres.count).ToInteger - 1 do
    begin
      DSPFM_Data.Query.Close;
      DSPFM_Data.Query.SQL.Clear;
      DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';

      DSPFM_Data.Query.ParamByName('id').AsString := genres.genres[vi].id;
      DSPFM_Data.Query.ParamByName('name').AsString := genres.genres[vi].name;

      DSPFM_Data.Query.ExecSQL;
      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Gerne "' + genres.genres[vi].name + '"';

      application.ProcessMessages;
    end;
    name_s := 'Database has " ' + vi.ToString + '" new Genres';
  end
  else
  begin
    count_old := 0;
    count_new := 0;
    for vi := 0 to (genres.count).ToInteger - 1 do
    begin
      gen_id := genres.genres[vi].id;

      DSPFM_Data.Query.Close;
      DSPFM_Data.Query.SQL.Clear;
      DSPFM_Data.Query.SQL.Text := 'SELECT name FROM ' + plat_name + ' WHERE id=''' + gen_id + '''';
      DSPFM_Data.Query.Open;

      gen_name := DSPFM_Data.Query.Fields[0].AsString;

      if gen_name <> '' then
      begin
        DSPFM_Data.Query.Close;
        DSPFM_Data.Query.SQL.Clear;
        DSPFM_Data.Query.SQL.Text := 'UPDATE ' + plat_name + ' SET id=:id, name=:name WHERE id=:id';

        DSPFM_Data.Query.ParamByName('id').AsString := gen_id;
        DSPFM_Data.Query.ParamByName('name').AsString := gen_name;

        DSPFM_Data.Query.ExecSQL;

        ptxt.Text := 'Updating Gerne "' + gen_name + '"';
        Inc(count_old);
      end
      else
      begin
        DSPFM_Data.Query.Close;
        DSPFM_Data.Query.SQL.Clear;
        DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';

        DSPFM_Data.Query.ParamByName('id').AsString := genres.genres[vi].id;
        DSPFM_Data.Query.ParamByName('name').AsString := genres.genres[vi].name;

        DSPFM_Data.Query.ExecSQL;
        ptxt.Text := 'Add new Genre "' + genres.genres[vi].name + '"';
        Inc(count_new);
      end;
      pbar.Value := vi + 1;
      application.ProcessMessages;
    end;
    name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString +
      ' " Gernes ';
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER.get_tgdb_publishers(update: boolean; platform_name: string; pbar: TProgressBar;
  ptxt: TText);
var
  vi, count_db: integer;
  publishers: T_TGDB_SCRAPER_PUBLISHERS;
  pub_id, pub_name, plat_name: string;
  count_new, count_old: integer;
begin
  plat_name := platform_name + '_tgdb_publishers';
  count_db := DSPFM_Data.table_count_db(plat_name);

  publishers := vScraper_TGDB.get_publishers;
  pbar.Visible := True;
  ptxt.Visible := True;
  pbar.Value := 0;
  pbar.Min := 0;
  pbar.Max := (publishers.count).ToInteger;
  if update = false then
  begin
    if (count_db > 0) and (update = false) then
      DSPFM_Data.table_delete(plat_name);

    for vi := 0 to (publishers.count).ToInteger - 1 do
    begin
      DSPFM_Data.Query.Close;
      DSPFM_Data.Query.SQL.Clear;
      DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';

      DSPFM_Data.Query.ParamByName('id').AsString := publishers.publishers[vi].id;
      DSPFM_Data.Query.ParamByName('name').AsString := publishers.publishers[vi].name;

      DSPFM_Data.Query.ExecSQL;
      pbar.Value := vi + 1;
      ptxt.Text := 'Adding Publisher "' + publishers.publishers[vi].name + '"';

      application.ProcessMessages;
    end;
  end
  else
  begin
    count_old := 0;
    count_new := 0;
    for vi := 0 to (publishers.count).ToInteger - 1 do
    begin
      pub_id := publishers.publishers[vi].id;

      DSPFM_Data.Query.Close;
      DSPFM_Data.Query.SQL.Clear;
      DSPFM_Data.Query.SQL.Text := 'SELECT name FROM ' + plat_name + ' WHERE id=''' + pub_id + '''';
      DSPFM_Data.Query.Open;

      pub_name := DSPFM_Data.Query.Fields[0].AsString;

      if pub_name <> '' then
      begin
        DSPFM_Data.Query.Close;
        DSPFM_Data.Query.SQL.Clear;
        DSPFM_Data.Query.SQL.Text := 'UPDATE ' + plat_name + ' SET id=:id, name=:name WHERE id=:id';

        DSPFM_Data.Query.ParamByName('id').AsString := pub_id;
        DSPFM_Data.Query.ParamByName('name').AsString := pub_name;

        DSPFM_Data.Query.ExecSQL;

        ptxt.Text := 'Updating Publisher "' + pub_name + '"';
        Inc(count_old);
      end
      else
      begin
        DSPFM_Data.Query.Close;
        DSPFM_Data.Query.SQL.Clear;
        DSPFM_Data.Query.SQL.Text := 'INSERT INTO ' + plat_name + ' (id, name) VALUES (:id, :name)';

        DSPFM_Data.Query.ParamByName('id').AsString := publishers.publishers[vi].id;
        DSPFM_Data.Query.ParamByName('name').AsString := publishers.publishers[vi].name;

        DSPFM_Data.Query.ExecSQL;
        ptxt.Text := 'Add new Genre "' + publishers.publishers[vi].name + '"';
        Inc(count_new);
      end;
      pbar.Value := vi + 1;
      application.ProcessMessages;
    end;
    name_s := 'Updating " ' + count_old.ToString + ' and adding new " ' + count_new.ToString +
      ' " Publishers ';
  end;
  ptxt.Text := name_s;
end;

procedure TSCRAPER.prepear_start(Platform_Type: TEMULATION_STATE; GameName: String);
begin
  frm_scraper.lbl_scraper_platform_value.Text := vScraper_TGDB.get_platform_string(Platform_Type);
  if GameName = '' then
  begin
    frm_scraper.lbl_scraper_count_value.Text := count_roms.ToString;
    frm_scraper.lbl_scraper_games_value.Text := 'All';
    frm_scraper.lbl_scraper_missing_value.Text := vScraper_TGDB.Get_Missing(Platform_Type).ToString;
    frm_scraper.prbar_scraper.Max := count_roms;
  end
  else
  begin
    frm_scraper.lbl_scraper_games_value.Text := GameName;
    frm_scraper.prbar_scraper.Max := 1;
    if main.frm_main.lbl_grid_info_header.Tag = 10 then
      frm_scraper.lbl_scraper_missing_value.Text := '0'
    else if main.frm_main.lbl_grid_info_header.Tag = 100 then
      frm_scraper.lbl_scraper_missing_value.Text := '1';
    frm_scraper.lbl_scraper_count_value.Text := '1';
  end;
  frm_scraper.prbar_scraper.Min := 0;
  frm_scraper.prbar_scraper.Value := 0;
  frm_scraper.lbl_scraper_info_game.Text := '';
  frm_scraper.lbl_scraper_info.Text := '';
  frm_scraper.lbl_scraper_warning.Visible := false;
  main.frm_main.eff_blur_main.Enabled := True;
  frm_scraper.rect_scraper.Visible := True;
end;

procedure TSCRAPER.start(Platform_Type: TEMULATION_STATE);
begin
  if frm_scraper.lbl_scraper_count_value.Text = '1' then
  begin
    Data_Emu.scrape_by_platform_one_game(vScraper_TGDB.get_platform_string(Platform_Type),
      vScraper_TGDB.get_platform_id(Platform_Type),
      vScraper_TGDB.get_platform_const_num(Platform_Type), count_roms)
  end
  else
  begin
    Data_Emu.scrape_by_platform(vScraper_TGDB.get_platform_string(Platform_Type),
      vScraper_TGDB.get_platform_id(Platform_Type),
      vScraper_TGDB.get_platform_const_num(Platform_Type), count_roms);
  end;
end;

end.
