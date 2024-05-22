unit uTheGamesDatabase;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  REST.Types,
  FireDac.Stan.Param,
  umain_config,
  FMX.Dialogs,
  vars_consts;

const
  Api_Key_Public = '3bfc532f63837c8df59c0c55e5fff1ef8f57a606a1afdf113a9151b7b195b5e1';
  Api_Key_Private = '3467df2f024f4b929ffcaa180c085cbf1a2da9bd8b8d457a93345957528f7761';

type
  THEADER = record
    code: string;
    status: string;
  end;

type
  TALLOWANCE = record
    remain: string;
    extra: string;
    refresh_timer: string;
  end;

type
  TBOX_ART_BASE_URL = record
    original: string;
    small: string;
    thumb: string;
    cropped: string;
    medium: string;
    large: string;
  end;

type
  TBOX_ART_DATA = record
    game_id: string;
    id: string;
    vtype: string;
    side: string;
    filename: string;
    resolution: string;
  end;

type
  TBOX_ART_DATA_GAME_ID = record
    data: array of TBOX_ART_DATA;
  end;

type
  TBOX_ART_PLATFORM_DATA = record
    id: string;
    name: string;
    alias: string;
  end;

type
  TBOX_ART_PLATFORM = record
    data: TBOX_ART_PLATFORM_DATA;
  end;

type
  TBOX_ART = record
    base_url: TBOX_ART_BASE_URL;
    game: array of TBOX_ART_DATA_GAME_ID;
    vplatform: array of TBOX_ART_PLATFORM;
  end;

type
  TDATA = record
    id: string;
    title: string;
    release_date: string;
    platform_id: string;
    players: string;
    overview: string;
    last_updated: string;
    rating: string;
    coop: string;
    youtube: string;
    os: string;
    processor: string;
    ram: string;
    hdd: string;
    video: string;
    sound: string;
    developers: array of string;
    genres: array of string;
    publishers: array of string;
    alternates: array of string;
  end;

type
  TPAGES = record
    previous: string;
    current: string;
    next: string;
  end;

type
  T_TGDB_SCRAPER_GAME = record
    header: THEADER;
    count: string;
    games: array of TDATA;
    box_art: TBOX_ART;
    pages: TPAGES;
    allowance: TALLOWANCE;
  end;

type
  TPLATFORMS_PLATFORM = record
    id: string;
    name: string;
    alias: string;
    icon: string;
    console: string;
    controller: string;
    developer: string;
    manufacturer: string;
    media: string;
    cpu: string;
    memory: string;
    graphics: string;
    sound: string;
    maxcontrollers: string;
    display: string;
    overview: string;
    youtube: string;
  end;

type
  T_TGDB_SCRAPER_PLATFORMS = record
    header: THEADER;
    count: string;
    platforms: array of TPLATFORMS_PLATFORM;
    allowance: TALLOWANCE;
  end;

type
  T_TGDB_SCRAPER_PLATFORM_ID = record
    header: THEADER;
    count: string;
    platforms: TPLATFORMS_PLATFORM;
    allowance: TALLOWANCE;
  end;

type
  T_TGDB_SCRAPER_PLATFORM_NAME = record
    header: THEADER;
    count: string;
    platforms: TPLATFORMS_PLATFORM;
    allowance: TALLOWANCE;
  end;

type
  TPLATFORM_IMAGES_IMAGE = record
    id: string;
    vtype: string;
    filename: string;
  end;

type
  T_TGDB_SCRAPER_PLATFORM_IMAGES = record
    header: THEADER;
    count: string;
    base_url: TBOX_ART_BASE_URL;
    images: array of TPLATFORM_IMAGES_IMAGE;
    pages: TPAGES;
    allowance: TALLOWANCE;
  end;

type
  TGENRES_GENRE = record
    id: string;
    name: string;
  end;

type
  T_TGDB_SCRAPER_GENRES = record
    header: THEADER;
    count: string;
    genres: array of TGENRES_GENRE;
    allowance: TALLOWANCE;
  end;

type
  TDEVELOPERS_DEVELOPER = record
    id: string;
    name: string;
  end;

type
  T_TGDB_SCRAPER_DEVELOPERS = record
    header: THEADER;
    count: string;
    developers: array of TDEVELOPERS_DEVELOPER;
    allowance: TALLOWANCE;
  end;

type
  TPUBLISHERS_PUBLISHER = record
    id: string;
    name: string;
  end;

type
  T_TGDB_SCRAPER_PUBLISHERS = record
    header: THEADER;
    count: string;
    publishers: array of TPUBLISHERS_PUBLISHER;
    allowance: TALLOWANCE;
  end;

type
  TGAME_IMAGES_IMAGE = record
    id: string;
    vtype: string;
    side: string;
    filename: string;
    resolution: string;
  end;

type
  T_TGDB_SCRAPER_GAME_IMAGES = record
    header: THEADER;
    count: string;
    base_url: TBOX_ART_BASE_URL;
    images: array of TGAME_IMAGES_IMAGE;
    pages: TPAGES;
    allowance: TALLOWANCE;
  end;

type
  TAPI_NUM = (vAPI_1, vAPI_1_1);

type
  TTGDB_SCRAPER = class
  private
    vScrape_TGDB_Game: T_TGDB_SCRAPER_GAME;
    vScrape_TGDB_Game_Images: T_TGDB_SCRAPER_GAME_IMAGES;
    vScrape_TGDB_Platforms: T_TGDB_SCRAPER_PLATFORMS;
    vScrape_TGDB_Platform_ID: T_TGDB_SCRAPER_PLATFORM_ID;
    vScrape_TGDB_Platform_Name: T_TGDB_SCRAPER_PLATFORM_NAME;
    vScrape_TGDB_Platform_Images: T_TGDB_SCRAPER_PLATFORM_IMAGES;
    vScrape_TGDB_Developers: T_TGDB_SCRAPER_DEVELOPERS;
    vScrape_TGDB_Publishers: T_TGDB_SCRAPER_PUBLISHERS;
    vScrape_TGDB_Genres: T_TGDB_SCRAPER_GENRES;

    function get_games_by_game_name(vAPI_num: TAPI_NUM; vGame_Name, vPlatform_id: string)
      : T_TGDB_SCRAPER_GAME;
    function get_games_by_platform_id(vPlatform_id: string): T_TGDB_SCRAPER_GAME;
    function get_games_updates: string;

    function get_platforms_list: T_TGDB_SCRAPER_PLATFORMS;
    function get_platforms_by_platform_id(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_ID;
    function get_platforms_by_platform_name(vPlatform_Name: string): T_TGDB_SCRAPER_PLATFORM_NAME;
    function get_platforms_images(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_IMAGES;

  public
    function get_games_by_game_id(vGame_ID: string): T_TGDB_SCRAPER_GAME;
    function Check_Platforms_id_in_Database: boolean;
    function Get_Missing(Platform_Type: TEmulatorSelected): Integer;
    function get_platform_string(Platform_Type: TEmulatorSelected): String;
    function get_platform_string_by_id(Platform_Type: Integer): String;
    function get_platform_id(Platform_Type: TEmulatorSelected): Integer;
    function get_platform_const_num(Platform_Type: TEmulatorSelected): Integer;
    function Scrape_With_GameName(platform_id: Integer; Game_Name: String): T_TGDB_SCRAPER_GAME;
    function get_games_images(vGame_ID: string): T_TGDB_SCRAPER_GAME_IMAGES;

    { basics }
    function get_genres: T_TGDB_SCRAPER_GENRES;
    function get_developers: T_TGDB_SCRAPER_DEVELOPERS;
    function get_publishers: T_TGDB_SCRAPER_PUBLISHERS;

    function get_genre_by_id(id: string): string;
    function get_id_genre_by_name(name: string): string;
    function get_developer_by_id(id: string): string;
    function get_id_developer_by_name(name: string): string;
    function get_publisher_by_id(id: string): string;
    function get_id_publisher_by_name(name: string): string;

  end;

var
  vJSON: TJSONValue;
  vScraper_TGDB: TTGDB_SCRAPER;

implementation

uses
  uInternet_files,
  emu_functions,
  uDataModule;

{ TTGDB_SCRAPER }

function TTGDB_SCRAPER.Check_Platforms_id_in_Database: boolean;
var
  vScrape_TGDB_Platforms: T_TGDB_SCRAPER_PLATFORMS;
  vi: Integer;
begin
  if dm.tTGDBPlatforms.RecordCount = 0 then
  begin
    vScrape_TGDB_Platforms := get_platforms_list;

    for vi := 0 to High(vScrape_TGDB_Platforms.platforms) - 1 do
    begin
      dm.tTGDBPlatforms.Edit;
      dm.tTGDBPlatformsid.AsString := vScrape_TGDB_Platforms.platforms[vi].id;
      dm.tTGDBPlatformsname.AsString := vScrape_TGDB_Platforms.platforms[vi].name;
      dm.tTGDBPlatformsalias.AsString := vScrape_TGDB_Platforms.platforms[vi].alias;
      dm.tTGDBPlatformsicon.AsString := vScrape_TGDB_Platforms.platforms[vi].icon;
      dm.tTGDBPlatformsconsole.AsString := vScrape_TGDB_Platforms.platforms[vi].console;
      dm.tTGDBPlatformscontroller.AsString := vScrape_TGDB_Platforms.platforms[vi].controller;
      dm.tTGDBPlatformsdeveloper.AsString := vScrape_TGDB_Platforms.platforms[vi].developer;
      dm.tTGDBPlatformsmanufactor.AsString := vScrape_TGDB_Platforms.platforms[vi].manufacturer;
      dm.tTGDBPlatformsmedia.AsString := vScrape_TGDB_Platforms.platforms[vi].media;
      dm.tTGDBPlatformscpu.AsString := vScrape_TGDB_Platforms.platforms[vi].cpu;
      dm.tTGDBPlatformsmemory.AsString := vScrape_TGDB_Platforms.platforms[vi].memory;
      dm.tTGDBPlatformsgraphics.AsString := vScrape_TGDB_Platforms.platforms[vi].graphics;
      dm.tTGDBPlatformssound.AsString := vScrape_TGDB_Platforms.platforms[vi].sound;
      dm.tTGDBPlatformsmax_controllers.AsString := vScrape_TGDB_Platforms.platforms[vi].maxcontrollers;
      dm.tTGDBPlatformsdisplay.AsString := vScrape_TGDB_Platforms.platforms[vi].display;
      dm.tTGDBPlatformsoverview.AsString := vScrape_TGDB_Platforms.platforms[vi].overview;
      dm.tTGDBPlatformsyoutube.AsString := vScrape_TGDB_Platforms.platforms[vi].youtube;
      dm.tTGDBPlatforms.Post;
    end;
    dm.tTGDBPlatforms.ApplyUpdates();
  end;
  result := True;
end;

function TTGDB_SCRAPER.get_developers: T_TGDB_SCRAPER_DEVELOPERS;
var
  vi, vk: Integer;
  vOutValue: string;
begin
  vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Developers?apikey=' +
    Api_Key_Public, TRESTRequestMethod.rmGET);

  { Header }
  if vJSON.TryGetValue<string>('code', vOutValue) then
    result.header.code := vOutValue;
  if vJSON.TryGetValue<string>('status', vOutValue) then
    result.header.status := vOutValue;

  { Developers }
  if vJSON.TryGetValue<string>('data.count', vOutValue) then
    result.count := vOutValue;

  vk := 0;
  for vi := 0 to 20000 do
  begin
    if vJSON.TryGetValue<string>('data.developers.' + vi.ToString + '.id', vOutValue) then
    begin
      SetLength(result.developers, vk + 1);

      result.developers[vk].id := vOutValue;
      if vJSON.TryGetValue<string>('data.developers.' + vi.ToString + '.name', vOutValue) then
        result.developers[vk].name := vOutValue;

      inc(vk);
    end;
  end;

  { Allowance }
  if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
    result.allowance.remain := vOutValue;
  if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
    result.allowance.extra := vOutValue;
  if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
    result.allowance.refresh_timer := vOutValue;
end;

function TTGDB_SCRAPER.get_developer_by_id(id: string): string;
begin
  dm.tTGDBDevelopers.Locate('id', id);
  result := dm.tTGDBDevelopersname.AsString;
end;

function TTGDB_SCRAPER.get_games_by_game_id(vGame_ID: string): T_TGDB_SCRAPER_GAME;
var
  vOutValue: string;
  vi: Integer;
  vFound: boolean;
begin
  vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Games/ByGameID?apikey=' +
    Api_Key_Public + '&id=' + vGame_ID +
    '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform',
    TRESTRequestMethod.rmGET);

  { Header }
  if vJSON.TryGetValue<string>('code', vOutValue) then
    result.header.code := vOutValue;
  if vJSON.TryGetValue<string>('status', vOutValue) then
    result.header.status := vOutValue;

  { Games }
  if vJSON.TryGetValue<string>('data.count', vOutValue) then
    result.count := vOutValue;

  SetLength(result.games, 1);

  if vJSON.TryGetValue<string>('data.games[0].id', vOutValue) then
    result.games[0].id := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].game_title', vOutValue) then
    result.games[0].title := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].release_date', vOutValue) then
    result.games[0].release_date := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].platform', vOutValue) then
    result.games[0].platform_id := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].players', vOutValue) then
    result.games[0].players := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].overview', vOutValue) then
    result.games[0].overview := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].last_updated', vOutValue) then
    result.games[0].last_updated := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].rating', vOutValue) then
    result.games[0].rating := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].coop', vOutValue) then
    result.games[0].coop := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].youtube', vOutValue) then
    result.games[0].youtube := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].os', vOutValue) then
    result.games[0].os := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].processor', vOutValue) then
    result.games[0].processor := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].ram', vOutValue) then
    result.games[0].ram := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].hdd', vOutValue) then
    result.games[0].hdd := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].video', vOutValue) then
    result.games[0].video := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].sound', vOutValue) then
    result.games[0].sound := vOutValue;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].developers[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(result.games[0].developers, vi + 1);
      result.games[0].developers[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := True;
  until vFound;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].genres[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(result.games[0].genres, vi + 1);
      result.games[0].genres[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := True;
  until vFound;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].publishers[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(result.games[0].publishers, vi + 1);
      result.games[0].publishers[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := True;
  until vFound;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].alternates[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(result.games[0].alternates, vi + 1);
      result.games[0].alternates[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := True;
  until vFound;

  { Base URL }
  if vJSON.TryGetValue<string>('include.boxart.base_url.original', vOutValue) then
    result.box_art.base_url.original := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.small', vOutValue) then
    result.box_art.base_url.small := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.thumb', vOutValue) then
    result.box_art.base_url.thumb := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.cropped_center_thumb', vOutValue) then
    result.box_art.base_url.cropped := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.medium', vOutValue) then
    result.box_art.base_url.medium := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.large', vOutValue) then
    result.box_art.base_url.large := vOutValue;

  { Box Art }

  vFound := False;
  vi := 0;
  SetLength(result.box_art.game, vi + 1);
  repeat
    if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].id', vOutValue)
    then
    begin

      SetLength(result.box_art.game[0].data, vi + 1);

      result.box_art.game[0].data[vi].game_id := vGame_ID;
      result.box_art.game[0].data[vi].id := vOutValue;

      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].type', vOutValue)
      then
        result.box_art.game[0].data[vi].vtype := vOutValue;
      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].side', vOutValue)
      then
        result.box_art.game[0].data[vi].side := vOutValue;
      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].filename',
        vOutValue) then
        result.box_art.game[0].data[vi].filename := vOutValue;
      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].resolution',
        vOutValue) then
        result.box_art.game[0].data[vi].resolution := vOutValue;

      inc(vi);
    end
    else
      vFound := True;
  until vFound;

  { Platform }
  vi := 0;
  SetLength(result.box_art.vplatform, vi + 1);
  if vJSON.TryGetValue<string>('include.platform.data.' + result.games[0].platform_id + '.id', vOutValue) then
  begin

    result.box_art.vplatform[0].data.id := vOutValue;

    if vJSON.TryGetValue<string>('include.platform.data.' + result.games[0].platform_id + '.name', vOutValue)
    then
      result.box_art.vplatform[vi].data.name := vOutValue;
    if vJSON.TryGetValue<string>('include.platform.data.' + result.games[0].platform_id + '.alias', vOutValue)
    then
      result.box_art.vplatform[vi].data.alias := vOutValue;

    inc(vi);
  end;

  { Pages }
  if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
    result.pages.previous := vOutValue;
  if vJSON.TryGetValue<string>('pages.current', vOutValue) then
    result.pages.current := vOutValue;
  if vJSON.TryGetValue<string>('pages.next', vOutValue) then
    result.pages.next := vOutValue;

  { Allowance }
  if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
    result.allowance.remain := vOutValue;
  if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
    result.allowance.extra := vOutValue;
  if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
    result.allowance.refresh_timer := vOutValue;

end;

function TTGDB_SCRAPER.get_games_by_game_name(vAPI_num: TAPI_NUM; vGame_Name, vPlatform_id: string)
  : T_TGDB_SCRAPER_GAME;
var
  vi, vk: Integer;
  vFound: boolean;
  vOutValue: string;
  TList: TStringList;
begin
  if uInternet_files.Internet_Connected then
  begin
    if vPlatform_id = '' then
    begin
      if vAPI_num = vAPI_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB',
          'https://api.thegamesdb.net/v1/Games/ByGameName?apikey=' + Api_Key_Public + '&name=' + vGame_Name +
          '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform',
          TRESTRequestMethod.rmGET)
      else if vAPI_num = vAPI_1_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB',
          'https://api.thegamesdb.net/v1.1/Games/ByGameName?apikey=' + Api_Key_Public + '&name=' + vGame_Name
          + '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform',
          TRESTRequestMethod.rmGET);
    end
    else
    begin
      if vAPI_num = vAPI_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB',
          'https://api.thegamesdb.net/v1/Games/ByGameName?apikey=' + Api_Key_Public + '&name=' + vGame_Name +
          '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform&filter[platform]='
          + vPlatform_id + '', TRESTRequestMethod.rmGET)
      else if vAPI_num = vAPI_1_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB',
          'https://api.thegamesdb.net/v1.1/Games/ByGameName?apikey=' + Api_Key_Public + '&name=' + vGame_Name
          + '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform&filter[platform]='
          + vPlatform_id + '', TRESTRequestMethod.rmGET);
    end;
    TList := TStringList.Create;
    TList.Add(vJSON.ToString);
//    TList.SaveToFile(config.main.prj_path + 'outJson.json');
    FreeAndNil(TList);
    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Games }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    SetLength(result.games, result.count.ToInteger + 1);

    for vi := 0 to result.count.ToInteger - 1 do
    begin
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].id', vOutValue) then
        result.games[vi].id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].game_title', vOutValue) then
        result.games[vi].title := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].release_date', vOutValue) then
        result.games[vi].release_date := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].platform', vOutValue) then
        result.games[vi].platform_id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].players', vOutValue) then
        result.games[vi].players := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].overview', vOutValue) then
        result.games[vi].overview := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].last_updated', vOutValue) then
        result.games[vi].last_updated := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].rating', vOutValue) then
        result.games[vi].rating := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].coop', vOutValue) then
        result.games[vi].coop := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].youtube', vOutValue) then
        result.games[vi].youtube := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].os', vOutValue) then
        result.games[vi].os := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].processor', vOutValue) then
        result.games[vi].processor := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].ram', vOutValue) then
        result.games[vi].ram := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].hdd', vOutValue) then
        result.games[vi].hdd := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].video', vOutValue) then
        result.games[vi].video := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].sound', vOutValue) then
        result.games[vi].sound := vOutValue;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].developers[' + vk.ToString + ']',
          vOutValue) then
        begin
          SetLength(result.games[vi].developers, vk + 1);
          result.games[vi].developers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].genres[' + vk.ToString + ']', vOutValue)
        then
        begin
          SetLength(result.games[vi].genres, vk + 1);
          result.games[vi].genres[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].publishers[' + vk.ToString + ']',
          vOutValue) then
        begin
          SetLength(result.games[vi].publishers, vk + 1);
          result.games[vi].publishers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].alternates[' + vk.ToString + ']',
          vOutValue) then
        begin
          SetLength(result.games[vi].alternates, vk + 1);
          result.games[vi].alternates[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;
    end;

    { Base URL }
    if vJSON.TryGetValue<string>('include.boxart.base_url.original', vOutValue) then
      result.box_art.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.small', vOutValue) then
      result.box_art.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.thumb', vOutValue) then
      result.box_art.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.cropped_center_thumb', vOutValue) then
      result.box_art.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.medium', vOutValue) then
      result.box_art.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.large', vOutValue) then
      result.box_art.base_url.large := vOutValue;

    { Box Art }

    for vi := 0 to result.count.ToInteger - 1 do
    begin
      vFound := False;
      vk := 0;
      repeat
        if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
          '].id', vOutValue) then
        begin
          if vk = 0 then
            SetLength(result.box_art.game, vi + 1);
          SetLength(result.box_art.game[vi].data, vk + 1);

          result.box_art.game[vi].data[vk].game_id := result.games[vi].id;
          result.box_art.game[vi].data[vk].id := vOutValue;

          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].type', vOutValue) then
            result.box_art.game[vi].data[vk].vtype := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].side', vOutValue) then
            result.box_art.game[vi].data[vk].side := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].filename', vOutValue) then
            result.box_art.game[vi].data[vk].filename := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].resolution', vOutValue) then
            result.box_art.game[vi].data[vk].resolution := vOutValue;

          inc(vk);
        end
        else
          vFound := True;
      until vFound;
    end;

    { Platform }
    vk := 0;
    for vi := 0 to 100000 do
    begin
      if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(result.box_art.vplatform, vk + 1);
        result.box_art.vplatform[vk].data.id := vOutValue;

        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.name', vOutValue) then
          result.box_art.vplatform[vk].data.name := vOutValue;
        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.alias', vOutValue) then
          result.box_art.vplatform[vk].data.alias := vOutValue;

        inc(vk);
      end;
    end;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_games_by_platform_id(vPlatform_id: string): T_TGDB_SCRAPER_GAME;
var
  vi, vk: Integer;
  vFound: boolean;
  vOutValue: string;
begin
  if uInternet_files.Internet_Connected then
  begin

    vJSON := uInternet_files.JSONValue('The_Games_DB',
      'https://api.thegamesdb.net/v1/Games/ByPlatformID?apikey=' + Api_Key_Public + '&id=' + vPlatform_id +
      '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform',
      TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Games }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    SetLength(result.games, result.count.ToInteger + 1);

    for vi := 0 to result.count.ToInteger - 1 do
    begin
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].id', vOutValue) then
        result.games[vi].id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].game_title', vOutValue) then
        result.games[vi].title := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].release_date', vOutValue) then
        result.games[vi].release_date := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].platform', vOutValue) then
        result.games[vi].platform_id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].players', vOutValue) then
        result.games[vi].players := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].overview', vOutValue) then
        result.games[vi].overview := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].last_updated', vOutValue) then
        result.games[vi].last_updated := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].rating', vOutValue) then
        result.games[vi].rating := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].coop', vOutValue) then
        result.games[vi].coop := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].youtube', vOutValue) then
        result.games[vi].youtube := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].os', vOutValue) then
        result.games[vi].os := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].processor', vOutValue) then
        result.games[vi].processor := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].ram', vOutValue) then
        result.games[vi].ram := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].hdd', vOutValue) then
        result.games[vi].hdd := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].video', vOutValue) then
        result.games[vi].video := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].sound', vOutValue) then
        result.games[vi].sound := vOutValue;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].developers[' + vk.ToString + ']',
          vOutValue) then
        begin
          SetLength(result.games[vi].developers, vk + 1);
          result.games[vi].developers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].genres[' + vk.ToString + ']', vOutValue)
        then
        begin
          SetLength(result.games[vi].genres, vk + 1);
          result.games[vi].genres[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].publishers[' + vk.ToString + ']',
          vOutValue) then
        begin
          SetLength(result.games[vi].publishers, vk + 1);
          result.games[vi].publishers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].alternates[' + vk.ToString + ']',
          vOutValue) then
        begin
          SetLength(result.games[vi].alternates, vk + 1);
          result.games[vi].alternates[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := True;
      until vFound;
    end;

    { Base URL }
    if vJSON.TryGetValue<string>('include.boxart.base_url.original', vOutValue) then
      result.box_art.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.small', vOutValue) then
      result.box_art.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.thumb', vOutValue) then
      result.box_art.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.cropped_center_thumb', vOutValue) then
      result.box_art.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.medium', vOutValue) then
      result.box_art.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.large', vOutValue) then
      result.box_art.base_url.large := vOutValue;

    { Box Art }

    for vi := 0 to result.count.ToInteger - 1 do
    begin
      vFound := False;
      vk := 0;
      repeat
        if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
          '].id', vOutValue) then
        begin
          if vk = 0 then
            SetLength(result.box_art.game, vi + 1);
          SetLength(result.box_art.game[vi].data, vk + 1);

          result.box_art.game[vi].data[vk].game_id := result.games[vi].id;
          result.box_art.game[vi].data[vk].id := vOutValue;

          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].type', vOutValue) then
            result.box_art.game[vi].data[vk].vtype := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].side', vOutValue) then
            result.box_art.game[vi].data[vk].side := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].filename', vOutValue) then
            result.box_art.game[vi].data[vk].filename := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + result.games[vi].id + '[' + vk.ToString +
            '].resolution', vOutValue) then
            result.box_art.game[vi].data[vk].resolution := vOutValue;

          inc(vk);
        end
        else
          vFound := True;
      until vFound;
    end;

    { Platform }
    vk := 0;
    for vi := 0 to 100000 do
    begin
      if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(result.box_art.vplatform, vk + 1);
        result.box_art.vplatform[vk].data.id := vOutValue;

        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.name', vOutValue) then
          result.box_art.vplatform[vk].data.name := vOutValue;
        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.alias', vOutValue) then
          result.box_art.vplatform[vk].data.alias := vOutValue;

        inc(vk);
      end;
    end;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_games_images(vGame_ID: string): T_TGDB_SCRAPER_GAME_IMAGES;
var
  vOutValue: String;
  vi: Integer;
  vFound: boolean;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Games/Images?apikey=' +
      Api_Key_Public + '&games_id=' + vGame_ID +
      '&filter[type]=''fanart'',''banner'',''boxart'',''screenshot'',''clearlogo'',''titlescreen''',
      TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Images }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.base_url.original', vOutValue) then
      result.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.small', vOutValue) then
      result.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.thumb', vOutValue) then
      result.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.cropped_center_thumb', vOutValue) then
      result.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.medium', vOutValue) then
      result.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.large', vOutValue) then
      result.base_url.large := vOutValue;

    vFound := False;
    vi := 0;
    repeat
      if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].id', vOutValue) then
      begin
        SetLength(result.images, vi + 1);

        result.images[vi].id := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].type', vOutValue) then
          result.images[vi].vtype := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].side', vOutValue) then
          result.images[vi].side := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].filename', vOutValue)
        then
          result.images[vi].filename := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].resolution', vOutValue)
        then
          result.images[vi].resolution := vOutValue;

        inc(vi);
      end
      else
        vFound := True;
    until vFound;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_games_updates: string;
begin

end;

function TTGDB_SCRAPER.get_genres: T_TGDB_SCRAPER_GENRES;
var
  vi: Integer;
  vOutValue: String;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Genres?apikey=' +
      Api_Key_Public, TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Genres }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    SetLength(result.genres, result.count.ToInteger + 1);

    for vi := 0 to result.count.ToInteger - 1 do
    begin
      if vJSON.TryGetValue<string>('data.genres.' + vi.ToString + '.id', vOutValue) then
      begin
        result.genres[vi].id := vOutValue;

        if vJSON.TryGetValue<string>('data.genres.' + vi.ToString + '.name', vOutValue) then
          result.genres[vi].name := vOutValue;
      end;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_genre_by_id(id: string): string;
begin
  dm.tTGDBGenres.Locate('id', id);
  result := dm.tTGDBGenresname.AsString;
end;

function TTGDB_SCRAPER.get_id_developer_by_name(name: string): string;
begin
  dm.tTGDBDevelopers.Locate('name', name);
  result := dm.tTGDBDevelopersid.AsString;
end;

function TTGDB_SCRAPER.get_id_genre_by_name(name: string): string;
begin
  dm.tTGDBGenres.Locate('name', name);
  result := dm.tTGDBGenresid.AsString;
end;

function TTGDB_SCRAPER.get_id_publisher_by_name(name: string): string;
begin
  dm.tTGDBPublishers.Locate('name', name);
  result := dm.tTGDBPublishersname.AsString;
end;

function TTGDB_SCRAPER.Get_Missing(Platform_Type: TEmulatorSelected): Integer;
begin
  result := 0;
  dm.tArcade.First;
  while not dm.tArcade.Eof do
  begin
    dm.query.SQL.Text := 'Select Count(rom) from arcade where rom=:rom';
    dm.query.ParamByName('rom').AsString := dm.tArcadename.AsString;
    try
      dm.query.Open;
      if dm.query.Fields[0].AsInteger = 0 then
        inc(result);
    finally
      dm.query.Close;
    end;
    dm.tArcade.next;
  end;
end;

function TTGDB_SCRAPER.get_platforms_by_platform_id(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_ID;
var
  vOutValue: String;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB',
      'https://api.thegamesdb.net/v1/Platforms/ByPlatformID?apikey=' + Api_Key_Public + '&id=' + vPlatform_id
      + '&fields=icon,console,controller,developer,manufacturer,media,cpu,memory,graphics,sound,maxcontrollers,display,overview,youtube',
      TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Platform }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.id', vOutValue) then
    begin
      result.platforms.id := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.name', vOutValue) then
        result.platforms.name := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.alias', vOutValue) then
        result.platforms.alias := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.icon', vOutValue) then
        result.platforms.icon := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.console', vOutValue) then
        result.platforms.console := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.controller', vOutValue) then
        result.platforms.controller := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.developer', vOutValue) then
        result.platforms.developer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.manufacturer', vOutValue) then
        result.platforms.manufacturer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.media', vOutValue) then
        result.platforms.media := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.cpu', vOutValue) then
        result.platforms.cpu := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.memory', vOutValue) then
        result.platforms.memory := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.graphics', vOutValue) then
        result.platforms.graphics := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.sound', vOutValue) then
        result.platforms.sound := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.maxcontrollers', vOutValue) then
        result.platforms.maxcontrollers := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.display', vOutValue) then
        result.platforms.display := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.overview', vOutValue) then
        result.platforms.overview := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.youtube', vOutValue) then
        result.platforms.youtube := vOutValue;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_platforms_by_platform_name(vPlatform_Name: string): T_TGDB_SCRAPER_PLATFORM_NAME;
var
  vOutValue: String;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB',
      'https://api.thegamesdb.net/v1/Platforms/ByPlatformName?apikey=' + Api_Key_Public + '&name=' +
      vPlatform_Name +
      '&fields=icon,console,controller,developer,manufacturer,media,cpu,memory,graphics,sound,maxcontrollers,display,overview,youtube',
      TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Platform }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.platforms[0].id', vOutValue) then
    begin
      result.platforms.id := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].name', vOutValue) then
        result.platforms.name := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].alias', vOutValue) then
        result.platforms.alias := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].icon', vOutValue) then
        result.platforms.icon := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].console', vOutValue) then
        result.platforms.console := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].controller', vOutValue) then
        result.platforms.controller := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].developer', vOutValue) then
        result.platforms.developer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].manufacturer', vOutValue) then
        result.platforms.manufacturer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].media', vOutValue) then
        result.platforms.media := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].cpu', vOutValue) then
        result.platforms.cpu := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].memory', vOutValue) then
        result.platforms.memory := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].graphics', vOutValue) then
        result.platforms.graphics := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].sound', vOutValue) then
        result.platforms.sound := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].maxcontrollers', vOutValue) then
        result.platforms.maxcontrollers := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].display', vOutValue) then
        result.platforms.display := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].overview', vOutValue) then
        result.platforms.overview := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].youtube', vOutValue) then
        result.platforms.youtube := vOutValue;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_platforms_images(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_IMAGES;
var
  vOutValue: String;
  vi: Integer;
  vFound: boolean;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB',
      'https://api.thegamesdb.net/v1/Platforms/Images?apikey=' + Api_Key_Public + '&platforms_id=' +
      vPlatform_id + '&filter[type]=fanart,banner,boxart', TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Images }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.base_url.original', vOutValue) then
      result.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.small', vOutValue) then
      result.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.thumb', vOutValue) then
      result.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.cropped_center_thumb', vOutValue) then
      result.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.medium', vOutValue) then
      result.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.large', vOutValue) then
      result.base_url.large := vOutValue;

    vFound := False;
    vi := 0;
    repeat
      if vJSON.TryGetValue<string>('data.images.' + vPlatform_id + '[' + vi.ToString + '].id', vOutValue) then
      begin
        SetLength(result.images, vi + 1);

        result.images[vi].id := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vPlatform_id + '[' + vi.ToString + '].type', vOutValue)
        then
          result.images[vi].vtype := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vPlatform_id + '[' + vi.ToString + '].filename',
          vOutValue) then
          result.images[vi].filename := vOutValue;

        inc(vi);
      end
      else
        vFound := True;
    until vFound;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_platforms_list: T_TGDB_SCRAPER_PLATFORMS;
var
  vOutValue: String;
  vi, vk: Integer;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Platforms?apikey=' +
      Api_Key_Public +
      '&fields=icon,console,controller,developer,manufacturer,media,cpu,memory,graphics,sound,maxcontrollers,display,overview,youtube',
      TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Platforms }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    vk := 0;
    for vi := 0 to 10000 do
    begin
      if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(result.platforms, vk + 1);
        result.platforms[vk].id := vOutValue;

        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.name', vOutValue) then
          result.platforms[vk].name := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.alias', vOutValue) then
          result.platforms[vk].alias := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.icon', vOutValue) then
          result.platforms[vk].icon := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.console', vOutValue) then
          result.platforms[vk].console := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.controller', vOutValue) then
          result.platforms[vk].controller := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.developer', vOutValue) then
          result.platforms[vk].developer := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.manufacturer', vOutValue) then
          result.platforms[vk].manufacturer := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.media', vOutValue) then
          result.platforms[vk].media := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.cpu', vOutValue) then
          result.platforms[vk].cpu := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.memory', vOutValue) then
          result.platforms[vk].memory := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.graphics', vOutValue) then
          result.platforms[vk].graphics := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.sound', vOutValue) then
          result.platforms[vk].sound := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.maxcontrollers', vOutValue) then
          result.platforms[vk].maxcontrollers := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.display', vOutValue) then
          result.platforms[vk].display := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.overview', vOutValue) then
          result.platforms[vk].overview := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.youtube', vOutValue) then
          result.platforms[vk].youtube := vOutValue;

        inc(vk);
      end;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.get_platform_const_num(Platform_Type: TEmulatorSelected): Integer;
begin
  case Platform_Type of
    emus_Arcade:
      result := 0;
    emus_Nes:
      result := 5;
    emus_Gameboy_Color:
      result := 6;
    emus_Colecovision:
      result := 7;
    emus_Chip8:
      result := 8;
    emus_MasterSystem:
      result := 9;
    emus_SG1000:
      result := 10;
    emus_Gamegear:
      result := 11;
    emus_Epoch_SCV:
      result := 12;
    emus_MegaDrive:
      result := 13;
    emus_GandW:
      result := 4;
    emus_Spectrum:
      result := 1;
    emus_Amstrad:
      result := 2;
    emus_Commodore64:
      result := 3;
  end;
end;

function TTGDB_SCRAPER.get_platform_id(Platform_Type: TEmulatorSelected): Integer;
begin
  case Platform_Type of
    emus_Arcade:
      result := 23;
    emus_Nes:
      result := 7;
    emus_Gameboy_Color:
      result := 4;
    emus_Colecovision:
      result := 31;
    emus_Chip8:
      result := -1;
    emus_MasterSystem:
      result := 35;
    emus_SG1000:
      result := 4949;
    emus_Gamegear:
      result := 20;
    emus_Epoch_SCV:
      result := 4966;
    emus_MegaDrive:
      result := 36;
    emus_GandW:
      result := 4950;
    emus_Spectrum:
      result := 4913;
    emus_Amstrad:
      result := 4914;
    emus_Commodore64:
      result := 40;
  end;
end;

function TTGDB_SCRAPER.get_platform_string_by_id(Platform_Type: Integer): String;
begin
  case Platform_Type of
    23:
      result := 'Arcade';
    7:
      result := 'NES';
    4:
      result := 'Gameboy Color';
    31:
      result := 'ColecoVision';
    -1:
      result := 'Chip8';
    35:
      result := 'Master System';
    4949:
      result := 'SG-1000';
    20:
      result := 'Sega Gamegear';
    4966:
      result := 'Epoch Super Cassette Vision';
    36:
      result := 'Sega MegaDrive';
    4950:
      result := 'Game And Watch';
    4913:
      result := 'Spectrum';
    4914:
      result := 'Amstrad CPC';
    40:
      result := 'Commodore 64';
  end;
end;

function TTGDB_SCRAPER.get_platform_string(Platform_Type: TEmulatorSelected): String;
begin
  case Platform_Type of
    emus_Arcade:
      result := 'Arcade';
    emus_Nes:
      result := 'Nintendo Entertainment System (NES)';
    emus_Gameboy_Color:
      result := 'Nintendo Game Boy';
    emus_Colecovision:
      result := 'Colecovision';
    emus_Chip8:
      result := 'CHIP 8';
    emus_MasterSystem:
      result := 'Sega Master System';
    emus_SG1000:
      result := 'SEGA SG-1000';
    emus_Gamegear:
      result := 'Sega Game Gear';
    emus_Epoch_SCV:
      result := 'Epoch Super Cassette Vision';
    emus_MegaDrive:
      result := 'Sega Mega Drive';
    emus_GandW:
      result := 'Game & Watch';
    emus_Spectrum:
      result := 'Sinclair ZX Spectrum';
    emus_Amstrad:
      result := 'Amstrad CPC';
    emus_Commodore64:
      result := 'Commodore 64';
  end;
end;

function TTGDB_SCRAPER.get_publishers: T_TGDB_SCRAPER_PUBLISHERS;
var
  vi, vk: Integer;
  vOutValue: string;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Publishers?apikey=' +
      Api_Key_Public, TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      result.header.status := vOutValue;

    { Developers }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      result.count := vOutValue;

    vk := 0;
    for vi := 0 to 20000 do
    begin
      if vJSON.TryGetValue<string>('data.publishers.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(result.publishers, vk + 1);

        result.publishers[vk].id := vOutValue;
        if vJSON.TryGetValue<string>('data.publishers.' + vi.ToString + '.name', vOutValue) then
          result.publishers[vk].name := vOutValue;

        inc(vk);
      end;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');

end;

function TTGDB_SCRAPER.get_publisher_by_id(id: string): string;
begin
  dm.tTGDBPublishers.Locate('id', id);
  result := dm.tTGDBPublishersname.AsString;
end;

function TTGDB_SCRAPER.Scrape_With_GameName(platform_id: Integer; Game_Name: String): T_TGDB_SCRAPER_GAME;
begin
  result := get_games_by_game_name(vAPI_1_1, Game_Name, platform_id.ToString);
end;

end.
