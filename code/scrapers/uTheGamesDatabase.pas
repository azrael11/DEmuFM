unit uTheGamesDatabase;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  REST.Types,
  FireDac.Stan.Param,
  FireDac.Comp.Client,
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
    function getGameByName(vAPI_num: TAPI_NUM; vGame_Name, vPlatform_id: string): T_TGDB_SCRAPER_GAME;
    function getGameByPlatformID(vPlatform_id: string): T_TGDB_SCRAPER_GAME;
    function getGameUpdates: string;

    function getPlatformsList: T_TGDB_SCRAPER_PLATFORMS;
    function getPlatformByID(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_ID;
    function getPlatformByName(vPlatform_Name: string): T_TGDB_SCRAPER_PLATFORM_NAME;
    function getPlatformImages(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_IMAGES;

  public
    function getGameByID(vGame_ID: string): T_TGDB_SCRAPER_GAME;

    function getMissingRoms(platformType: string): Integer;

    function checkPlatformID: boolean;
    function getPlatformNameByID(Platform_Type: Integer): String;
    function getPlatformID(platformType: string): Integer;

//    function get_platform_const_num(Platform_Type: TEmulatorSelected): Integer;

    function getScrapeRom(platform_id: Integer; Game_Name: String): T_TGDB_SCRAPER_GAME;
    function getScrapeRomImages(vGame_ID: string): T_TGDB_SCRAPER_GAME_IMAGES;

    { basics }
    function getGenres: T_TGDB_SCRAPER_GENRES;
    function getDevelopers: T_TGDB_SCRAPER_DEVELOPERS;
    function getPublishers: T_TGDB_SCRAPER_PUBLISHERS;

    function getGenreByID(id: string): string;
    function getIDGenreByName(name: string): string;
    function getDeveloperById(id: string): string;
    function getIDDeveloperByName(name: string): string;
    function getPublisherById(id: string): string;
    function getIDPublisherByName(name: string): string;

  end;

var
  scraperTGDB: TTGDB_SCRAPER;

implementation

uses
  uInternet_files,
  emu_functions,
  front_main,
  uDataModule;

{ TTGDB_SCRAPER }

function TTGDB_SCRAPER.checkPlatformID: boolean;
var
  TGDBPlatforms: T_TGDB_SCRAPER_PLATFORMS;
  vi: Integer;
begin
  Result := False;
  TGDBPlatforms := getPlatformsList;
  try
    if dm.tTGDBPlatforms.RecordCount <> Length(TGDBPlatforms.platforms) then
    begin
      dm.tTGDBPlatforms.Active := true;
      for vi := 0 to High(TGDBPlatforms.platforms) do
      begin
        if not dm.tTGDBPlatforms.Locate('id', TGDBPlatforms.platforms[vi].id, []) then
        begin
          dm.tTGDBPlatforms.Append;
          dm.tTGDBPlatformsid.AsString := TGDBPlatforms.platforms[vi].id;
          dm.tTGDBPlatformsname.AsString := TGDBPlatforms.platforms[vi].name;
          dm.tTGDBPlatformsalias.AsString := TGDBPlatforms.platforms[vi].alias;
          dm.tTGDBPlatformsicon.AsString := TGDBPlatforms.platforms[vi].icon;
          dm.tTGDBPlatformsconsole.AsString := TGDBPlatforms.platforms[vi].console;
          dm.tTGDBPlatformscontroller.AsString := TGDBPlatforms.platforms[vi].controller;
          dm.tTGDBPlatformsdeveloper.AsString := TGDBPlatforms.platforms[vi].developer;
          dm.tTGDBPlatformsmanufactor.AsString := TGDBPlatforms.platforms[vi].manufacturer;
          dm.tTGDBPlatformsmedia.AsString := TGDBPlatforms.platforms[vi].media;
          dm.tTGDBPlatformscpu.AsString := TGDBPlatforms.platforms[vi].cpu;
          dm.tTGDBPlatformsmemory.AsString := TGDBPlatforms.platforms[vi].memory;
          dm.tTGDBPlatformsgraphics.AsString := TGDBPlatforms.platforms[vi].graphics;
          dm.tTGDBPlatformssound.AsString := TGDBPlatforms.platforms[vi].sound;
          dm.tTGDBPlatformsmax_controllers.AsString := TGDBPlatforms.platforms[vi].maxcontrollers;
          dm.tTGDBPlatformsdisplay.AsString := TGDBPlatforms.platforms[vi].display;
          dm.tTGDBPlatformsoverview.AsString := TGDBPlatforms.platforms[vi].overview;
          dm.tTGDBPlatformsyoutube.AsString := TGDBPlatforms.platforms[vi].youtube;

          dm.tTGDBPlatforms.Post;
        end;
      end;
      dm.tTGDBPlatforms.ApplyUpdates;
      dm.tTGDBPlatforms.Active := False;
    end;

    Result := true;
  except
    on E: Exception do
    begin
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TTGDB_SCRAPER.getDevelopers: T_TGDB_SCRAPER_DEVELOPERS;
var
  vi, vk: Integer;
  vOutValue: string;
  vJSON: TJSONValue;
begin
  vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Developers?apikey=' + Api_Key_Private, TRESTRequestMethod.rmGET);

  { Header }
  if vJSON.TryGetValue<string>('code', vOutValue) then
    Result.header.code := vOutValue;
  if vJSON.TryGetValue<string>('status', vOutValue) then
    Result.header.status := vOutValue;

  { Developers }
  if vJSON.TryGetValue<string>('data.count', vOutValue) then
    Result.count := vOutValue;

  vk := 0;
  for vi := 0 to 20000 do
  begin
    if vJSON.TryGetValue<string>('data.developers.' + vi.ToString + '.id', vOutValue) then
    begin
      SetLength(Result.developers, vk + 1);

      Result.developers[vk].id := vOutValue;
      if vJSON.TryGetValue<string>('data.developers.' + vi.ToString + '.name', vOutValue) then
        Result.developers[vk].name := vOutValue;

      inc(vk);
    end;
  end;

  { Allowance }
  if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
    Result.allowance.remain := vOutValue;
  if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
    Result.allowance.extra := vOutValue;
  if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
    Result.allowance.refresh_timer := vOutValue;
end;

function TTGDB_SCRAPER.getDeveloperById(id: string): string;
begin
  dm.tTGDBDevelopers.Locate('id', id);
  Result := dm.tTGDBDevelopersname.AsString;
end;

function TTGDB_SCRAPER.getGameByID(vGame_ID: string): T_TGDB_SCRAPER_GAME;
var
  vOutValue: string;
  vi: Integer;
  vFound: boolean;
  vJSON: TJSONValue;
begin
  vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Games/ByGameID?apikey=' + Api_Key_Private + '&id=' + vGame_ID +
    '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform', TRESTRequestMethod.rmGET);

  { Header }
  if vJSON.TryGetValue<string>('code', vOutValue) then
    Result.header.code := vOutValue;
  if vJSON.TryGetValue<string>('status', vOutValue) then
    Result.header.status := vOutValue;

  { Games }
  if vJSON.TryGetValue<string>('data.count', vOutValue) then
    Result.count := vOutValue;

  SetLength(Result.games, 1);

  if vJSON.TryGetValue<string>('data.games[0].id', vOutValue) then
    Result.games[0].id := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].game_title', vOutValue) then
    Result.games[0].title := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].release_date', vOutValue) then
    Result.games[0].release_date := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].platform', vOutValue) then
    Result.games[0].platform_id := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].players', vOutValue) then
    Result.games[0].players := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].overview', vOutValue) then
    Result.games[0].overview := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].last_updated', vOutValue) then
    Result.games[0].last_updated := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].rating', vOutValue) then
    Result.games[0].rating := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].coop', vOutValue) then
    Result.games[0].coop := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].youtube', vOutValue) then
    Result.games[0].youtube := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].os', vOutValue) then
    Result.games[0].os := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].processor', vOutValue) then
    Result.games[0].processor := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].ram', vOutValue) then
    Result.games[0].ram := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].hdd', vOutValue) then
    Result.games[0].hdd := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].video', vOutValue) then
    Result.games[0].video := vOutValue;
  if vJSON.TryGetValue<string>('data.games[0].sound', vOutValue) then
    Result.games[0].sound := vOutValue;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].developers[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(Result.games[0].developers, vi + 1);
      Result.games[0].developers[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := true;
  until vFound;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].genres[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(Result.games[0].genres, vi + 1);
      Result.games[0].genres[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := true;
  until vFound;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].publishers[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(Result.games[0].publishers, vi + 1);
      Result.games[0].publishers[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := true;
  until vFound;

  vi := 0;
  vFound := False;
  repeat
    if vJSON.TryGetValue<string>('data.games[0].alternates[' + vi.ToString + ']', vOutValue) then
    begin
      SetLength(Result.games[0].alternates, vi + 1);
      Result.games[0].alternates[vi] := vOutValue;
      inc(vi);
    end
    else
      vFound := true;
  until vFound;

  { Base URL }
  if vJSON.TryGetValue<string>('include.boxart.base_url.original', vOutValue) then
    Result.box_art.base_url.original := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.small', vOutValue) then
    Result.box_art.base_url.small := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.thumb', vOutValue) then
    Result.box_art.base_url.thumb := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.cropped_center_thumb', vOutValue) then
    Result.box_art.base_url.cropped := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.medium', vOutValue) then
    Result.box_art.base_url.medium := vOutValue;
  if vJSON.TryGetValue<string>('include.boxart.base_url.large', vOutValue) then
    Result.box_art.base_url.large := vOutValue;

  { Box Art }

  vFound := False;
  vi := 0;
  SetLength(Result.box_art.game, vi + 1);
  repeat
    if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].id', vOutValue) then
    begin

      SetLength(Result.box_art.game[0].data, vi + 1);

      Result.box_art.game[0].data[vi].game_id := vGame_ID;
      Result.box_art.game[0].data[vi].id := vOutValue;

      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].type', vOutValue) then
        Result.box_art.game[0].data[vi].vtype := vOutValue;
      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].side', vOutValue) then
        Result.box_art.game[0].data[vi].side := vOutValue;
      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].filename', vOutValue) then
        Result.box_art.game[0].data[vi].filename := vOutValue;
      if vJSON.TryGetValue<string>('include.boxart.data.' + vGame_ID + '[' + vi.ToString + '].resolution', vOutValue) then
        Result.box_art.game[0].data[vi].resolution := vOutValue;

      inc(vi);
    end
    else
      vFound := true;
  until vFound;

  { Platform }
  vi := 0;
  SetLength(Result.box_art.vplatform, vi + 1);
  if vJSON.TryGetValue<string>('include.platform.data.' + Result.games[0].platform_id + '.id', vOutValue) then
  begin

    Result.box_art.vplatform[0].data.id := vOutValue;

    if vJSON.TryGetValue<string>('include.platform.data.' + Result.games[0].platform_id + '.name', vOutValue) then
      Result.box_art.vplatform[vi].data.name := vOutValue;
    if vJSON.TryGetValue<string>('include.platform.data.' + Result.games[0].platform_id + '.alias', vOutValue) then
      Result.box_art.vplatform[vi].data.alias := vOutValue;

    inc(vi);
  end;

  { Pages }
  if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
    Result.pages.previous := vOutValue;
  if vJSON.TryGetValue<string>('pages.current', vOutValue) then
    Result.pages.current := vOutValue;
  if vJSON.TryGetValue<string>('pages.next', vOutValue) then
    Result.pages.next := vOutValue;

  { Allowance }
  if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
    Result.allowance.remain := vOutValue;
  if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
    Result.allowance.extra := vOutValue;
  if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
    Result.allowance.refresh_timer := vOutValue;

end;

function TTGDB_SCRAPER.getGameByName(vAPI_num: TAPI_NUM; vGame_Name, vPlatform_id: string): T_TGDB_SCRAPER_GAME;
var
  vi, vk: Integer;
  vFound: boolean;
  vOutValue: string;
  TList: TStringList;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    if vPlatform_id = '' then
    begin
      if vAPI_num = vAPI_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Games/ByGameName?apikey=' + Api_Key_Private + '&name=' + vGame_Name +
          '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform', TRESTRequestMethod.rmGET)
      else if vAPI_num = vAPI_1_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1.1/Games/ByGameName?apikey=' + Api_Key_Private + '&name=' + vGame_Name +
          '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform', TRESTRequestMethod.rmGET);
    end
    else
    begin
      if vAPI_num = vAPI_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Games/ByGameName?apikey=' + Api_Key_Private + '&name=' + vGame_Name +
          '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform&filter[platform]=' + vPlatform_id + '', TRESTRequestMethod.rmGET)
      else if vAPI_num = vAPI_1_1 then
        vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1.1/Games/ByGameName?apikey=' + Api_Key_Private + '&name=' + vGame_Name +
          '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform&filter[platform]=' + vPlatform_id + '', TRESTRequestMethod.rmGET);
    end;
    TList := TStringList.Create;
    TList.Add(vJSON.ToString);
    // TList.SaveToFile(config.main.prj_path + 'outJson.json');
    FreeAndNil(TList);
    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Games }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    SetLength(Result.games, Result.count.ToInteger + 1);

    for vi := 0 to Result.count.ToInteger - 1 do
    begin
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].id', vOutValue) then
        Result.games[vi].id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].game_title', vOutValue) then
        Result.games[vi].title := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].release_date', vOutValue) then
        Result.games[vi].release_date := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].platform', vOutValue) then
        Result.games[vi].platform_id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].players', vOutValue) then
        Result.games[vi].players := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].overview', vOutValue) then
        Result.games[vi].overview := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].last_updated', vOutValue) then
        Result.games[vi].last_updated := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].rating', vOutValue) then
        Result.games[vi].rating := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].coop', vOutValue) then
        Result.games[vi].coop := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].youtube', vOutValue) then
        Result.games[vi].youtube := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].os', vOutValue) then
        Result.games[vi].os := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].processor', vOutValue) then
        Result.games[vi].processor := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].ram', vOutValue) then
        Result.games[vi].ram := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].hdd', vOutValue) then
        Result.games[vi].hdd := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].video', vOutValue) then
        Result.games[vi].video := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].sound', vOutValue) then
        Result.games[vi].sound := vOutValue;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].developers[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].developers, vk + 1);
          Result.games[vi].developers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].genres[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].genres, vk + 1);
          Result.games[vi].genres[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].publishers[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].publishers, vk + 1);
          Result.games[vi].publishers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].alternates[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].alternates, vk + 1);
          Result.games[vi].alternates[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;
    end;

    { Base URL }
    if vJSON.TryGetValue<string>('include.boxart.base_url.original', vOutValue) then
      Result.box_art.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.small', vOutValue) then
      Result.box_art.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.thumb', vOutValue) then
      Result.box_art.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.cropped_center_thumb', vOutValue) then
      Result.box_art.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.medium', vOutValue) then
      Result.box_art.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.large', vOutValue) then
      Result.box_art.base_url.large := vOutValue;

    { Box Art }

    for vi := 0 to Result.count.ToInteger - 1 do
    begin
      vFound := False;
      vk := 0;
      repeat
        if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].id', vOutValue) then
        begin
          if vk = 0 then
            SetLength(Result.box_art.game, vi + 1);
          SetLength(Result.box_art.game[vi].data, vk + 1);

          Result.box_art.game[vi].data[vk].game_id := Result.games[vi].id;
          Result.box_art.game[vi].data[vk].id := vOutValue;

          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].type', vOutValue) then
            Result.box_art.game[vi].data[vk].vtype := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].side', vOutValue) then
            Result.box_art.game[vi].data[vk].side := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].filename', vOutValue) then
            Result.box_art.game[vi].data[vk].filename := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].resolution', vOutValue) then
            Result.box_art.game[vi].data[vk].resolution := vOutValue;

          inc(vk);
        end
        else
          vFound := true;
      until vFound;
    end;

    { Platform }
    vk := 0;
    for vi := 0 to 100000 do
    begin
      if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(Result.box_art.vplatform, vk + 1);
        Result.box_art.vplatform[vk].data.id := vOutValue;

        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.name', vOutValue) then
          Result.box_art.vplatform[vk].data.name := vOutValue;
        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.alias', vOutValue) then
          Result.box_art.vplatform[vk].data.alias := vOutValue;

        inc(vk);
      end;
    end;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      Result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      Result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      Result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.getGameByPlatformID(vPlatform_id: string): T_TGDB_SCRAPER_GAME;
var
  vi, vk: Integer;
  vFound: boolean;
  vOutValue: string;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin

    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Games/ByPlatformID?apikey=' + Api_Key_Private + '&id=' + vPlatform_id +
      '&fields=players,publishers,genres,overview,last_updated,rating,platform,coop,youtube,os,processor,ram,hdd,video,sound,alternates&include=boxart,platform', TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Games }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    SetLength(Result.games, Result.count.ToInteger + 1);

    for vi := 0 to Result.count.ToInteger - 1 do
    begin
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].id', vOutValue) then
        Result.games[vi].id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].game_title', vOutValue) then
        Result.games[vi].title := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].release_date', vOutValue) then
        Result.games[vi].release_date := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].platform', vOutValue) then
        Result.games[vi].platform_id := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].players', vOutValue) then
        Result.games[vi].players := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].overview', vOutValue) then
        Result.games[vi].overview := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].last_updated', vOutValue) then
        Result.games[vi].last_updated := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].rating', vOutValue) then
        Result.games[vi].rating := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].coop', vOutValue) then
        Result.games[vi].coop := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].youtube', vOutValue) then
        Result.games[vi].youtube := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].os', vOutValue) then
        Result.games[vi].os := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].processor', vOutValue) then
        Result.games[vi].processor := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].ram', vOutValue) then
        Result.games[vi].ram := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].hdd', vOutValue) then
        Result.games[vi].hdd := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].video', vOutValue) then
        Result.games[vi].video := vOutValue;
      if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].sound', vOutValue) then
        Result.games[vi].sound := vOutValue;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].developers[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].developers, vk + 1);
          Result.games[vi].developers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].genres[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].genres, vk + 1);
          Result.games[vi].genres[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].publishers[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].publishers, vk + 1);
          Result.games[vi].publishers[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;

      vk := 0;
      vFound := False;
      repeat
        if vJSON.TryGetValue<string>('data.games[' + vi.ToString + '].alternates[' + vk.ToString + ']', vOutValue) then
        begin
          SetLength(Result.games[vi].alternates, vk + 1);
          Result.games[vi].alternates[vk] := vOutValue;
          inc(vk);
        end
        else
          vFound := true;
      until vFound;
    end;

    { Base URL }
    if vJSON.TryGetValue<string>('include.boxart.base_url.original', vOutValue) then
      Result.box_art.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.small', vOutValue) then
      Result.box_art.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.thumb', vOutValue) then
      Result.box_art.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.cropped_center_thumb', vOutValue) then
      Result.box_art.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.medium', vOutValue) then
      Result.box_art.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('include.boxart.base_url.large', vOutValue) then
      Result.box_art.base_url.large := vOutValue;

    { Box Art }

    for vi := 0 to Result.count.ToInteger - 1 do
    begin
      vFound := False;
      vk := 0;
      repeat
        if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].id', vOutValue) then
        begin
          if vk = 0 then
            SetLength(Result.box_art.game, vi + 1);
          SetLength(Result.box_art.game[vi].data, vk + 1);

          Result.box_art.game[vi].data[vk].game_id := Result.games[vi].id;
          Result.box_art.game[vi].data[vk].id := vOutValue;

          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].type', vOutValue) then
            Result.box_art.game[vi].data[vk].vtype := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].side', vOutValue) then
            Result.box_art.game[vi].data[vk].side := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].filename', vOutValue) then
            Result.box_art.game[vi].data[vk].filename := vOutValue;
          if vJSON.TryGetValue<string>('include.boxart.data.' + Result.games[vi].id + '[' + vk.ToString + '].resolution', vOutValue) then
            Result.box_art.game[vi].data[vk].resolution := vOutValue;

          inc(vk);
        end
        else
          vFound := true;
      until vFound;
    end;

    { Platform }
    vk := 0;
    for vi := 0 to 100000 do
    begin
      if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(Result.box_art.vplatform, vk + 1);
        Result.box_art.vplatform[vk].data.id := vOutValue;

        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.name', vOutValue) then
          Result.box_art.vplatform[vk].data.name := vOutValue;
        if vJSON.TryGetValue<string>('include.platform.data.' + vi.ToString + '.alias', vOutValue) then
          Result.box_art.vplatform[vk].data.alias := vOutValue;

        inc(vk);
      end;
    end;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      Result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      Result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      Result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.getScrapeRomImages(vGame_ID: string): T_TGDB_SCRAPER_GAME_IMAGES;
var
  vOutValue: String;
  vi: Integer;
  vFound: boolean;
  sl: TStringList;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Games/Images?apikey=' + Api_Key_Private + '&games_id=' + vGame_ID + '&filter[type]=''fanart'',''banner'',''boxart'',''screenshot'',''clearlogo'',''titlescreen''', TRESTRequestMethod.rmGET);

    sl := TStringList.Create;
    sl.Add(vJSON.ToJSON);
    sl.SaveToFile('temp/' + vGame_ID + '.json');

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Images }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.base_url.original', vOutValue) then
      Result.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.small', vOutValue) then
      Result.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.thumb', vOutValue) then
      Result.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.cropped_center_thumb', vOutValue) then
      Result.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.medium', vOutValue) then
      Result.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.large', vOutValue) then
      Result.base_url.large := vOutValue;

    vFound := False;
    vi := 0;
    repeat
      if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].id', vOutValue) then
      begin
        SetLength(Result.images, vi + 1);

        Result.images[vi].id := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].type', vOutValue) then
          Result.images[vi].vtype := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].side', vOutValue) then
          Result.images[vi].side := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].filename', vOutValue) then
          Result.images[vi].filename := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vGame_ID + '[' + vi.ToString + '].resolution', vOutValue) then
          Result.images[vi].resolution := vOutValue;

        inc(vi);
      end
      else
        vFound := true;
    until vFound;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      Result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      Result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      Result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.getGameUpdates: string;
begin

end;

function TTGDB_SCRAPER.getGenres: T_TGDB_SCRAPER_GENRES;
var
  vi: Integer;
  vOutValue: String;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Genres?apikey=' + Api_Key_Private, TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Genres }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    SetLength(Result.genres, Result.count.ToInteger + 1);

    for vi := 0 to Result.count.ToInteger - 1 do
    begin
      if vJSON.TryGetValue<string>('data.genres.' + vi.ToString + '.id', vOutValue) then
      begin
        Result.genres[vi].id := vOutValue;

        if vJSON.TryGetValue<string>('data.genres.' + vi.ToString + '.name', vOutValue) then
          Result.genres[vi].name := vOutValue;
      end;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.getGenreByID(id: string): string;
begin
  dm.tTGDBGenres.Locate('id', id);
  Result := dm.tTGDBGenresname.AsString;
end;

function TTGDB_SCRAPER.getIDDeveloperByName(name: string): string;
begin
  dm.tTGDBDevelopers.Locate('name', name);
  Result := dm.tTGDBDevelopersid.AsString;
end;

function TTGDB_SCRAPER.getIDGenreByName(name: string): string;
begin
  dm.tTGDBGenres.Locate('name', name);
  Result := dm.tTGDBGenresid.AsString;
end;

function TTGDB_SCRAPER.getIDPublisherByName(name: string): string;
begin
  dm.tTGDBPublishers.Locate('name', name);
  Result := dm.tTGDBPublishersname.AsString;
end;

function TTGDB_SCRAPER.getMissingRoms(platformType: string): Integer;
var
  sSQL: string;
begin
  Result := 0;
  sSQL := 'SELECT COUNT(*) AS MissingCount FROM ' + platformType + ' LEFT JOIN ' + platformType + '_tgdb ON ' + platformType + '.rom = ' + platformType + '_tgdb.rom WHERE ' + platformType + '_tgdb.rom IS NULL';
  dm.query.SQL.Text := sSQL;
  try
    dm.query.Open;
    Result := dm.query.FieldByName('MissingCount').AsInteger;
  finally
    dm.query.Close;
  end;
end;

function TTGDB_SCRAPER.getPlatformByID(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_ID;
var
  vOutValue: String;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Platforms/ByPlatformID?apikey=' + Api_Key_Private + '&id=' + vPlatform_id +
      '&fields=icon,console,controller,developer,manufacturer,media,cpu,memory,graphics,sound,maxcontrollers,display,overview,youtube', TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Platform }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.id', vOutValue) then
    begin
      Result.platforms.id := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.name', vOutValue) then
        Result.platforms.name := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.alias', vOutValue) then
        Result.platforms.alias := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.icon', vOutValue) then
        Result.platforms.icon := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.console', vOutValue) then
        Result.platforms.console := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.controller', vOutValue) then
        Result.platforms.controller := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.developer', vOutValue) then
        Result.platforms.developer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.manufacturer', vOutValue) then
        Result.platforms.manufacturer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.media', vOutValue) then
        Result.platforms.media := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.cpu', vOutValue) then
        Result.platforms.cpu := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.memory', vOutValue) then
        Result.platforms.memory := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.graphics', vOutValue) then
        Result.platforms.graphics := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.sound', vOutValue) then
        Result.platforms.sound := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.maxcontrollers', vOutValue) then
        Result.platforms.maxcontrollers := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.display', vOutValue) then
        Result.platforms.display := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.overview', vOutValue) then
        Result.platforms.overview := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms.' + vPlatform_id + '.youtube', vOutValue) then
        Result.platforms.youtube := vOutValue;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.getPlatformByName(vPlatform_Name: string): T_TGDB_SCRAPER_PLATFORM_NAME;
var
  vOutValue: String;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Platforms/ByPlatformName?apikey=' + Api_Key_Private + '&name=' + vPlatform_Name +
      '&fields=icon,console,controller,developer,manufacturer,media,cpu,memory,graphics,sound,maxcontrollers,display,overview,youtube', TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Platform }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.platforms[0].id', vOutValue) then
    begin
      Result.platforms.id := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].name', vOutValue) then
        Result.platforms.name := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].alias', vOutValue) then
        Result.platforms.alias := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].icon', vOutValue) then
        Result.platforms.icon := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].console', vOutValue) then
        Result.platforms.console := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].controller', vOutValue) then
        Result.platforms.controller := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].developer', vOutValue) then
        Result.platforms.developer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].manufacturer', vOutValue) then
        Result.platforms.manufacturer := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].media', vOutValue) then
        Result.platforms.media := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].cpu', vOutValue) then
        Result.platforms.cpu := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].memory', vOutValue) then
        Result.platforms.memory := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].graphics', vOutValue) then
        Result.platforms.graphics := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].sound', vOutValue) then
        Result.platforms.sound := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].maxcontrollers', vOutValue) then
        Result.platforms.maxcontrollers := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].display', vOutValue) then
        Result.platforms.display := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].overview', vOutValue) then
        Result.platforms.overview := vOutValue;
      if vJSON.TryGetValue<string>('data.platforms[0].youtube', vOutValue) then
        Result.platforms.youtube := vOutValue;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.getPlatformImages(vPlatform_id: string): T_TGDB_SCRAPER_PLATFORM_IMAGES;
var
  vOutValue: String;
  vi: Integer;
  vFound: boolean;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Platforms/Images?apikey=' + Api_Key_Private + '&platforms_id=' + vPlatform_id + '&filter[type]=fanart,banner,boxart', TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Images }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    if vJSON.TryGetValue<string>('data.base_url.original', vOutValue) then
      Result.base_url.original := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.small', vOutValue) then
      Result.base_url.small := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.thumb', vOutValue) then
      Result.base_url.thumb := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.cropped_center_thumb', vOutValue) then
      Result.base_url.cropped := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.medium', vOutValue) then
      Result.base_url.medium := vOutValue;
    if vJSON.TryGetValue<string>('data.base_url.large', vOutValue) then
      Result.base_url.large := vOutValue;

    vFound := False;
    vi := 0;
    repeat
      if vJSON.TryGetValue<string>('data.images.' + vPlatform_id + '[' + vi.ToString + '].id', vOutValue) then
      begin
        SetLength(Result.images, vi + 1);

        Result.images[vi].id := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vPlatform_id + '[' + vi.ToString + '].type', vOutValue) then
          Result.images[vi].vtype := vOutValue;
        if vJSON.TryGetValue<string>('data.images.' + vPlatform_id + '[' + vi.ToString + '].filename', vOutValue) then
          Result.images[vi].filename := vOutValue;

        inc(vi);
      end
      else
        vFound := true;
    until vFound;

    { Pages }
    if vJSON.TryGetValue<string>('pages.previous', vOutValue) then
      Result.pages.previous := vOutValue;
    if vJSON.TryGetValue<string>('pages.current', vOutValue) then
      Result.pages.current := vOutValue;
    if vJSON.TryGetValue<string>('pages.next', vOutValue) then
      Result.pages.next := vOutValue;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

function TTGDB_SCRAPER.getPlatformsList: T_TGDB_SCRAPER_PLATFORMS;
var
  vOutValue: String;
  vi, vk: Integer;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Platforms?apikey=' + Api_Key_Private + '&fields=icon,console,controller,developer,manufacturer,media,cpu,memory,graphics,sound,maxcontrollers,display,overview,youtube', TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Platforms }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    vk := 0;
    for vi := 0 to 10000 do
    begin
      if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(Result.platforms, vk + 1);
        Result.platforms[vk].id := vOutValue;

        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.name', vOutValue) then
          Result.platforms[vk].name := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.alias', vOutValue) then
          Result.platforms[vk].alias := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.icon', vOutValue) then
          Result.platforms[vk].icon := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.console', vOutValue) then
          Result.platforms[vk].console := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.controller', vOutValue) then
          Result.platforms[vk].controller := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.developer', vOutValue) then
          Result.platforms[vk].developer := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.manufacturer', vOutValue) then
          Result.platforms[vk].manufacturer := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.media', vOutValue) then
          Result.platforms[vk].media := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.cpu', vOutValue) then
          Result.platforms[vk].cpu := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.memory', vOutValue) then
          Result.platforms[vk].memory := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.graphics', vOutValue) then
          Result.platforms[vk].graphics := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.sound', vOutValue) then
          Result.platforms[vk].sound := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.maxcontrollers', vOutValue) then
          Result.platforms[vk].maxcontrollers := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.display', vOutValue) then
          Result.platforms[vk].display := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.overview', vOutValue) then
          Result.platforms[vk].overview := vOutValue;
        if vJSON.TryGetValue<string>('data.platforms.' + vi.ToString + '.youtube', vOutValue) then
          Result.platforms[vk].youtube := vOutValue;

        inc(vk);
      end;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');
end;

//function TTGDB_SCRAPER.get_platform_const_num(Platform_Type: TEmulatorSelected): Integer;
//begin
//  case Platform_Type of
//    emus_Arcade:
//      Result := 0;
//    emus_Nes:
//      Result := 5;
//    emus_Gameboy_Color:
//      Result := 6;
//    emus_Colecovision:
//      Result := 7;
//    emus_Chip8:
//      Result := 8;
//    emus_MasterSystem:
//      Result := 9;
//    emus_SG1000:
//      Result := 10;
//    emus_Gamegear:
//      Result := 11;
//    emus_Epoch_SCV:
//      Result := 12;
//    emus_MegaDrive:
//      Result := 13;
//    emus_GandW:
//      Result := 4;
//    emus_Spectrum:
//      Result := 1;
//    emus_Amstrad:
//      Result := 2;
//    emus_Commodore64:
//      Result := 3;
//  end;
//end;

{$REGION '-------Platform get info functions--------'}

function TTGDB_SCRAPER.getPlatformID(platformType: string): Integer;
begin
  dm.tTGDBPlatforms.Active:= true;
  dm.tTGDBPlatforms.Locate('alias', platformType, []);
  result := dm.tTGDBPlatformsid.AsInteger;
  dm.tTGDBPlatforms.Active:= false;
end;

function TTGDB_SCRAPER.getPlatformNameByID(Platform_Type: Integer): String;
begin
  case Platform_Type of
    23:
      Result := 'Arcade';
    7:
      Result := 'NES';
    4:
      Result := 'Gameboy Color';
    31:
      Result := 'ColecoVision';
    -1:
      Result := 'Chip8';
    35:
      Result := 'Master System';
    4949:
      Result := 'SG-1000';
    20:
      Result := 'Sega Gamegear';
    4966:
      Result := 'Epoch Super Cassette Vision';
    36:
      Result := 'Sega MegaDrive';
    4950:
      Result := 'Game And Watch';
    4913:
      Result := 'Spectrum';
    4914:
      Result := 'Amstrad CPC';
    40:
      Result := 'Commodore 64';
  end;
end;

{$ENDREGION '-------Platform get info functions--------'}

function TTGDB_SCRAPER.getPublishers: T_TGDB_SCRAPER_PUBLISHERS;
var
  vi, vk: Integer;
  vOutValue: string;
  vJSON: TJSONValue;
begin
  if uInternet_files.Internet_Connected then
  begin
    vJSON := uInternet_files.JSONValue('The_Games_DB', 'https://api.thegamesdb.net/v1/Publishers?apikey=' + Api_Key_Private, TRESTRequestMethod.rmGET);

    { Header }
    if vJSON.TryGetValue<string>('code', vOutValue) then
      Result.header.code := vOutValue;
    if vJSON.TryGetValue<string>('status', vOutValue) then
      Result.header.status := vOutValue;

    { Developers }
    if vJSON.TryGetValue<string>('data.count', vOutValue) then
      Result.count := vOutValue;

    vk := 0;
    for vi := 0 to 20000 do
    begin
      if vJSON.TryGetValue<string>('data.publishers.' + vi.ToString + '.id', vOutValue) then
      begin
        SetLength(Result.publishers, vk + 1);

        Result.publishers[vk].id := vOutValue;
        if vJSON.TryGetValue<string>('data.publishers.' + vi.ToString + '.name', vOutValue) then
          Result.publishers[vk].name := vOutValue;

        inc(vk);
      end;
    end;

    { Allowance }
    if vJSON.TryGetValue<string>('remaining_monthly_allowance', vOutValue) then
      Result.allowance.remain := vOutValue;
    if vJSON.TryGetValue<string>('extra_allowance', vOutValue) then
      Result.allowance.extra := vOutValue;
    if vJSON.TryGetValue<string>('allowance_refresh_timer', vOutValue) then
      Result.allowance.refresh_timer := vOutValue;
  end
  else
    ShowMessage('Internet is not Connected.');

end;

function TTGDB_SCRAPER.getPublisherById(id: string): string;
begin
  dm.tTGDBPublishers.Locate('id', id);
  Result := dm.tTGDBPublishersname.AsString;
end;

function TTGDB_SCRAPER.getScrapeRom(platform_id: Integer; Game_Name: String): T_TGDB_SCRAPER_GAME;
begin
  Result := getGameByName(vAPI_1_1, Game_Name, platform_id.ToString);
end;

end.
