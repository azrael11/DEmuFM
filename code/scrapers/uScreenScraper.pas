unit uScreenScraper;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  REST.Types;

const
  dev_id = 'azrael11';
  dev_id_false = 'xxx';
  dev_password = 'upSn78rMSP0';
  dev_password_false = 'yyy';
  dev_debug_password = 'fzkuTA52bCu';
  dev_nick_name = 'azrael11';
  dev_nick_pass = '11azrael';
  dev_email = 'spoooky11@hotmail.gr';
  dev_project_name = 'ExtraFE';
  dev_project_name_false = 'zzz';

type
  THEADER = record
    api_version: string;
    date_time: string;
    command: string;
    success: string;
    error: string;
  end;

type
  TSERVERS = record
    cpu_1: string;
    cpu_2: string;
    threads_min: string;
    num_scrapers: string;
    api_access: string;
    close_for_no_member: string;
    close_for_leecher: string;
  end;

type
  TSYSTEMS_PRONCOUCE = record
    europe: string;
    us: string;
    japan: string;
    recalbox: string;
    retropie: string;
    launchbox: string;
    hyperspin: string;
    all: string;
  end;

type
  TSYSTEMS_MEDIA = record
    vtype: string;
    parent: string;
    url: string;
    region: string;
    crc: string;
    md5: string;
    sha1: string;
    format: string;
  end;

type
  TSCREENSCRAPER_USER = record
    id: string;
    password: string;
    num_id: string;
    lever: string;
    contribution: string;
    upload_systems: string;
    upload_infos: string;
    upload_roms: string;
    upload_media: string;
    proposition_ok: string;
    proposition_ko: string;
    quota_fu: string;
    max_threads: string;
    max_download_speed: string;
    requests_today: string;
    requests_ko_today: string;
    max_requests_per_min: string;
    max_requests_per_day: string;
    maxrequests_ko_per_day: string;
    visits: string;
    date_last_visit: string;
    fav_region: string;
  end;

type
  TSYSTEMS = record
    id: string;
    pronounce: TSYSTEMS_PRONCOUCE;
    extensions: string;
    company: string;
    vtype: string;
    date_debut: string;
    date_fin: string;
    rom_type: string;
    support_type: string;
    media: array of TSYSTEMS_MEDIA;
    media_count: integer;
  end;

type
  TSYSTEMS_LIST = record
    header: THEADER;
    servers: TSERVERS;
    user: TSCREENSCRAPER_USER;
    systems: array of TSYSTEMS;
    count: integer;
  end;

type
  TMEDIA = record
    id: string;
    call_name: string;
    name: string;
    category: string;
    platform_types: string;
    platforms: string;
    vtype: string;
    file_format: string;
    file_format_2: string;
    auto_gen: string;
    multi_regions: string;
    multi_supports: string;
    multi_versions: string;
    extra_info: string;
  end;

type
  TMEDIA_LIST = record
    header: THEADER;
    media: array of TMEDIA;
  end;

type
  TSCREENSCAPER = class
  private const
    ss_id = 'azrael11';
    ss_pass = '11azrael';
    function get_header(vJSON_File: TJSONValue): THEADER;
    function get_servers(vJSON_File: TJSONValue): TSERVERS;
    function get_user(vJSON_File: TJSONValue): TSCREENSCRAPER_USER;
  public
    constructor create(AOwner: TComponent);
    destructor destroy; override;
    //
    { Get the list of Systems }
    function get_systems: TSYSTEMS_LIST;
    { Get the list of Media }
    function get_media_list: TMEDIA_LIST;
  end;

var
  vJSON: TJSONValue;
  vSS: TSCREENSCAPER;

implementation

uses
  uInternet_files;

{ TSCREENSCAPER }

constructor TSCREENSCAPER.create(AOwner: TComponent);
begin
  vSS := TSCREENSCAPER.create(nil);
end;

destructor TSCREENSCAPER.destroy;
begin
  FreeAndNil(vSS);
  inherited;
end;

function TSCREENSCAPER.get_header(vJSON_File: TJSONValue): THEADER;
begin
  Result.api_version := vJSON_File.GetValue<String>('header.APIversion');
  Result.date_time := vJSON_File.GetValue<String>('header.dateTime');
  Result.command := vJSON_File.GetValue<String>('header.commandRequested');
  Result.success := vJSON_File.GetValue<String>('header.success');
  Result.error := vJSON_File.GetValue<String>('header.error');
end;

function TSCREENSCAPER.get_media_list: TMEDIA_LIST;
var
  vi: integer;
  vOutValue: string;
begin

  vJSON := uInternet_files.JSONValue('ScreenScraper',
    'https://www.screenscraper.fr/api2/mediasSystemeListe.php?devid=' + dev_id_false +
    '&devpassword=' + dev_password_false + '&softname=' + dev_project_name_false +
    '&output=json&ssid=' + ss_id + '&sspassword=' + ss_pass, TRESTRequestMethod.rmGET);

  if vJSON <> nil then
  begin
    Result.header := get_header(vJSON);

    for vi := 0 to 1000 do
    begin
      if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].id', vOutValue) then
      begin
        SetLength(Result.media, vi + 1);

        Result.media[vi].id := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].nomcourt', vOutValue) then
          Result.media[vi].call_name := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].nom', vOutValue) then
          Result.media[vi].name := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].categorie', vOutValue) then
          Result.media[vi].category := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].plateformtypes', vOutValue) then
          Result.media[vi].platform_types := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].plateforms  ', vOutValue) then
          Result.media[vi].platforms := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].type', vOutValue) then
          Result.media[vi].vtype := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].fileformat', vOutValue) then
          Result.media[vi].file_format := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].fileformat2', vOutValue) then
          Result.media[vi].file_format_2 := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].autogen', vOutValue) then
          Result.media[vi].auto_gen := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].multiregions', vOutValue) then
          Result.media[vi].multi_regions := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].multisupports', vOutValue) then
          Result.media[vi].multi_supports := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].multiversions', vOutValue) then
          Result.media[vi].multi_versions := vOutValue;
        if vJSON.TryGetValue<string>('medias[' + vi.ToString + '].extrainfostxt', vOutValue) then
          Result.media[vi].extra_info := vOutValue;
      end;
    end;
  end;
end;

function TSCREENSCAPER.get_servers(vJSON_File: TJSONValue): TSERVERS;
begin
  Result.cpu_1 := vJSON_File.GetValue<String>('response.serveurs.cpu1');
  Result.cpu_2 := vJSON_File.GetValue<String>('response.serveurs.cpu2');
  Result.threads_min := vJSON_File.GetValue<String>('response.serveurs.threadsmin');
  Result.num_scrapers := vJSON_File.GetValue<String>('response.serveurs.nbscrapeurs');
  Result.api_access := vJSON_File.GetValue<String>('response.serveurs.apiacces');
  Result.close_for_no_member := vJSON_File.GetValue<String>('response.serveurs.closefornomember');
  Result.close_for_leecher := vJSON_File.GetValue<String>('response.serveurs.closeforleecher');
end;

function TSCREENSCAPER.get_systems: TSYSTEMS_LIST;
var
  vi, vk, vl: integer;
  vOutValue: String;
  vNoMore, vNoMedia: Boolean;
begin
  vJSON := uInternet_files.JSONValue('ScreenScraper',
    'https://www.screenscraper.fr/api2/systemesListe.php?devid=' + dev_id_false + '&devpassword=' +
    dev_password_false + '&softname=' + dev_project_name_false + '&output=json&ssid=' + ss_id +
    '&sspassword=' + ss_pass, TRESTRequestMethod.rmGET);

  if vJSON <> nil then
  begin

    Result.header := get_header(vJSON);
    Result.servers := get_servers(vJSON);
    Result.user := get_user(vJSON);

    vi := 0;
    vNoMore := False;
    repeat
      if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].id', vOutValue) then
      begin
        SetLength(Result.systems, vi + 1);
        Result.systems[vi].id := vOutValue;

        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.nom_eu', vOutValue)
        then
          Result.systems[vi].pronounce.europe := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.nom_us', vOutValue)
        then
          Result.systems[vi].pronounce.us := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.nom_japan',
          vOutValue) then
          Result.systems[vi].pronounce.japan := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.nom_recalbox',
          vOutValue) then
          Result.systems[vi].pronounce.recalbox := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.nom_retropie',
          vOutValue) then
          Result.systems[vi].pronounce.retropie := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.nom_launchbox',
          vOutValue) then
          Result.systems[vi].pronounce.launchbox := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.nom_hyperspin',
          vOutValue) then
          Result.systems[vi].pronounce.hyperspin := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].noms.noms_commun',
          vOutValue) then
          Result.systems[vi].pronounce.all := vOutValue;

        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].extensions', vOutValue)
        then
          Result.systems[vi].extensions := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].compagnie', vOutValue)
        then
          Result.systems[vi].company := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].type', vOutValue) then
          Result.systems[vi].vtype := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].datedebut', vOutValue)
        then
          Result.systems[vi].date_debut := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].datefin', vOutValue)
        then
          Result.systems[vi].date_fin := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].romtype', vOutValue)
        then
          Result.systems[vi].rom_type := vOutValue;
        if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].supporttype', vOutValue)
        then
          Result.systems[vi].support_type := vOutValue;

        vk := 0;
        vNoMedia := False;
        repeat
          if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
            vk.ToString + '].type', vOutValue) then
          begin
            SetLength(Result.systems[vi].media, vk + 1);

            Result.systems[vi].media[vk].vtype := vOutValue;
            if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
              vk.ToString + '].parent', vOutValue) then
              Result.systems[vi].media[vk].parent := vOutValue;
            if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
              vk.ToString + '].url', vOutValue) then
              Result.systems[vi].media[vk].url := vOutValue;
            if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
              vk.ToString + '].region', vOutValue) then
              Result.systems[vi].media[vk].region := vOutValue;
            if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
              vk.ToString + '].crc', vOutValue) then
              Result.systems[vi].media[vk].crc := vOutValue;
            if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
              vk.ToString + '].md5', vOutValue) then
              Result.systems[vi].media[vk].md5 := vOutValue;
            if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
              vk.ToString + '].sha1', vOutValue) then
              Result.systems[vi].media[vk].sha1 := vOutValue;
            if vJSON.TryGetValue<string>('response.systemes[' + vi.ToString + '].medias[' +
              vk.ToString + '].format', vOutValue) then
              Result.systems[vi].media[vk].format := vOutValue;

            inc(vk);
          end
          else
          begin
            vNoMedia := True;
            Result.systems[vi].media_count := vk;
          end;

        until vNoMedia = True;
        inc(vi);
      end
      else
      begin
        vNoMore := True;
        Result.count := vi;
      end;
    until vNoMore = True;
  end;
end;

function TSCREENSCAPER.get_user(vJSON_File: TJSONValue): TSCREENSCRAPER_USER;
begin
  Result.id := vJSON.GetValue<String>('response.ssuser.id');
  Result.num_id := vJSON.GetValue<String>('response.ssuser.numid');
  Result.lever := vJSON.GetValue<String>('response.ssuser.niveau');
  Result.contribution := vJSON.GetValue<String>('response.ssuser.contribution');
  Result.upload_systems := vJSON.GetValue<String>('response.ssuser.uploadsysteme');
  Result.upload_infos := vJSON.GetValue<String>('response.ssuser.uploadinfos');
  Result.upload_roms := vJSON.GetValue<String>('response.ssuser.romasso  ');
  Result.upload_media := vJSON.GetValue<String>('response.ssuser.uploadmedia');
  Result.proposition_ok := vJSON.GetValue<String>('response.ssuser.propositionok');
  Result.proposition_ko := vJSON.GetValue<String>('response.ssuser.propositionko');
  Result.quota_fu := vJSON.GetValue<String>('response.ssuser.quotarefu');
  Result.max_threads := vJSON.GetValue<String>('response.ssuser.maxthreads');
  Result.max_download_speed := vJSON.GetValue<String>('response.ssuser.maxdownloadspeed');
  Result.requests_today := vJSON.GetValue<String>('response.ssuser.requeststoday');
  Result.requests_ko_today := vJSON.GetValue<String>('response.ssuser.requestskotoday');
  Result.max_requests_per_min := vJSON.GetValue<String>('response.ssuser.maxrequestspermin');
  Result.max_requests_per_day := vJSON.GetValue<String>('response.ssuser.maxrequestsperday');
  Result.maxrequests_ko_per_day := vJSON.GetValue<String>('response.ssuser.maxrequestskoperday');
  Result.visits := vJSON.GetValue<String>('response.ssuser.visites');
  Result.date_last_visit := vJSON.GetValue<String>('response.ssuser.datedernierevisite');
  Result.fav_region := vJSON.GetValue<String>('response.ssuser.favregion');
end;

end.
