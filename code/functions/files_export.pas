unit files_export;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.StdCtrls,
  FMX.Forms,
  vars_consts;

procedure Create_CSS_File(dir: string);

procedure CreateAndSave_Markup(filename: string; single: boolean);
procedure CreateAndSave_Html(filename: string; single: boolean; pbar: TProgressBar);
procedure CreateAndSave_json(filename: string);
procedure CreateAndSave_xml(filename: string);

implementation

uses
  uTheGamesDatabase,
  prj_functions,
  configuration,
  uDataModule;

procedure Create_CSS_File(dir: string);
var
  strlst: TStringList;
begin

  strlst := TStringList.Create;
  strlst.Add('body {');
  strlst.Add('  font-family: sans-serif;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('table {');
  strlst.Add('  border-collapse: collapse;');
  strlst.Add('  border: 1px solid black;');
  strlst.Add('  text-align: center;');
  strlst.Add('  vertical-align: middle;');
  strlst.Add('  width: 100%;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('th, td {');
  strlst.Add('  padding: 8px;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('th {');
  strlst.Add('  width: 33%;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('thead {');
  strlst.Add('  background-color: #333;');
  strlst.Add('  color: white;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.table_properties tbody tr:nth-child(odd) {');
  strlst.Add('  background-color: #fff;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.table_properties tbody tr:nth-child(even) td {');
  strlst.Add('  background-color: #B2BEB5;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.table_properties tbody td:nth-child(1) {');
  strlst.Add('  background-color: #36c;');
  strlst.Add('  color: #fff;');
  strlst.Add('}');
  strlst.Add('hr {');
  strlst.Add('  margin-top: 10px;');
  strlst.Add('  margin-bottom: 10px;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add(' ');
  strlst.Add('textarea {');
  strlst.Add('  width: 100%;');
  strlst.Add('  height: 150px;');
  strlst.Add('  padding: 12px 20px;');
  strlst.Add('  box-sizing: border-box;');
  strlst.Add('  border:2px solid #ccc;');
  strlst.Add('  border-radius: 4px;');
  strlst.Add('  background-color: #f8f8f8;');
  strlst.Add('  font-size: 16px;');
  strlst.Add('  resize: none;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('table_header {');
  strlst.Add('  margin-top: 30px;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.table_properties {');
  strlst.Add('  width: 50%;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.table_properties td:nth-child(1) {');
  strlst.Add('  width: 25%;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.copyright {');
  strlst.Add('  text-align: right;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.state_icon_green {');
  strlst.Add('  width: 30px;');
  strlst.Add('  height: 30px;');
  strlst.Add('  margin: 0 auto;');
  strlst.Add('  background-color: #43A22B;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.state_icon_light_blue {');
  strlst.Add('  width: 30px;');
  strlst.Add('  height: 30px;');
  strlst.Add('  margin: 0 auto;');
  strlst.Add('  background-color: #5C93ED;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.state_icon_yellow {');
  strlst.Add('  width: 30px;');
  strlst.Add('  height: 30px;');
  strlst.Add('  margin: 0 auto;');
  strlst.Add('  background-color: #F2D624;');
  strlst.Add('}');
  strlst.Add(' ');
  strlst.Add('.state_icon_red {');
  strlst.Add('  width: 30px;');
  strlst.Add('  height: 30px;');
  strlst.Add('  margin: 0 auto;');
  strlst.Add('  background-color: #940101;');
  strlst.Add('}');
  strlst.SaveToFile(dir + PathDelim + 'dspfm.css');
  FreeAndNil(strlst);

end;

procedure CreateAndSave_Markup(filename: string; single: boolean);
var
  list: TStringList;
  wWorking, wMinor, wMajor, wNot: integer;
  iState: string;
  procedure createHeader;
  begin
    list.Add('### Arcade Compatibility');
    list.Add('');
    list.Add('-----');
    list.Add('Supported Games: (' + dm.tArcade.RecordCount.ToString + ') _______________________  ![](' +
      cHomepage_git + 'working.png) (' + wWorking.ToString + ')  ![](' + cHomepage_git +
      'working_minor.png) (' + wMinor.ToString + ')  ![](' + cHomepage_git + 'working_major.png) (' +
      wMajor.ToString + ')  ![](' + cHomepage_git + 'not_working.png) (' + wNot.ToString + ')');
    list.Add('| Game Title  | Compatibility | Last Date Testing | Page Details |');
    list.Add('| --------    | --------      | --------     | --------       |');
  end;

begin
  list := TStringList.Create;
  createHeader;
  if single then
  begin
    case dm.tArcadestate_icon.AsInteger of
      0:
        iState := 'working.png';
      1:
        iState := 'working_minor.png';
      2:
        iState := 'working_major.png';
      3:
        iState := 'not_working.png';
    end;
    list.Add('| ' + dm.tArcadename.AsString + '    | ![](' + cHomepage_git + iState + ') ' +
      dm.tArcadestate.AsString + '  | ' + dm.tArcadeyear.AsString + ' | ' +
      dm.tArcadestate_desc.AsString + ' |');

    list.SaveToFile(filename);
  end
  else
  begin
    dm.query.SQL.Text := 'Select Count(state_icon) from arcade where state_icon=:state_icon';
    dm.query.ParamByName('state_icon').AsString := '0';
    try
      dm.query.Open;
      wWorking := dm.query.Fields[0].AsInteger;
    finally
      dm.query.Close;
    end;

    dm.query.SQL.Text := 'Select Count(state_icon) from arcade where state_icon=:state_icon';
    dm.query.ParamByName('state_icon').AsString := '1';
    try
      dm.query.Open;
      wMinor := dm.query.Fields[0].AsInteger;
    finally
      dm.query.Close;
    end;

    dm.query.SQL.Text := 'Select Count(state_icon) from arcade where state_icon=:state_icon';
    dm.query.ParamByName('state_icon').AsString := '2';
    try
      dm.query.Open;
      wMajor := dm.query.Fields[0].AsInteger;
    finally
      dm.query.Close;
    end;

    dm.query.SQL.Text := 'Select Count(state_icon) from arcade where state_icon=:state_icon';
    dm.query.ParamByName('state_icon').AsString := '2';
    try
      dm.query.Open;
      wNot := dm.query.Fields[0].AsInteger;
    finally
      dm.query.Close;
    end;
    dm.tArcade.First;

    while not dm.tArcade.Eof do
    begin
      case dm.tArcadestate_icon.AsInteger of
        0:
          iState := 'working.png';
        1:
          iState := 'working_minor.png';
        2:
          iState := 'working_major.png';
        3:
          iState := 'not_working.png';
      end;
      list.Add('| ' + dm.tArcadename.AsString + '    | ![](' + cHomepage_git + iState + ') ' +
        dm.tArcadestate.AsString + '  | ' + dm.tArcadeyear.AsString + ' | ' +
        dm.tArcadestate_desc.AsString + ' |');

      dm.tArcade.Next;
    end;

    list.SaveToFile(filename + 'dsp_all_games.txt');
  end;

  FreeAndNil(list);
end;

procedure CreateAndSave_Html(filename: string; single: boolean; pbar: TProgressBar);
type
  Tgame_info = record
    game_name: string;
    emulator_type: string;
    version: string;
    rom_name: string;
    cdevelopmed: string;
    cpublished: string;
    year: string;
    players: string;
    coop: string;
    genre: string;
    hiscore: string;
    description: string;
    box_art: string;
    snapshot: string;
    movie: string;
    state: string;
    state_reason: string;
    state_icon: integer;
  end;

var
  list: TStringList;
  vi: integer;
  temp_rom_name: string;
  query1, query2: string;
  fGameInfo: Tgame_info;

  function get_game_info: Tgame_info;
  begin
    Result.game_name := dm.tArcadename.AsString;
    Result.rom_name := dm.tArcaderom.AsString;

    dm.tArcadeTGDB.Locate('rom', dm.tArcaderom.AsString);

    Result.cdevelopmed := vScraper_TGDB.get_developer_by_id(dm.tArcadeTGDBdevelopers.AsString);
    Result.cpublished := vScraper_TGDB.get_publisher_by_id(dm.tArcadeTGDBpublishers.AsString);
    Result.emulator_type := 'Arcade';
    Result.version := get_version;
    Result.year := dm.tArcadeyear.AsString;
    Result.players := dm.tArcadeTGDBplayers.AsString;
    Result.coop := dm.tArcadeTGDBcoop.AsString;
    Result.genre := vScraper_TGDB.get_genre_by_id(dm.tArcadeTGDBgenres.AsString);
    if dm.tArcadehiscore.AsInteger = 1 then
      Result.hiscore := 'Supported'
    else
      Result.hiscore := 'Not Supported';
    Result.description := dm.tArcadeTGDBoverview.AsString;

    dm.tArcadeMedia.Locate('rom', dm.tArcaderom.AsString);

    Result.box_art := dm.tArcadeMediabox_art.AsString;;
    Result.snapshot := '';
    Result.movie := '';
    Result.state := dm.tArcadestate.AsString;
    Result.state_icon := dm.tArcadestate_icon.AsInteger;
    Result.state_reason := dm.tArcadestate_desc.AsString;
  end;

  procedure create_html(gInfo: Tgame_info; save_path: string);
  begin
    list := TStringList.Create;

    list := TStringList.Create;
    list.Add('<! DOCTYPE html>');
    list.Add('<html lang="en">');
    list.Add('<head>');
    list.Add('<meta charset="utf-8">');
    list.Add('<title>' + gInfo.game_name + '</title>');
    list.Add('<link rel="stylesheet" href="dspfm.css">');
    list.Add('<script src="script.js"></script>');
    list.Add('</head>');
    list.Add('<body>');
    list.Add('<table class="table_header">');
    list.Add('<thead>');
    list.Add('<th>Game Name</th>');
    list.Add('<th>Emulator </th>');
    list.Add('<th>Version</th>');
    list.Add('</thead>');
    list.Add('<tr>');
    list.Add('<td>' + gInfo.game_name + '</td>');
    list.Add('<td>' + gInfo.emulator_type + '</td>');
    list.Add('<td>' + gInfo.version + '</td>');
    list.Add('</tr>');
    list.Add('</table>');
    list.Add('<h1 class="properties">Properties</h1>');
    list.Add('<table class="table_properties">');
    list.Add('<tr>');
    list.Add('<td>Rom Name</td>');
    list.Add('<td>' + gInfo.rom_name + '</td>');
    list.Add('</tr>');
    list.Add('<tr>');
    list.Add('<td>Developmed By</td>');
    list.Add('<td>' + gInfo.cdevelopmed + '</td>');
    list.Add('</tr>');
    list.Add('<tr>');
    list.Add('<td>Published By</td>');
    list.Add('<td>' + gInfo.cpublished + '</td>');
    list.Add('</tr>');
    list.Add('<tr>');
    list.Add('<td>Year</td>');
    list.Add('<td>' + gInfo.year + '</td>');
    list.Add('</tr>');
    list.Add('<tr>');
    list.Add('<td>Players</td>');
    list.Add('<td>' + gInfo.players + '</td>');
    list.Add('</tr>');
    list.Add('<tr>');
    list.Add('<td>Co Oparation</td>');
    list.Add('<td>' + gInfo.coop + '</td>');
    list.Add('</tr>');
    list.Add('<tr>');
    list.Add('<td>Genre</td>');
    list.Add('<td>' + gInfo.genre + '</td>');
    list.Add('</tr>');
    list.Add('<tr>');
    list.Add('<td>Support Hi Score</td>');
    list.Add('<td>' + gInfo.hiscore + '</td>');
    list.Add('</tr>');
    list.Add('</table>');
    list.Add('<h1 class="description">Description</h1>');
    list.Add('<form>');
    list.Add('<textarea>' + gInfo.description + '</textarea>');
    list.Add('</form>');
    list.Add('<h1 class="media">Media</h1>');
    list.Add('<table class="table_media">');
    list.Add('<thead>');
    list.Add('<th>Box art</th>');
    list.Add('<th>Snapshot</th>');
    list.Add('<th>Video</th>');
    list.Add('</thead>');
    list.Add('<tr>');
    list.Add('<td><img src="' + gInfo.box_art + '" alt="" width="100%" height="400"></td>');
    list.Add('<td><img src="' + gInfo.snapshot + '" alt="" width="100%" height="400"></td>');
    list.Add('<td><video width="320" height="240" controls><source src="' + gInfo.movie +
      '" type="video/mp4"><source src="' + gInfo.movie +
      '" type="video/ogg">Your browser does not support the video tag.</video> </td>');
    list.Add('</tr>');
    list.Add('</table>');
    list.Add('<h1 class="game_state">Game State</h1>');
    list.Add('<table class="table_state">');
    list.Add('<thead>');
    list.Add('<th>State</th>');
    list.Add('<th>Icon</th>');
    list.Add('<th>Reason</th>');
    list.Add('</thead>');
    list.Add('<tr>');
    list.Add('<td>' + gInfo.state + '</td>');
    list.Add('<td>');
    case gInfo.state_icon of
      0:
        begin
          list.Add('<div class="state_icon_green">');
          list.Add('</div>');
        end;
      1:
        begin
          list.Add('<div class="state_icon_light_blue">');
          list.Add('</div>');
        end;
      2:
        begin
          list.Add('<div class="state_icon_yellow">');
          list.Add('</div>');
        end;
      3:
        begin
          list.Add('<div class="state_icon_red">');
          list.Add('</div>');
        end;
    end;
    list.Add('</td>');
    list.Add('<td>');
    list.Add('<form>');
    list.Add('<textarea>' + gInfo.state_reason + '</textarea>');
    list.Add('</form>');
    list.Add('</td>');
    list.Add('</tr>');
    list.Add('</table>');
    list.Add('<hr>');
    list.Add('<h3 class="copyright">Copyright <a href="https://www.az-creations.duckdns.org">Az-Creations</a></h2>');
    list.Add('</body>');
    list.Add('</html>');

    list.SaveToFile(save_path + gInfo.rom_name + '.html');
    FreeAndNil(list);
  end;

begin
  if single then
  begin
    vPath := ExtractFilePath(filename);
    Create_CSS_File(vPath);
    dm.tArcade.Locate('rom', dm.tArcaderom.AsString);
    fGameInfo := get_game_info;
    create_html(fGameInfo, vPath);
  end
  else
  begin
    Create_CSS_File(filename);
    pbar.Min := 0;
    pbar.Max := dm.tArcade.RecordCount;
    pbar.Value := 0;

    pbar.Visible := True;
    dm.tArcade.First;
    vi := 0;
    while not dm.tArcade.Eof do
    begin
      fGameInfo := get_game_info;
      create_html(fGameInfo, filename + PathDelim);
      inc(vi);
      pbar.Value := vi;
      application.ProcessMessages;

      dm.tArcade.Next;
    end;
    pbar.Visible := false;
  end;
end;

procedure CreateAndSave_json(filename: string);
begin

end;

procedure CreateAndSave_xml(filename: string);
begin

end;

end.
