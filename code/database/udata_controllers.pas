unit udata_controllers;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.ComboEdit,
  FMX.StdCtrls;

procedure get_selected_key_map;
procedure get_selected_key_map_ingame;
procedure get_selected_key_map_frontend;

procedure save_key_map_to_database(emulator_name, key_name: string; key_id: word);

function get_key_map_from_database(player_name, platform_name, player, key_type, key: string): word;

implementation

uses
  controls_engine,
  configuration,
  prj_functions,
  uDataModule;

procedure get_selected_key_map;
const
  players: array [0 .. 3] of string = ('1', '2', '3', '4');
  ptype: array [0 .. 14] of string = ('arcade', 'spectrum', 'amstrad', 'commodore_64', 'nes',
    'gameboy', 'gameboy_color', 'colecovision', 'master_system', 'sg_1000', 'gamegear', 'epoch_scv',
    'mega_drive', 'chip8', 'game_and_watch');
  pkey_type: array [0 .. 1] of string = ('key', 'joy');
var
  vi, vk, vl: integer;
  num_state: integer;
  name, player, platform_name, key_type: string;
begin
  num_state := 0;
  for vk := 0 to 15 do
  begin
    for vi := 0 to 3 do
    begin
      for vl := 0 to 1 do
      begin
        dm.tPlayers.Filtered := false;
        if vl = 0 then
          dm.tPlayers.Filter := 'player=' + players[vi] + ' AND selected=1 AND platform=' + pType[vk] + ' AND key=1'
        else
          dm.tPlayers.Filter := 'player=' + players[vi] + ' AND selected=1 AND platform=' + pType[vk] + ' AND joy=1';
        dm.tPlayers.Filtered := true;

        p_contrls.name := dm.tPlayersname.AsString;
        p_contrls.controller := pkey_type[vl];
        p_contrls.player := dm.tPlayersplayer.AsString;
        p_contrls.platform_type := dm.tPlayersplatform.AsString;
        name := p_contrls.name;
        player := p_contrls.player;
        platform_name := p_contrls.platform_type;
        key_type := p_contrls.controller;
        dm.tPlayers.Filtered := false;

        case vl of
          0:
            begin
              p_contrls.map_arcade.ncoin[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_coin');
              p_contrls.map_arcade.nstart[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_start');
              p_contrls.map_arcade.nup[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_up');
              p_contrls.map_arcade.ndown[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_down');
              p_contrls.map_arcade.nleft[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_left');
              p_contrls.map_arcade.nright[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_right');
              p_contrls.map_arcade.nbut0[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_b0');
              p_contrls.map_arcade.nbut1[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_b1');
              p_contrls.map_arcade.nbut2[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_b2');
              p_contrls.map_arcade.nbut3[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_b3');
              p_contrls.map_arcade.nbut4[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_b4');
              p_contrls.map_arcade.nbut5[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'key_b5');
            end;
          1:
            begin
              // p_contrls.map_arcade.ncoin[vi] := get_key_map_from_database(name, platform_name, player,
              // 'joy_coin');
              // p_contrls.map_arcade.nstart[vi] := get_key_map_from_database(name, platform_name, player,
              // 'joy_start');
              p_contrls.map_arcade.joy_up[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_up');
              p_contrls.map_arcade.joy_down[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_down');
              p_contrls.map_arcade.joy_left[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_left');
              p_contrls.map_arcade.joy_right[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_right');
              p_contrls.map_arcade.jbut0[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_b0');
              p_contrls.map_arcade.jbut1[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_b1');
              p_contrls.map_arcade.jbut2[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_b2');
              p_contrls.map_arcade.jbut3[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_b3');
              p_contrls.map_arcade.jbut4[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_b4');
              p_contrls.map_arcade.jbut5[vi] :=
                get_key_map_from_database(name, platform_name, player, key_type, 'joy_b5');
            end;
        end;
        inc(num_state);
      end;
      case vk of
        0:
          ;
        1:
          if vi = 1 then
            break;
        2:
          if vi = 1 then
            break;
        3:
          if vi = 1 then
            break;
        4:
          if vi = 1 then
            break;
        5:
          if vi = 0 then
            break;
        6:
          if vi = 0 then
            break;
        7:
          if vi = 1 then
            break;
        8:
          if vi = 1 then
            break;
        9:
          if vi = 1 then
            break;
        10:
          if vi = 0 then
            break;
        11:
          if vi = 1 then
            break;
        12:
          ;
        13:
          if vi = 0 then
            break;
        14:
          if vi = 0 then
            break;
      end;
    end;
  end;
end;

procedure get_selected_key_map_ingame;
begin

end;

procedure save_key_map_to_database(emulator_name, key_name: string; key_id: word);
begin

end;

function get_key_map_from_database(player_name, platform_name, player, key_type, key: string): word;
var
  ctrl_map_id, ctrl_map_name: string;
begin
  dm.tPlayers.Filtered := false;
  if key_type = 'key' then
    dm.tPlayers.Filter := 'name=' + player_name + ' AND player=' + player + ' AND key=1'
  else
    dm.tPlayers.Filter := 'name=' + player_name + ' AND player=' + player + ' AND joy=1';
  dm.tPlayers.Filtered := true;

  ctrl_map_name := dm.tPlayersname.AsString;
  ctrl_map_id := dm.tPlayerskey_map_num.AsString;

  dm.tKeyboard.Open('Select ' + key + ' From key_map where num='+ctrl_map_id);
  result := dm.tKeyboard.Fields[0].AsLongWord;
  dm.tPlayers.Filtered := false;
end;

procedure get_selected_key_map_frontend;
begin

end;

end.
