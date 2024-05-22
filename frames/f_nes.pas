unit f_nes;

{ Nes emulator uses the global number 5 in paths and games }

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Layouts,
  FMX.TabControl,
  System.IOUtils;

type
  Tn_e_s = class(TFrame)
    od_nes_config: TOpenDialog;
    tc_nes: TTabControl;
    ti_nes_general: TTabItem;
    ti_nes_dirs: TTabItem;
    verts_nes_dirs: TVertScrollBox;
    rect_nes_dirs: TRectangle;
    edt_nes_dirs_images: TEdit;
    edt_nes_dirs_manuals: TEdit;
    edt_nes_dirs_roms: TEdit;
    edt_nes_dirs_video: TEdit;
    spb_nes_dirs_images: TSpeedButton;
    img_nes_dirs_images: TImage;
    spb_nes_dirs_manuals: TSpeedButton;
    img_nes_dirs_manuals: TImage;
    spb_nes_dirs_roms: TSpeedButton;
    img_nes_dirs_roms: TImage;
    spb_nes_dirs_video: TSpeedButton;
    img_nes_dirs_video: TImage;
    txt_nes_dirs_images: TText;
    txt_nes_dirs_manuals: TText;
    txt_nes_dirs_roms: TText;
    txt_nes_dirs_video: TText;
    pb_nes_dirs: TProgressBar;
    txt_nes_dirs: TText;
    spb_nes_dirs_const_roms_refresh: TSpeedButton;
    img_nes_dirs_const_roms_refresh: TImage;
    procedure OnShow;
    procedure spb_nes_dirs_romsClick(Sender: TObject);
  private
    { Private declarations }
    procedure fields_active(enable: boolean);
    procedure reload_main;
    function scan_dir_roms(dir: string; refresh: boolean): integer;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  umain_config,
  unes_actions, uDataModule;

{ Tn_e_s }

procedure Tn_e_s.fields_active(enable: boolean);
begin
  edt_nes_dirs_images.Enabled := enable;
  edt_nes_dirs_manuals.Enabled := enable;
  edt_nes_dirs_roms.Enabled := enable;
  edt_nes_dirs_video.Enabled := enable;
  spb_nes_dirs_images.Enabled := enable;
  spb_nes_dirs_manuals.Enabled := enable;
  spb_nes_dirs_roms.Enabled := enable;
  spb_nes_dirs_video.Enabled := enable;
  spb_nes_dirs_const_roms_refresh.Enabled := enable;
end;

procedure Tn_e_s.OnShow;
begin
//  dm.tNes_Config.Edit; // Not Ready yet


  edt_nes_dirs_roms.Text := config.emu_path[5].roms;
  edt_nes_dirs_manuals.Text := config.emu_path[5].manuals;
  edt_nes_dirs_video.Text := config.emu_path[5].video;
  edt_nes_dirs_images.Text := config.emu_path[5].snapshots;
end;

procedure Tn_e_s.reload_main;
begin
  unes_actions.Nes_Action.show_games_in_grid;
end;

function Tn_e_s.scan_dir_roms(dir: string; refresh: boolean): integer;

  function is_game_exist_in_database(rom_path: string): boolean;
  begin
    Result := False;
    if dm.tNesrom_path.AsString <> '' then
      Result := True;
  end;

  procedure insert_game_in_database(rom_name, rom_path: string; pb_value: integer);
  begin
    dm.tNes.Edit;
    dm.tNesrom.AsString := rom_name;
    dm.tNesrom_path.AsString := rom_path;
    dm.tNes.Post;
    dm.tNes.ApplyUpdates();

    dm.tNesMedia.Edit;
    dm.tNesMediarom.AsString := rom_name;
    dm.tNesMedia.Post;
    dm.tNesMedia.ApplyUpdates();

    pb_nes_dirs.Value := pb_value;
    Application.ProcessMessages;
  end;

var
  games_list: TStringList;
  vi, vk: integer;
  dir_list_zip, dir_list_nes: TArray<System.String>;
  final_name: string;
  vPos: integer;
begin
  pb_nes_dirs.Visible := True;

  if refresh = False then
  begin
    dir_list_zip := System.IOUtils.TDirectory.GetFiles(dir, '*.zip');
    dir_list_nes := System.IOUtils.TDirectory.GetFiles(dir, '*.nes');
  end
  else
  begin
    dir_list_zip := System.IOUtils.TDirectory.GetFiles(config.emu_path[5].roms, '*.zip');
    dir_list_nes := System.IOUtils.TDirectory.GetFiles(config.emu_path[5].roms, '*.nes');
  end;

  pb_nes_dirs.Min := 0;
  pb_nes_dirs.Max := High(dir_list_zip);

  for vi := 0 to High(dir_list_zip) do
  begin
    final_name := ExtractFileName(dir_list_zip[vi]);
    vPos := Pos('(', final_name);
    if vPos <> 0 then
      final_name := Trim(Copy(final_name, 0, (vPos - 1)));
    vPos := Pos(', The', final_name);
    if vPos <> 0 then
    begin
      Delete(final_name, vPos, 5);
      final_name := 'The ' + final_name;
    end;
    if is_game_exist_in_database(dir_list_zip[vi]) = False then
      insert_game_in_database(final_name, dir_list_zip[vi], (vi + 1));
  end;

  pb_nes_dirs.Min := 0;
  pb_nes_dirs.Max := High(dir_list_nes);

  for vi := 0 to High(dir_list_nes) do
  begin
    final_name := ExtractFileName(dir_list_nes[vi]);
    vPos := Pos('(', final_name);
    if vPos <> 0 then
      final_name := Trim(Copy(final_name, 0, (vPos - 1)));
    vPos := Pos(', The', final_name);
    if vPos <> 0 then
    begin
      Delete(final_name, vPos, 5);
      final_name := 'The ' + final_name;
    end;
    if is_game_exist_in_database(dir_list_nes[vi]) = False then
      insert_game_in_database(final_name, dir_list_nes[vi], (vi + 1));
  end;
end;

procedure Tn_e_s.spb_nes_dirs_romsClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select Roms Directory...', '', dir) then
  begin
    scan_dir_roms(dir, False);
    fields_active(False);
    edt_nes_dirs_roms.Text := dir;
    config.emu_path[5].roms := dir;
    reload_main;
    fields_active(True);
    pb_nes_dirs.Visible := False;
  end;
end;

end.
