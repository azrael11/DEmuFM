unit file_engine;

interface

uses
  WinApi.Windows,
  Zlib,
  zip,
  System.SysUtils,
  Fmx.Dialogs,
  language,
  sound_engine,
  inifiles,
  main_engine,
  controls_engine,
  misc_functions,
  timer_engine;

type
  tzip_find_files = record
    posicion_dentro_zip: integer;
    nombre_zip, file_mask: string;
  end;

  // Hi Score
procedure save_hi(nombre: string; posicion: pbyte; longitud: dword);
function load_hi(nombre: string; posicion: pbyte; longitud: word): boolean;
// Iniciar fichero INI
procedure file_ini_save;
// Start configuration of DSPFM from database
procedure load_dspfm_configuration;
procedure save_dspfm_configuration;
// Ficheros normales
function read_file_size(nombre_file: string; var longitud: integer): boolean;
function read_file(nombre_file: string; donde: pbyte; var longitud: integer): boolean;
function write_file(nombre_file: string; donde: pbyte; longitud: integer): boolean;
function file_name_only(cadena: string): string;
// Parte ZIP
function search_file_from_zip(nombre_zip, file_mask: string; var nombre_file: string; var longitud: integer; crc: dword; warning: boolean): boolean;
function find_next_file_zip(var nombre_file: string; var longitud: integer; crc: dword): boolean;
function load_file_from_zip(nombre_zip, nombre_file: string; donde: pbyte; var longitud: integer; var crc: dword; warning: boolean = true): boolean;
function load_file_from_zip_crc(nombre_zip: string; donde: pbyte; var longitud: integer; crc: dword; warning: boolean = true): boolean;
// Parte ZLIB
procedure compress_zlib(in_buffer: pointer; in_size: integer; out_buffer: pointer; var out_size: integer);
procedure decompress_zlib(in_buffer: pointer; in_size: integer; var out_buffer: pointer; var out_size: integer);

var
  zip_find_files_data: tzip_find_files;

implementation

uses
  spectrum_misc,
  main,
  amstrad_cpc,
  sms,
  gb,
  uDataModule;

procedure load_dspfm_configuration;
begin

end;

procedure save_dspfm_configuration;
begin

end;

// Hi-score
procedure save_hi(nombre: string; posicion: pbyte; longitud: dword);
var
  fichero: file of byte;
begin
{$I-}
  assignfile(fichero, dm.tConfighiscore.AsString + nombre);
  rewrite(fichero);
  blockwrite(fichero, posicion^, longitud);
  closefile(fichero);
{$I+}
end;

function load_hi(nombre: string; posicion: pbyte; longitud: word): boolean;
var
  fichero: file of byte;
  l: integer;
begin
  load_hi := false;
  if not(fileexists(dm.tConfighiscore.AsString + nombre)) then
    exit;
{$I-}
  assignfile(fichero, dm.tConfighiscore.AsString + nombre);
  reset(fichero);
  blockread(fichero, posicion^, longitud, l);
  closefile(fichero);
{$I+}
  if l <> longitud then
    exit;
  load_hi := true;
end;

// INI Files
function test_dir(cadena: string): string;
var
  f: word;
begin
  for f := length(cadena) downto 1 do
    if cadena[f] <> pathdelim then
      break;
  test_dir := System.copy(cadena, 1, f);
end;

procedure file_ini_save;
var
  inifile: Tinifile;
  f: byte;
begin
  // Inicializacion de Diretorios
  inifile.Writestring('dir', 'spectrum_rom_48', test_dir(Directory.spectrum_48));
  inifile.Writestring('dir', 'spectrum_rom_128', test_dir(Directory.spectrum_128));
  inifile.Writestring('dir', 'spectrum_rom_plus3', test_dir(Directory.spectrum_3));
  inifile.Writestring('dir', 'dir_gif', test_dir(Directory.spectrum_image));
  inifile.Writestring('dir', 'dir_preview', test_dir(Directory.Preview));
  inifile.Writestring('dir', 'dir_save', test_dir(Directory.spectrum_tap_snap));
  inifile.Writestring('dir', 'dir_dsk', test_dir(Directory.spectrum_disk));
  inifile.Writestring('dir', 'ams_tap', test_dir(Directory.amstrad_tap));
  inifile.Writestring('dir', 'ams_dsk', test_dir(Directory.amstrad_disk));
  inifile.Writestring('dir', 'ams_snap', test_dir(Directory.amstrad_snap));
  inifile.Writestring('dir', 'ams_rom', test_dir(Directory.amstrad_rom));
  inifile.Writestring('dir', 'c64_tap', test_dir(Directory.c64_tap));
  inifile.Writestring('dir', 'c64_disk', test_dir(Directory.c64_disk));
  // Config general
  inifile.WriteInteger('dsp', 'video', main_screen.video_mode);
  inifile.WriteInteger('dsp', 'maquina', main_vars.machine_type);
  inifile.WriteInteger('dsp', 'auto_exec', byte(main_vars.auto_exec));
  inifile.WriteInteger('dsp', 'show_crc_error', byte(main_vars.show_crc_error));
  inifile.WriteInteger('dsp', 'center_screen', byte(main_vars.center_screen));
  // Config Spectrum
  inifile.WriteInteger('spectrum', 'issue', byte(var_spectrum.issue2));
  inifile.WriteInteger('spectrum', 'joystick', var_spectrum.tipo_joy);
  inifile.WriteInteger('spectrum', 'border', borde.tipo);
  inifile.WriteInteger('spectrum', 'tipo_mouse', mouse.tipo);
  inifile.WriteInteger('spectrum', 'audioload', byte(var_spectrum.audio_load));
  inifile.WriteInteger('spectrum', 'beeperoversample', byte(var_spectrum.speaker_oversample));
  inifile.WriteInteger('spectrum', 'turbo_sound', byte(var_spectrum.turbo_sound));
  inifile.WriteInteger('spectrum', 'audio_128k', var_spectrum.audio_128k);
  inifile.WriteInteger('spectrum', 'ulaplus', byte(ulaplus.enabled));
  // Configuracion CPC
  for f := 0 to 6 do
    inifile.Writestring('cpc', 'rom_dir_' + inttostr(f), cpc_rom[f].name);
  inifile.WriteInteger('cpc', 'cpcmodel', cpc_ga.cpc_model);
  inifile.WriteInteger('cpc', 'cpcramexp', cpc_ga.ram_exp);
  inifile.WriteInteger('cpc', 'cpccolor', byte(cpc_crt.color_monitor));
  inifile.WriteInteger('cpc', 'cpcbright', cpc_crt.bright);
  // Config SMS
  // inifile.WriteInteger('sms', 'model', sms_model);
  // Config GB
  // inifile.WriteInteger('gb', 'palette', gb_palette);
  // Teclas P1
  inifile.WriteInteger('keyboard', 'up_0', p_contrls.map_arcade.nup[0]);
  inifile.WriteInteger('keyboard', 'down_0', p_contrls.map_arcade.ndown[0]);
  inifile.WriteInteger('keyboard', 'left_0', p_contrls.map_arcade.nleft[0]);
  inifile.WriteInteger('keyboard', 'right_0', p_contrls.map_arcade.nright[0]);
  inifile.WriteInteger('keyboard', 'but0_0', p_contrls.map_arcade.nbut0[0]);
  inifile.WriteInteger('keyboard', 'but1_0', p_contrls.map_arcade.nbut1[0]);
  inifile.WriteInteger('keyboard', 'but2_0', p_contrls.map_arcade.nbut2[0]);
  inifile.WriteInteger('keyboard', 'but3_0', p_contrls.map_arcade.nbut3[0]);
  inifile.WriteInteger('keyboard', 'but4_0', p_contrls.map_arcade.nbut4[0]);
  inifile.WriteInteger('keyboard', 'but5_0', p_contrls.map_arcade.nbut5[0]);
  inifile.WriteInteger('keyboard', 'jbut0_0', p_contrls.map_arcade.jbut0[0]);
  inifile.WriteInteger('keyboard', 'jbut1_0', p_contrls.map_arcade.jbut1[0]);
  inifile.WriteInteger('keyboard', 'jbut2_0', p_contrls.map_arcade.jbut2[0]);
  inifile.WriteInteger('keyboard', 'jbut3_0', p_contrls.map_arcade.jbut3[0]);
  inifile.WriteInteger('keyboard', 'jbut4_0', p_contrls.map_arcade.jbut4[0]);
  inifile.WriteInteger('keyboard', 'jbut5_0', p_contrls.map_arcade.jbut5[0]);
  // Teclas Misc
  inifile.WriteInteger('keyboard', 'coin_0', p_contrls.map_arcade.ncoin[0]);
  inifile.WriteInteger('keyboard', 'coin_1', p_contrls.map_arcade.ncoin[1]);
  inifile.WriteInteger('keyboard', 'start_0', p_contrls.map_arcade.nstart[0]);
  inifile.WriteInteger('keyboard', 'start_1', p_contrls.map_arcade.nstart[1]);
  // Teclas P2
  inifile.WriteInteger('keyboard', 'up_1', p_contrls.map_arcade.nup[1]);
  inifile.WriteInteger('keyboard', 'down_1', p_contrls.map_arcade.ndown[1]);
  inifile.WriteInteger('keyboard', 'left_1', p_contrls.map_arcade.nleft[1]);
  inifile.WriteInteger('keyboard', 'right_1', p_contrls.map_arcade.nright[1]);
  inifile.WriteInteger('keyboard', 'but0_1', p_contrls.map_arcade.nbut0[1]);
  inifile.WriteInteger('keyboard', 'but1_1', p_contrls.map_arcade.nbut1[1]);
  inifile.WriteInteger('keyboard', 'but2_1', p_contrls.map_arcade.nbut2[1]);
  inifile.WriteInteger('keyboard', 'but3_1', p_contrls.map_arcade.nbut3[1]);
  inifile.WriteInteger('keyboard', 'but4_1', p_contrls.map_arcade.nbut4[1]);
  inifile.WriteInteger('keyboard', 'but5_1', p_contrls.map_arcade.nbut5[1]);
  inifile.WriteInteger('keyboard', 'jbut0_1', p_contrls.map_arcade.jbut0[1]);
  inifile.WriteInteger('keyboard', 'jbut1_1', p_contrls.map_arcade.jbut1[1]);
  inifile.WriteInteger('keyboard', 'jbut2_1', p_contrls.map_arcade.jbut2[1]);
  inifile.WriteInteger('keyboard', 'jbut3_1', p_contrls.map_arcade.jbut3[1]);
  inifile.WriteInteger('keyboard', 'jbut4_1', p_contrls.map_arcade.jbut4[1]);
  inifile.WriteInteger('keyboard', 'jbut5_1', p_contrls.map_arcade.jbut5[1]);
  // Autofire
  inifile.WriteInteger('keyboard', 'autofire_p1_but0', byte(timers.autofire_enabled[0]));
  inifile.WriteInteger('keyboard', 'autofire_p1_but1', byte(timers.autofire_enabled[1]));
  inifile.WriteInteger('keyboard', 'autofire_p1_but2', byte(timers.autofire_enabled[2]));
  inifile.WriteInteger('keyboard', 'autofire_p1_but3', byte(timers.autofire_enabled[3]));
  inifile.WriteInteger('keyboard', 'autofire_p1_but4', byte(timers.autofire_enabled[4]));
  inifile.WriteInteger('keyboard', 'autofire_p1_but5', byte(timers.autofire_enabled[5]));
  inifile.WriteInteger('keyboard', 'autofire_p2_but0', byte(timers.autofire_enabled[6]));
  inifile.WriteInteger('keyboard', 'autofire_p2_but1', byte(timers.autofire_enabled[7]));
  inifile.WriteInteger('keyboard', 'autofire_p2_but2', byte(timers.autofire_enabled[8]));
  inifile.WriteInteger('keyboard', 'autofire_p2_but3', byte(timers.autofire_enabled[9]));
  inifile.WriteInteger('keyboard', 'autofire_p2_but4', byte(timers.autofire_enabled[10]));
  inifile.WriteInteger('keyboard', 'autofire_p2_but5', byte(timers.autofire_enabled[11]));
  inifile.WriteInteger('keyboard', 'autofire_general', byte(timers.autofire_on));
  // tipo y numero joystick
  inifile.WriteInteger('keyboard', 'use_keyb_0', byte(not(p_contrls.map_arcade.use_key[0])));
  inifile.WriteInteger('keyboard', 'use_keyb_1', byte(not(p_contrls.map_arcade.use_key[1])));
  inifile.WriteInteger('keyboard', 'num_joy_0', p_contrls.map_arcade.num_joystick[0]);
  inifile.WriteInteger('keyboard', 'num_joy_1', p_contrls.map_arcade.num_joystick[1]);
  // Joystick calibration
  for f := 0 to NUM_PLAYERS do
  begin
    inifile.WriteInteger('keyboard', 'joy_up_' + inttostr(f), p_contrls.map_arcade.joy_up[f]);
    inifile.WriteInteger('keyboard', 'joy_down_' + inttostr(f), p_contrls.map_arcade.joy_down[f]);
    inifile.WriteInteger('keyboard', 'joy_left_' + inttostr(f), p_contrls.map_arcade.joy_left[f]);
    inifile.WriteInteger('keyboard', 'joy_right_' + inttostr(f), p_contrls.map_arcade.joy_right[f]);
  end;
  // Cerrar
  inifile.free;
end;

function file_name_only(cadena: string): string;
var
  f: word;
  cadena2: string;
begin
  for f := length(cadena) downto 1 do
  begin
    if cadena[f] = pathdelim then
    begin
      cadena2 := System.copy(cadena, f + 1, length(cadena) - f);
      break;
    end;
  end;
  if cadena2 = '' then
    cadena2 := cadena;
  file_name_only := cadena2;
end;

function read_file_size(nombre_file: string; var longitud: integer): boolean;
var
  fichero: file of byte;
begin
  read_file_size := false;
{$I-}
  filemode := fmOpenRead;
  assignfile(fichero, nombre_file);
  reset(fichero);
  if ioresult <> 0 then
    exit;
  longitud := filesize(fichero);
  closefile(fichero);
  filemode := fmOpenReadWrite;
{$I+}
  if longitud > 0 then
    read_file_size := true;
end;

function read_file(nombre_file: string; donde: pbyte; var longitud: integer): boolean;
var
  fichero: file of byte;
begin
  read_file := false;
{$I-}
  filemode := fmOpenRead;
  assignfile(fichero, nombre_file);
  reset(fichero);
  if ioresult <> 0 then
  begin
    // MessageDlg('Cannot open file: ' + '"' + nombre_file + '"', mtError, [mbOk], 0);
    exit;
  end;
  longitud := filesize(fichero);
  blockread(fichero, donde^, longitud);
  closefile(fichero);
  filemode := fmOpenReadWrite;
{$I+}
  if longitud > 0 then
    read_file := true;
end;

function write_file(nombre_file: string; donde: pbyte; longitud: integer): boolean;
var
  fichero: file of byte;
  escrito: integer;
begin
  write_file := false;
{$I-}
  assignfile(fichero, nombre_file);
  rewrite(fichero);
  if ioresult <> 0 then
  begin
    // MessageDlg('Cannot write file: ' + '"' + nombre_file + '"', mtError, [mbOk], 0);
    exit;
  end;
  blockwrite(fichero, donde^, longitud, escrito);
  if longitud <> escrito then
  begin
    // MessageDlg('Error writing data: ' + '"' + nombre_file + '"', mtError, [mbOk], 0);
    close(fichero);
    exit;
  end;
  close(fichero);
{$I+}
  write_file := true;
end;

// ZIP
function search_file_from_zip(nombre_zip, file_mask: string; var nombre_file: string; var longitud: integer; crc: dword; warning: boolean): boolean;
var
  f: integer;
  extension, extension2: string;
  res: boolean;
  ZipFile: TZipFile;
begin
  res := false;
  if not(fileexists(nombre_zip)) then
    exit;
  ZipFile := TZipFile.Create;
  ZipFile.Open(nombre_zip, zmRead);
  for f := 0 to (ZipFile.FileCount - 1) do
  begin
    nombre_file := ZipFile.FileNames[f];
    longitud := ZipFile.FileInfos[f].UncompressedSize;
    crc := ZipFile.FileInfos[f].CRC32;
    zip_find_files_data.posicion_dentro_zip := f;
    zip_find_files_data.nombre_zip := nombre_zip;
    zip_find_files_data.file_mask := file_mask;
    if file_mask[1] = '*' then
    begin
      if file_mask = '*.*' then
      begin
        res := true;
        break;
      end;
      extension := extension_fichero(nombre_file);
      extension2 := extension_fichero(file_mask);
      if extension = extension2 then
      begin
        res := true;
        break;
      end;
    end
    else
    begin
      if lowercase(nombre_file) = lowercase(file_mask) then
      begin
        res := true;
        break;
      end;
    end;
  end;
  ZipFile.close;
  ZipFile.free;
  search_file_from_zip := res;
  // if (warning and not(res)) then
  // MessageDlg(leng[main_vars.language].errores[0] + ' "' + nombre_file + '" ' +
  // leng[main_vars.language].errores[1] + ' ' + nombre_zip, mtError, [mbOk], 0);
end;

function find_next_file_zip(var nombre_file: string; var longitud: integer; crc: dword): boolean;
var
  f: integer;
  extension, extension2: string;
  ZipFile: TZipFile;
begin
  find_next_file_zip := false;
  ZipFile := TZipFile.Create;
  ZipFile.Open(zip_find_files_data.nombre_zip, zmRead);
  for f := (zip_find_files_data.posicion_dentro_zip + 1) to (ZipFile.FileCount - 1) do
  begin
    nombre_file := ZipFile.FileNames[f];
    longitud := ZipFile.FileInfos[f].UncompressedSize;
    crc := ZipFile.FileInfos[f].CRC32;
    zip_find_files_data.posicion_dentro_zip := f;
    if zip_find_files_data.file_mask[1] = '*' then
    begin
      if zip_find_files_data.file_mask = '*.*' then
      begin
        find_next_file_zip := true;
        break;
      end;
      extension := extension_fichero(nombre_file);
      extension2 := extension_fichero(zip_find_files_data.file_mask);
      if extension = extension2 then
      begin
        find_next_file_zip := true;
        break;
      end;
    end
    else
    begin
      if lowercase(nombre_file) = lowercase(zip_find_files_data.file_mask) then
      begin
        find_next_file_zip := true;
        break;
      end;
    end;
  end;
  ZipFile.close;
  ZipFile.free;
end;

function load_file_from_zip(nombre_zip, nombre_file: string; donde: pbyte; var longitud: integer; var crc: dword; warning: boolean = true): boolean;
var
  f: word;
  find: boolean;
  ZipFile: TZipFile;
  buffer: Tbytes;
begin
  load_file_from_zip := false;
  // Si no existe el ZIP -> Error
  if not(fileexists(nombre_zip)) then
    exit;
  find := false;
  ZipFile := TZipFile.Create;
  if not(ZipFile.IsValid(nombre_zip)) then
    exit;
  ZipFile.Open(nombre_zip, zmRead);
  for f := 0 to (ZipFile.FileCount - 1) do
  begin
    if lowercase(ZipFile.FileNames[f]) = lowercase(nombre_file) then
    begin
      find := true;
      break;
    end;
  end;
  if not(find) then
  begin
    ZipFile.close;
    ZipFile.free;
    // if warning then
    // MessageDlg(leng[main_vars.language].errores[0] + ' "' + nombre_file + '" ' +
    // leng[main_vars.language].errores[1] + ' ' + nombre_zip, mtError, [mbOk], 0);
    exit;
  end;
  longitud := ZipFile.FileInfos[f].UncompressedSize;
  crc := ZipFile.FileInfos[f].CRC32;
  SetLength(buffer, longitud);
  ZipFile.Read(f, buffer);
  copymemory(donde, @buffer[0], longitud);
  SetLength(buffer, 0);
  ZipFile.close;
  ZipFile.free;
  load_file_from_zip := true;
end;

function load_file_from_zip_crc(nombre_zip: string; donde: pbyte; var longitud: integer; crc: dword; warning: boolean = true): boolean;
var
  f: word;
  find: boolean;
  ZipFile: TZipFile;
  buffer: Tbytes;
begin
  load_file_from_zip_crc := false;
  // Si no existe el ZIP -> Error
  if not(fileexists(nombre_zip)) then
  begin
    // MessageDlg(leng[main_vars.language].errores[2] + ' "' + extractfilename(nombre_zip) + '" ',
    // mtError, [mbOk], 0);
    exit;
  end;
  find := false;
  ZipFile := TZipFile.Create;
  ZipFile.Open(nombre_zip, zmRead);
  for f := 0 to (ZipFile.FileCount - 1) do
  begin
    if ZipFile.FileInfos[f].CRC32 = cardinal(crc) then
    begin
      find := true;
      break;
    end;
  end;
  if not(find) then
  begin
    ZipFile.close;
    ZipFile.free;
    exit;
  end;
  longitud := ZipFile.FileInfos[f].UncompressedSize;
  SetLength(buffer, longitud);
  ZipFile.Read(f, buffer);
  copymemory(donde, @buffer[0], longitud);
  SetLength(buffer, 0);
  ZipFile.close;
  ZipFile.free;
  load_file_from_zip_crc := true;
end;

// Funciones de zlib
procedure compress_zlib(in_buffer: pointer; in_size: integer; out_buffer: pointer; var out_size: integer);
var
  buffer: pointer;
begin
  buffer := nil;
  ZCompress(pointer(in_buffer), in_size, buffer, out_size, zcDefault);
  copymemory(out_buffer, buffer, out_size);
  freemem(buffer);
end;

procedure decompress_zlib(in_buffer: pointer; in_size: integer; var out_buffer: pointer; var out_size: integer);
var
  buffer: pointer;
begin
  buffer := nil;
  ZDecompress(pointer(in_buffer), in_size, buffer, out_size);
  if out_buffer = nil then
    getmem(out_buffer, out_size);
  copymemory(out_buffer, buffer, out_size);
  freemem(buffer);
end;

end.
