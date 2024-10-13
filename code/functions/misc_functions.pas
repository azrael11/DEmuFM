unit misc_functions;

interface

uses
  WinApi.Windows,
  vcl.imaging.pngimage,
  System.SysUtils,
  Fmx.Forms,
  Fmx.Dialogs,
  Fmx.Controls;

const
  SARCADE = 0;
  SNES = 1;
  SCOLECO = 2;
  SGB = 3;
  SCHIP8 = 4;
  SAMSTRADCPC = 5;
  SSMS = 6;
  SSPECTRUM = 7;
  SSG1000 = 8;
  SC64 = 9;
  SGG = 10;
  SSUPERCASSETTE = 11;
  SORIC = 12;
  SPV1000 = 14;
  SPV2000 = 15;
  SAMSTRADROM = 16;
  SROM = 17;
  SEXPORT = 18;
  SBITMAP = 19;
  SGENESIS = 20;
  SGANDW = 21;

function extension_fichero(nombre: string): string;
function calc_crc(p: pointer; byteCount: dword): dword;
function mul_video: byte;
// bit func
function bit(data: dword; bitpos: byte): boolean;
function bit_n(data: dword; bitpos: byte): byte;
function BITSWAP8(val, B7, B6, B5, B4, B3, B2, B1, B0: byte): byte;
function BITSWAP16(val: word; B15, B14, B13, B12, B11, B10, B9, B8, B7, B6, B5, B4, B3, B2, B1,
  B0: byte): word;
function BITSWAP24(val: dword; B23, B22, B21, B20, B19, B18, B17, B16, B15, B14, B13, B12, B11, B10,
  B9, B8, B7, B6, B5, B4, B3, B2, B1, B0: byte): dword;
function BITSWAP32(val: dword; B31, B30, B29, B28, B27, B26, B25, B24, B23, B22, B21, B20, B19, B18,
  B17, B16, B15, B14, B13, B12, B11, B10, B9, B8, B7, B6, B5, B4, B3, B2, B1, B0: byte): dword;
// Load/Save Systems ROM
function openrom(var name: string): boolean;
function saverom(var name: string; var index: byte): boolean;
// Load data
function extract_data(romfile: string; data_des: pbyte; var longitud: integer;
  var file_name: string): boolean;

implementation

uses
  main,
  main_engine,
  file_engine;

function bit(data: dword; bitpos: byte): boolean;
begin
  bit := ((data shr bitpos) and 1) <> 0;
end;

function bit_n(data: dword; bitpos: byte): byte;
begin
  bit_n := (data shr bitpos) and 1;
end;

function BITSWAP8(val, B7, B6, B5, B4, B3, B2, B1, B0: byte): byte;
var
  src: byte;
begin
  src := 0;
  if bit(val, B7) then
    src := src or (1 shl 7);
  if bit(val, B6) then
    src := src or (1 shl 6);
  if bit(val, B5) then
    src := src or (1 shl 5);
  if bit(val, B4) then
    src := src or (1 shl 4);
  if bit(val, B3) then
    src := src or (1 shl 3);
  if bit(val, B2) then
    src := src or (1 shl 2);
  if bit(val, B1) then
    src := src or (1 shl 1);
  if bit(val, B0) then
    src := src or (1 shl 0);
  BITSWAP8 := src;
end;

function BITSWAP16(val: word; B15, B14, B13, B12, B11, B10, B9, B8, B7, B6, B5, B4, B3, B2, B1,
  B0: byte): word;
var
  src: word;
begin
  src := 0;
  if bit(val, B15) then
    src := src or (1 shl 15);
  if bit(val, B14) then
    src := src or (1 shl 14);
  if bit(val, B13) then
    src := src or (1 shl 13);
  if bit(val, B12) then
    src := src or (1 shl 12);
  if bit(val, B11) then
    src := src or (1 shl 11);
  if bit(val, B10) then
    src := src or (1 shl 10);
  if bit(val, B9) then
    src := src or (1 shl 9);
  if bit(val, B8) then
    src := src or (1 shl 8);
  if bit(val, B7) then
    src := src or (1 shl 7);
  if bit(val, B6) then
    src := src or (1 shl 6);
  if bit(val, B5) then
    src := src or (1 shl 5);
  if bit(val, B4) then
    src := src or (1 shl 4);
  if bit(val, B3) then
    src := src or (1 shl 3);
  if bit(val, B2) then
    src := src or (1 shl 2);
  if bit(val, B1) then
    src := src or (1 shl 1);
  if bit(val, B0) then
    src := src or (1 shl 0);
  BITSWAP16 := src;
end;

function BITSWAP24(val: dword; B23, B22, B21, B20, B19, B18, B17, B16, B15, B14, B13, B12, B11, B10,
  B9, B8, B7, B6, B5, B4, B3, B2, B1, B0: byte): dword;
var
  src: dword;
begin
  src := 0;
  if bit(val, B23) then
    src := src or (1 shl 23);
  if bit(val, B22) then
    src := src or (1 shl 22);
  if bit(val, B21) then
    src := src or (1 shl 21);
  if bit(val, B20) then
    src := src or (1 shl 20);
  if bit(val, B19) then
    src := src or (1 shl 19);
  if bit(val, B18) then
    src := src or (1 shl 18);
  if bit(val, B17) then
    src := src or (1 shl 17);
  if bit(val, B16) then
    src := src or (1 shl 16);
  if bit(val, B15) then
    src := src or (1 shl 15);
  if bit(val, B14) then
    src := src or (1 shl 14);
  if bit(val, B13) then
    src := src or (1 shl 13);
  if bit(val, B12) then
    src := src or (1 shl 12);
  if bit(val, B11) then
    src := src or (1 shl 11);
  if bit(val, B10) then
    src := src or (1 shl 10);
  if bit(val, B9) then
    src := src or (1 shl 9);
  if bit(val, B8) then
    src := src or (1 shl 8);
  if bit(val, B7) then
    src := src or (1 shl 7);
  if bit(val, B6) then
    src := src or (1 shl 6);
  if bit(val, B5) then
    src := src or (1 shl 5);
  if bit(val, B4) then
    src := src or (1 shl 4);
  if bit(val, B3) then
    src := src or (1 shl 3);
  if bit(val, B2) then
    src := src or (1 shl 2);
  if bit(val, B1) then
    src := src or (1 shl 1);
  if bit(val, B0) then
    src := src or (1 shl 0);
  BITSWAP24 := src;
end;

function BITSWAP32(val: dword; B31, B30, B29, B28, B27, B26, B25, B24, B23, B22, B21, B20, B19, B18,
  B17, B16, B15, B14, B13, B12, B11, B10, B9, B8, B7, B6, B5, B4, B3, B2, B1, B0: byte): dword;
var
  src: dword;
begin
  src := 0;
  if bit(val, B31) then
    src := src or (1 shl 31);
  if bit(val, B30) then
    src := src or (1 shl 30);
  if bit(val, B29) then
    src := src or (1 shl 29);
  if bit(val, B28) then
    src := src or (1 shl 28);
  if bit(val, B27) then
    src := src or (1 shl 27);
  if bit(val, B26) then
    src := src or (1 shl 26);
  if bit(val, B25) then
    src := src or (1 shl 25);
  if bit(val, B24) then
    src := src or (1 shl 24);
  if bit(val, B23) then
    src := src or (1 shl 23);
  if bit(val, B22) then
    src := src or (1 shl 22);
  if bit(val, B21) then
    src := src or (1 shl 21);
  if bit(val, B20) then
    src := src or (1 shl 20);
  if bit(val, B19) then
    src := src or (1 shl 19);
  if bit(val, B18) then
    src := src or (1 shl 18);
  if bit(val, B17) then
    src := src or (1 shl 17);
  if bit(val, B16) then
    src := src or (1 shl 16);
  if bit(val, B15) then
    src := src or (1 shl 15);
  if bit(val, B14) then
    src := src or (1 shl 14);
  if bit(val, B13) then
    src := src or (1 shl 13);
  if bit(val, B12) then
    src := src or (1 shl 12);
  if bit(val, B11) then
    src := src or (1 shl 11);
  if bit(val, B10) then
    src := src or (1 shl 10);
  if bit(val, B9) then
    src := src or (1 shl 9);
  if bit(val, B8) then
    src := src or (1 shl 8);
  if bit(val, B7) then
    src := src or (1 shl 7);
  if bit(val, B6) then
    src := src or (1 shl 6);
  if bit(val, B5) then
    src := src or (1 shl 5);
  if bit(val, B4) then
    src := src or (1 shl 4);
  if bit(val, B3) then
    src := src or (1 shl 3);
  if bit(val, B2) then
    src := src or (1 shl 2);
  if bit(val, B1) then
    src := src or (1 shl 1);
  if bit(val, B0) then
    src := src or (1 shl 0);
  BITSWAP32 := src;
end;

function extension_fichero(nombre: string): string;
var
  f: word;
  final_, final2: string;
begin
  final_ := extractfileext(nombre);
  final2 := '';
  if final_ <> '' then
    for f := 1 to length(final_) do
    begin
      final_[f] := upcase(final_[f]);
      if final_[f] <> '.' then
        final2 := final2 + final_[f];
    end;
  extension_fichero := final2;
end;

function mul_video: byte;
begin
  case main_screen.video_mode of
    2, 4, 6:
      mul_video := 2;
    5:
      mul_video := 3;
  else
    mul_video := 1;
  end;
end;

{$IFNDEF fpc}

function calc_crc(p: pointer; byteCount: dword): dword;
begin
  // calc_crc := not(update_crc($FFFFFFFF, p, byteCount));
end;
{$ELSE}

function calc_crc(p: pointer; byteCount: dword): dword;
var
  crc: cardinal;
begin
  crc := crc32(0, nil, 0);
  crc := crc32(crc, p, byteCount);
  calc_crc := crc;
end;
{$ENDIF}

function extract_data(romfile: string; data_des: pbyte; var longitud: integer;
  var file_name: string): boolean;
var
  nombre_file, extension: string;
  datos: pbyte;
  salir, resultado: boolean;
  crc: dword;
  ext: array [1 .. 10] of string;
  f, total_ext: byte;
begin
  case main_vars.system_type of
    SNES:
      begin
        ext[1] := 'NES';
        total_ext := 1;
      end;
    SCOLECO:
      begin
        ext[1] := 'COL';
        ext[2] := 'ROM';
        ext[3] := 'CSN';
        ext[4] := 'DSP';
        total_ext := 4;
      end;
    SGB:
      begin
        ext[1] := 'GB';
        ext[2] := 'GBC';
        total_ext := 2;
      end;
    SSG1000:
      begin
        ext[1] := 'SG';
        ext[2] := 'MV';
        ext[3] := 'DSP';
        total_ext := 3;
      end;
    SGG:
      begin
        ext[1] := 'GG';
        ext[2] := 'DSP';
        total_ext := 2;
      end;
    SSMS:
      begin
        ext[1] := 'SMS';
        ext[2] := 'ROM';
        ext[3] := 'DSP';
        total_ext := 3;
      end;
    SSUPERCASSETTE:
      begin
        ext[1] := '0';
        ext[2] := 'BIN';
        ext[3] := 'DSP';
        total_ext := 3;
      end;
    SCHIP8:
      begin
        ext[1] := 'BIN';
        ext[2] := 'CH8';
        ext[3] := 'DSP';
        total_ext := 3;
      end;
    SORIC:
      begin
        ext[1] := 'TAP';
        ext[2] := 'WAV';
        total_ext := 2;
      end;
    SAMSTRADCPC:
      begin
        ext[1] := 'CDT';
        ext[2] := 'TZX';
        ext[3] := 'CSW';
        ext[4] := 'ROM';
        ext[5] := 'WAV';
        ext[6] := 'SNA';
        total_ext := 6;
      end;
    SC64:
      begin
        ext[1] := 'TAP';
        ext[2] := 'PRG';
        ext[3] := 'T64';
        ext[4] := 'WAV';
        ext[5] := 'VSF';
        total_ext := 5;
      end;
    SPV1000, SPV2000:
      begin
        ext[1] := 'BIN';
        ext[2] := 'DSP';
            ext[3]:='ROM';
            total_ext:=3;
      end;
  else
    begin
//      MessageDlg('Sistema sin definir!!!', mtInformation, [mbOk], 0);
      extract_data := false;
      exit;
    end;
  end;
  extension := extension_fichero(romfile);
  datos := nil;
  if extension = 'ZIP' then
  begin
    resultado := false;
    f := 1;
    salir := false;
    while not(salir) do
    begin
      if search_file_from_zip(romfile, '*.' + ext[f], nombre_file, longitud, crc, false) then
      begin
        resultado := true;
        salir := true;
      end;
      f := f + 1;
      if f > total_ext then
        salir := true;
    end;
    if resultado then
    begin
      getmem(datos, longitud);
      if not(load_file_from_zip(romfile, nombre_file, datos, longitud, crc, true)) then
        resultado := false;
    end;
  end
  else
  begin
    resultado := false;
    for f := 1 to total_ext do
      if extension = ext[f] then
        resultado := true;
    if resultado then
    begin
      if read_file_size(romfile, longitud) then
      begin
        getmem(datos, longitud);
        if read_file(romfile, datos, longitud) then
        begin
          resultado := true;
          nombre_file := extractfilename(romfile)
        end;
      end
      else
        resultado := false;
    end;
  end;
  if not(resultado) then
  begin
//    MessageDlg('Error cargando.' + chr(10) + chr(13) + 'Error loading.', mtInformation, [mbOk], 0);
    if datos <> nil then
      freemem(datos);
    extract_data := false;
    nombre_file := '';
    exit;
  end;
  copymemory(data_des, datos, longitud);
  file_name := nombre_file;
  if datos <> nil then
    freemem(datos);
  extract_data := true;
end;

function openrom(var name: string): boolean;
var
  opendialog: topendialog;
begin
{  opendialog := topendialog.Create(principal1);
  case main_vars.system_type of
    SCOLECO:
      begin
        opendialog.InitialDir := directory.coleco;
        opendialog.Filter :=
          'ColecoVision Game/Snapshots (*.col;*.rom;*.csn;*.dsp;*.bin;*.zip)|*.col;*.rom;*.csn;*.dsp;*.bin;*.zip';
      end;
    SNES:
      begin
        opendialog.InitialDir := directory.nes;
        opendialog.Filter := 'NES Game (*.nes;*zip)|*.nes;*.zip';
      end;
    SSMS:
      begin
        opendialog.InitialDir := directory.sms;
        opendialog.Filter := 'SMS Game/Snapshot (*.sms;*.rom;*.dsp;*.zip)|*.sms;*.rom;*.dsp;*.zip';
      end;
    SGB:
      begin
        opendialog.InitialDir := directory.gameboy;
        opendialog.Filter := 'GB Game (*.gb;*.gbc;*zip)|*.gb;*.gbc;*.zip';
      end;
    SCHIP8:
      begin
        opendialog.InitialDir := directory.chip8;
        opendialog.Filter := 'Chip-8 Files (*.ch8;*.bin;*.dsp;*zip)|*.ch8;*.bin;*.dsp;*.zip';
      end;
    SAMSTRADCPC:
      begin
        opendialog.InitialDir := directory.amstrad_tap;
        opendialog.Filter :=
          'CPC Tape/Snapshot/ROM (*.rom;*.cdt;*.tzx;*.csw;*.wav;*.sna;*zip;)|*.rom;*.cdt;*.tzx;*.csw;*.wav;*.sna;*.zip';
      end;
    SROM:
      begin
        opendialog.InitialDir := directory.arcade_list_roms[0];
        opendialog.Filter := 'ROM Files (*.rom;*.zip)|*.rom;*.zip';
      end;
    SAMSTRADROM:
      begin
        opendialog.InitialDir := directory.Amstrad_rom;
        opendialog.Filter := 'CPC ROM Files (*.rom;*.zip)|*.rom;*.zip';
      end;
    SSG1000:
      begin
        opendialog.InitialDir := directory.sg1000;
        opendialog.Filter := 'SG-1000 Game/Snapshot (*.sg;*.mv;*.dsp;*.zip)|*.sg;*.mv;*.dsp;*.zip';
      end;
    SC64:
      begin
        opendialog.InitialDir := directory.c64_tap;
        opendialog.Filter :=
          'C64 Tape/Snapshot (*.prg;*.t64;*.tap;*.wav;*.vsf;*.zip)|*.prg;*.t64;*.tap;*.wav;*.vsf;*.zip';
      end;
    SGG:
      begin
        opendialog.InitialDir := directory.gg;
        opendialog.Filter := 'GG Game/Snapshot (*.gg;*.dsp;*.zip)|*.gg;*.dsp;*.zip';
      end;
    SSUPERCASSETTE:
      begin
        opendialog.InitialDir := directory.scv;
        opendialog.Filter := 'SCV Game/Snapshot (*.bin;*.dsp;*.zip)|*.bin;*.dsp;*.zip';
      end;
    SORIC:
      begin
        opendialog.InitialDir := directory.oric_tap;
        opendialog.Filter := 'Oric Tape (*.tap;*.wav;*.zip)|*.tap;*.wav;*.zip';
      end;
    SPV1000:
      begin
        opendialog.InitialDir := directory.pv1000;
        Opendialog.Filter:='PV1000 Game/Snapshot (*.rom;*.bin;*.dsp;*.zip)|*.rom;*.bin;*.dsp;*.zip';
      end;
    SPV2000:
      begin
        opendialog.InitialDir := directory.pv2000;
        Opendialog.Filter:='PV2000 Game/Snapshot (*.rom;*.bin;*.dsp;*.zip)|*.rom;*.bin;*.dsp;*.zip';
      end;
  end;
  openrom := opendialog.execute;
  name := opendialog.FileName;
  opendialog.free;  }
end;

function saverom(var name: string; var index: byte): boolean;
var
  SaveDialog: tsavedialog;
begin
{  SaveDialog := tsavedialog.Create(principal1);
  case main_vars.system_type of
    SCOLECO:
      begin
        SaveDialog.InitialDir := directory.coleco;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp|CSN Format (*.csn)|*.csn';
      end;
    SAMSTRADCPC:
      begin
        SaveDialog.InitialDir := directory.amstrad_snap;
        SaveDialog.Filter := 'SNA Format (*.sna)|*.sna';
      end;
    SEXPORT:
      begin
        SaveDialog.Filter := 'DAT File (*.dat)|*.dat';
        SaveDialog.FileName := 'dsp_roms_dat.dat';
      end;
    SSPECTRUM:
      begin
        SaveDialog.InitialDir := directory.spectrum_tap_snap;
        if ((main_vars.tipo_maquina = 2) or (main_vars.tipo_maquina = 3)) then
          SaveDialog.Filter :=
            'SZX Format (*.SZX)|*.SZX|Z80 Format (*.Z80)|*.Z80|DSP Format (*.DSP)|*.DSP'
        else
          SaveDialog.Filter :=
            'SZX Format (*.SZX)|*.SZX|Z80 Format (*.Z80)|*.Z80|DSP Format (*.DSP)|*.DSP|SNA Format (*.SNA)|*.SNA';
      end;
    SBITMAP:
      begin
        SaveDialog.InitialDir := directory.spectrum_image;
        SaveDialog.Filter :=
          'Imagen PNG(*.PNG)|*.png|Imagen JPG(*.JPG)|*.jpg|Imagen GIF(*.GIF)|*.gif';
        SaveDialog.FileName := StringReplace(StringReplace(llamadas_maquina.caption, '/', '-',
          [rfReplaceAll, rfIgnoreCase]), ':', ' ', [rfReplaceAll, rfIgnoreCase]);
      end;
    SNES:
      begin
        SaveDialog.InitialDir := directory.nes;
        SaveDialog.Filter := 'DSP Format (*.DSP)|*.DSP';
      end;
    SSG1000:
      begin
        SaveDialog.InitialDir := directory.sg1000;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp';
      end;
    SSMS:
      begin
        SaveDialog.InitialDir := directory.sms;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp';
      end;
    SGG:
      begin
        SaveDialog.InitialDir := directory.gg;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp';
      end;
    SCHIP8:
      begin
        SaveDialog.InitialDir := directory.chip8;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp';
      end;
    SSUPERCASSETTE:
      begin
        SaveDialog.InitialDir := directory.scv;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp';
      end;
    SPV1000:
      begin
        SaveDialog.InitialDir := directory.pv1000;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp';
      end;
    SPV2000:
      begin
        SaveDialog.InitialDir := directory.pv2000;
        SaveDialog.Filter := 'DSP Format (*.dsp)|*.dsp';
      end;
  end;
  saverom := SaveDialog.execute;
  name := SaveDialog.FileName;
  index := SaveDialog.FilterIndex;
  SaveDialog.free;    }
end;

end.
