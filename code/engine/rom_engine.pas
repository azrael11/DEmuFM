unit Rom_engine;

interface

uses
  WinApi.Windows,
  System.SysUtils,
  FMX.Dialogs,
  file_engine,
  main_engine,
  System.UITypes;

type
  tipo_roms = record
    n: string;
    l: dword;
    p: dword;
    crc: dword;
  end;

  ptipo_roms = ^tipo_roms;

function carga_rom_zip(nombre_zip, nombre_rom: string; donde: pbyte; longitud, crc: integer;
  warning: boolean): boolean;
function carga_rom_zip_crc(nombre_zip, nombre_rom: string; donde: pointer; longitud: integer;
  crc: dword): boolean;
function roms_load(place: pbyte; const ctipo_roms: array of tipo_roms; parent: boolean = false;
  nombre: string = ''): boolean;
function roms_load16b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
function roms_load16w(place: pword; const ctipo_roms: array of tipo_roms): boolean;
function roms_load32b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
function roms_load32b_b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
function roms_load32dw(place: pdword; const ctipo_roms: array of tipo_roms): boolean;
function roms_load64b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
function roms_load_swap_word(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
function roms_load64b_b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;

implementation

uses
  init_games, uDataModule;

function carga_rom_zip(nombre_zip, nombre_rom: string; donde: pbyte; longitud, crc: integer;
  warning: boolean): boolean;
var
  long_rom: integer;
  crc_rom: dword;
begin
  carga_rom_zip := false;
  // Cargar el archivo
  if not(load_file_from_zip(nombre_zip, nombre_rom, donde, long_rom, crc_rom, warning)) then
    exit;
  // Es la longitud correcta?
  if ((longitud <> long_rom) and warning) then
  begin
    MessageDlg('ROM file size error: ' + '"' + nombre_rom + '"', TMSgDlgType.mtError, [TMSgDlgBtn.mbOK], 0);
    exit;
  end;
  // Tiene el CRC correcto?
  if ((crc_rom <> crc) and (crc <> 0) and warning and main_vars.show_crc_error) then
    MessageDlg('CRC Error file: ' + '"' + nombre_rom + '".' + chr(10) + chr(13) + 'Have: 0x' +
      inttohex(crc_rom, 8) + ' must be: 0x' + inttohex(crc, 8), TMSgDlgType.mtError, [TMSgDlgBtn.mbOK], 0);
  carga_rom_zip := true;
end;

function carga_rom_zip_crc(nombre_zip, nombre_rom: string; donde: pointer; longitud: integer;
  crc: dword): boolean;
var
  long_rom: integer;
begin
  carga_rom_zip_crc := false;
  if not(load_file_from_zip_crc(nombre_zip, donde, long_rom, crc)) then
    exit;
  // Es la longitud correcta?
  if (longitud <> long_rom) then
  begin
    MessageDlg('ROM file size error: ' + '"' + nombre_rom + '"', TMSgDlgType.mtError, [TMSgDlgBtn.mbOK], 0);
    exit;
  end;
  carga_rom_zip_crc := true;
end;

function roms_load(place: pbyte; const ctipo_roms: array of tipo_roms; parent: boolean = false;
  nombre: string = ''): boolean;
// Εδώ είναι που φορτώνει τα roms
var
  ptemp: pbyte;
  f, roms_size: word;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p);
      // dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
      begin
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, ptemp, ctipo_roms[f].l,
          integer(ctipo_roms[f].crc))) then
        begin
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, ptemp, ctipo_roms[f].l, ctipo_roms[f].crc,
            true)) then
            exit;
        end;
      end;
    end;
  end;
  Result := true;
end;

function roms_load16b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
var
  ptemp, ptemp2, mem_temp: pbyte;
  h: dword;
  nombre_zip, dir: string;
  f, roms_size: word;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      // Creo un puntero byte
      getmem(mem_temp, ctipo_roms[f].l);
      // Cargo los datos como byte
      // dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
            ctipo_roms[f].crc, true)) then
            exit;
      // Los convierto a word
      ptemp2 := mem_temp;
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p);
      for h := 0 to (ctipo_roms[f].l - 1) do
      begin
        ptemp^ := ptemp2^;
        inc(ptemp2);
        inc(ptemp, 2);
      end;
      freemem(mem_temp);
    end;
  end;
  roms_load16b := true;
end;

function roms_load16w(place: pword; const ctipo_roms: array of tipo_roms): boolean;
var
  ptemp: pword;
  ptemp2, mem_temp: pbyte;
  h: dword;
  alto: boolean;
  f, roms_size, valor: word;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      // Cargo los datos en tipo byte
      getmem(mem_temp, ctipo_roms[f].l);
      // dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
            ctipo_roms[f].crc, true)) then
            exit;
      // Y ahora los pongo como word
      ptemp2 := mem_temp;
      ptemp := place;
      alto := (ctipo_roms[f].p and $1) <> 0;
      inc(ptemp, ctipo_roms[f].p shr 1);
      for h := 0 to (ctipo_roms[f].l - 1) do
      begin
        if not(alto) then
          valor := (ptemp2^ shl 8) or (ptemp^ and $FF)
        else
          valor := ptemp2^ or (ptemp^ and $FF00);
        ptemp^ := valor;
        inc(ptemp2);
        inc(ptemp);
      end;
      freemem(mem_temp);
    end;
  end;
  roms_load16w := true;
end;

function roms_load32b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
var
  ptemp, ptemp2, mem_temp: pbyte;
  f, h: dword;
  roms_size: word;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      getmem(mem_temp, ctipo_roms[f].l);
      // dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
            ctipo_roms[f].crc, true)) then
            exit;
      ptemp2 := mem_temp;
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p);
      for h := 0 to ((ctipo_roms[f].l shr 1) - 1) do
      begin
        ptemp^ := ptemp2^;
        inc(ptemp);
        inc(ptemp2);
        ptemp^ := ptemp2^;
        inc(ptemp2);
        inc(ptemp, 3);
      end;
      freemem(mem_temp);
    end;
  end;
  roms_load32b := true;
end;

function roms_load32b_b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
var
  roms_size, f: word;
  ptemp, ptemp2, mem_temp: pbyte;
  h: dword;
  nombre_zip, dir: string;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      getmem(mem_temp, ctipo_roms[f].l);
      // dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
            ctipo_roms[f].crc, true)) then
            exit;
      ptemp2 := mem_temp;
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p);
      for h := 0 to (ctipo_roms[f].l - 1) do
      begin
        ptemp^ := ptemp2^;
        inc(ptemp, 4);
        inc(ptemp2);
      end;
      freemem(mem_temp);
    end;
  end;
  roms_load32b_b := true;
end;

function roms_load32dw(place: pdword; const ctipo_roms: array of tipo_roms): boolean;
var
  ptemp: pdword;
  ptemp2, mem_temp: pbyte;
  h, valor: dword;
  f, roms_size: word;
  nombre_zip, dir: string;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      // Cargo los datos en tipo byte
      getmem(mem_temp, ctipo_roms[f].l);
      // dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
            ctipo_roms[f].crc, true)) then
            exit;
      // Y ahora los pongo como word
      ptemp2 := mem_temp;
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p shr 2);
      for h := 0 to (ctipo_roms[f].l - 1) do
      begin
        valor := ptemp^;
        case (ctipo_roms[f].p and $3) of
          0:
            ptemp^ := ptemp2^ or (valor and $FFFFFF00);
          1:
            ptemp^ := (ptemp2^ shl 8) or (valor and $FFFF00FF);
          2:
            ptemp^ := (ptemp2^ shl 16) or (valor and $FF00FFFF);
          3:
            ptemp^ := (ptemp2^ shl 24) or (valor and $00FFFFFF);
        end;
        inc(ptemp2);
        inc(ptemp);
      end;
      freemem(mem_temp);
    end;
  end;
  roms_load32dw := true;
end;

function roms_load64b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
var
  roms_size, f: word;
  ptemp, ptemp2, mem_temp: pbyte;
  h: dword;
  nombre_zip, dir: string;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      getmem(mem_temp, ctipo_roms[f].l);
      dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
            ctipo_roms[f].crc, true)) then
            exit;
      ptemp2 := mem_temp;
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p);
      for h := 0 to ((ctipo_roms[f].l shr 1) - 1) do
      begin
        ptemp^ := ptemp2^;
        inc(ptemp);
        inc(ptemp2);
        ptemp^ := ptemp2^;
        inc(ptemp2);
        inc(ptemp, 7);
      end;
      freemem(mem_temp);
    end;
  end;
  roms_load64b := true;
end;

function roms_load_swap_word(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
var
  v1, v2: byte;
  ptemp, ptemp2: pbyte;
  roms_size, f, h: dword;
  nombre_zip, dir: string;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p);
      dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, ptemp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, ptemp, ctipo_roms[f].l, ctipo_roms[f].crc,
            true)) then
            exit;
      ptemp2 := ptemp;
      for h := 0 to (ctipo_roms[f].l div 2) - 1 do
      begin
        v1 := ptemp2^;
        inc(ptemp2);
        v2 := ptemp2^;
        dec(ptemp2);
        ptemp2^ := v2;
        inc(ptemp2);
        ptemp2^ := v1;
        inc(ptemp2);
      end;
    end;
  end;
  roms_load_swap_word := true;
end;

function roms_load64b_b(place: pbyte; const ctipo_roms: array of tipo_roms): boolean;
var
  roms_size, f: word;
  ptemp, ptemp2, mem_temp: pbyte;
  h: dword;
  nombre_zip, dir: string;
  zip_path, zip_rom: string;
begin
  Result := false;

  dm.tArcade.Locate('exe_num', main_vars.machine_type.ToString, []);

  if dm.tArcade.FieldByName('rom_path').AsString <> '' then
  begin
    zip_path := ExtractFilePath(dm.tArcade.FieldByName('rom_path').AsString);
    zip_rom := ExtractFileName(dm.tArcade.FieldByName('rom_path').AsString);
    roms_size := sizeof(ctipo_roms) div sizeof(tipo_roms);
    for f := 0 to (roms_size - 1) do
    begin
      getmem(mem_temp, ctipo_roms[f].l);
      dir := directory.arcade_list_roms[find_rom_multiple_dirs(nombre_zip)];
      if ctipo_roms[f].crc <> 0 then
        if not(carga_rom_zip_crc(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
          ctipo_roms[f].crc)) then
          if not(carga_rom_zip(zip_path + zip_rom, ctipo_roms[f].n, mem_temp, ctipo_roms[f].l,
            ctipo_roms[f].crc, true)) then
            exit;
      ptemp2 := mem_temp;
      ptemp := place;
      inc(ptemp, ctipo_roms[f].p);
      for h := 0 to (ctipo_roms[f].l - 1) do
      begin
        ptemp^ := ptemp2^;
        inc(ptemp2);
        inc(ptemp, 8);
      end;
      freemem(mem_temp);
    end;
  end;
  roms_load64b_b := true;
end;

end.
