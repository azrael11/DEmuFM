unit fd1089;

interface

uses
  WinApi.Windows,
  misc_functions;

const
  fd_typeA = 1;
  fd_typeB = 2;

procedure fd1089_decrypt(size: dword; srcptr, opcodesptr, dataptr: pword; m_key: pbyte;
  fd_type: byte);

implementation

const
  s_basetable_fd1089: array [0 .. $FF] of byte = ($00, $1C, $76, $6A, $5E, $42, $24, $38, $4B, $67,
    $AD, $81, $E9, $C5, $03, $2F, $45, $69, $AF, $83, $E7, $CB, $01, $2D, $02, $1E, $78, $64, $5C,
    $40, $2A, $36, $32, $2E, $44, $58, $E4, $F8, $9E, $82, $29, $05, $CF, $E3, $93, $BF, $79, $55,
    $3F, $13, $D5, $F9, $85, $A9, $63, $4F, $B8, $A4, $C2, $DE, $6E, $72, $18, $04, $0C, $10, $7A,
    $66, $FC, $E0, $86, $9A, $47, $6B, $A1, $8D, $BB, $97, $51, $7D, $17, $3B, $FD, $D1, $EB, $C7,
    $0D, $21, $A0, $BC, $DA, $C6, $50, $4C, $26, $3A, $3E, $22, $48, $54, $46, $5A, $3C, $20, $25,
    $09, $C3, $EF, $C1, $ED, $2B, $07, $6D, $41, $87, $AB, $89, $A5, $6F, $43, $1A, $06, $60, $7C,
    $62, $7E, $14, $08, $0A, $16, $70, $6C, $DC, $C0, $AA, $B6, $4D, $61, $A7, $8B, $F7, $DB, $11,
    $3D, $5B, $77, $BD, $91, $E1, $CD, $0B, $27, $80, $9C, $F6, $EA, $56, $4A, $2C, $30, $B0, $AC,
    $CA, $D6, $EE, $F2, $98, $84, $37, $1B, $DD, $F1, $95, $B9, $73, $5F, $39, $15, $DF, $F3, $9B,
    $B7, $71, $5D, $B2, $AE, $C4, $D8, $EC, $F0, $96, $8A, $A8, $B4, $D2, $CE, $D0, $CC, $A6, $BA,
    $1F, $33, $F5, $D9, $FB, $D7, $1D, $31, $57, $7B, $B1, $9D, $B3, $9F, $59, $75, $8C, $90, $FA,
    $E6, $F4, $E8, $8E, $92, $12, $0E, $68, $74, $E2, $FE, $94, $88, $65, $49, $8F, $A3, $99, $B5,
    $7F, $53, $35, $19, $D3, $FF, $C9, $E5, $23, $0F, $BE, $A2, $C8, $D4, $4E, $52, $34, $28);

  s_addr_params: array [0 .. 15, 0 .. 8] of byte = (($23, 6, 4, 5, 7, 3, 0, 1, 2),
    ($92, 2, 5, 3, 6, 7, 1, 0, 4), ($B8, 6, 7, 4, 2, 0, 5, 1, 3), ($74, 5, 3, 7, 1, 4, 6, 0, 2),
    ($CF, 7, 4, 1, 0, 6, 2, 3, 5), ($C4, 3, 1, 6, 4, 5, 0, 2, 7), ($51, 5, 7, 2, 4, 3, 1, 6, 0),
    ($14, 7, 2, 0, 6, 1, 3, 4, 5), ($7F, 3, 5, 6, 0, 2, 1, 7, 4), ($03, 2, 3, 4, 0, 6, 7, 5, 1),
    ($96, 3, 1, 7, 5, 2, 4, 6, 0), ($30, 7, 6, 2, 3, 0, 4, 5, 1), ($E2, 1, 0, 3, 7, 4, 5, 2, 6),
    ($72, 1, 6, 0, 5, 7, 2, 4, 3), ($F5, 0, 4, 1, 2, 6, 5, 7, 3), ($5B, 0, 7, 5, 3, 1, 4, 2, 6));

  // data decryption parameters for the A variant
  s_data_params_a: array [0 .. 15, 0 .. 8] of byte = (($55, 6, 5, 1, 0, 7, 4, 2, 3),
    ($94, 7, 6, 4, 2, 0, 5, 1, 3), ($8D, 1, 4, 2, 3, 0, 6, 7, 5), ($9A, 4, 3, 5, 6, 0, 2, 1, 7),
    ($72, 4, 3, 7, 0, 5, 6, 1, 2), ($FF, 1, 7, 2, 3, 6, 4, 5, 0), ($06, 6, 5, 3, 2, 4, 1, 0, 7),
    ($C5, 3, 5, 1, 4, 2, 7, 0, 6), ($EC, 4, 7, 5, 1, 6, 0, 2, 3), ($89, 3, 5, 0, 6, 1, 2, 7, 4),
    ($5C, 1, 3, 0, 7, 5, 2, 4, 6), ($3F, 7, 3, 0, 2, 4, 6, 1, 5), ($57, 6, 4, 7, 2, 1, 5, 3, 0),
    ($F7, 6, 3, 7, 0, 5, 4, 2, 1), ($3A, 6, 1, 3, 2, 7, 4, 5, 0), ($AC, 1, 6, 3, 5, 0, 7, 4, 2));

function rearrange_key(table: byte; opcode: boolean): byte;
begin
  if not(opcode) then
  begin
    table := table xor (1 shl 4);
    table := table xor (1 shl 5);
    if (BIT(not(table), 3)) then
      table := table xor (1 shl 1);
    table := BITSWAP8(table, 1, 0, 6, 4, 3, 5, 2, 7);
    if (BIT(table, 6)) then
      table := BITSWAP8(table, 7, 6, 2, 4, 5, 3, 1, 0);
  end
  else
  begin
    table := table xor (1 shl 2);
    table := table xor (1 shl 3);
    table := table xor (1 shl 4);
    if (BIT(not(table), 3)) then
      table := table xor (1 shl 5);
    if (BIT(table, 7)) then
      table := table xor (1 shl 6);
    table := BITSWAP8(table, 5, 7, 6, 4, 2, 3, 1, 0);
    if (BIT(table, 6)) then
      table := BITSWAP8(table, 7, 6, 5, 3, 2, 4, 1, 0);
  end;
  if (BIT(table, 6)) then
  begin
    if (BIT(table, 5)) then
      table := table xor (1 shl 4);
  end
  else
  begin
    if (BIT(not(table), 4)) then
      table := table xor (1 shl 5);
  end;
  rearrange_key := table;
end;

function decode_a(val, key: byte; opcode: boolean): byte;
var
  table, family: byte;
  xorval, s0, s1, s2, s3, s4, s5, s6, s7: byte;
begin
  // special case - don't decrypt
  if (key = $0) then
  begin
    decode_a := val;
    exit;
  end;
  table := rearrange_key(key, opcode);
  xorval := s_addr_params[table shr 4, 0];
  s7 := s_addr_params[table shr 4, 1];
  s6 := s_addr_params[table shr 4, 2];
  s5 := s_addr_params[table shr 4, 3];
  s4 := s_addr_params[table shr 4, 4];
  s3 := s_addr_params[table shr 4, 5];
  s2 := s_addr_params[table shr 4, 6];
  s1 := s_addr_params[table shr 4, 7];
  s0 := s_addr_params[table shr 4, 8];
  val := BITSWAP8(val, s7, s6, s5, s4, s3, s2, s1, s0) xor xorval;
  if (BIT(table, 3)) then
    val := val xor $01;
  if (BIT(table, 0)) then
    val := val xor $B1;
  if opcode then
    val := val xor $34;
  if not(opcode) then
  begin
    if (BIT(table, 6)) then
      val := val xor $01;
  end;
  val := s_basetable_fd1089[val];
  family := table and $07;
  if not(opcode) then
  begin
    if (BIT(not(table), 6) and BIT(table, 2)) then
      family := family xor 8;
    if (BIT(table, 4)) then
      family := family xor 8;
  end
  else
  begin
    if (BIT(table, 6) and BIT(table, 2)) then
      family := family xor 8;
    if (BIT(table, 5)) then
      family := family xor 8;
  end;
  if (BIT(table, 0)) then
  begin
    if (BIT(val, 0)) then
      val := val xor $C0;
    if (BIT(not(val), 6) xor BIT(val, 4)) then
      val := BITSWAP8(val, 7, 6, 5, 4, 1, 0, 2, 3);
  end
  else
  begin
    if (BIT(not(val), 6) xor BIT(val, 4)) then
      val := BITSWAP8(val, 7, 6, 5, 4, 0, 1, 3, 2);
  end;
  if (BIT(not(val), 6)) then
    val := BITSWAP8(val, 7, 6, 5, 4, 2, 3, 0, 1);
  xorval := s_data_params_a[family, 0];
  s7 := s_data_params_a[family, 1];
  s6 := s_data_params_a[family, 2];
  s5 := s_data_params_a[family, 3];
  s4 := s_data_params_a[family, 4];
  s3 := s_data_params_a[family, 5];
  s2 := s_data_params_a[family, 6];
  s1 := s_data_params_a[family, 7];
  s0 := s_data_params_a[family, 8];
  val := val xor xorval;
  val := BITSWAP8(val, s7, s6, s5, s4, s3, s2, s1, s0);
  decode_a := val;
end;

function decode_b(val, key: byte; opcode: boolean): word;
var
  table: byte;
  xorval, s7, s6, s5, s4, s3, s2, s1, s0: byte;
begin
  // special case - don't decrypt
  if (key = $0) then
  begin
    decode_b := val;
    exit;
  end;
  table := rearrange_key(key, opcode);
  xorval := s_addr_params[table shr 4, 0];
  s7 := s_addr_params[table shr 4, 1];
  s6 := s_addr_params[table shr 4, 2];
  s5 := s_addr_params[table shr 4, 3];
  s4 := s_addr_params[table shr 4, 4];
  s3 := s_addr_params[table shr 4, 5];
  s2 := s_addr_params[table shr 4, 6];
  s1 := s_addr_params[table shr 4, 7];
  s0 := s_addr_params[table shr 4, 8];
  val := BITSWAP8(val, s7, s6, s5, s4, s3, s2, s1, s0) xor xorval;
  if BIT(table, 3) then
    val := val xor $01;
  if BIT(table, 0) then
    val := val xor $B1;
  if opcode then
    val := val xor $34;
  if not(opcode) then
  begin
    if (BIT(table, 6)) then
      val := val xor $01;
  end;
  val := s_basetable_fd1089[val];
  xorval := 0;
  if not(opcode) then
  begin
    if (BIT(not(table), 6) and BIT(table, 2)) then
      xorval := xorval xor $01;
    if (BIT(table, 4)) then
      xorval := xorval xor $01;
  end
  else
  begin
    if (BIT(table, 6) and BIT(table, 2)) then
      xorval := xorval xor $01;
    if (BIT(table, 5)) then
      xorval := xorval xor $01;
  end;
  val := val xor xorval;
  if (BIT(table, 2)) then
  begin
    val := BITSWAP8(val, 7, 6, 5, 4, 1, 0, 3, 2);
    if (BIT(table, 0) xor BIT(table, 1)) then
      val := BITSWAP8(val, 7, 6, 5, 4, 0, 1, 3, 2);
  end
  else
  begin
    val := BITSWAP8(val, 7, 6, 5, 4, 3, 2, 0, 1);
    if (BIT(table, 0) xor BIT(table, 1)) then
      val := BITSWAP8(val, 7, 6, 5, 4, 1, 0, 2, 3);
  end;
  decode_b := val;
end;

function decrypt_one(addr: dword; val: word; key: pbyte; opcode: boolean; fd_type: byte): word;
var
  tbl_num: dword;
  src: word;
  ptemp: pbyte;
begin
  ptemp := key;
  if not(opcode) then
    inc(ptemp, $1000);
  // pick the translation table from bits ff022a of the address
  tbl_num := ((addr and $000002) shr 1) or ((addr and $000008) shr 2) or ((addr and $000020) shr 3)
    or ((addr and $000200) shr 6) or ((addr and $FF0000) shr 12);
  inc(ptemp, tbl_num);
  src := ((val and $0008) shr 3) or ((val and $0040) shr 5) or ((val and $FC00) shr 8);
  if fd_type = fd_typeA then
    src := decode_a(src, ptemp^, opcode)
  else
    src := decode_b(src, ptemp^, opcode);
  src := ((src and $01) shl 3) or ((src and $02) shl 5) or ((src and $FC) shl 8);
  decrypt_one := (val and not($FC48)) or src;
end;

procedure fd1089_decrypt(size: dword; srcptr, opcodesptr, dataptr: pword; m_key: pbyte;
  fd_type: byte);
var
  offset, half_size: dword;
  src: word;
  ptemp, ptemp2, ptemp3: pword;
begin
  ptemp := srcptr;
  ptemp2 := opcodesptr;
  ptemp3 := dataptr;
  half_size := (size shr 1) - 1;
  for offset := 0 to half_size do
  begin
    src := ptemp^;
    inc(ptemp);
    ptemp2^ := decrypt_one(offset shl 1, src, m_key, true, fd_type);
    inc(ptemp2);
    ptemp3^ := decrypt_one(offset shl 1, src, m_key, false, fd_type);
    inc(ptemp3);
  end;
end;

end.
