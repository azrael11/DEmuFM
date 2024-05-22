unit opwolf_cchip;

interface

uses
  WinApi.Windows,
  controls_engine,
  timer_engine;

procedure opwolf_init_cchip(num: byte);
procedure opwolf_cchip_reset;
function opwolf_cchip_data_r(direccion: word): word;
procedure opwolf_cchip_data_w(direccion, valor: word);
function opwolf_cchip_status_r: word;
procedure opwolf_cchip_status_w(valor: word);
procedure opwolf_cchip_bank_w(valor: word);
procedure opwolf_timer;
procedure opwolf_timer_callback;

implementation

uses operationwolf_hw;

const
  level_data: array [0 .. 6, 0 .. $CB] of word = (($0480, $1008, $0300, $5701, $0001, $0010, $0480,
    $1008, $0300, $5701, $0001, $002B, $0780, $0009, $0300, $4A01, $0004, $0020, $0780, $1208,
    $0300, $5D01, $0004, $0030, $0780, $0209, $0300, $4C01, $0004, $0038, $0780, $0309, $0300,
    $4D01, $0004, $0048, $0980, $1108, $0300, $5A01, $C005, $0018, $0980, $0109, $0300, $4B01,
    $C005, $0028, $0B80, $020A, $0000, $6401, $8006, $0004, $0C80, $010B, $0000, $F201, $8006,
    $8002, $0B80, $020A, $0000, $6401, $8006, $0017, $0C80, $010B, $0000, $F201, $8006, $8015,
    $0B80, $020A, $0000, $6401, $0007, $0034, $0C80, $010B, $0000, $F201, $0007, $8032, $0B80,
    $020A, $0000, $6401, $8006, $803E, $0C80, $010B, $0000, $F201, $8006, $803D, $0B80, $100A,
    $0000, $6001, $0007, $0008, $0B80, $100A, $0000, $6001, $0007, $000B, $0B80, $100A, $0000,
    $6001, $0007, $001B, $0B80, $100A, $0000, $6001, $0007, $001E, $0B80, $100A, $0000, $6001,
    $8007, $0038, $0B80, $100A, $0000, $6001, $8007, $003B, $0B80, $100A, $0000, $6001, $0007,
    $8042, $0B80, $100A, $0000, $6001, $0007, $8045, $0C80, $000B, $0000, $F101, $800B, $8007,
    $0C80, $000B, $0000, $F101, $800B, $801A, $0C80, $000B, $0000, $F101, $000C, $8037, $0C80,
    $000B, $0000, $F101, $800B, $0042, $0C80, $D04B, $0000, $F301, $8006, $8009, $0C80, $D04B,
    $0000, $F301, $8006, $801C, $0C80, $D04B, $0000, $F301, $8006, $0044, $0C80, $030B, $0000,
    $F401, $0008, $0028, $0C80, $030B, $0000, $F401, $0008, $804B, $0C00, $040B, $0000, $F501,
    $0008, $8026), ($0780, $0209, $0300, $4C01, $0004, $0010, $0780, $0209, $0300, $4C01, $4004,
    $0020, $0780, $0309, $0300, $4D01, $E003, $0030, $0780, $0309, $0300, $4D01, $8003, $0040,
    $0780, $0209, $0300, $4C01, $8004, $0018, $0780, $0309, $0300, $4D01, $C003, $0028, $0B80,
    $000B, $0000, $0B02, $8009, $0029, $0B80, $0409, $0000, $0F02, $8008, $8028, $0B80, $040A,
    $0000, $3502, $000A, $8028, $0B80, $050A, $0000, $1002, $8006, $8028, $0B80, $120A, $0000,
    $3602, $0008, $004D, $0B80, $120A, $0000, $3602, $0008, $004F, $0B80, $120A, $0000, $3602,
    $0008, $0001, $0B80, $120A, $0000, $3602, $0008, $0003, $0B80, $130A, $0000, $3A02, $0007,
    $0023, $0B80, $130A, $0000, $3A02, $0007, $8025, $0B80, $130A, $0000, $3A02, $8009, $0023,
    $0B80, $130A, $0000, $3A02, $8009, $8025, $0B80, $140A, $0000, $3E02, $0007, $000D, $0B80,
    $140A, $0000, $3E02, $0007, $800F, $0B80, $000B, $0000, $0102, $0007, $804E, $0B80, $D24B,
    $0000, $0302, $0007, $000E, $0B80, $000B, $0000, $0402, $8006, $0020, $0B80, $D34B, $0000,
    $0502, $8006, $0024, $0B80, $000B, $0000, $0602, $8009, $0001, $0B80, $D44B, $0000, $0702,
    $800B, $800B, $0B80, $D54B, $0000, $0802, $800B, $000E, $0B80, $000B, $0000, $0902, $800B,
    $0010, $0B80, $000B, $0000, $0A02, $0009, $0024, $0B80, $D64B, $0000, $0C02, $000C, $8021,
    $0B80, $000B, $0000, $0D02, $000C, $0025, $0B80, $000B, $0000, $0E02, $8009, $004E, $0B80,
    $0609, $0300, $4E01, $8006, $8012, $0B80, $0609, $0300, $4E01, $0007, $8007),
    ($0480, $000B, $0300, $4501, $0001, $0018, $0480, $000B, $0300, $4501, $2001, $0030, $0780,
    $1208, $0300, $5D01, $0004, $0010, $0780, $1208, $0300, $5D01, $2004, $001C, $0780, $1208,
    $0300, $5D01, $E003, $0026, $0780, $1208, $0300, $5D01, $8003, $0034, $0780, $1208, $0300,
    $5D01, $3004, $0040, $0780, $010C, $0300, $4601, $4004, $0022, $0780, $010C, $0300, $4601,
    $6004, $0042, $0780, $000C, $0500, $7B01, $800B, $0008, $0780, $010C, $0300, $4601, $2004,
    $0008, $0000, $0000, $0000, $F001, $0000, $0000, $0000, $0000, $0000, $F001, $0000, $0000,
    $0000, $0000, $0000, $F001, $0000, $0000, $0B80, $000B, $0000, $1902, $000B, $0004, $0B80,
    $000B, $0000, $1A02, $0009, $8003, $0B80, $000B, $0000, $1902, $000B, $000C, $0B80, $000B,
    $0000, $1A02, $0009, $800B, $0B80, $000B, $0000, $1902, $000B, $001C, $0B80, $000B, $0000,
    $1A02, $0009, $801B, $0B80, $000B, $0000, $1902, $000B, $002C, $0B80, $000B, $0000, $1A02,
    $0009, $802B, $0B80, $000B, $0000, $1902, $000B, $0044, $0B80, $000B, $0000, $1A02, $0009,
    $8043, $0B80, $000B, $0000, $1902, $000B, $004C, $0B80, $000B, $0000, $1A02, $0009, $804B,
    $0B80, $020C, $0300, $4801, $A009, $0010, $0B80, $020C, $0300, $4801, $A009, $0028, $0B80,
    $020C, $0300, $4801, $A009, $0036, $0000, $0000, $0000, $F001, $0000, $0000, $0000, $0000,
    $0000, $F001, $0000, $0000, $0000, $0000, $0000, $F001, $0000, $0000, $0000, $0000, $0000,
    $F001, $0000, $0000, $0000, $0000, $0000, $F001, $0000, $0000),
    ($0480, $000B, $0300, $4501, $0001, $0018, $0480, $000B, $0300, $4501, $2001, $002B, $0780,
    $010C, $0300, $4601, $0004, $000D, $0780, $000C, $0500, $7B01, $800B, $0020, $0780, $010C,
    $0300, $4601, $2004, $0020, $0780, $010C, $0300, $4601, $8003, $0033, $0780, $010C, $0300,
    $4601, $0004, $003C, $0780, $010C, $0300, $4601, $D003, $0045, $0780, $000C, $0500, $7B01,
    $900B, $0041, $0780, $010C, $0300, $4601, $3004, $0041, $0B80, $020C, $0300, $4801, $0007,
    $0000, $0B80, $410A, $0000, $2B02, $E006, $4049, $0B80, $020C, $0300, $4801, $8007, $000B,
    $0B80, $000B, $0000, $2702, $800A, $8005, $0B80, $000B, $0000, $1E02, $0008, $800E, $0B80,
    $000B, $0000, $1F02, $8007, $0011, $0B80, $000B, $0000, $2802, $000B, $0012, $0B80, $000B,
    $0000, $2002, $0007, $8015, $0B80, $000B, $0000, $2102, $0007, $801B, $0B80, $000B, $0000,
    $2902, $800A, $001A, $0B80, $000B, $0000, $2202, $8007, $001E, $0B80, $000B, $0000, $1E02,
    $0008, $0025, $0B80, $000B, $0000, $2302, $8007, $802C, $0B80, $000B, $0000, $2802, $000B,
    $8028, $0B80, $020C, $0300, $4801, $0007, $0030, $0B80, $400A, $0000, $2E02, $4007, $002D,
    $0B80, $000B, $0000, $2702, $800A, $8035, $0B80, $020C, $0300, $4801, $8007, $0022, $0B80,
    $000B, $0000, $2402, $8007, $0047, $0B80, $000B, $0000, $2A02, $800A, $004B, $0B80, $000B,
    $0000, $2502, $0007, $804B, $0B80, $000B, $0000, $2602, $0007, $004E, $0B80, $020C, $0300,
    $4801, $0007, $8043, $0B80, $020C, $0300, $4801, $8007, $803D),
    ($0780, $0209, $0300, $4C01, $0004, $0010, $0780, $0209, $0300, $4C01, $4004, $0020, $0780,
    $0309, $0300, $4D01, $E003, $0030, $0780, $0309, $0300, $4D01, $8003, $0040, $0780, $0209,
    $0300, $4C01, $8004, $0018, $0780, $0309, $0300, $4D01, $C003, $0028, $0780, $000B, $0300,
    $5601, $8004, $0008, $0780, $000B, $0300, $5601, $8004, $0038, $0780, $000B, $0300, $5501,
    $8004, $0048, $0980, $0509, $0F00, $0F01, $4005, $4007, $0980, $0509, $0F00, $0F01, $4005,
    $4037, $0B80, $030A, $0000, $1302, $8006, $0040, $0B80, $110A, $0000, $1502, $8008, $8048,
    $0B80, $110A, $0000, $1502, $8008, $8049, $0B80, $000B, $0000, $F601, $0007, $8003, $0B80,
    $000B, $0000, $F701, $0007, $0005, $0B80, $000B, $0000, $F901, $0007, $8008, $0B80, $000B,
    $0000, $F901, $0007, $0010, $0B80, $000B, $0000, $FA01, $0007, $8013, $0B80, $000B, $0000,
    $F801, $800B, $800B, $0B80, $000B, $0000, $0002, $800B, $801A, $0B80, $000B, $0000, $F901,
    $0007, $8017, $0B80, $000B, $0000, $FA01, $0007, $001B, $0B80, $000B, $0000, $F801, $800B,
    $0013, $0B80, $000B, $0000, $4202, $800B, $0016, $0B80, $000B, $0000, $FB01, $8007, $8020,
    $0B80, $000B, $0000, $F601, $0007, $8023, $0B80, $000B, $0000, $4202, $800B, $800E, $0B80,
    $000B, $0000, $4302, $800B, $801D, $0B80, $000B, $0000, $F701, $0007, $0025, $0B80, $000B,
    $0000, $FD01, $8006, $003F, $0B80, $000B, $0000, $FE01, $0007, $0046, $0B80, $000B, $0000,
    $FF01, $8007, $8049, $0B80, $000B, $0000, $FC01, $8009, $0042),
    ($0480, $1008, $0300, $5701, $0001, $0010, $0480, $1008, $0300, $5701, $0001, $002B, $0780,
    $0009, $0300, $4A01, $0004, $0020, $0780, $1208, $0300, $5D01, $0004, $0030, $0780, $0209,
    $0300, $4C01, $0004, $0038, $0780, $0309, $0300, $4D01, $0004, $0048, $0980, $1108, $0300,
    $5A01, $C005, $0018, $0980, $0109, $0300, $4B01, $C005, $0028, $0B80, $020A, $0000, $6401,
    $8006, $0004, $0C80, $010B, $0000, $F201, $8006, $8002, $0B80, $020A, $0000, $6401, $8006,
    $0017, $0C80, $010B, $0000, $F201, $8006, $8015, $0B80, $020A, $0000, $6401, $0007, $0034,
    $0C80, $010B, $0000, $F201, $0007, $8032, $0B80, $020A, $0000, $6401, $8006, $803E, $0C80,
    $010B, $0000, $F201, $8006, $803D, $0B80, $100A, $0000, $6001, $0007, $0008, $0B80, $100A,
    $0000, $6001, $0007, $000B, $0B80, $100A, $0000, $6001, $0007, $001B, $0B80, $100A, $0000,
    $6001, $0007, $001E, $0B80, $100A, $0000, $6001, $8007, $0038, $0B80, $100A, $0000, $6001,
    $8007, $003B, $0B80, $100A, $0000, $6001, $0007, $8042, $0B80, $100A, $0000, $6001, $0007,
    $8045, $0C80, $000B, $0000, $F101, $800B, $8007, $0C80, $000B, $0000, $F101, $800B, $801A,
    $0C80, $000B, $0000, $F101, $000C, $8037, $0C80, $000B, $0000, $F101, $800B, $0042, $0C80,
    $D04B, $0000, $F301, $8006, $8009, $0C80, $D04B, $0000, $F301, $8006, $801C, $0C80, $D04B,
    $0000, $F301, $8006, $0044, $0C80, $030B, $0000, $F401, $0008, $0028, $0C80, $030B, $0000,
    $F401, $0008, $804B, $0C00, $040B, $0000, $F501, $0008, $8026),
    ($0000, $1008, $0300, $5701, $0001, $0010, $0000, $1008, $0300, $5701, $0001, $002B, $0000,
    $0000, $0000, $0000, $0000, $0000, $0700, $0009, $0300, $4A01, $0004, $0020, $0700, $1208,
    $0300, $5D01, $0004, $0030, $0700, $0209, $0300, $4C01, $0004, $0038, $0700, $0309, $0300,
    $4D01, $0004, $0048, $0900, $1108, $0300, $5A01, $C005, $0018, $0900, $0109, $0300, $4B01,
    $C005, $0028, $0000, $000B, $0000, $0000, $0018, $0000, $0000, $000B, $0000, $0000, $0018,
    $0000, $0000, $000B, $0000, $0000, $0018, $0000, $0000, $000B, $0000, $0000, $0018, $0000,
    $0000, $000B, $0000, $0000, $0018, $0000, $0000, $000B, $0000, $0000, $0018, $0000, $0000,
    $000B, $0000, $0000, $0018, $0000, $0000, $000B, $0000, $0000, $0018, $0000, $0000, $000B,
    $0000, $0000, $0018, $0000, $0980, $DB4C, $0000, $3202, $0006, $0004, $0B80, $0609, $0300,
    $4E01, $5006, $8002, $0B80, $0609, $0300, $4E01, $5006, $8003, $0B80, $0609, $0300, $4E01,
    $5006, $8004, $0B80, $0609, $0300, $4E01, $5006, $0008, $0B80, $0609, $0300, $4E01, $5006,
    $0010, $0B80, $0609, $0300, $4E01, $5006, $0012, $0B80, $0609, $0300, $4E01, $5006, $0014,
    $0B80, $0609, $0300, $4E01, $5006, $0016, $0B80, $0609, $0300, $4E01, $5006, $0018, $0B80,
    $0609, $0300, $4E01, $5006, $0020, $0B80, $0609, $0300, $4E01, $5006, $0023, $0B80, $0609,
    $0300, $4E01, $5006, $0030, $0B80, $0609, $0300, $4E01, $5006, $0038, $0B80, $0609, $0300,
    $4E01, $5006, $0040, $0B80, $0609, $0300, $4E01, $5006, $0042));

var
  cchip_ram: array [0 .. ($400 * 8) - 1] of byte;
  current_cmd, current_bank, cc_timer, cchip_last_7a, cchip_last_04, cchip_last_05, c588, c589,
    c58a: byte;
  cchip_coins, cchip_coins_for_credit, cchip_credits_for_coin: array [0 .. 1] of byte;

procedure updateDifficulty(mode: byte);
begin
  // The game is made up of 6 rounds, when you complete the
  // sixth you return to the start but with harder difficulty.
  if (mode = 0) then
  begin
    case (cchip_ram[$15] and $3) of // Dipswitch B
      3:
        begin
          cchip_ram[$2C] := $31;
          cchip_ram[$77] := $05;
          cchip_ram[$25] := $0F;
          cchip_ram[$26] := $0B;
        end;
      0:
        begin
          cchip_ram[$2C] := $20;
          cchip_ram[$77] := $06;
          cchip_ram[$25] := $07;
          cchip_ram[$26] := $03;
        end;
      1:
        begin
          cchip_ram[$2C] := $31;
          cchip_ram[$77] := $05;
          cchip_ram[$25] := $0F;
          cchip_ram[$26] := $0B;
        end;
      2:
        begin
          cchip_ram[$2C] := $3C;
          cchip_ram[$77] := $04;
          cchip_ram[$25] := $13;
          cchip_ram[$26] := $0F;
        end;
    end;
  end
  else
  begin
    case (cchip_ram[$15] and 3) of // Dipswitch B
      3:
        begin
          cchip_ram[$2C] := $46;
          cchip_ram[$77] := $05;
          cchip_ram[$25] := $11;
          cchip_ram[$26] := $0E;
        end;
      0:
        begin
          cchip_ram[$2C] := $30;
          cchip_ram[$77] := $06;
          cchip_ram[$25] := $0B;
          cchip_ram[$26] := $03;
        end;
      1:
        begin
          cchip_ram[$2C] := $3A;
          cchip_ram[$77] := $05;
          cchip_ram[$25] := $0F;
          cchip_ram[$26] := $09;
        end;
      2:
        begin
          cchip_ram[$2C] := $4C;
          cchip_ram[$77] := $04;
          cchip_ram[$25] := $19;
          cchip_ram[$26] := $11;
        end;
    end;
  end;
end;

procedure opwolf_init_cchip(num: byte);
begin
  timers.init(num, 8000000 / 60, opwolf_timer, nil, true);
  cc_timer := timers.init(num, 80000, opwolf_timer_callback, nil, false);
end;

function opwolf_cchip_data_r(direccion: word): word;
begin
  direccion := direccion shr 1;
  opwolf_cchip_data_r := cchip_ram[(current_bank * $400) + direccion];
end;

procedure opwolf_cchip_data_w(direccion, valor: word);
var
  coin_table: array [0 .. 1] of dword;
  coin_offset: array [0 .. 1] of byte;
  slot: byte;
begin
  direccion := direccion shr 1;
  cchip_ram[(current_bank * $400) + direccion] := valor and $FF;
  if (current_bank = 0) then
  begin
    // Dip switch A is written here by the 68k - precalculate the coinage values
    // Shouldn't we directly read the values from the ROM area ?
    if (direccion = $14) then
    begin
      coin_table[0] := $03FFDE;
      coin_table[1] := $03FFEE;
      coin_offset[0] := 12 - (4 * ((valor and $30) shr 4));
      coin_offset[1] := 12 - (4 * ((valor and $C0) shr 6));
      for slot := 0 to 1 do
      begin
        if (coin_table[slot] <> 0) then
        begin
          cchip_coins_for_credit[slot] :=
            rom[(coin_table[slot] + coin_offset[slot] + 0) shr 1] and $FF;
          cchip_credits_for_coin[slot] :=
            rom[(coin_table[slot] + coin_offset[slot] + 2) shr 1] and $FF;
        end;
      end;
    end;
    // Dip switch B
    if (direccion = $15) then
      updateDifficulty(0);
  end;
end;

function opwolf_cchip_status_r: word;
begin
  opwolf_cchip_status_r := $1;
end;

procedure opwolf_cchip_status_w(valor: word);
begin
  cchip_ram[$3D] := 1;
  cchip_ram[$7A] := 1;
  updateDifficulty(0);
end;

procedure opwolf_cchip_bank_w(valor: word);
begin
  current_bank := valor and $7;
end;

procedure opwolf_cchip_reset;
begin
  current_bank := 0;
  current_cmd := 0;
  cchip_last_7a := 0;
  cchip_last_04 := $FC;
  cchip_last_05 := $FF;
  c588 := 0;
  c589 := 0;
  c58a := 0;
  cchip_coins[0] := 0;
  cchip_coins[1] := 0;
  cchip_coins_for_credit[0] := 1;
  cchip_credits_for_coin[0] := 1;
  cchip_coins_for_credit[1] := 1;
  cchip_credits_for_coin[1] := 1;
end;

procedure opwolf_timer;
var
  slot: integer;
begin
  // Update input ports, these are used by both the 68k directly and by the c-chip
  cchip_ram[$4] := marcade.in0; // ioport("IN0")->read();
  cchip_ram[$5] := marcade.in1; // ioport("IN1")->read();
  // Coin slots
  if (cchip_ram[$4] <> cchip_last_04) then
  begin
    slot := -1;
    if (cchip_ram[$4] and 1) <> 0 then
      slot := 0;
    if (cchip_ram[$4] and 2) <> 0 then
      slot := 1;
    if (slot <> -1) then
    begin
      cchip_coins[slot] := cchip_coins[slot] + 1;
      if (cchip_coins[slot] >= cchip_coins_for_credit[slot]) then
      begin
        cchip_ram[$53] := cchip_ram[$53] + cchip_credits_for_coin[slot];
        cchip_ram[$51] := $55;
        cchip_ram[$52] := $55;
        cchip_coins[slot] := cchip_coins[slot] - cchip_coins_for_credit[slot];
      end;
    end;
    if (cchip_ram[$53] > 9) then
      cchip_ram[$53] := 9;
  end;
  cchip_last_04 := cchip_ram[$4];
  // Service switch
  if (cchip_ram[$5] <> cchip_last_05) then
  begin
    if ((cchip_ram[$5] and 4) = 0) then
    begin
      cchip_ram[$53] := cchip_ram[$53] + 1;
      cchip_ram[$51] := $55;
      cchip_ram[$52] := $55;
    end;
  end;
  cchip_last_05 := cchip_ram[$5];
  // Special handling for last level
  if (cchip_ram[$1B] = $6) then
  begin
    // Check for triggering final helicopter (end boss)
    if (c58a = 0) then
    begin
      if (((cchip_ram[$72] and $7F) >= 8) and (cchip_ram[$74] = 0) and (cchip_ram[$1C] = 0) and
        (cchip_ram[$1D] = 0) and (cchip_ram[$1F] = 0)) then
      begin
        cchip_ram[$30] := 1;
        cchip_ram[$74] := 1;
        c58a := 1;
      end;
    end;
    if (cchip_ram[$1A] = $90) then
      cchip_ram[$74] := 0;
    if (c58a <> 0) then
    begin
      if ((c589 = 0) and (cchip_ram[$27] = 0) and (cchip_ram[$75] = 0) and (cchip_ram[$1C] = 0) and
        (cchip_ram[$1D] = 0) and (cchip_ram[$1E] = 0) and (cchip_ram[$1F] = 0)) then
      begin
        cchip_ram[$31] := 1;
        cchip_ram[$75] := 1;
        c589 := 1;
      end;
    end;
    if (cchip_ram[$2B] = $1) then
    begin
      cchip_ram[$2B] := 0;
      if (cchip_ram[$30] = $1) then
      begin
        if (cchip_ram[$1A] <> $90) then
          cchip_ram[$1A] := cchip_ram[$1A] - 1;
      end;
      if (cchip_ram[$72] = $9) then
      begin
        if (cchip_ram[$76] <> $4) then
          cchip_ram[$76] := 3;
      end
      else
      begin
        // This timer is derived from the bootleg rather than the real board, I'm not 100% sure about it
        c588 := c588 or $80;
        cchip_ram[$72] := c588;
        c588 := c588 + 1;
        cchip_ram[$1A] := cchip_ram[$1A] - 1;
        cchip_ram[$1A] := cchip_ram[$1A] - 1;
        cchip_ram[$1A] := cchip_ram[$1A] - 1;
      end;
    end;
    // Update difficulty settings
    if (cchip_ram[$76] = 0) then
    begin
      cchip_ram[$76] := 1;
      updateDifficulty(1);
    end;
  end;
  // These variables are cleared every frame during attract mode and the intro.
  if (cchip_ram[$34] < 2) then
  begin
    updateDifficulty(0);
    cchip_ram[$76] := 0;
    cchip_ram[$75] := 0;
    cchip_ram[$74] := 0;
    cchip_ram[$72] := 0;
    cchip_ram[$71] := 0;
    cchip_ram[$70] := 0;
    cchip_ram[$66] := 0;
    cchip_ram[$2B] := 0;
    cchip_ram[$30] := 0;
    cchip_ram[$31] := 0;
    cchip_ram[$32] := 0;
    cchip_ram[$27] := 0;
    c588 := 0;
    c589 := 0;
    c58a := 0;
  end;
  // Check for level completion (all enemies destroyed)
  if ((cchip_ram[$1C] = 0) and (cchip_ram[$1D] = 0) and (cchip_ram[$1E] = 0) and
    (cchip_ram[$1F] = 0) and (cchip_ram[$20] = 0)) then
  begin
    // Special handling for end of level 6
    if (cchip_ram[$1B] = $6) then
    begin
      // Don't signal end of level until final boss is destroyed
      if (cchip_ram[$27] = $1) then
        cchip_ram[$32] := 1;
    end
    else
    begin
      // Signal end of level
      cchip_ram[$32] := 1;
    end;
  end;
  if (cchip_ram[$E] = 1) then
  begin
    cchip_ram[$E] := $FD;
    cchip_ram[$61] := $04;
  end;
  // Access level data command (address $f5 goes from 1 -> 0)
  if ((cchip_ram[$7A] = 0) and (cchip_last_7a <> 0) and (current_cmd <> $F5)) then
  begin
    // Simulate time for command to execute (exact timing unknown, this is close)
    current_cmd := $F5;
    timers.enabled(cc_timer, true);
  end;
  cchip_last_7a := cchip_ram[$7A];
  // This seems to some kind of periodic counter - results are expected
  // by the 68k when the counter reaches $a
  if (cchip_ram[$7F] = $A) then
  begin
    cchip_ram[$FE] := $F7;
    cchip_ram[$FF] := $6E;
  end;
  // These are set every frame
  cchip_ram[$64] := 0;
  cchip_ram[$66] := 0;
end;

procedure opwolf_timer_callback;
var
  i, level: byte;
begin
  timers.enabled(cc_timer, false);
  if (current_cmd = $F5) then
  begin
    level := cchip_ram[$1B];
    for i := 0 to $CB do
    begin
      cchip_ram[$200 + i * 2 + 0] := level_data[level, i] shr 8;
      cchip_ram[$200 + i * 2 + 1] := level_data[level, i] and $FF;
    end;
    // The bootleg cchip writes 0 to these locations - hard to tell what the real one writes
    cchip_ram[$0] := 0;
    cchip_ram[$76] := 0;
    cchip_ram[$75] := 0;
    cchip_ram[$74] := 0;
    cchip_ram[$72] := 0;
    cchip_ram[$71] := 0;
    cchip_ram[$70] := 0;
    cchip_ram[$66] := 0;
    cchip_ram[$2B] := 0;
    cchip_ram[$30] := 0;
    cchip_ram[$31] := 0;
    cchip_ram[$32] := 0;
    cchip_ram[$27] := 0;
    c588 := 0;
    c589 := 0;
    c58a := 0;
    cchip_ram[$1A] := 0;
    cchip_ram[$7A] := 1; // Signal command complete
  end;
  current_cmd := 0;
end;

end.
