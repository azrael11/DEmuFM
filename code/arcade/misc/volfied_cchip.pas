unit volfied_cchip;

interface

uses controls_engine,
  timer_engine;

const
  palette_data: array [1 .. $11, 0 .. $4F] of word = (($0000, $DE7B, $DE03, $5E01, $5E02, $C07B,
    $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $104A, $CE41,
    $8C39, $5252, $D662, $4A31, $0000, $1E00, $1000, $9E01, $1E02, $DE02, $0000, $0000, $0000,
    $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $104A, $CE41, $8C39, $5252, $D662, $4A31, $0000, $1E00, $1000,
    $9E01, $1E02, $DE02, $0000, $0000, $0000, $0000, $D62A, $1002, $CE01, $5A3B, $DE7B, $4A31,
    $0000, $1E00, $1000, $9E01, $1E02, $DE02, $0038, $0E38, $0000),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $4008, $0029, $C641, $4C52, $5473, $DE7B, $1863, $524A, $CE39,
    $0821, $9C01, $1200, $8001, $C002, $CE39, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $0000, $4A29, $CE39,
    $DE7B, $4001, $4002, $C003, $9E01, $1E00, $0078, $0E00, $5401, $0040, $DE03, $1600, $0000,
    $4208, $0C39, $D061, $547A, $1472, $DE7B, $DE7B, $187B, $947A, $0821, $9E79, $1040, $8079,
    $C07A, $0000),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $C038, $4049, $C059, $406A, $C07A, $4208, $0821, $8C31, $1042,
    $9C73, $1E03, $1A02, $0C00, $1860, $1E78, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $0000, $4A29, $CE39,
    $DE7B, $4001, $4002, $C003, $9E01, $1E00, $0078, $0E00, $5401, $0040, $DE03, $1600, $0000,
    $C001, $4002, $8002, $C002, $C002, $0001, $C001, $9201, $C002, $C003, $0003, $8002, $4001,
    $C002, $4003),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $1042, $CE39, $8C31, $524A, $D65A, $4A29, $0000, $1E00, $1000,
    $8C21, $CE29, $0039, $0038, $0E38, $0038, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $1002, $CE01, $8C01, $5202, $D602, $4A01, $0000, $1E00, $1000, $0000, $0000, $0000, $0000,
    $0000, $0000),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $1200, $1600, $1A00, $9E01, $8021, $C029, $0032, $803A, $4208,
    $0821, $1042, $D65A, $9C73, $DE03, $5C02, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $5202, $D602, $5A03, $DE03, $8021, $C029, $0032, $803A, $4208, $0821, $1042, $D65A, $9C73,
    $DE03, $5C02),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $9E52, $9028, $9428, $9828, $9E28, $4208, $DE7B, $DE03, $9C02,
    $C03A, $0063, $586B, $9252, $8A31, $5E31, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0263,
    $9E52, $8058, $0879, $8C79, $107A, $4208, $DE7B, $DE01, $1E01, $C03A, $0063, $586B, $9252,
    $8A31, $527A),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $C038, $4049, $C059, $406A, $C07A, $4208, $0821, $8C31, $1042,
    $9C73, $1E03, $1A02, $0C00, $1860, $1E78, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $0000, $4A29, $CE39,
    $DE7B, $4001, $4002, $C003, $9E01, $1E00, $0078, $0E00, $5401, $0040, $DE03, $1600, $0000,
    $8001, $0002, $8002, $0003, $8003, $4208, $0821, $8C31, $1042, $9C73, $1E00, $5C02, $0C00,
    $1860, $1E78),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $1042, $CE39, $8C31, $524A, $D65A, $4A29, $0000, $1E00, $1000,
    $9E01, $5E02, $5E03, $0038, $0E38, $0000, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $5202, $1002, $CE19, $9432, $1843, $8C11, $0000, $1E00, $1000, $9E01, $5E02, $5E03, $0038,
    $0E38, $0000),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $1048, $1250, $1458, $1660, $D418, $9E02, $C203, $4208, $4A29,
    $8C31, $1042, $1E78, $166B, $0C38, $1868, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $1600, $1A21, $5C29, $DE39, $D418, $9E02, $C203, $4208, $4A29, $8C31, $1042, $1E42, $186B,
    $9210, $9E31),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $0000, $0038, $4A29, $CE39, $9452, $9218, $DE7B, $C001, $C003,
    $DE03, $1403, $CC01, $4A01, $0668, $4672, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $0000, $0038, $4A29, $5401, $9C02, $9218, $DE7B, $0003, $C003, $5E02, $DE01, $5201, $D200,
    $0668, $4672),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0050, $8001, $C001, $0002, $C002, $D043, $9C73, $524A, $CE39, $8C31,
    $4208, $DE03, $9C02, $1E60, $1A00, $1000, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $8C01, $CE01, $1002, $D62A, $DE4B, $9C73, $5202, $CE01, $8C01, $4208, $DE03, $9C02, $1E60,
    $1A00, $1000),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $0000, $0038, $4A29, $CE39, $9452, $9218, $9E52, $C001, $C003,
    $1E00, $1400, $0C00, $4A01, $0668, $4672, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $0000, $0038, $4A29, $CE39, $9452, $9218, $DE7B, $C001, $C003, $DE03, $1403, $CC01, $4A01,
    $0668, $4672),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0078, $4208, $1052, $9462, $1873, $5A73, $DE7B, $1863, $524A, $CE39,
    $0821, $1600, $1000, $D201, $DE03, $0A42, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0078, $4208, $1052, $9462,
    $1873, $5A73, $DE7B, $1863, $524A, $CE39, $0821, $1600, $1000, $D201, $DE03, $0A42, $0000,
    $4208, $5029, $9431, $D839, $5A4A, $9E52, $5862, $DE4B, $8E39, $0821, $1600, $1000, $D201,
    $1E00, $0A42),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $0E01, $5001, $9201, $D401, $1602, $1200, $1600, $4208, $0821,
    $8C31, $1042, $5A6B, $8001, $0002, $9A02, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $0000, $4A29, $CE39,
    $DE7B, $4001, $4002, $C003, $9E01, $1E00, $0078, $0E00, $5401, $0040, $DE03, $1600, $0000,
    $8A21, $0A32, $4C3A, $8E4A, $504B, $D203, $C003, $4208, $0821, $8C31, $1042, $5A6B, $8001,
    $0002, $545B),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $C038, $4049, $C059, $406A, $C07A, $0000, $0821, $9C31, $1042,
    $9C73, $1E02, $1A02, $0C00, $4002, $C001, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $DE7B, $1E00, $C003,
    $1042, $DE03, $0000, $D65A, $CE39, $8C31, $4A29, $0078, $C07B, $1E02, $1E78, $C003, $0000,
    $CE00, $5201, $D601, $5A02, $DE02, $0000, $0821, $8C31, $1042, $9C73, $1E03, $1A02, $0C00,
    $9E01, $0E00),

    ($0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000, $DE7B, $0058, $4079, $407A, $407B, $D47B,
    $0000, $0000, $0000, $0000, $0601, $8A09, $0E1A, $922A, $163B, $DE7B, $D65A, $CE39, $0821,
    $0000, $0C00, $5208, $1A02, $9E03, $CE39, $0000, $DE7B, $DE03, $5E01, $5E02, $C07B, $0000,
    $DE7B, $0058, $4079, $407A, $407B, $D47B, $0000, $0000, $0000, $0000, $1400, $8002, $0068,
    $0000, $5E01, $5E02, $1E03, $DE03, $CE39, $CE39, $CE39, $CE39, $CE39, $CE39, $CE39, $0078,
    $4208, $1052, $9462, $1873, $5A73, $DE7B, $1863, $524A, $CE39, $0821, $1600, $1000, $D201,
    $DE03, $0A42),

    ($0000, $4A29, $8C31, $CE39, $1042, $524A, $9452, $D65A, $1863, $0000, $DE39, $DE7B, $C001,
    $8002, $1800, $1E00, $0000, $DE7B, $1E00, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $1E00, $1E00, $1E00, $1E00, $1E00, $1E00, $1E00,
    $1E00, $1E00, $1E00, $1E00, $1E00, $1E00, $1E00, $1E00, $1E00, $DE03, $DE03, $DE03, $DE03,
    $DE03, $DE03, $DE03, $DE03, $DE03, $DE03, $DE03, $DE03, $DE03, $DE03, $DE03, $DE03, $DE03,
    $0E00, $9E4A, $0000, $1042, $DE7B, $9452, $4A29, $CE39, $1C02, $0000, $0000, $0000, $0000,
    $0000, $0000));

var
  cchip_ram: array [0 .. ($400 * 8) - 1] of byte;
  current_cmd, current_flag, current_bank, cc_port, cc_timer: byte;

procedure volfied_init_cchip(num: byte);
procedure volfied_cchip_reset;
function volfied_cchip_ram_r(direccion: word): word;
procedure volfied_cchip_ram_w(direccion, valor: word);
function volfied_cchip_ctrl_r: word;
procedure volfied_cchip_ctrl_w(valor: word);
procedure volfied_cchip_bank_w(valor: word);
procedure volfied_timer;

implementation

procedure volfied_init_cchip(num: byte);
begin
  cc_timer := timers.init(num, 1, volfied_timer, nil, false);
end;

function volfied_cchip_ram_r(direccion: word): word;
begin
  direccion := direccion shr 1;
  // Check for input ports */
  if (current_bank = 0) then
  begin
    case direccion of
      $03:
        volfied_cchip_ram_r := marcade.in0; // ioport("F00007")->read();    /* STARTn + SERVICE1 */
      $04:
        volfied_cchip_ram_r := marcade.in1; // return ioport("F00009")->read();    /* COINn */
      $05:
        volfied_cchip_ram_r := marcade.in2;
        // return ioport("F0000B")->read();    /* Player controls + TILT */
      $06:
        volfied_cchip_ram_r := $FF;
        // return ioport("F0000D")->read();    /* Player controls (cocktail) */
      $08:
        volfied_cchip_ram_r := cc_port;
      $3FE:
        volfied_cchip_ram_r := current_cmd; // Current command status
      $3FF:
        volfied_cchip_ram_r := 2 * current_flag; // fixes freeze after shield runs out
    else
      volfied_cchip_ram_r := cchip_ram[(current_bank * $400) + direccion];
    end;
    exit;
  end;
  // Unknown
  if ((current_bank = 2) and (direccion = $005)) then
  begin
    { Not fully understood - Game writes:
      0001a0c2:  volfied c write 0005 00aa
      0001a0ca:  volfied c write 0006 0055
      0001a0d2:  volfied c write 0004 0065
      Then expects $7c to replace the $aa some time later. }
    volfied_cchip_ram_r := $7C; // makes worm in round 1 appear
    exit;
  end;
  // Unknown - some kind of timer
  volfied_cchip_ram_r := cchip_ram[(current_bank * $400) + direccion];
end;

procedure volfied_cchip_ram_w(direccion, valor: word);
begin
  direccion := direccion shr 1;
  cchip_ram[(current_bank * $400) + direccion] := valor;
  if (current_bank = 0) then
  begin
    if (direccion = $008) then
      cc_port := valor;
    if (direccion = $3FE) then
    begin
      { *******************
        (This table stored in ROM at $146a8)
        (Level number stored at $100198.b, from $100118.b, from $100098.b)
        (Level number at $b34 stored to $100098.b)
        round 01 => data $0A
        round 02 => data $01
        round 03 => data $03
        round 04 => data $08
        round 05 => data $05
        round 06 => data $04
        round 07 => data $0B
        round 08 => data $09
        round 09 => data $07
        round 10 => data $06
        round 11 => data $0E
        round 12 => data $0D
        round 13 => data $02
        round 14 => data $0C
        round 15 => data $0F
        round 16 => data $10
        final    => data $11
        ******************** }
      current_cmd := valor;
      // Palette request cmd - verified to take around 122242 68000 cycles to complete
      if ((current_cmd >= $1) and (current_cmd < $12)) then
      begin
        timers.timer[cc_timer].time_final := 122242;
        timers.enabled(cc_timer, true);
      end
      // Unknown cmd - verified to take around 105500 68000 cycles to complete
      else if ((current_cmd >= $81) and (current_cmd < $92)) then
      begin
        timers.timer[cc_timer].time_final := 105500;
        timers.enabled(cc_timer, true);
      end
      else
      begin
        // logerror("unknown cchip cmd %02x\n", data);
        current_cmd := 0;
      end;
    end;
    // Some kind of timer command
    if (direccion = $3FF) then
      current_flag := valor;
  end;
end;

function volfied_cchip_ctrl_r: word;
begin
  volfied_cchip_ctrl_r := $1;
end;

procedure volfied_cchip_ctrl_w(valor: word);
begin
end;

procedure volfied_cchip_reset;
begin
  current_bank := 0;
  current_flag := 0;
  cc_port := 0;
  current_cmd := 0;
end;

procedure volfied_cchip_bank_w(valor: word);
begin
  current_bank := valor and $7;
end;

procedure volfied_timer;
var
  i: byte;
begin
  timers.enabled(cc_timer, false);
  // Palette commands - palette data written to bank 0: $10 - $af
  if ((current_cmd >= $1) and (current_cmd < $12)) then
  begin
    for i := 0 to $4F do
    begin
      cchip_ram[$10 + i * 2 + 0] := palette_data[current_cmd, i] shr 8;
      cchip_ram[$10 + i * 2 + 1] := palette_data[current_cmd, i] and $FF;
    end;
  end;
  // Unknown command - result written to bank 0: $23
  if ((current_cmd >= $81) and (current_cmd < $92)) then
  begin
    case current_cmd of
      $81:
        cchip_ram[$23] := $F;
      $82:
        cchip_ram[$23] := $1;
      $83:
        cchip_ram[$23] := $6;
      $84:
        cchip_ram[$23] := $F;
      $85:
        cchip_ram[$23] := $9;
      $86:
        cchip_ram[$23] := $6;
      $87:
        cchip_ram[$23] := $6;
      $88:
        cchip_ram[$23] := $F;
      $89:
        cchip_ram[$23] := $8;
      $8A:
        cchip_ram[$23] := $1;
      $8B:
        cchip_ram[$23] := $A;
      $8C:
        cchip_ram[$23] := $1;
      $8D:
        cchip_ram[$23] := $1;
      $8E:
        cchip_ram[$23] := $8;
      $8F:
        cchip_ram[$23] := $6;
      $90:
        cchip_ram[$23] := $A;
      $91:
        cchip_ram[$23] := $0;
    end;
  end;
  current_cmd := 0;
end;

end.
