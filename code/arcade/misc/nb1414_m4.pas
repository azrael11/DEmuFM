unit nb1414_m4;

interface

uses
  WinApi.Windows,
  gfx_engine;

type
  tnb1414_m4 = class
    constructor create(memoria: pbyte);
    destructor free;
  public
    frame: byte;
    procedure exec(var scroll_fg_x, scroll_fg_y: word; frame: byte);
    function get_internal_rom: pbyte;
    procedure reset;
  private
    rom: array [0 .. $3FFF] of byte;
    mem: pbyte;
    procedure dma(src, dst, size: word; condition: byte);
    procedure fill(dst: word; tile, pal: byte);
    procedure kozure_score_msg(dst: word; src_base: byte);
    procedure insert_coin_msg;
    procedure credit_msg;
    procedure cmd_0200(command: byte);
    procedure cmd_0600(is2p: byte);
    procedure cmd_0e00(command: byte);
  end;

var
  nb1414m4_0: tnb1414_m4;

implementation

constructor tnb1414_m4.create(memoria: pbyte);
begin
  self.mem := memoria;
end;

destructor tnb1414_m4.free;
begin
end;

procedure tnb1414_m4.reset;
begin
  self.frame := 0;
end;

function tnb1414_m4.get_internal_rom: pbyte;
begin
  get_internal_rom := @self.rom[0];
end;

procedure tnb1414_m4.exec(var scroll_fg_x, scroll_fg_y: word; frame: byte);
var
  command: word;
begin
  scroll_fg_x := self.mem[$D] or (self.mem[$E] shl 8);
  scroll_fg_y := self.mem[$B] or (self.mem[$C] shl 8);
  command := (self.mem[0] shl 8) or self.mem[1];
  self.frame := frame;
  case (command and $FF00) of
    0:
      begin
        self.insert_coin_msg;
        self.credit_msg;
      end;
    $200:
      self.cmd_0200(command and $87);
    $600:
      self.cmd_0600(command and 1);
    $E00:
      self.cmd_0e00(command and $FF);
    $8000:
      ; // Ninja Emaki, attract mode
    $FF00:
      ; // Ninja Emaki POST, presumably invalid
  end;
  fillchar(gfx[0].buffer[0], $800, 1);
end;

procedure tnb1414_m4.dma(src, dst, size: word; condition: byte);
var
  f: word;
begin
  for f := 0 to (size - 1) do
  begin
    if (f + dst) < 18 then
      continue; // avoid param overwrite
    if (condition <> 0) then
      self.mem[f + dst] := self.rom[f + src]
    else
      self.mem[f + dst] := $20;
    self.mem[f + dst + $400] := self.rom[f + size + src];
  end;
end;

procedure tnb1414_m4.insert_coin_msg;
var
  dst: word;
  fl_cond: byte;
begin
  fl_cond := self.frame and $10; // for insert coin "flickering"
  if (self.mem[$F] = 0) then
  begin
    dst := ((self.rom[$01] shl 8) or self.rom[$02]) and $3FFF;
    self.dma($3, dst, $10, fl_cond);
  end
  else
  begin
    dst := ((self.rom[$49] shl 8) or self.rom[$4A]) and $3FFF;
    self.dma($4B, dst, $18, 1);
  end;
end;

procedure tnb1414_m4.credit_msg;
var
  dst: word;
  credit_count, fl_cond: byte;
begin
  credit_count := self.mem[$F];
  fl_cond := self.frame and $10; // for insert coin "flickering"
  // credit
  dst := ((self.rom[$23] shl 8) or self.rom[$24]) and $3FFF;
  self.dma($25, dst, $10, 1);
  // credit num
  dst := ((self.rom[$45] shl 8) or self.rom[$46]) and $3FFF;
  dst := dst + 1; // m_data is 0x5e, needs to be 0x5f ...
  self.mem[dst] := credit_count + $30;
  self.mem[dst + $400] := self.rom[$48];
  if (credit_count = 1) then
  begin // ONE PLAYER ONLY
    dst := ((self.rom[$7B] shl 8) or self.rom[$7C]) and $3FFF;
    dma($7D, dst, $18, fl_cond);
  end
  else if (credit_count > 1) then
  begin // ONE OR TWO PLAYERS
    dst := ((self.rom[$AD] shl 8) or self.rom[$AE]) and $3FFF;
    dma($AF, dst, $18, fl_cond);
  end;
end;

procedure tnb1414_m4.fill(dst: word; tile, pal: byte);
var
  f: word;
begin
  for f := 0 to $3FF do
  begin
    if (f + dst) < 18 then
      continue; // avoid param overwrite
    self.mem[f + dst] := tile;
    self.mem[f + dst + $400] := pal;
  end;
end;

procedure tnb1414_m4.cmd_0200(command: byte);
var
  dst: word;
begin
  dst := ((self.rom[$330 + ((command and $F) * 2)] shl 8) or self.rom[$331 + ((command and $F) * 2)]
    ) and $3FFF;
  if (dst and $7FF) <> 0 then
    fill(0, self.rom[dst], self.rom[dst + 1])
  else
    self.dma(dst, 0, $400, 1);
end;

procedure tnb1414_m4.cmd_0600(is2p: byte);
var
  dst: word;
  f: byte;
begin
  dst := ((self.rom[$1F5] shl 8) or self.rom[$1F6]) and $3FFF;
  self.mem[dst] := (self.mem[7] and $7) + $30;
  dst := ((self.rom[$1F8] shl 8) or self.rom[$1F9]) and $3FFF;
  self.dma($1FA + (((self.mem[7] and $30) shr 4) * $18), dst, 12, 1);
  // 0x25a - 0x261 unknown meaning
  dst := ((self.rom[$262] shl 8) or self.rom[$263]) and $3FFF;
  self.dma($264 + (((self.mem[7] and $80) shr 7) * $18), dst, 12, 1);
  dst := ((self.rom[$294] shl 8) or self.rom[$295]) and $3FFF;
  self.dma($296 + (((self.mem[7] and $40) shr 6) * $18), dst, 12, 1);
  dst := ((self.rom[$2C6] shl 8) or self.rom[$2C7]) and $3FFF;
  self.mem[dst] := ((self.mem[$F] and $F0) shr 4) + $30;
  dst := ((self.rom[$2C9] shl 8) or self.rom[$2CA]) and $3FFF;
  self.mem[dst] := ((self.mem[$F] and $0F) shr 0) + $30;
  dst := ((self.rom[$2CC] shl 8) or self.rom[$2CD]) and $3FFF;
  self.mem[dst] := ((self.mem[$10] and $F0) shr 4) + $30;
  dst := ((self.rom[$2CF] shl 8) or self.rom[$2D0]) and $3FFF;
  self.mem[dst] := ((self.mem[$10] and $0F) shr 0) + $30;
  dst := ((self.rom[$2D2] shl 8) or self.rom[$2D3]) and $3FFF;
  self.mem[dst + 0] := ((self.mem[$11] and $F0) shr 4) + $30;
  self.mem[dst + 1] := (self.mem[$11] and $0F) + $30;
  // 1p / 2p string
  dst := ((self.rom[$2D6] shl 8) or self.rom[$2D7]) and $3FFF;
  dma($2D8 + (is2p * $18), dst, 12, 1);
  // system inputs
  dst := ((self.rom[$308] shl 8) or self.rom[$309]) and $3FFF;
  for f := 0 to 4 do
    self.dma($310 + (((self.mem[$04] shr (4 - f)) and 1) * 6), dst + (f * $20), $3, 1);
  // 1p / 2p inputs
  dst := ((self.rom[$30A] shl 8) or self.rom[$30B]) and $3FFF;
  for f := 0 to 6 do
    self.dma($310 + (((self.mem[$02 + is2p] shr (6 - f)) and 1) * 6), dst + (f * $20), $3, 1);
  // dips
  dst := ((self.rom[$30C] shl 8) or self.rom[$30D]) and $3FFF;
  for f := 0 to 7 do
    self.dma($310 + (((self.mem[$05] shr (7 - f)) and 1) * 6), dst + (f * $20), $3, 1);
  // dips
  dst := ((self.rom[$30E] shl 8) or self.rom[$30F]) and $3FFF;
  for f := 0 to 7 do
    self.dma($310 + (((self.mem[$06] shr (7 - f)) and 1) * 6), dst + (f * $20), $3, 1);
end;

procedure tnb1414_m4.cmd_0e00(command: byte);
var
  dst: word;
begin
  // hi-score
  dst := ((self.rom[$DF] shl 8) or self.rom[$E0]) and $3FFF;
  self.dma($E1, dst, 8, 1);
  if (command and $04) <> 0 then
  begin
    // 1p-msg
    dst := ((self.rom[$FB] shl 8) or self.rom[$FC]) and $3FFF;
    self.dma($FD, dst, 8, byte((command and 1) = 0));
    // 1p score
    dst := ((self.rom[$10D] shl 8) or self.rom[$10E]) and $3FFF;
    self.kozure_score_msg(dst, 0);
    if (command and $80) <> 0 then
    begin
      // 2p-msg
      dst := ((self.rom[$117] shl 8) or self.rom[$118]) and $3FFF;
      self.dma($0119, dst, 8, byte((command and 2) = 0));
      // 2p score
      dst := ((self.rom[$129] shl 8) or self.rom[$12A]) and $3FFF;
      self.kozure_score_msg(dst, 1);
    end;
  end
  else
  begin
    dst := ((self.rom[$133] shl 8) or self.rom[$134]) and $3FFF;
    // game over
    self.dma($135, dst, $10, byte((command and 1) = 0));
    self.insert_coin_msg;
    // TODO: either one of these two disables credit display
    if ((command and $18) = 0) then
      self.credit_msg;
  end;
end;

procedure tnb1414_m4.kozure_score_msg(dst: word; src_base: byte);
var
  f, first_digit, res: byte;
begin
  first_digit := 0;
  for f := 0 to 5 do
  begin
    res := ((self.mem[(f shr 1) + 5 + src_base * 3] shr (not(f and 1) * 4)) and $F);
    if ((first_digit <> 0) or (res <> 0)) then
    begin
      self.mem[f + dst] := res + $30;
      first_digit := 1;
    end
    else
      self.mem[f + dst] := $20;
    self.mem[f + dst + $400] := self.rom[$10F + (src_base * $1C) + f];
  end;
  self.mem[6 + dst] := $30;
  self.mem[6 + dst + $0400] := self.rom[$10F + (src_base * $1C) + 6];
  self.mem[7 + dst] := $30;
  self.mem[7 + dst + $0400] := self.rom[$10F + (src_base * $1C) + 7];
end;

end.
