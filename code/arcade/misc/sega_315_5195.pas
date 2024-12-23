unit sega_315_5195;

interface

uses
  WinApi.Windows,
  m68000,
  nz80,
  main_engine;

type

  tsound_call = procedure(sound_latch: byte);
  topen_bus=function:byte;
  t315_5195 = class
          constructor create(cpu_m68k:cpu_m68000;isound_call:tsound_call);
    destructor free;
  public
    dirs_start: array [0 .. 7] of dword;
    dirs_end: array [0 .. 7] of dword;
    from_sound: byte;
    procedure reset;
    procedure set_map;
    function read_reg(dir: byte): byte;
    procedure write_reg(dir, valor: byte);
          procedure change_open_bus(open_bus:topen_bus);
  protected
    regs: array [0 .. $1F] of byte;
    m68k: cpu_m68000;
    iz80: cpu_z80;
    sound_call: tsound_call;
          open_bus:topen_bus;
  end;

var
  s315_5195_0: t315_5195;

implementation

constructor t315_5195.create(cpu_m68k:cpu_m68000;isound_call:tsound_call);
begin
  self.m68k := cpu_m68k;
  self.sound_call := isound_call;
  self.open_bus:=nil;
end;

procedure t315_5195.change_open_bus(open_bus:topen_bus);
begin
  self.open_bus:=open_bus;
end;

destructor t315_5195.free;
begin
end;

procedure t315_5195.reset;
var
  f: byte;
begin
  for f := 0 to $1F do
    self.regs[f] := 0;
  self.set_map;
  self.from_sound := 0;
end;

procedure t315_5195.set_map;
var
  f: byte;
const
  size: array [0 .. 3] of dword = ($10000, $20000, $80000, $200000);
begin
  for f := 0 to 7 do
  begin
    self.dirs_start[f] := self.regs[$11 + (f * 2)] shl 16;
  self.dirs_end[f]:=self.dirs_start[f]+size[self.regs[$10+(f*2)] and 3];
  end;
end;

function t315_5195.read_reg(dir: byte): byte;
var
  res: byte;
begin
  res := $FF;
  case dir of
    0, 1:
      res := self.regs[dir];
    2:
      if (self.regs[2] and 3) = 3 then
        res := 0
      else
        res := $F;
    3:
      res := self.from_sound;
    else if @self.open_bus<>nil then res:=self.open_bus;
  end;
  read_reg := res;
end;

procedure t315_5195.write_reg(dir, valor: byte);
var
  old_val: byte;
  addr: dword;
  res: word;
begin
  old_val := self.regs[dir];
  self.regs[dir] := valor;
  case dir of
    2:
      if ((old_val xor valor) and 3) <> 0 then
      begin
        if (valor and 3) = 3 then
          self.m68k.change_reset(ASSERT_LINE)
        else
          m68000_0.change_reset(CLEAR_LINE);
      end;
    3:
      self.sound_call(valor);
    4:
      if ((valor > 0) and (valor < $F)) then
        self.m68k.irq[(not(valor) and 7)] := HOLD_LINE;
    5:
      case valor of
        1:
          begin
            addr := (self.regs[$A] shl 17) or (self.regs[$B] shl 9) or (self.regs[$C] shl 1);
            res := (self.regs[0] shl 8) or self.regs[1];
            m68000_0.putword_(addr, res);
          end;
        2:
          begin
            addr := (self.regs[7] shl 17) or (self.regs[8] shl 9) or (self.regs[9] shl 1);
            res := m68000_0.getword_(addr);
            self.regs[0] := res shr 8;
            self.regs[1] := res and $FF;
          end;
      end;
    7, 8, 9, $A, $B, $C:
      ; // write latch
    $10 .. $1F:
      if old_val <> valor then
        self.set_map;
  end;
end;

end.
