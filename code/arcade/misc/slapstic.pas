unit slapstic;

interface

uses
  m68000,
  WinApi.Windows;

type
  mask_value = record
    mask, value: integer;
  end;

  slapstic_data = record
    bankstart: integer;
    bank: array [0 .. 3] of integer;
    alt1, alt2, alt3, alt4: mask_value;
    altshift: integer;
    bit1, bit2c0, bit2s0, bit2c1, bit2s1, bit3: mask_value;
    add1, add2, addplus1, addplus2, add3: mask_value;
  end;

  slapstic_type = class
    constructor create(num: byte; is_68k: boolean);
    destructor free;
  public
    current_bank: byte;
    procedure reset;
    function slapstic_tweak(offset: word): byte;
  private
    state: byte;
    access_68k: boolean;
    alt_bank, bit_bank, add_bank, bit_xor: byte;
    slapstic: slapstic_data;
    function alt2_kludge(offset: word): byte;
  end;

var
  slapstic_0: slapstic_type;

implementation

const
  DISABLED = 0;
  ENABLED = 1;
  ALTERNATE1 = 2;
  ALTERNATE2 = 3;
  ALTERNATE3 = 4;
  BITWISE1 = 5;
  BITWISE2 = 6;
  BITWISE3 = 7;
  ADDITIVE1 = 8;
  ADDITIVE2 = 9;
  ADDITIVE3 = 10;

  UNKNOWN = $FFFF;

  slapstic101: slapstic_data = (
    // basic banking */
    bankstart: 3; // starting bank */
    bank: ($0080, $0090, $00A0, $00B0); // bank select values */
    // alternate banking */
    alt1: (mask: $007F; value: UNKNOWN); // 1st mask/value in sequence */
    alt2: (mask: $1FFF; value: $1DFF); // 2nd mask/value in sequence */
    alt3: (mask: $1FFC; value: $1B5C); // 3rd mask/value in sequence */
    alt4: (mask: $1FCF; value: $0080); // 4th mask/value in sequence */
    altshift: 0; // shift to get bank from 3rd */
    // bitwise banking */
    bit1: (mask: $1FF0; value: $1540); // 1st mask/value in sequence */
    bit2c0: (mask: $1FF3; value: $1540); // clear bit 0 value */
    bit2s0: (mask: $1FF3; value: $1541); // set bit 0 value */
    bit2c1: (mask: $1FF3; value: $1542); // clear bit 1 value */
    bit2s1: (mask: $1FF3; value: $1543); // set bit 1 value */
    bit3: (mask: $1FF8; value: $1550); // final mask/value in sequence */
    // additive banking */
    add1: (mask: UNKNOWN; value: UNKNOWN); add2: (mask: UNKNOWN; value: UNKNOWN);
    addplus1: (mask: UNKNOWN; value: UNKNOWN); addplus2: (mask: UNKNOWN; value: UNKNOWN);
    add3: (mask: UNKNOWN; value: UNKNOWN));

  slapstic103: slapstic_data = (
    // basic banking */
    bankstart: 3; // starting bank */
    bank: ($0040, $0050, $0060, $0070); // bank select values */
    // alternate banking */
    alt1: (mask: $007F; value: $002D); // 1st mask/value in sequence */
    alt2: (mask: $3FFF; value: $3D14); // 2nd mask/value in sequence */
    alt3: (mask: $3FFC; value: $3D24); // 3rd mask/value in sequence */
    alt4: (mask: $3FCF; value: $0040); // 4th mask/value in sequence */
    altshift: 0; // shift to get bank from 3rd */
    // bitwise banking */
    bit1: (mask: $3FF0; value: $34C0); // 1st mask/value in sequence */
    bit2c0: (mask: $3FF3; value: $34C0); // clear bit 0 value */
    bit2s0: (mask: $3FF3; value: $34C1); // set bit 0 value */
    bit2c1: (mask: $3FF3; value: $34C2); // clear bit 1 value */
    bit2s1: (mask: $3FF3; value: $34C3); // set bit 1 value */
    bit3: (mask: $3FF8; value: $34D0); // final mask/value in sequence */
    // additive banking */
    add1: (mask: UNKNOWN; value: UNKNOWN); add2: (mask: UNKNOWN; value: UNKNOWN);
    addplus1: (mask: UNKNOWN; value: UNKNOWN); addplus2: (mask: UNKNOWN; value: UNKNOWN);
    add3: (mask: UNKNOWN; value: UNKNOWN));

  slapstic104: slapstic_data = (
    // basic banking */
    bankstart: 3; // starting bank */
    bank: ($0020, $0028, $0030, $0038); // bank select values */
    // alternate banking */
    alt1: (mask: $007F; value: $0069); // 1st mask/value in sequence */
    alt2: (mask: $3FFF; value: $3735); // 2nd mask/value in sequence */
    alt3: (mask: $3FFC; value: $3764); // 3rd mask/value in sequence */
    alt4: (mask: $3FE7; value: $0020); // 4th mask/value in sequence */
    altshift: 0; // shift to get bank from 3rd */
    // bitwise banking */
    bit1: (mask: $3FF0; value: $3D90); // 1st mask/value in sequence */
    bit2c0: (mask: $3FF3; value: $3D90); // clear bit 0 value */
    bit2s0: (mask: $3FF3; value: $3D91); // set bit 0 value */
    bit2c1: (mask: $3FF3; value: $3D92); // clear bit 1 value */
    bit2s1: (mask: $3FF3; value: $3D93); // set bit 1 value */
    bit3: (mask: $3FF8; value: $3DA0); // final mask/value in sequence */
    // additive banking */
    add1: (mask: UNKNOWN; value: UNKNOWN); add2: (mask: UNKNOWN; value: UNKNOWN);
    addplus1: (mask: UNKNOWN; value: UNKNOWN); addplus2: (mask: UNKNOWN; value: UNKNOWN);
    add3: (mask: UNKNOWN; value: UNKNOWN));

  slapstic105: slapstic_data = (
    // basic banking */
    bankstart: 3; // starting bank */
    bank: ($0010, $0014, $0018, $001C); // bank select values */
    // alternate banking */
    alt1: (mask: $007F; value: $003D); // 1st mask/value in sequence */
    alt2: (mask: $3FFF; value: $0092); // 2nd mask/value in sequence */
    alt3: (mask: $3FFC; value: $00A4); // 3rd mask/value in sequence */
    alt4: (mask: $3FF3; value: $0010); // 4th mask/value in sequence */
    altshift: 0; // shift to get bank from 3rd */
    // bitwise banking */
    bit1: (mask: $3FF0; value: $35B0); // 1st mask/value in sequence */
    bit2c0: (mask: $3FF3; value: $35B0); // clear bit 0 value */
    bit2s0: (mask: $3FF3; value: $35B1); // set bit 0 value */
    bit2c1: (mask: $3FF3; value: $35B2); // clear bit 1 value */
    bit2s1: (mask: $3FF3; value: $35B3); // set bit 1 value */
    bit3: (mask: $3FF8; value: $35C0); // final mask/value in sequence */
    // additive banking */
    add1: (mask: UNKNOWN; value: UNKNOWN); add2: (mask: UNKNOWN; value: UNKNOWN);
    addplus1: (mask: UNKNOWN; value: UNKNOWN); addplus2: (mask: UNKNOWN; value: UNKNOWN);
    add3: (mask: UNKNOWN; value: UNKNOWN));

  slapstic106: slapstic_data = (
    // basic banking */
    bankstart: 3; // starting bank */
    bank: ($0008, $000A, $000C, $000E); // bank select values */
    // alternate banking */
    alt1: (mask: $007F; value: $002B); // 1st mask/value in sequence */
    alt2: (mask: $3FFF; value: $0052); // 2nd mask/value in sequence */
    alt3: (mask: $3FFC; value: $0064); // 3rd mask/value in sequence */
    alt4: (mask: $3FF9; value: $0008); // 4th mask/value in sequence */
    altshift: 0; // shift to get bank from 3rd */
    // bitwise banking */
    bit1: (mask: $3FF0; value: $3DA0); // 1st mask/value in sequence */
    bit2c0: (mask: $3FF3; value: $3DA0); // clear bit 0 value */
    bit2s0: (mask: $3FF3; value: $3DA1); // set bit 0 value */
    bit2c1: (mask: $3FF3; value: $3DA2); // clear bit 1 value */
    bit2s1: (mask: $3FF3; value: $3DA3); // set bit 1 value */
    bit3: (mask: $3FF8; value: $3DB0); // final mask/value in sequence */
    // additive banking */
    add1: (mask: UNKNOWN; value: UNKNOWN); add2: (mask: UNKNOWN; value: UNKNOWN);
    addplus1: (mask: UNKNOWN; value: UNKNOWN); addplus2: (mask: UNKNOWN; value: UNKNOWN);
    add3: (mask: UNKNOWN; value: UNKNOWN));

  slapstic107: slapstic_data = (
    // basic banking */
    bankstart: 3; // starting bank */
    bank: ($0018, $001A, $001C, $001E); // bank select values */
    // alternate banking */
    alt1: (mask: $007F; value: $006B); // 1st mask/value in sequence */
    alt2: (mask: $3FFF; value: $3D52); // 2nd mask/value in sequence */
    alt3: (mask: $3FFC; value: $3D64); // 3rd mask/value in sequence */
    alt4: (mask: $3FF9; value: $0018); // 4th mask/value in sequence */
    altshift: 0; // shift to get bank from 3rd */
    // bitwise banking */
    bit1: (mask: $3FF0; value: $00A0); // 1st mask/value in sequence */
    bit2c0: (mask: $3FF3; value: $00A0); // clear bit 0 value */
    bit2s0: (mask: $3FF3; value: $00A1); // set bit 0 value */
    bit2c1: (mask: $3FF3; value: $00A2); // clear bit 1 value */
    bit2s1: (mask: $3FF3; value: $00A3); // set bit 1 value */
    bit3: (mask: $3FF8; value: $00B0); // final mask/value in sequence */
    // additive banking */
    add1: (mask: UNKNOWN; value: UNKNOWN); add2: (mask: UNKNOWN; value: UNKNOWN);
    addplus1: (mask: UNKNOWN; value: UNKNOWN); addplus2: (mask: UNKNOWN; value: UNKNOWN);
    add3: (mask: UNKNOWN; value: UNKNOWN));

constructor slapstic_type.create(num: byte; is_68k: boolean);
begin
  case num of
    101:
      self.slapstic := slapstic101;
    103:
      self.slapstic := slapstic103;
    104:
      self.slapstic := slapstic104;
    105:
      self.slapstic := slapstic105;
    106:
      self.slapstic := slapstic106;
    107:
      self.slapstic := slapstic107;
  else
    halt(0);
  end;
  self.access_68k := is_68k;
  self.reset;
end;

destructor slapstic_type.free;
begin
end;

function MATCHES_MASK_VALUE(val: word; maskval: mask_value): boolean;
begin
  MATCHES_MASK_VALUE := ((val and maskval.mask) = maskval.value);
end;

procedure slapstic_type.reset;
begin
  // reset the chip */
  self.state := DISABLED;
  // the 111 and later chips seem to reset to bank 0 */
  self.current_bank := self.slapstic.bankstart;
end;

function slapstic_type.alt2_kludge(offset: word): byte;
var
  opcode: word;
  regval: dword;
  ret: byte;
begin
  ret := ALTERNATE2;
  if self.access_68k then
  begin
    // first verify that the prefetched PC matches the first alternate
    if (MATCHES_MASK_VALUE(m68000_0.r.pc.l shr 1, self.slapstic.alt1)) then
    begin
      // now look for a move.w (An),(An) or cmpm.w (An)+,(An)+ */
      opcode := m68000_0.getword_(m68000_0.r.ppc.l and $FFFFFF);
      if (((opcode and $F1F8) = $3090) or ((opcode and $F1F8) = $B148)) then
      begin
        // fetch the value of the register for the second operand, and see */
        // if it matches the third alternate */
        regval := m68000_0.r.a[((opcode shr 9) and 7)].l shr 1;
        if (MATCHES_MASK_VALUE(regval, slapstic.alt3)) then
        begin
          self.alt_bank := (regval shr self.slapstic.altshift) and 3;
          ret := ALTERNATE3;
        end;
      end;
    end
    else
    begin
      // if there's no second memory hit within this instruction, the next */
      // opcode fetch will botch the operation, so just fall back to */
      // the enabled state */
      ret := ENABLED;
    end;
  end;
  // kludge for ESB */
  alt2_kludge := ret;
end;

function slapstic_type.slapstic_tweak(offset: word): byte;
begin
  // reset is universal */
  if (offset = $0000) then
  begin
    self.state := ENABLED;
  end
  else
  begin // otherwise, use the state machine
    case self.state of
      // DISABLED state: everything is ignored except a reset */
      DISABLED:
        ;
      // ENABLED state: the chip has been activated and is ready for a bankswitch */
      ENABLED:
        begin
          if MATCHES_MASK_VALUE(offset, self.slapstic.bit1) then
          begin
            // check for request to enter bitwise state */
            self.state := BITWISE1;
          end
          else if MATCHES_MASK_VALUE(offset, self.slapstic.add1) then
          begin
            // check for request to enter additive state */
            self.state := ADDITIVE1;
          end
          else if MATCHES_MASK_VALUE(offset, self.slapstic.alt1) then
          begin
            // check for request to enter alternate state */
            self.state := ALTERNATE1
          end
          else if MATCHES_MASK_VALUE(offset, self.slapstic.alt2) then
          begin
            // special kludge for catching the second alternate address if */
            // the first one was missed (since it's usually an opcode fetch) */
            self.state := alt2_kludge(offset);
          end
          else if (offset = self.slapstic.bank[0]) then
          begin // check for standard bankswitches */
            self.state := DISABLED;
            self.current_bank := 0;
          end
          else if (offset = self.slapstic.bank[1]) then
          begin
            self.state := DISABLED;
            self.current_bank := 1;
          end
          else if (offset = self.slapstic.bank[2]) then
          begin
            self.state := DISABLED;
            self.current_bank := 2;
          end
          else if (offset = self.slapstic.bank[3]) then
          begin
            self.state := DISABLED;
            self.current_bank := 3;
          end;
        end;
      // ALTERNATE1 state: look for alternate2 offset, or else fall back to ENABLED */
      ALTERNATE1:
        if MATCHES_MASK_VALUE(offset, self.slapstic.alt2) then
          self.state := ALTERNATE2
        else
          self.state := ENABLED;
      // ALTERNATE2 state: look for altbank offset, or else fall back to ENABLED */
      ALTERNATE2:
        if MATCHES_MASK_VALUE(offset, slapstic.alt3) then
        begin
          self.state := ALTERNATE3;
          self.alt_bank := (offset shr self.slapstic.altshift) and 3;
        end
        else
        begin
          self.state := ENABLED;
        end;
      // ALTERNATE3 state: wait for the final value to finish the transaction */
      ALTERNATE3:
        if MATCHES_MASK_VALUE(offset, slapstic.alt4) then
        begin
          self.state := DISABLED;
          current_bank := self.alt_bank;
        end;
      // BITWISE1 state: waiting for a bank to enter the BITWISE state */
      BITWISE1:
        if ((offset = self.slapstic.bank[0]) or (offset = self.slapstic.bank[1]) or
          (offset = self.slapstic.bank[2]) or (offset = self.slapstic.bank[3])) then
        begin
          self.state := BITWISE2;
          self.bit_bank := self.current_bank;
          self.bit_xor := 0;
        end;
      // BITWISE2 state: watch for twiddling and the escape mechanism */
      BITWISE2:
        begin
          // check for clear bit 0 case */
          if MATCHES_MASK_VALUE(offset xor self.bit_xor, self.slapstic.bit2c0) then
          begin
            self.bit_bank := self.bit_bank and $FE;
            self.bit_xor := self.bit_xor xor 3;
          end // check for set bit 0 case */
          else if MATCHES_MASK_VALUE(offset xor self.bit_xor, self.slapstic.bit2s0) then
          begin
            self.bit_bank := self.bit_bank or 1;
            self.bit_xor := self.bit_xor xor 3;
          end // check for clear bit 1 case */
          else if MATCHES_MASK_VALUE(offset xor self.bit_xor, self.slapstic.bit2c1) then
          begin
            self.bit_bank := self.bit_bank and $FD;
            self.bit_xor := self.bit_xor xor 3;
          end // check for set bit 1 case */
          else if MATCHES_MASK_VALUE(offset xor self.bit_xor, self.slapstic.bit2s1) then
          begin
            self.bit_bank := self.bit_bank or 2;
            self.bit_xor := self.bit_xor xor 3;
          end // check for escape case */
          else if MATCHES_MASK_VALUE(offset, self.slapstic.bit3) then
          begin
            self.state := BITWISE3;
          end;
        end;
      // BITWISE3 state: waiting for a bank to seal the deal */
      BITWISE3:
        if ((offset = self.slapstic.bank[0]) or (offset = self.slapstic.bank[1]) or
          (offset = self.slapstic.bank[2]) or (offset = self.slapstic.bank[3])) then
        begin
          self.state := DISABLED;
          self.current_bank := self.bit_bank;
        end;
      // ADDITIVE1 state: look for add2 offset, or else fall back to ENABLED */
      ADDITIVE1:
        if MATCHES_MASK_VALUE(offset, self.slapstic.add2) then
        begin
          self.state := ADDITIVE2;
          self.add_bank := self.current_bank;
        end
        else
        begin
          self.state := ENABLED;
        end;
      // ADDITIVE2 state: watch for twiddling and the escape mechanism */
      ADDITIVE2:
        begin
          // check for add 1 case -- can intermix */
          if MATCHES_MASK_VALUE(offset, self.slapstic.addplus1) then
            self.add_bank := (self.add_bank + 1) and 3;
          // check for add 2 case -- can intermix */
          if MATCHES_MASK_VALUE(offset, self.slapstic.addplus2) then
            self.add_bank := (self.add_bank + 2) and 3;
          // check for escape case -- can intermix with the above */
          if MATCHES_MASK_VALUE(offset, self.slapstic.add3) then
            self.state := ADDITIVE3;
        end;
      // ADDITIVE3 state: waiting for a bank to seal the deal */
      ADDITIVE3:
        if ((offset = self.slapstic.bank[0]) or (offset = self.slapstic.bank[1]) or
          (offset = self.slapstic.bank[2]) or (offset = self.slapstic.bank[3])) then
        begin
          self.state := DISABLED;
          self.current_bank := self.add_bank;
        end;
    end;
  end;
  // return the active bank
  slapstic_tweak := self.current_bank;
end;

end.
