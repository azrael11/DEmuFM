unit deco_104;

interface

uses
  WinApi.Windows,
  main_engine,
  deco_146;

type
  cpu_deco_104 = class(cpu_deco_146)
    constructor create;
  public
  private
  end;

var
  main_deco104: cpu_deco_104;

implementation

const
  deco_104ram: array [0 .. $3FF] of ram_type = ((wo: $04;
    m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B); ux: true;
    un: true), // 0x000
    (wo: $2A; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x002
    (wo: $5E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x004
    (wo: $98; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x006
    (wo: $94; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x008
    (wo: $BE; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x00a
    (wo: $D6; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x00c
    (wo: $E4; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x00e
    (wo: $90; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x010
    (wo: $A8; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x012
    (wo: $24; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x014
    (wo: $FC; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x016
    (wo: $08; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x018
    (wo: INPUT_PORT_C; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x01a
    (wo: $72; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x01c
    (wo: $C4; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x01e
    (wo: $D0; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x020
    (wo: $66; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x022
    (wo: $8C; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x024
    (wo: $EC; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x026
    (wo: $58; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x028
    (wo: $80; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x02a
    (wo: $82; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: true), // 0x02c
    (wo: $6A; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x02e
    (wo: $00; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x030
    (wo: $60; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x032
    (wo: INPUT_PORT_B; m: ($03, $00, $01, $02, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x034
    (wo: $AE; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x036
    (wo: $12; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: false), // 0x038
    (wo: $42; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x03a
    (wo: $1E; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x03c
    (wo: $F6; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x03e
    (wo: $DC; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x040
    (wo: $34; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x042
    (wo: $2C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x044
    (wo: $7A; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x046
    (wo: $10; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x048
    (wo: $9E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x04a
    (wo: $3E; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x04c
    (wo: $0E; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x04e
    (wo: $A4; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x050
    (wo: $0C; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x052
    (wo: $40; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x054
    (wo: $20; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x056
    (wo: $46; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x058
    (wo: $6C; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x05a
    (wo: $9A; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x05c
    (wo: $18; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x05e
    (wo: $E0; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x060
    (wo: $30; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x062
    (wo: $CA; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x064
    (wo: $AC; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x066
    (wo: $E8; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x068
    (wo: $66; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x06a
    (wo: $14; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x06c
    (wo: $96; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x06e
    (wo: $5C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x070
    (wo: $0A; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: true), // 0x072
    (wo: INPUT_PORT_B; m: ($01, $02, $03, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x074
    (wo: INPUT_PORT_C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: true), // 0x076
    (wo: $5A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x078
    (wo: $74; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x07a
    (wo: $B4; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x07c
    (wo: $86; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x07e
    (wo: $84; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x080
    (wo: $28; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x082
    (wo: $50; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x084
    (wo: $66; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x086
    (wo: INPUT_PORT_A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x088
    (wo: $1C; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x08a
    (wo: $48; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x08c
    (wo: $C2; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x08e
    (wo: $44; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x090
    (wo: $3C; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x092
    (wo: $FA; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x094
    (wo: $22; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x096
    (wo: $F0; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x098
    (wo: $2E; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x09a
    (wo: $06; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x09c
    (wo: $64; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: true; un: false), // 0x09e
    (wo: $CC; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x0a0
    (wo: $7A; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x0a2
    (wo: $B2; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x0a4
    (wo: $BE; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x0a6
    (wo: $DE; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x0a8
    (wo: $F8; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x0aa
    (wo: $CA; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: false), // 0x0ac
    (wo: $3E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x0ae
    (wo: $B6; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: true), // 0x0b0
    (wo: $D8; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x0b2
    (wo: INPUT_PORT_C; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09,
    $0A, $0B); ux: false; un: false), // 0x0b4
    (wo: $C0; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x0b6
    (wo: $A2; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x0b8
    (wo: $E6; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: false), // 0x0ba
    (wo: $1A; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x0bc
    (wo: $B0; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x0be
    (wo: $4C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x0c0
    (wo: $56; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x0c2
    (wo: $EE; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x0c4
    (wo: $D4; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x0c6
    (wo: INPUT_PORT_A; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05,
    $06, $07); ux: true; un: false), // 0x0c8
    (wo: $DA; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x0ca
    (wo: $CE; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x0cc
    (wo: $F2; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x0ce
    (wo: $8E; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x0d0
    (wo: $3A; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x0d2
    (wo: $6E; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x0d4
    (wo: $A6; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x0d6
    (wo: $78; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x0d8
    (wo: $BC; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x0da
    (wo: $BA; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x0dc
    (wo: $36; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x0de
    (wo: $66; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x0e0
    (wo: $92; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: true), // 0x0e2
    (wo: $52; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x0e4
    (wo: $F4; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x0e6
    (wo: INPUT_PORT_B; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x0e8
    (wo: $C6; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x0ea
    (wo: $EA; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x0ec
    (wo: $FE; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x0ee
    (wo: $B2; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x0f0
    (wo: $32; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x0f2
    (wo: $76; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x0f4
    (wo: $E2; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x0f6
    (wo: $7C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x0f8
    (wo: $A0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x0fa
    (wo: $4A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x0fc
    (wo: $70; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x0fe
    (wo: $64; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x100
    (wo: $04; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x102
    (wo: $E2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x104
    (wo: $CA; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x106
    (wo: $A4; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x108
    (wo: $12; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x10a
    (wo: $AA; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x10c
    (wo: $7C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x10e
    (wo: $F4; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x110
    (wo: $DA; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x112
    (wo: $7A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x114
    (wo: $3A; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x116
    (wo: $00; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x118
    (wo: $66; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x11a
    (wo: $CE; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x11c
    (wo: $B6; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x11e
    (wo: $1C; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x120
    (wo: $82; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x122
    (wo: $4A; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x124
    (wo: $58; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x126
    (wo: $BE; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x128
    (wo: $36; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x12a
    (wo: $9A; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: false; un: false), // 0x12c
    (wo: $02; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x12e
    (wo: $EA; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x130
    (wo: $AE; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x132
    (wo: $52; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x134
    (wo: $DC; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: false), // 0x136
    (wo: $9E; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x138
    (wo: $FC; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: true; un: true), // 0x13a
    (wo: $4C; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x13c
    (wo: $76; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x13e
    (wo: $8C; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x140
    (wo: $D4; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x142
    (wo: $3E; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x144
    (wo: $16; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x146
    (wo: $B8; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: false), // 0x148
    (wo: $50; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x14a
    (wo: $42; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x14c
    (wo: $D6; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x14e
    (wo: $7E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x150
    (wo: $92; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x152
    (wo: INPUT_PORT_A; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x154
    (wo: $DE; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x156
    (wo: $1A; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x158
    (wo: $9C; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x15a
    (wo: $14; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x15c
    (wo: $98; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x15e
    (wo: $40; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x160
    (wo: $6E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x162
    (wo: $C4; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x164
    (wo: $C6; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x166
    (wo: $84; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x168
    (wo: $8E; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x16a
    (wo: $96; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: true), // 0x16c
    (wo: $6A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x16e
    (wo: $F0; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x170
    (wo: $2A; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x172
    (wo: $1E; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x174
    (wo: $62; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x176
    (wo: $88; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x178
    (wo: $E0; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x17a
    (wo: INPUT_PORT_A; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09,
    $0A, $0B); ux: true; un: true), // 0x17c
    (wo: $CC; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x17e
    (wo: INPUT_PORT_A; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x180
    (wo: $46; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x182
    (wo: $90; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x184
    (wo: $72; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x186
    (wo: $EE; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x188
    (wo: $2C; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x18a
    (wo: $22; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x18c
    (wo: $38; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x18e
    (wo: $44; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x190
    (wo: $C0; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x192
    (wo: $54; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x194
    (wo: $6C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x196
    (wo: $FA; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x198
    (wo: $34; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x19a
    (wo: $A8; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x19c
    (wo: $3C; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x19e
    (wo: $7E; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x1a0
    (wo: $4E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1a2
    (wo: $9C; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x1a4
    (wo: $16; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x1a6
    (wo: $38; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x1a8
    (wo: $C8; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x1aa
    (wo: $68; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x1ac
    (wo: $54; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1ae
    (wo: $BA; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x1b0
    (wo: $78; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1b2
    (wo: $CC; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x1b4
    (wo: $FE; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x1b6
    (wo: $86; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x1b8
    (wo: $18; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x1ba
    (wo: $0E; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: true), // 0x1bc
    (wo: $C2; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x1be
    (wo: INPUT_PORT_C; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01,
    $02, $03); ux: false; un: false), // 0x1c0
    (wo: $0C; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x1c2
    (wo: INPUT_PORT_A; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x1c4
    (wo: $70; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1c6
    (wo: INPUT_PORT_A; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x1c8
    (wo: $5C; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: true), // 0x1ca
    (wo: $AC; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x1cc
    (wo: $48; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x1ce
    (wo: $0A; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x1d0
    (wo: $94; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: false), // 0x1d2
    (wo: $66; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x1d4
    (wo: $F8; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x1d6
    (wo: $68; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1d8
    (wo: $24; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x1da
    (wo: $66; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x1dc
    (wo: $A6; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x1de
    (wo: $74; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1e0
    (wo: $D0; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: false), // 0x1e2
    (wo: $5E; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: true), // 0x1e4
    (wo: $E6; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1e6
    (wo: $66; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1e8
    (wo: INPUT_PORT_B; m: ($04, $05, $06, $07, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x1ea
    (wo: INPUT_PORT_B; m: ($06, $07, $04, $05, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: true), // 0x1ec
    (wo: $C8; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x1ee
    (wo: $A2; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x1f0
    (wo: $60; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x1f2
    (wo: $BC; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: false; un: true), // 0x1f4
    (wo: $06; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x1f6
    (wo: $2E; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x1f8
    (wo: $26; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x1fa
    (wo: $56; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x1fc
    (wo: $D2; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x1fe
    (wo: $A0; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x200
    (wo: $34; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x202
    (wo: $F6; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x204
    (wo: INPUT_PORT_A; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05,
    $06, $07); ux: false; un: false), // 0x206
    (wo: $AE; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x208
    (wo: $F4; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x20a
    (wo: INPUT_PORT_C; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D,
    $0E, $0F); ux: true; un: true), // 0x20c
    (wo: $4E; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x20e
    (wo: $0E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x210
    (wo: $6E; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x212
    (wo: $4A; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: true), // 0x214
    (wo: $0A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x216
    (wo: $82; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x218
    (wo: $66; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x21a
    (wo: $6C; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x21c
    (wo: $B8; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x21e
    (wo: $12; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x220
    (wo: $06; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x222
    (wo: $00; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x224
    (wo: $72; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x226
    (wo: $E4; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x228
    (wo: $90; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x22a
    (wo: $C4; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x22c
    (wo: $08; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x22e
    (wo: $98; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x230
    (wo: $DA; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x232
    (wo: $3A; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x234
    (wo: $CC; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x236
    (wo: $7C; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x238
    (wo: $86; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x23a
    (wo: $56; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: false; un: true), // 0x23c
    (wo: $8A; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: true), // 0x23e
    (wo: $A0; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x240
    (wo: $04; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x242
    (wo: $32; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x244
    (wo: $48; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x246
    (wo: $8C; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x248
    (wo: $A2; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x24a
    (wo: $FA; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x24c
    (wo: $46; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x24e
    (wo: $62; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x250
    (wo: $E0; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x252
    (wo: $7E; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x254
    (wo: $CE; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x256
    (wo: $F8; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x258
    (wo: $6A; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x25a
    (wo: $58; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x25c
    (wo: $C0; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: true; un: false), // 0x25e
    (wo: $E2; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x260
    (wo: $30; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x262
    (wo: $88; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x264
    (wo: $E8; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x266
    (wo: $AC; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x268
    (wo: $E6; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x26a
    (wo: $C2; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x26c
    (wo: $A8; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: false), // 0x26e
    (wo: INPUT_PORT_C; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01,
    $02, $03); ux: false; un: false), // 0x270
    (wo: $3C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x272
    (wo: $68; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x274
    (wo: $2E; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x276
    (wo: $18; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x278
    (wo: $02; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x27a
    (wo: $70; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x27c
    (wo: $94; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x27e
    (wo: $5E; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x280
    (wo: $26; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x282
    (wo: $14; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x284
    (wo: $7A; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x286
    (wo: $D2; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x288
    (wo: $D4; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x28a
    (wo: $A6; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x28c
    (wo: $36; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x28e
    (wo: $EE; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x290
    (wo: INPUT_PORT_C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x292
    (wo: $66; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x294
    (wo: $5A; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x296
    (wo: $64; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x298
    (wo: $60; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x29a
    (wo: $EC; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x29c
    (wo: $80; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x29e
    (wo: $D0; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x2a0
    (wo: $44; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x2a2
    (wo: INPUT_PORT_C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09,
    $0A, $0B); ux: false; un: false), // 0x2a4
    (wo: $BC; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x2a6
    (wo: $22; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x2a8
    (wo: $B2; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x2aa
    (wo: $1E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2ac
    (wo: $9C; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2ae
    (wo: $96; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: false; un: false), // 0x2b0
    (wo: $8E; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x2b2
    (wo: $B6; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x2b4
    (wo: $FE; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x2b6
    (wo: $66; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x2b8
    (wo: $F0; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x2ba
    (wo: $20; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x2bc
    (wo: $40; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x2be
    (wo: $4C; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x2c0
    (wo: $76; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x2c2
    (wo: $CA; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x2c4
    (wo: $50; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x2c6
    (wo: $1C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2c8
    (wo: $1A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2ca
    (wo: INPUT_PORT_B; m: ($00, $01, $02, $03, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x2cc
    (wo: $92; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x2ce
    (wo: $66; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x2d0
    (wo: $28; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x2d2
    (wo: $74; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x2d4
    (wo: $16; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2d6
    (wo: $9E; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2d8
    (wo: $DE; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x2da
    (wo: $BA; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x2dc
    (wo: $78; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x2de
    (wo: $9A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2e0
    (wo: $C6; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x2e2
    (wo: $42; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x2e4
    (wo: $EA; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x2e6
    (wo: $0C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2e8
    (wo: $D8; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x2ea
    (wo: $5C; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x2ec
    (wo: $AA; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x2ee
    (wo: $B4; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x2f0
    (wo: $F2; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x2f2
    (wo: INPUT_PORT_C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: true), // 0x2f4
    (wo: $84; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x2f6
    (wo: $A4; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: true), // 0x2f8
    (wo: INPUT_PORT_C; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x2fa
    (wo: $FC; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x2fc
    (wo: $10; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x2fe
    (wo: $A4; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x300
    (wo: $24; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x302
    (wo: $E2; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x304
    (wo: $32; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x306
    (wo: $2A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x308
    (wo: $66; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x30a
    (wo: $86; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x30c
    (wo: $AA; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x30e
    (wo: $66; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x310
    (wo: $4E; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x312
    (wo: $84; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x314
    (wo: $96; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x316
    (wo: $26; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x318
    (wo: $64; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x31a
    (wo: $AC; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x31c
    (wo: $78; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x31e
    (wo: $FE; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: false), // 0x320
    (wo: $2E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x322
    (wo: $06; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x324
    (wo: $92; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x326
    (wo: $34; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x328
    (wo: $C0; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x32a
    (wo: $8A; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x32c
    (wo: $46; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x32e
    (wo: $D8; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x330
    (wo: $C4; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x332
    (wo: $30; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x334
    (wo: $1A; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x336
    (wo: $D0; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x338
    (wo: $60; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x33a
    (wo: $F6; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x33c
    (wo: $00; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: true), // 0x33e
    (wo: $90; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x340
    (wo: $FC; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x342
    (wo: $08; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x344
    (wo: $A8; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x346
    (wo: $44; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x348
    (wo: $04; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x34a
    (wo: $3C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x34c
    (wo: $DE; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x34e
    (wo: $72; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x350
    (wo: $62; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x352
    (wo: $3A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x354
    (wo: $BA; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x356
    (wo: $68; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x358
    (wo: $76; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x35a
    (wo: $9E; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x35c
    (wo: $C6; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x35e
    (wo: $E4; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x360
    (wo: $02; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x362
    (wo: $6C; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x364
    (wo: $EC; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x366
    (wo: INPUT_PORT_C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x368
    (wo: INPUT_PORT_A; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09,
    $0A, $0B); ux: false; un: false), // 0x36a
    (wo: INPUT_PORT_B; m: ($00, $01, $02, $03, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x36c
    (wo: $0C; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x36e
    (wo: $80; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x370
    (wo: $82; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x372
    (wo: $B8; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x374
    (wo: $50; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x376
    (wo: $20; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x378
    (wo: $F4; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: true), // 0x37a
    (wo: $8C; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x37c
    (wo: $E6; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x37e
    (wo: $DA; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x380
    (wo: $F2; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x382
    (wo: $DC; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x384
    (wo: $9C; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x386
    (wo: INPUT_PORT_A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x388
    (wo: INPUT_PORT_B; m: ($04, $05, $06, $07, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x38a
    (wo: $28; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x38c
    (wo: $B6; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x38e
    (wo: $2C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x390
    (wo: $CE; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x392
    (wo: $0E; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x394
    (wo: $5C; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: false), // 0x396
    (wo: $52; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x398
    (wo: $7E; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x39a
    (wo: $6A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x39c
    (wo: $A0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x39e
    (wo: $52; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3a0
    (wo: $94; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x3a2
    (wo: $2C; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x3a4
    (wo: $C8; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x3a6
    (wo: $18; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x3a8
    (wo: $56; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: false), // 0x3aa
    (wo: $54; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x3ac
    (wo: $58; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x3ae
    (wo: INPUT_PORT_B; m: ($04, $05, $06, $07, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x3b0
    (wo: $14; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x3b2
    (wo: $38; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x3b4
    (wo: $F8; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x3b6
    (wo: $B4; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x3b8
    (wo: $D6; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x3ba
    (wo: $5C; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x3bc
    (wo: $1C; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x3be
    (wo: $22; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x3c0
    (wo: $66; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x3c2
    (wo: $8E; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x3c4
    (wo: $F0; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x3c6
    (wo: $AE; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3c8
    (wo: $1E; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x3ca
    (wo: $EE; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x3cc
    (wo: $EA; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: true), // 0x3ce
    (wo: $E8; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: true), // 0x3d0
    (wo: $88; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3d2
    (wo: INPUT_PORT_C; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09,
    $0A, $0B); ux: false; un: false), // 0x3d4
    (wo: $66; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x3d6
    (wo: INPUT_PORT_B; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x3d8
    (wo: $FA; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x3da
    (wo: $4C; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x3dc
    (wo: $36; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x3de
    (wo: $E0; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x3e0
    (wo: $D4; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3e2
    (wo: $C8; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3e4
    (wo: $C2; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x3e6
    (wo: $0A; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x3e8
    (wo: $9A; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x3ea
    (wo: $7C; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x3ec
    (wo: $4A; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3ee
    (wo: $D2; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x3f0
    (wo: $48; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3f2
    (wo: $6E; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x3f4
    (wo: $A6; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x3f6
    (wo: $42; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x3f8
    (wo: $5E; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x3fa
    (wo: $BC; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3fc
    (wo: $10; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x3fe
    (wo: $02; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x400
    (wo: $72; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x402
    (wo: $74; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x404
    (wo: $96; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: true), // 0x406
    (wo: $54; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x408
    (wo: $DA; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x40a
    (wo: $D6; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x40c
    (wo: $8A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x40e
    (wo: $DE; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x410
    (wo: $A4; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x412
    (wo: $A2; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x414
    (wo: $E4; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x416
    (wo: $04; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x418
    (wo: $84; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x41a
    (wo: $2A; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x41c
    (wo: $4C; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: true), // 0x41e
    (wo: $2E; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x420
    (wo: $86; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x422
    (wo: $60; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x424
    (wo: $BA; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x426
    (wo: $8C; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x428
    (wo: $EE; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: true; un: false), // 0x42a
    (wo: $AC; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x42c
    (wo: $32; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x42e
    (wo: $D2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x430
    (wo: $0E; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x432
    (wo: $06; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: true; un: true), // 0x434
    (wo: $66; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x436
    (wo: $EC; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x438
    (wo: $FE; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x43a
    (wo: INPUT_PORT_B; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x43c
    (wo: $7A; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x43e
    (wo: $FC; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x440
    (wo: $10; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x442
    (wo: $66; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x444
    (wo: $16; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x446
    (wo: $90; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x448
    (wo: $B4; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x44a
    (wo: INPUT_PORT_B; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: true), // 0x44c
    (wo: $44; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x44e
    (wo: INPUT_PORT_C; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: true; un: false), // 0x450
    (wo: $30; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x452
    (wo: $82; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x454
    (wo: $26; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x456
    (wo: $52; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x458
    (wo: $C6; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x45a
    (wo: $D8; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x45c
    (wo: $18; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x45e
    (wo: $C8; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x460
    (wo: $64; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x462
    (wo: $BE; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x464
    (wo: $42; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x466
    (wo: $C0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x468
    (wo: $38; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x46a
    (wo: $D0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x46c
    (wo: $9C; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x46e
    (wo: $A8; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x470
    (wo: $4A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x472
    (wo: $1C; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x474
    (wo: $F0; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x476
    (wo: $B6; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x478
    (wo: $C2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x47a
    (wo: $7E; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x47c
    (wo: INPUT_PORT_B; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x47e
    (wo: $5A; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x480
    (wo: $5C; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x482
    (wo: $5A; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x484
    (wo: $06; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x486
    (wo: $2C; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x488
    (wo: $76; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x48a
    (wo: $BC; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x48c
    (wo: $46; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: false), // 0x48e
    (wo: $66; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x490
    (wo: $EA; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x492
    (wo: $9A; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x494
    (wo: $B8; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x496
    (wo: $36; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x498
    (wo: $56; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x49a
    (wo: $6C; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x49c
    (wo: $12; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x49e
    (wo: $A4; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4a0
    (wo: $A8; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4a2
    (wo: $CE; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: true; un: true), // 0x4a4
    (wo: $8C; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4a6
    (wo: $B0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4a8
    (wo: $48; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x4aa
    (wo: INPUT_PORT_A; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01,
    $02, $03); ux: false; un: false), // 0x4ac
    (wo: $92; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x4ae
    (wo: $10; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x4b0
    (wo: $74; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x4b2
    (wo: $08; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x4b4
    (wo: $94; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: false), // 0x4b6
    (wo: $46; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: false), // 0x4b8
    (wo: $24; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x4ba
    (wo: $42; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x4bc
    (wo: $A6; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4be
    (wo: $08; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x4c0
    (wo: $80; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x4c2
    (wo: $3C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x4c4
    (wo: $94; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x4c6
    (wo: INPUT_PORT_A; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x4c8
    (wo: $20; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x4ca
    (wo: $F6; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x4cc
    (wo: $66; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x4ce
    (wo: $E2; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x4d0
    (wo: INPUT_PORT_B; m: ($02, $03, $00, $01, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x4d2
    (wo: $D4; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4d4
    (wo: $A0; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: false; un: false), // 0x4d6
    (wo: $48; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x4d8
    (wo: $CA; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4da
    (wo: $62; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x4dc
    (wo: $70; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x4de
    (wo: $FA; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x4e0
    (wo: $9E; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x4e2
    (wo: $40; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x4e4
    (wo: $68; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x4e6
    (wo: $88; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x4e8
    (wo: $3E; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x4ea
    (wo: $7C; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x4ec
    (wo: $CC; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x4ee
    (wo: $34; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x4f0
    (wo: INPUT_PORT_A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09,
    $0A, $0B); ux: false; un: false), // 0x4f2
    (wo: $28; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x4f4
    (wo: $E6; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: false), // 0x4f6
    (wo: $6A; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x4f8
    (wo: $AA; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x4fa
    (wo: $50; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x4fc
    (wo: $A6; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x4fe
    (wo: $F0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x500
    (wo: $88; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: true), // 0x502
    (wo: $5C; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x504
    (wo: INPUT_PORT_C; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x506
    (wo: INPUT_PORT_A; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x508
    (wo: $C0; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x50a
    (wo: $26; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x50c
    (wo: $72; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x50e
    (wo: $8A; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x510
    (wo: $06; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x512
    (wo: $32; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x514
    (wo: $B6; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x516
    (wo: $02; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x518
    (wo: $B2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x51a
    (wo: $0A; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x51c
    (wo: $D6; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x51e
    (wo: $AA; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x520
    (wo: $CA; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x522
    (wo: $54; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x524
    (wo: $08; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x526
    (wo: INPUT_PORT_B; m: ($08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x528
    (wo: $8E; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x52a
    (wo: $E0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x52c
    (wo: $BA; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x52e
    (wo: $18; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: true), // 0x530
    (wo: $B0; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x532
    (wo: $1A; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x534
    (wo: $7A; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x536
    (wo: $3C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x538
    (wo: $0C; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x53a
    (wo: $FA; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x53c
    (wo: $9E; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x53e
    (wo: $3E; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x540
    (wo: $9C; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x542
    (wo: $5A; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x544
    (wo: $62; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x546
    (wo: $F6; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x548
    (wo: $DE; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x54a
    (wo: $38; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x54c
    (wo: $E6; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x54e
    (wo: $A2; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x550
    (wo: $4C; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x552
    (wo: $AE; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x554
    (wo: $AC; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x556
    (wo: $68; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x558
    (wo: $F2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x55a
    (wo: $66; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x55c
    (wo: $EA; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x55e
    (wo: $82; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x560
    (wo: INPUT_PORT_B; m: ($00, $01, $02, $03, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: true; un: false), // 0x562
    (wo: $80; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x564
    (wo: $F8; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x566
    (wo: $6C; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x568
    (wo: $7C; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x56a
    (wo: INPUT_PORT_A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01,
    $02, $03); ux: false; un: false), // 0x56c
    (wo: $98; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x56e
    (wo: $24; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x570
    (wo: $C2; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x572
    (wo: $DC; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x574
    (wo: $52; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x576
    (wo: $2A; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x578
    (wo: $B8; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x57a
    (wo: $28; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x57c
    (wo: $D8; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x57e
    (wo: $0C; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x580
    (wo: $8E; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: false), // 0x582
    (wo: $22; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x584
    (wo: $00; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x586
    (wo: $04; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x588
    (wo: $22; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x58a
    (wo: $E8; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x58c
    (wo: $74; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x58e
    (wo: $0E; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x590
    (wo: $66; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x592
    (wo: $40; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x594
    (wo: $C8; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x596
    (wo: $70; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x598
    (wo: $16; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x59a
    (wo: $12; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: false), // 0x59c
    (wo: $36; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x59e
    (wo: $5E; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x5a0
    (wo: $24; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x5a2
    (wo: $CE; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5a4
    (wo: $B0; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x5a6
    (wo: $F2; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x5a8
    (wo: $98; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x5aa
    (wo: $6E; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x5ac
    (wo: $DC; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5ae
    (wo: $C4; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5b0
    (wo: INPUT_PORT_B; m: ($00, $01, $02, $03, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x5b2
    (wo: $E0; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x5b4
    (wo: $92; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x5b6
    (wo: $4E; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x5b8
    (wo: $F4; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5ba
    (wo: $78; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x5bc
    (wo: $58; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: true), // 0x5be
    (wo: $FE; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5c0
    (wo: $4A; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x5c2
    (wo: $3A; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x5c4
    (wo: $2C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x5c6
    (wo: $96; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x5c8
    (wo: $20; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x5ca
    (wo: $C6; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x5cc
    (wo: $A8; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x5ce
    (wo: $E2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5d0
    (wo: $66; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: false), // 0x5d2
    (wo: $F4; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5d4
    (wo: $EC; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5d6
    (wo: $BE; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x5d8
    (wo: $E8; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5da
    (wo: $6E; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x5dc
    (wo: $1E; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x5de
    (wo: $14; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5e0
    (wo: $84; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x5e2
    (wo: $A0; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x5e4
    (wo: $34; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x5e6
    (wo: $E4; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x5e8
    (wo: $58; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x5ea
    (wo: INPUT_PORT_B; m: ($08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x5ec
    (wo: $42; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x5ee
    (wo: $8C; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x5f0
    (wo: $10; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x5f2
    (wo: INPUT_PORT_C; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x5f4
    (wo: $04; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x5f6
    (wo: $4E; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x5f8
    (wo: $D2; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x5fa
    (wo: $7E; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x5fc
    (wo: $CC; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x5fe
    (wo: $C6; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x600
    (wo: $20; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x602
    (wo: $36; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x604
    (wo: $FC; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x606
    (wo: $1C; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x608
    (wo: $CA; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x60a
    (wo: INPUT_PORT_A; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09,
    $0A, $0B); ux: false; un: false), // 0x60c
    (wo: $A0; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x60e
    (wo: $A4; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x610
    (wo: $64; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x612
    (wo: $96; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x614
    (wo: $7E; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x616
    (wo: $0C; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x618
    (wo: $38; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: true), // 0x61a
    (wo: $66; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x61c
    (wo: $22; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x61e
    (wo: $1A; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x620
    (wo: $AE; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x622
    (wo: $9A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x624
    (wo: $4E; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x626
    (wo: $5A; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x628
    (wo: $8A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x62a
    (wo: INPUT_PORT_A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D,
    $0E, $0F); ux: false; un: true), // 0x62c
    (wo: $58; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x62e
    (wo: INPUT_PORT_B; m: ($00, $01, $02, $03, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: true), // 0x630
    (wo: $6A; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x632
    (wo: $C2; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x634
    (wo: $C4; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x636
    (wo: $94; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x638
    (wo: $3C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x63a
    (wo: $74; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x63c
    (wo: $0A; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x63e
    (wo: $30; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x640
    (wo: $68; m: ($04, $05, $06, $07, $00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x642
    (wo: $40; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: true), // 0x644
    (wo: $DA; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: false), // 0x646
    (wo: $60; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x648
    (wo: $DE; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x64a
    (wo: $04; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x64c
    (wo: $86; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x64e
    (wo: $78; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x650
    (wo: $4A; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x652
    (wo: $4C; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x654
    (wo: $2E; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x656
    (wo: $BC; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x658
    (wo: $C0; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x65a
    (wo: $44; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x65c
    (wo: $9C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x65e
    (wo: $48; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x660
    (wo: $8C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x662
    (wo: $9E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x664
    (wo: $FE; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x666
    (wo: $5E; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x668
    (wo: $16; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x66a
    (wo: $DC; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x66c
    (wo: $EC; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x66e
    (wo: $F4; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x670
    (wo: $6C; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x672
    (wo: INPUT_PORT_C; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x674
    (wo: $D2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x676
    (wo: $72; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x678
    (wo: $28; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x67a
    (wo: $1E; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x67c
    (wo: INPUT_PORT_B; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x67e
    (wo: $7A; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x680
    (wo: $BA; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x682
    (wo: $D8; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: false), // 0x684
    (wo: $46; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x686
    (wo: $E4; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x688
    (wo: $D0; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x68a
    (wo: $50; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x68c
    (wo: $92; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x68e
    (wo: $10; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x690
    (wo: $88; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x692
    (wo: $F2; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x694
    (wo: $CE; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x696
    (wo: $12; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x698
    (wo: $3E; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x69a
    (wo: $06; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x69c
    (wo: $E8; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x69e
    (wo: $B2; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x6a0
    (wo: $28; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x6a2
    (wo: $A0; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x6a4
    (wo: $E4; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x6a6
    (wo: $32; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x6a8
    (wo: $20; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x6aa
    (wo: $F6; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x6ac
    (wo: $F2; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x6ae
    (wo: $10; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x6b0
    (wo: $B4; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x6b2
    (wo: $A0; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x6b4
    (wo: $30; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: true), // 0x6b6
    (wo: $EA; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x6b8
    (wo: $F6; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x6ba
    (wo: $42; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x6bc
    (wo: INPUT_PORT_A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D,
    $0E, $0F); ux: true; un: false), // 0x6be
    (wo: $08; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: false; un: true), // 0x6c0
    (wo: $54; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x6c2
    (wo: $66; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x6c4
    (wo: $CC; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x6c6
    (wo: $52; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x6c8
    (wo: $D4; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x6ca
    (wo: INPUT_PORT_C; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x6cc
    (wo: $0E; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x6ce
    (wo: $B2; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x6d0
    (wo: $A2; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x6d2
    (wo: $B4; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x6d4
    (wo: INPUT_PORT_A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: true; un: false), // 0x6d6
    (wo: $AC; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x6d8
    (wo: $24; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: true), // 0x6da
    (wo: $BE; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x6dc
    (wo: $A8; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x6de
    (wo: $2C; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x6e0
    (wo: $90; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x6e2
    (wo: $98; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x6e4
    (wo: $70; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x6e6
    (wo: $B6; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x6e8
    (wo: $B8; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x6ea
    (wo: $66; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x6ec
    (wo: $2A; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: false), // 0x6ee
    (wo: $62; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x6f0
    (wo: $C8; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x6f2
    (wo: $14; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x6f4
    (wo: $A6; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x6f6
    (wo: $E6; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x6f8
    (wo: $EE; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x6fa
    (wo: $82; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x6fc
    (wo: $26; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x6fe
    (wo: $66; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x700
    (wo: $2C; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x702
    (wo: $7C; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x704
    (wo: $18; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x706
    (wo: $DA; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x708
    (wo: $DE; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x70a
    (wo: $6E; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x70c
    (wo: $26; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x70e
    (wo: $CA; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x710
    (wo: $F0; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x712
    (wo: $82; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x714
    (wo: $C0; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x716
    (wo: $8E; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x718
    (wo: $20; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x71a
    (wo: $4E; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x71c
    (wo: $E0; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x71e
    (wo: $66; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x720
    (wo: $DC; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x722
    (wo: $B2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x724
    (wo: INPUT_PORT_C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x726
    (wo: $D4; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x728
    (wo: $86; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x72a
    (wo: $78; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: false), // 0x72c
    (wo: $E4; m: ($06, $07, $04, $05, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x72e
    (wo: $3E; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x730
    (wo: $72; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x732
    (wo: $64; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x734
    (wo: $68; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x736
    (wo: $54; m: ($0D, $0E, $0F, $0C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x738
    (wo: $00; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x73a
    (wo: INPUT_PORT_A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: false), // 0x73c
    (wo: $AE; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x73e
    (wo: $6A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: false), // 0x740
    (wo: $2E; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: false), // 0x742
    (wo: $F6; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x744
    (wo: INPUT_PORT_C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: true; un: false), // 0x746
    (wo: $44; m: ($03, $00, $01, $02, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x748
    (wo: $14; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x74a
    (wo: $D2; m: ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x74c
    (wo: INPUT_PORT_C; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05,
    $06, $07); ux: false; un: false), // 0x74e
    (wo: $AA; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x750
    (wo: $BE; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x752
    (wo: $76; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x754
    (wo: $60; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x756
    (wo: $70; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x758
    (wo: $4C; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: true), // 0x75a
    (wo: $BC; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x75c
    (wo: $7E; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x75e
    (wo: $32; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: true), // 0x760
    (wo: $88; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x762
    (wo: $E2; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x764
    (wo: $66; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x766
    (wo: $16; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: false; un: true), // 0x768
    (wo: $FA; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x76a
    (wo: $52; m: ($01, $02, $03, $00, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x76c
    (wo: $D6; m: ($00, $01, $02, $03, $0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: true), // 0x76e
    (wo: INPUT_PORT_B; m: ($04, $05, $06, $07, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x770
    (wo: $C4; m: ($04, $05, $06, $07, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x772
    (wo: $38; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x774
    (wo: $6C; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x776
    (wo: $FC; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x778
    (wo: $84; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: false; un: true), // 0x77a
    (wo: $B6; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x77c
    (wo: $62; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: false; un: false), // 0x77e
    (wo: $B8; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x780
    (wo: $A2; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x782
    (wo: $8A; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x784
    (wo: $3A; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x786
    (wo: $F4; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x788
    (wo: $40; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x78a
    (wo: $7A; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x78c
    (wo: $1C; m: ($0B, $08, $09, $0A, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x78e
    (wo: $04; m: ($02, $03, $00, $01, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x790
    (wo: $1A; m: ($09, $0A, $0B, $08, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x792
    (wo: $0E; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x794
    (wo: $30; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $08, $09, $0A, $0B, $04, $05, $06, $07);
    ux: true; un: false), // 0x796
    (wo: $50; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true), // 0x798
    (wo: $C8; m: ($0D, $0E, $0F, $0C, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: false), // 0x79a
    (wo: $C6; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: true), // 0x79c
    (wo: $12; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x79e
    (wo: $6E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x7a0
    (wo: $56; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x7a2
    (wo: $E2; m: ($08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x7a4
    (wo: $00; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x7a6
    (wo: $FA; m: ($0C, $0D, $0E, $0F, $04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03);
    ux: true; un: false), // 0x7a8
    (wo: $76; m: ($07, $04, $05, $06, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: true; un: false), // 0x7aa
    (wo: $8E; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7ac
    (wo: $AA; m: ($04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x7ae
    (wo: $80; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7b0
    (wo: $3A; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x7b2
    (wo: $F8; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x7b4
    (wo: $02; m: ($00, $01, $02, $03, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x7b6
    (wo: $84; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7b8
    (wo: $D0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x7ba
    (wo: $D6; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x7bc
    (wo: $66; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x7be
    (wo: $EA; m: ($0E, $0F, $0C, $0D, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: false; un: false), // 0x7c0
    (wo: $E8; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: false; un: false), // 0x7c2
    (wo: $9C; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x7c4
    (wo: $02; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x7c6
    (wo: $9A; m: ($FF, $FF, $FF, $FF, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $04, $05, $06, $07);
    ux: true; un: true), // 0x7c8
    (wo: $A0; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7ca
    (wo: $0C; m: ($0F, $0C, $0D, $0E, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x7cc
    (wo: $80; m: ($0F, $0C, $0D, $0E, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x7ce
    (wo: $EC; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $00, $01, $02, $03, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7d0
    (wo: $1E; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x7d2
    (wo: $B4; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7d4
    (wo: $28; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: false; un: true), // 0x7d6
    (wo: $EE; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x7d8
    (wo: $56; m: ($08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07, $0C, $0D, $0E, $0F);
    ux: true; un: true), // 0x7da
    (wo: INPUT_PORT_A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D,
    $0E, $0F); ux: false; un: true), // 0x7dc
    (wo: $2A; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $04, $05, $06, $07, $00, $01, $02, $03);
    ux: true; un: true), // 0x7de
    (wo: $F8; m: ($0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x7e0
    (wo: $96; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x7e2
    (wo: $34; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $08, $09, $0A, $0B);
    ux: true; un: true), // 0x7e4
    (wo: $36; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x7e6
    (wo: $4A; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: true; un: false), // 0x7e8
    (wo: $0A; m: ($0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: false), // 0x7ea
    (wo: $66; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: false), // 0x7ec
    (wo: $C2; m: ($05, $06, $07, $04, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x7ee
    (wo: $BA; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7f0
    (wo: $5E; m: ($0C, $0D, $0E, $0F, $08, $09, $0A, $0B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: false; un: true), // 0x7f2
    (wo: $5C; m: ($08, $09, $0A, $0B, $04, $05, $06, $07, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF);
    ux: true; un: false), // 0x7f4
    (wo: $AC; m: ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
    ux: false; un: false), // 0x7f6
    (wo: $98; m: ($04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $00, $01, $02, $03);
    ux: false; un: true), // 0x7f8
    (wo: $80; m: ($0E, $0F, $0C, $0D, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
    ux: true; un: true), // 0x7fa
    (wo: INPUT_PORT_C; m: ($FF, $FF, $FF, $FF, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF); ux: false; un: false), // 0x7fc
    (wo: $D8; m: ($0A, $0B, $08, $09, $0C, $0D, $0E, $0F, $00, $01, $02, $03, $04, $05, $06, $07);
    ux: true; un: true)); // 0x7fe

constructor cpu_deco_104.create;
begin
  self.set_interface_scramble(9, 8, 7, 6, 5, 4, 3, 2, 1, 0);
  self.bankswitch_swap_read_address := $66;
  self.magic_read_address_xor := $2A4;
  self.magic_read_address_xor_enabled := false;
  self.xor_port := $42;
  self.mask_port := $EE;
  self.soundlatch_port := $A8;
  self.configregion := $C;
  copymemory(@self.internal_ram, @deco_104ram, sizeof(deco_104ram));
end;

end.
