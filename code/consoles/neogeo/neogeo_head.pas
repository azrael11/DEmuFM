unit NeoGeo;

interface

uses
  SysUtils, Math, Windows;

const
  MAX_SLOT = 8;

  NEO_SYS_CD = 1 shl 4;
  NEO_SYS_PCB = 1 shl 3;
  NEO_SYS_CART = 1 shl 2;
  NEO_SYS_AES = 1 shl 1;
  NEO_SYS_MVS = 1 shl 0;

type
  NeoGameInfo = record
    nCodeOffset: Integer;
    nCodeNum: Integer;
    nTextOffset: Integer;
    nSpriteOffset: Integer;
    nSpriteNum: Integer;
    nSoundOffset: Integer;
    nADPCMOffset: Integer;
    nADPCMANum: Integer;
    nADPCMBNum: Integer;
    nNeoSRAMProtection: Integer;
  end;

  NeoReallocInfo = record
    nCodeSize: Integer;
    nTextSize: Integer;
    nSpriteSize: Integer;
    nADPCMASize: Integer;
    nADPCMBSize: Integer;
  end;

  NEO_CALLBACK = record
    pInitialise: procedure;
    pResetCallback: procedure;
    pInstallHandlers: procedure;
    pRemoveHandlers: procedure;
    pBankswitch: procedure;
    pScan: function(nAction, pnMin: Integer): Integer;
  end;

var
  nNeoScreenWidth: Integer;

  NeoCallback: array[0..MAX_SLOT - 1] of NEO_CALLBACK;
  NeoCallbackActive: ^NEO_CALLBACK;

  pNRI: ^NeoReallocInfo;
  NeoGraphicsRAM: PByte;
  nNeoNumSlots: Byte;
  nNeoActiveSlot: UInt32;
  NeoButton1: array[0..31] of Byte;
  NeoButton2: array[0..31] of Byte;
  NeoButton3: array[0..31] of Byte;
  NeoButton4: array[0..31] of Byte;
  NeoJoy1: array[0..31] of Byte;
  NeoJoy2: array[0..31] of Byte;
  NeoJoy3: array[0..31] of Byte;
  NeoJoy4: array[0..31] of Byte;
  NeoAxis: array[0..31] of UInt16;
  NeoInput: array[0..31] of Byte;
  NeoDiag: array[0..31] of Byte;
  NeoDebugDip: array[0..7] of Byte;
  NeoReset, NeoSystem, NeoCDBios: Byte;
  Neo68KROMActive: PByte;
  NeoVectorActive: PByte;
  NeoZ80ROMActive: PByte;
  YM2610ADPCMAROM: array[0..MAX_SLOT - 1] of PByte;
  Neo68KFix: array[0..MAX_SLOT - 1] of PByte;
  nNeo68KROMBank: UInt32;
  nAllCodeSize: UInt32;
  NeoSpriteRAM, NeoTextRAM: PByte;
  bNeoEnableGraphics: Boolean;
  bDisableNeoWatchdog: Boolean;
  s1945pmode: Integer;
  cphdmode: Integer;
  fatfury2mode: Integer;
  vlinermode: Integer;

function NeoInit: Integer;
function NeoCDInit: Integer;
function NeoExit: Integer;
function NeoScan(nAction, pnMin: Integer): Integer;
function NeoRender: Integer;
function NeoFrame: Integer;

procedure NeoMapBank;
procedure NeoMap68KFix;
procedure NeoUpdateVector;

function NeoInitPalette: Integer;
procedure NeoExitPalette;
function NeoUpdatePalette: Integer;
procedure NeoSetPalette;

procedure NeoPalWriteByte(nAddress: UInt32; byteValue: Byte);
procedure NeoPalWriteWord(nAddress: UInt32; wordValue: UInt16);

function NeoInitText(nSlot: Integer): Integer;
procedure NeoSetTextSlot(nSlot: Integer);
procedure NeoExitText(nSlot: Integer);
function NeoRenderText: Integer;

procedure NeoDecodeTextBIOS(nOffset: Integer; const nSize: Integer; pData: PByte);
procedure NeoDecodeText(nOffset: Integer; const nSize: Integer; pData, pDest: PByte);
procedure NeoUpdateTextOne(nOffset: Integer; const byteValue: Byte);
procedure NeoUpdateText(nOffset: Integer; const nSize: Integer; pData, pDest: PByte);

procedure NeoUpdateSprites(nOffset, nSize: Integer);
procedure NeoSetSpriteSlot(nSlot: Integer);
function NeoInitSprites(nSlot: Integer): Integer;
procedure NeoExitSprites(nSlot: Integer);
function NeoRenderSprites: Integer;
procedure NeoSpriteCalcLimit;

procedure NeoCMC42Init;
procedure NeoCMC50Init;
procedure NeoCMCDecrypt(extra_xor: Byte; rom, buf: PByte; offset, block_size, rom_size: Integer);
procedure NeoCMCExtractSData(rom, sdata: PByte; rom_size, sdata_size: Integer);

procedure neogeo_cmc50_m1_decrypt;

procedure uPD4990AExit;
procedure uPD499ASetTicks(nTicksPerSecond: UInt32);
function uPD4990AInit(nTicksPerSecond: UInt32; cpu_totcyc_callback: function: Integer): Integer;
procedure uPD4990ANewFrame(nOverflowTicks: Integer);
procedure uPD4990AScan(nAction: Integer; pnMin: PInteger);
procedure uPD4990AUpdate;
procedure uPD4990AWrite(CLK, STB, DATA: Byte);
function uPD4990ARead: Byte;

procedure kf2k3pcb_bios_decode;

implementation

// Include other units here

end.
