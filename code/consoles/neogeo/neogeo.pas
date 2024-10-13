unit NeoGeo;

interface

uses
  SysUtils, Math, Windows;

var
  nNeoProtectionXor: Integer = -1;

function NeoLoadCode(nOffset, nNum: Integer; pDest: PByte): Integer;
procedure NeoSVCAddressDecrypt(src, dst: PByte; start, &end: Integer);
procedure NeoKOFAddressDecrypt(src, dst: PByte; start, &end: Integer);
procedure NeoPCBDataDecrypt(dst: PByte; size: Integer);
function NeoLoadSprites(nOffset, nNum: Integer; pDest: PByte; nSpriteSize: Cardinal): Integer;
procedure NeoDecodeSprites(pDest: PByte; nSize: Integer);
procedure NeoClearScreen;

implementation

uses
  Burn;

procedure NeoLoadCode(nOffset, nNum: Integer; pDest: PByte): Integer;
var
  ri: TBurnRomInfo;
  i, j: Integer;
begin
  for i := 0 to nNum - 1 do
  begin
    ri.nLen := 0;
    BurnDrvGetRomInfo(@ri, nOffset + i);

    if ((BurnDrvGetHardwareCode() and HARDWARE_SNK_P32) <> 0) and (i = 0) then
    begin
      if BurnLoadRom(pDest + 0, nOffset + i + 0, 2) then
        Exit(1);
      if BurnLoadRom(pDest + 1, nOffset + i + 1, 2) then
        Exit(1);

      for j := 0 to (ri.nLen shl 1) - 1 do
        BurnByteswap(pDest + j + 1, 2);

      Inc(i);
      Inc(pDest, ri.nLen shl 1);
      Continue;
    end;

    if BurnLoadRom(pDest, nOffset + i, 1) then
      Exit(1);

    if ((BurnDrvGetHardwareCode() and HARDWARE_SNK_SWAPP) <> 0) and (i = 0) then
    begin
      for j := 0 to (ri.nLen div 2) - 1 do
      begin
        var k := pDest[j];
        pDest[j] := pDest[j + (ri.nLen div 2)];
        pDest[j + (ri.nLen div 2)] := k;
      end;
    end;

    Inc(pDest, ri.nLen);
  end;

  Result := 0;
end;

procedure NeoSVCAddressDecrypt(src, dst: PByte; start, &end: Integer);
var
  i: Integer;
begin
  for i := start div 4 to (&end) div 4 - 1 do
    PDWORD(dst)[i] := PDWORD(src)^[(i and $FFE00000) or (0x0C8923 xor BITSWAP24((i and $1FFFFF), $17, $16, $15, $04, $0B, $0E, $08, $0C, $10, $00, $0a, $13, $03, $06, $02, $07, $0D, $01, $11, $09, $14, $0f, $12, $05))];
end;

procedure NeoKOFAddressDecrypt(src, dst: PByte; start, &end: Integer);
var
  i, j: Integer;
begin
  for i := start to &end - 1 do
  begin
    j := i div $100;
    Move(src^[(i and $FF800000) or BURN_ENDIAN_SWAP_INT16((BITSWAP16((j and $7FFF), $0F, $0A, $0E, $0C, $0B, $09, $08, $07, $06, $05, $04, $03, $02, $0D, $01, $00)) shl 8)], dst^[(j shl 8)], $100);
  end;
end;

procedure NeoPCBDataDecrypt(dst: PByte; size: Integer);
var
  i: Integer;
begin
  for i := 0 to size div 4 - 1 do
    PDWORD(dst)[i] := BURN_ENDIAN_SWAP_INT32(BITSWAP32($E9C42134 xor BURN_ENDIAN_SWAP_INT32(PDWORD(dst)[i]), $09, $0D, $13, $00, $17, $0F, $03, $05, $04, $0C, $11, $1E, $12, $15, $0B, $06, $1B, $0A, $1A, $1C, $14, $02, $0e, $1D, $18, $08, $01, $10, $19, $1F, $07, $16));
end;

function NeoLoadSprites(nOffset, nNum: Integer; pDest: PByte; nSpriteSize: Cardinal): Integer;
var
  ri: TBurnRomInfo;
  nRomSize, nBuf1Len: Cardinal;
  pBuf1, pBuf2: PByte;
  i, j: Integer;
begin
  nRomSize := 0;

  if (BurnDrvGetHardwareCode() and (HARDWARE_SNK_CMC42 or HARDWARE_SNK_CMC50)) <> 0 then
  begin
    pBuf1 := nil;
    pBuf2 := nil;

    BurnDrvGetRomInfo(@ri, nOffset);
    nRomSize := ri.nLen;

    if (BurnDrvGetHardwareCode() and HARDWARE_SNK_CMC42) <> 0 then
      NeoCMC42Init()
    else
    if (BurnDrvGetHardwareCode() and HARDWARE_SNK_CMC50) <> 0 then
      NeoCMC50Init();

    nBuf1Len := nRomSize shl 1;
    if bDoIpsPatch then
      Inc(nBuf1Len, nIpsMemExpLen[GRA1_ROM]);

    pBuf1 := PByte(BurnMalloc(nBuf1Len));
    if pBuf1 = nil then
      Exit(1);

    if (BurnDrvGetHardwareCode() and HARDWARE_PUBLIC_MASK) = HARDWARE_SNK_DEDICATED_PCB then
    begin
      pBuf2 := PByte(BurnMalloc(nRomSize * 2));
      if pBuf2 = nil then
        Exit(1);
    end;

    for i := 0 to (nNum shr 1) - 1 do
    begin
      if (BurnDrvGetHardwareCode() and HARDWARE_PUBLIC_MASK) = HARDWARE_SNK_DEDICATED_PCB then
      begin
        if nRomSize = $02000000 then
        begin
          BurnLoadRom(pBuf2 + 0 * nRomSize, nOffset + (i shl 1), 1);
          BurnLoadRom(pBuf2 + 1 * nRomSize, nOffset + 1 + (i shl 1), 1);
        end
        else
        if nRomSize = $01000000 then
        begin
          BurnLoadRom(pBuf2 + 0 * nRomSize, nOffset + (i shl 1), 2);
          BurnLoadRom(pBuf2 + 1 * nRomSize, nOffset + 2 + (i shl 1), 2);
        end
        else
          BurnLoadRom(pBuf2 + i * nRomSize, nOffset + i, 1);
      end;

      if bDoIpsPatch then
      begin
        if (BurnDrvGetHardwareCode() and HARDWARE_SNK_DEDICATED_PCB) = HARDWARE_SNK_DEDICATED_PCB then
          FillMemory(pBuf1, nBuf1Len, 0);

        BurnLoadRom(pBuf1, nOffset + nNum + i, 1);

        if (BurnDrvGetHardwareCode() and HARDWARE_SNK_DEDICATED_PCB) = HARDWARE_SNK_DEDICATED_PCB then
        begin
          NeoCMC42Decrypt(pBuf1, nRomSize, nRomSize);
          for j := 0 to nRomSize - 1 do
            pBuf1[j] := pBuf1[j] xor pBuf2[j];
        end;

        DecodePSB(pBuf1, nRomSize);
      end
      else
      begin
        if (BurnDrvGetHardwareCode() and HARDWARE_SNK_DEDICATED_PCB) = HARDWARE_SNK_DEDICATED_PCB then
          NeoCMC42Decrypt(pBuf2 + (i shl 1) * nRomSize, nRomSize, nRomSize);

        if (BurnDrvGetHardwareCode() and HARDWARE_SNK_DEDICATED_PCB) = HARDWARE_SNK_DEDICATED_PCB then
          DecodePSB(pBuf2 + (i shl 1) * nRomSize, nRomSize)
        else
          DecodePSB(pBuf2 + i * nRomSize, nRomSize);
      end;

      if nSpriteSize = 16 then
      begin
        if (BurnDrvGetHardwareCode() and HARDWARE_SNK_DEDICATED_PCB) = HARDWARE_SNK_DEDICATED_PCB then
          pBuf1 := pBuf2 + (i shl 1) * nRomSize
        else
          pBuf1 := pBuf2 + i * nRomSize;

        for j := 0 to nRomSize div 4 - 1 do
          PDWORD(pDest)^[(i * nRomSize) shr 2 + j] := BURN_ENDIAN_SWAP_INT32(BITSWAP32(PDWORD(pBuf1)[j], $01, $00, $03, $02));
      end
      else
      begin
        if (BurnDrvGetHardwareCode() and HARDWARE_SNK_DEDICATED_PCB) = HARDWARE_SNK_DEDICATED_PCB then
          pBuf1 := pBuf2 + (i shl 1) * nRomSize
        else
          pBuf1 := pBuf2 + i * nRomSize;

        for j := 0 to nRomSize div 2 - 1 do
          PWord(pDest)^[(i * nRomSize) shr 1 + j] := BURN_ENDIAN_SWAP_INT16(BITSWAP16(PWord(pBuf1)[j], $01, $00, $03, $02));
      end;
    end;

    if pBuf1 <> nil then
      BurnFree(pBuf1);
    if pBuf2 <> nil then
      BurnFree(pBuf2);
  end
  else
    Exit(1);

  Result := 0;
end;

procedure NeoDecodeSprites(pDest: PByte; nSize: Integer);
var
  i: Integer;
begin
  for i := 0 to nSize div 128 - 1 do
    DecodeGfx(pDest + i * 128, 128);
end;

procedure NeoClearScreen;
begin
  ClearSurface();
end;

end.
