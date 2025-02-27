unit ZXing.ScanManager;

{$IFDEF FPC}
  {$mode delphi}{$H+}
  {$define USE_VCL_BITMAP}
{$ENDIF}

interface

uses
  SysUtils,
  Generics.Collections,
  {$ifndef FPC}
{$IFDEF USE_VCL_BITMAP}
  VCL.Graphics,
{$ELSE}
  FMX.Graphics,
{$ENDIF}
  {$else}
  Graphics,
  {$endif}
  ZXing.LuminanceSource,
  ZXing.RGBLuminanceSource,
  //ZXing.InvertedLuminanceSource,
  ZXing.HybridBinarizer,
  ZXing.BinaryBitmap,
  ZXing.MultiFormatReader,
  ZXing.BarCodeFormat,
  ZXing.ResultPoint,
  ZXing.ReadResult,
  ZXing.DecodeHintType;

type
  TScanManager = class
  private
    FEnableInversion: Boolean;
    FHints: THints;
    FResultPointEvent: TResultPointCallback;
    FMultiFormatReader: TMultiFormatReader;
    listFormats: TBarcodeFormatList;

    function GetMultiFormatReader(const format: TBarcodeFormat)
      : TMultiFormatReader;

    procedure SetResultPointEvent(const AValue: TResultPointCallback);
  public
    constructor Create(const format: TBarcodeFormat; Hints: THints);
    destructor Destroy; override;

    function Scan(const pBitmapForScan: TBitmap): TReadResult;

    property OnResultPoint: TResultPointCallback read FResultPointEvent
      write SetResultPointEvent;
  end;

implementation

constructor TScanManager.Create(const format: TBarcodeFormat;
  Hints: THints);
begin
  inherited Create;
  FEnableInversion := False;
  FHints := Hints;
  FMultiFormatReader := GetMultiFormatReader(format);
end;

destructor TScanManager.Destroy;
var
  hint: TPair<TDecodeHintType, TDecodeHintValue>;
  o: TObject;
begin
  if Assigned(FHints) then
  begin

    for hint in FHints do
    begin
      o := hint.Value;
      if Assigned(o) then
        o.Free;
      o := nil;
    end;

    FHints.Clear();
    FreeAndNil(FHints);
  end;

  FreeAndNil(FMultiFormatReader);
  inherited;
end;

procedure TScanManager.SetResultPointEvent(const AValue: TResultPointCallback);
var
  ahKey: TDecodeHintType;
  ahValue: TObject;
begin
  FResultPointEvent := AValue;

  ahKey := TDecodeHintType.NEED_RESULT_POINT_CALLBACK;
  if Assigned(FResultPointEvent) then
  begin
    ahValue := TResultPointEventObject.Create(FResultPointEvent);
    FHints.AddOrSetValue(ahKey, ahValue);
  end
  else
  begin
    if Fhints.TryGetValue(ahKey, ahValue) then
    begin
      ahValue.Free;
      ahValue := nil;
      FHints.Remove(ahKey);
    end;
  end;
end;

function TScanManager.GetMultiFormatReader(const format: TBarcodeFormat)
  : TMultiFormatReader;
var
  o: TObject;

begin
  Result := TMultiFormatReader.Create;
  listFormats := nil;

  if FHints = nil then
    FHints := THints.Create();

  if FHints.ContainsKey(ZXing.DecodeHintType.ENABLE_INVERSION) then
    FEnableInversion := true;

  if format <> {$IFNDEF FPC}TBarcodeFormat{$ELSE}ZXing.BarCodeFormat{$ENDIF}.Auto then
  begin
    if (FHints.TryGetValue(ZXing.DecodeHintType.POSSIBLE_FORMATS, o)) then
      listFormats := o as TBarcodeFormatList
    else
    begin
      listFormats := TBarcodeFormatList.Create();
      FHints.Add(ZXing.DecodeHintType.POSSIBLE_FORMATS, listFormats);
    end;

    if (listFormats.Count = 0) then
      listFormats.Add(format)
    else
      listFormats.Insert(0, format); // favor the format parameter

  end;

  Result.Hints := FHints;
end;

function TScanManager.Scan(const pBitmapForScan: TBitmap): TReadResult;
var
  LuminanceSource, InvLuminanceSource: TLuminanceSource;
  HybridBinarizer: THybridBinarizer;
  BinaryBitmap: TBinaryBitmap;
begin
  InvLuminanceSource := nil;
  LuminanceSource := nil;
  HybridBinarizer := nil;
  BinaryBitmap := nil;
  try

    LuminanceSource := TRGBLuminanceSource.CreateFromBitmap(pBitmapForScan,
      pBitmapForScan.Width, pBitmapForScan.Height);
    HybridBinarizer := THybridBinarizer.Create(LuminanceSource);
    BinaryBitmap := TBinaryBitmap.Create(HybridBinarizer);
    Result := FMultiFormatReader.Decode(BinaryBitmap, true);

    if (Result = nil) then
    begin
      if (FEnableInversion) then
      begin
        if (BinaryBitmap <> nil) then
          FreeAndNil(BinaryBitmap);

        if (HybridBinarizer <> nil) then
          FreeAndNil(HybridBinarizer);

        InvLuminanceSource := LuminanceSource.invert();
        HybridBinarizer := THybridBinarizer.Create(InvLuminanceSource);
        BinaryBitmap := TBinaryBitmap.Create(HybridBinarizer);
        Result := FMultiFormatReader.Decode(BinaryBitmap, true);

      end;
    end;
  finally
    BinaryBitmap.Free;
    HybridBinarizer.Free;
    InvLuminanceSource.Free;
    LuminanceSource.Free;
  end;
end;

end.
