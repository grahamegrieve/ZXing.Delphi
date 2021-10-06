{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ZXing;

{$warn 5023 off : no warning about unused units}
interface

uses
  ZXing.ScanManager, ZXing.OneD.Code39Reader, ZXing.OneD.Code93Reader, 
  ZXing.OneD.Code128Reader, ZXing.OneD.EAN8Reader, ZXing.OneD.EAN13Reader, 
  ZXing.OneD.EANManufacturerOrgSupport, ZXing.OneD.ITFReader, 
  ZXing.OneD.OneDReader, ZXing.OneD.UPCAReader, 
  ZXing.OneD.UPCEANExtension2Support, ZXing.OneD.UPCEANExtension5Support, 
  ZXing.OneD.UPCEANExtensionSupport, ZXing.OneD.UPCEANReader, 
  ZXing.OneD.UPCEReader, ZXing.BarCodeFormat, ZXing.BitSource, 
  ZXing.ByteSegments, ZXing.CharacterSetECI, ZXing.Common.BitArray, 
  ZXing.Common.BitArrayImplementation, ZXing.Common.BitMatrix, 
  ZXing.Common.DetectorResult, ZXing.Common.GridSampler, 
  ZXing.Common.PerspectiveTransform, ZXing.DecodeHintType, 
  ZXing.DecoderResult, ZXing.DefaultGridSampler, ZXing.EncodeHintType, 
  ZXing.Helpers, ZXing.MultiFormatReader, ZXing.Reader, ZXing.ReadResult, 
  ZXing.ResultMetadataType, ZXing.ResultPoint, 
  ZXing.ResultPointImplementation, ZXing.StringUtils, 
  ZXing.Common.ReedSolomon.GenericGF, 
  ZXing.Common.ReedSolomon.ReedSolomonDecoder, 
  ZXing.Common.Detector.MathUtils, 
  ZXing.Common.Detector.WhiteRectangleDetector, ZXing.BaseLuminanceSource, 
  ZXing.Binarizer, ZXing.BinaryBitmap, ZXing.GlobalHistogramBinarizer, 
  ZXing.HybridBinarizer, ZXing.InvertedLuminanceSource, ZXing.LuminanceSource, 
  ZXing.RGBLuminanceSource, ZXing.Datamatrix.DataMatrixReader, 
  ZXing.QrCode.QRCodeReader, ZXing.Datamatrix.Internal.BitMatrixParser, 
  ZXing.Datamatrix.Internal.DataBlock, 
  ZXing.Datamatrix.Internal.DecodedBitStreamParser, 
  ZXing.Datamatrix.Internal.Decoder, ZXing.Datamatrix.Internal.Version, 
  ZXing.QrCode.Internal.BitMatrixParser, ZXing.QrCode.Internal.DataBlock, 
  ZXing.QrCode.Internal.DataMask, 
  ZXing.QrCode.Internal.DecodedBitStreamParser, ZXing.QrCode.Internal.Decoder, 
  ZXing.QrCode.Internal.ErrorCorrectionLevel, 
  ZXing.QrCode.Internal.FormatInformation, ZXing.QrCode.Internal.Mode, 
  ZXing.QrCode.Internal.QRCodeDecoderMetaData, ZXing.QrCode.Internal.Version, 
  ZXing.Datamatrix.Internal.Detector, ZXing.QrCode.Internal.AlignmentPattern, 
  ZXing.QrCode.Internal.AlignmentPatternFinder, 
  ZXing.QrCode.Internal.AlignmentPatternImplementation, 
  ZXing.QrCode.Internal.Detector, ZXing.QrCode.Internal.FinderPattern, 
  ZXing.QrCode.Internal.FinderPatternFinder, 
  ZXing.QrCode.Internal.FinderPatternImplementation, 
  ZXing.QrCode.Internal.FinderPatternInfo, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ZXing', @Register);
end.
