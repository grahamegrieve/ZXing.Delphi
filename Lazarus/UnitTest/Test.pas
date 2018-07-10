unit Test;

{$IFDEF FPC}
  {$mode delphi}{$H+}
  //{$CODEPAGE UTF8}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$ifdef GUI}
  Forms,
  StdCtrls,
  {$endif}
  fpcunit, testregistry,
  ExtCtrls, Graphics,
  Generics.Collections,
  fpimage,
  ZXing.ReadResult,
  ZXing.ResultPoint,
  ZXing.BarCodeFormat,
  ZXing.DecodeHintType,
  ZXing.ScanManager;

type
  TZXingLazarusTest = class(TTestCase)
    private
      {$ifdef GUI}
      aImage:TImage;
      aMemo:TMemo;
      {$endif}
      procedure IsNull(AObject: TObject; const AMessage: string);
      procedure IsNotNull(AObject: TObject; const AMessage: string);
      procedure IsTrue(ACondition: boolean; const AMessage: string);
      procedure AreEqual(Expected, Actual: string; something:boolean);
      procedure Contains(HayStack,Needle: string; something:boolean);
      function GetImage(Filename: string): TBitmap;
      function Decode(out aResult:TReadResult; const Filename: String; const CodeFormat: TBarcodeFormat;
               const additionalHints: THints = nil)
               : boolean;
    public
      {$ifdef GUI}
      constructor Create(Image:TImage;Memo:TMemo);overload;
      {$endif}
    published
      procedure AllCode39();
      procedure AllUpcA();
      procedure AllUpcE;
      procedure AllQRCode;
      procedure All_PURE_QRCode;
      procedure AllCode128();
      procedure AllCode93();
      procedure AllCodeITF;
      procedure AllCodeEAN8;
      procedure AllCodeEAN13;
      procedure AutoTypes();
      procedure AllDataMatrixCode();
  end;

implementation

procedure TZXingLazarusTest.AllQRCode;
var
  aScanresult: TReadResult;
  success:boolean;
begin
  try
    success := Decode(aScanResult,'qr1.png', TBarcodeFormat.QR_CODE);
    if success then
    begin
      IsNotNull(aScanresult, ' Nil result ');
      IsTrue(aScanresult.Text.Equals(
       'SPD*1.0*ACC:CZ2301000000001010101010+KOMBCZPP*AM:12100.00*CC'+
       ':CZK*DT:20160721*MSG:20160033 FIRMA  A.S.*X-VS:20160033*X-KS'+
       ':0308*X-INV:SID%2A1.0%2AID:20160033%2ADD:20160707%2AMSG:Konz'+
       'ultace 07/2016%2AON:2016/44%2AVII:CZ12345678%2AINI:12345678%'+
       '2AVIR:CZ25568736%2AINR:25568736%2ADUZP:20160701%2ADPPD:20160'+
       '701%2ATB0:10000.00%2AT0:2100.00%2AX-SW:iDoklad*'),
       'QR code result Text Incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;

    {success := Decode(aScanResult,'qr2.png', TBarcodeFormat.QR_CODE);
    if success then
    begin
      IsNotNull(aScanresult, ' Nil result ');
      IsTrue(aScanresult.Text.Equals(''),
       'QR code result Text Incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;}

    success := Decode(aScanResult,'qrcode.png', TBarcodeFormat.QR_CODE);
    if success then
    begin
      IsNotNull(aScanresult, ' Nil result ');
      IsTrue(aScanresult.Text.Equals('http://google.com'),
       'QR code result Text Incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;

   // From here a test set from: http://datagenetics.com/blog/november12013/index.html
   // Please take a look of what QR can do for you
   // NOTE: some test are expected to fail and are setup as such.

   // !Cancelled for test. Does not work with zxing.net either.
   // Rotation does not work.
   // success := Decode(aScanResult,'q3.png', TBarcodeFormat.QR_CODE);
   // if success then
   // begin
   //   IsNotNull(aScanresult, ' Nil result ');
   //   IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
   //   'QR code result Text Incorrect: ' + aScanresult.Text);
   //   FreeAndNil(aScanresult);
   // end;

   success := Decode(aScanResult,'q33.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('Never gonna give you up, ' + #$0A +
      'Never gonna let you down ' + #$0A +
      'Never gonna run around and desert you ' + #$0A +
      'Never gonna make you cry, ' + #$0A + 'Never gonna say goodbye ' + #$0A +
      'Never gonna tell a lie and hurt you'), 'QR code result Text Incorrect: '
      + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q1.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q2.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q2q.png', TBarcodeFormat.QR_CODE); // rotate 90 degrees
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q2m.png', TBarcodeFormat.QR_CODE); // rotate 120 degrees
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q4.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q5.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     // fails on example website but does work!
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q6.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q7.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q8.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q9.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNull(aScanresult, ' Should be nil result. Missing possition block ');
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q10.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q11.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNull(aScanresult, ' The code should not scan ');
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q12.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q13.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q14.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q15.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q16.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q17.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNull(aScanresult, ' Should not scan ');
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q18.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q21.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q22.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q23.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNull(aScanresult, ' to dizzy to scan');
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q25.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNull(aScanresult, 'Should not scan');
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q28.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q29.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNull(aScanresult, 'Should not scan');
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q30.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNull(aScanresult, 'Should not be scanned');
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q31.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
       IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'q32.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'qr-1.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('1'), 'QR code result Text Incorrect: ' +
        aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'qr-a1.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('a1'), 'QR code result Text Incorrect: ' +
        aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'qr-1a.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('1a'), 'QR code result Text Incorrect: ' +
        aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'qr-12.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('12'), 'QR code result Text Incorrect: ' +
        aScanresult.Text);
     FreeAndNil(aScanresult);
   end;


   success := Decode(aScanResult,'QRHiddenInBottom.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'big QR.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Contains
        ('Version 40 QR Code can contain up to 1852 chars.'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'CarloTest.jpg', TBarcodeFormat.QR_CODE);
   if success then begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Contains('gov.it'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'QR_Droid_2663.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     IsTrue(aScanresult.Text.Contains('Version 40 QR Code'),
        'QR code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'utf8-test.png', TBarcodeFormat.QR_CODE);
   if success then begin
     IsNotNull(aScanresult, ' Nil result ');
     AreEqual
        (#$0440#$0443#$0301#$0441#$0441#$043A#$0438#$0439#$20#$044F#$0437#$044B#$0301#$043A#$2C#$20'russkij'#$20'jazyk'#$20#$E8#$E0#$F2#$F9,
        aScanresult.Text, false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'contact information.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, 'Joe@bloggs.com', false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'Calendar.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, 'Christmas', false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'GeoLocation.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, '52.052490', false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'SMS.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, '0777777', false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'url.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, 'meetheed.com', false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'email.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, 'joe@bloggs.com', false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'Phone.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, '077777777', false);
     FreeAndNil(aScanresult);
   end;

   success := Decode(aScanResult,'Text.png', TBarcodeFormat.QR_CODE);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ');
     Contains(aScanresult.Text, 'just a lot of plain text', false);
     FreeAndNil(aScanresult);
   end;

  except
    FreeAndNil(aScanresult);
  end;
end;

// This ones will only work when PURE_BARCODE is existing in the additional hints.
procedure TZXingLazarusTest.All_PURE_QRCode();
var
  aScanresult: TReadResult;
  success:boolean;
  hints: THints;
begin
  try
    hints := THints.Create();
    hints.Add(TDecodeHintType.PURE_BARCODE, nil);
    success := Decode(aScanResult,'qr problem 1.jpg', TBarcodeFormat.QR_CODE, hints);
    if success then
    begin
      IsNotNull(aScanresult, ' Nil result ');
      IsTrue(aScanresult.Text.Contains('gov.it/'),
         'QR code result Text Incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;
  finally
    FreeAndNil(aScanresult);
  end;
end;

procedure TZXingLazarusTest.AllUpcA;
var
   aScanresult: TReadResult;
   success:boolean;
begin
   try
      success := Decode(aScanResult,'upca.png', TBarcodeFormat.UPC_A);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('123456789012'),
           'upca result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upcaHiddenInBottom.png', TBarcodeFormat.UPC_A);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('123456789012'),
           'upca result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upca 2.gif', TBarcodeFormat.UPC_A);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('725272730706'),
           'upca 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upca 3.gif', TBarcodeFormat.UPC_A);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('232323232312'),
           'upca 2 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

procedure TZXingLazarusTest.AllUpcE;
var
   aScanresult: TReadResult;
   success:boolean;
begin
   try
      success := Decode(aScanResult,'upce.png', TBarcodeFormat.UPC_E);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('01234565'), 'upce result Text Incorrect: '
           + aScanresult.Text);
         FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upceHiddenInBottom.png', TBarcodeFormat.UPC_E);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('01234565'), 'upce result Text Incorrect: '
           + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upc-e_09999008.png', TBarcodeFormat.UPC_E);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('09999008'), 'upce result Text Incorrect: '
           + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upc-e_09999992.png', TBarcodeFormat.UPC_E);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('09999992'), 'upce result Text Incorrect: '
           + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upce 2.png', TBarcodeFormat.UPC_E);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('01234565'), 'upce result Text Incorrect: '
           + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

procedure TZXingLazarusTest.AllCode39;
var
  aScanresult: TReadResult;
  success:boolean;
begin
  try
    success := Decode(aScanResult,'code39.png', TBarcodeFormat.CODE_39);
    if success then
    begin
      IsNotNull(aScanresult, ' nil result ');
      IsTrue(aScanresult.Text.Equals('1234567'),
         'Code 39 result Text incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;

    success := Decode(aScanResult,'code39 ABC 123456789.png', TBarcodeFormat.CODE_39);
    if success then
    begin
      IsNotNull(aScanresult, ' nil result ');
      IsTrue(aScanresult.Text.Equals('ABC 123456789'),
         'Code 39 result Text incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;

    success := Decode(aScanResult,'code39 Hello World.png', TBarcodeFormat.CODE_39);
    if success then
    begin
      IsNotNull(aScanresult, ' nil result ');
      IsTrue(aScanresult.Text.Equals('HELLO $WORLD$'),
         'Code 39 result Text incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;

    success := Decode(aScanResult,'code39HiddenInBottom.png', TBarcodeFormat.CODE_39);
    if success then
    begin
      IsNotNull(aScanresult, ' nil result ');
      IsTrue(aScanresult.Text.Equals('HELLO $WORLD$'),
         'Code 39 result Text incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;

    success := Decode(aScanResult,'Code 39 Axtel.png', TBarcodeFormat.CODE_39);
    if success then
    begin
      IsNotNull(aScanresult, ' nil result ');
      IsTrue(aScanresult.Text.Contains('AXTEL'),
         'Code 39 result Text incorrect: ' + aScanresult.Text);
      FreeAndNil(aScanresult);
    end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

procedure TZXingLazarusTest.AllDataMatrixCode();
var
  aScanresult: TReadResult;
  success:boolean;
  aFile:string;
begin
 try

   aFile:='dmc1.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('http://www.2D-IDent.com'),
        'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmwikirotated.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('Wikipédia, l''encyclopédie libre'),
      'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc2.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('Beispiel f'#$FC'r Wikipedia'),
     //IsTrue(aScanresult.Text.Equals('Beispiel f#252r Wikipedia'),
     //IsTrue(aScanresult.Text.Equals('Beispiel für Wikipedia'),
     'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc3.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
      // 'Wikip�dia, l''encyclop�die libre':
      // I escaped the above string because this source is not saved in UTF-8 format but in ascii, using the west european encoding
      // if your don't have a west-european windows installation, all the editors you use would try to interpret the non-ascii characters
      // whith the actual local charset. if this is the case you will not see the accented letters in this comment
      // ASCII charset, you risk the compiler to generate the wrong utf8 string when compiling this source
      // ES: 2016/12/9 not working here. Checking on text instead.
     IsTrue(aScanresult.Text.Equals('Wikipédia, l''encyclopédie libre'),
     //IsTrue(aScanresult.Text.Contains('die libre'),
        'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc4.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals(
       '1234567812345678123456781234567812345123456781234567812345678123'+
       '4567812345123456781234567812345678123456781234512345678123456781'+
       '2345678123456781234512345678123456781234567812345678123451234567'+
       '8123456781234567812345678123451234567812345678123456781234567812'+
       '3451234567812345612317821323112312213121231231123121231212312312'+
       '3123123123123123212312312312312sdfgssdfsdf3423423ffwewerwerwwerw'+
       '3r3rw3r12'),
     'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc5.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('Pause Hi-Tech' + #$0A +
        'Tech tips for the non-geek' + #$0A + 'http://www.pausehitech.com'),
        'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc6.bmp';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('12345678'),
        'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc7.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('DataMatrix'),
        'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc8.jpg';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('http://www.labeljoy.com'),
        'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc9.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' +
         'Test123Test123Test123Test123Test123' + 'Test123'),
         'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='dmc8.jpg';
   success := Decode(aScanResult,aFile, TBarcodeFormat.Auto);
   if success then
   begin
     IsNotNull(aScanresult, ' Nil result ' + aFile);
     IsTrue(aScanresult.Text.Equals('http://www.labeljoy.com'),
        'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
     FreeAndNil(aScanresult);
   end;

   aFile:='Code128.png';
   success := Decode(aScanResult,aFile, TBarcodeFormat.DATA_MATRIX);
   if success then
   begin
     IsNull(aScanresult, ' Should be Nil result ' + aFile);
     FreeAndNil(aScanresult);
   end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

procedure TZXingLazarusTest.AllCode128();
var
  aScanresult: TReadResult;
  success:boolean;
  x0, x1, y0, y1: single;
begin
   try
      success := Decode(aScanResult,'Code128.png', TBarcodeFormat.CODE_128);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code128red.png', TBarcodeFormat.CODE_128);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code128HiddenInBottom.png', TBarcodeFormat.CODE_128);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result: Code128HiddenInBottom');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code128HiddenInTop.png', TBarcodeFormat.CODE_128);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result: Code128HiddenInTop');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'code128 upsidedown.png', TBarcodeFormat.CODE_128);
      if success then begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);

        x0 := aScanResult.resultPoints[0].x;
        y0 := aScanResult.resultPoints[0].y;

        x1 := aScanResult.resultPoints[1].x;
        y1 := aScanResult.resultPoints[1].y;
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'code128 upsidedownchidden in bottom.png', TBarcodeFormat.CODE_128);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);

        x0 := aScanResult.resultPoints[0].x;
        y0 := aScanResult.resultPoints[0].y;

        x1 := aScanResult.resultPoints[1].x;
        y1 := aScanResult.resultPoints[1].y;
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;

end;

procedure TZXingLazarusTest.AllCode93();
var
  aScanresult: TReadResult;
  success:boolean;
begin
   try
      success := Decode(aScanResult,'Code93-1.png', TBarcodeFormat.CODE_93);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('THIS IS CODE93'),
           'Code 93 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code93-2.png', TBarcodeFormat.CODE_93);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('ABC CODE93-2'),
           'Code 93 - 2 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code93-3.png', TBarcodeFormat.CODE_93);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('ABC CODE96'),
           'Code 93 - 3 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code93-3.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('ABC CODE96'),
           'Auto Code 93 - 3 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

procedure TZXingLazarusTest.AllCodeITF();
var
  aScanresult: TReadResult;
  success:boolean;
begin
   try
      success := Decode(aScanResult,'q4.png', TBarcodeFormat.ITF);
      if success then
      begin
        IsNull(aScanresult, ' Should be nil result ');
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'ITF-1.png', TBarcodeFormat.ITF);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('55867492279103'),
           'Code ITF - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'ITF-2.png', TBarcodeFormat.ITF);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('04601234567893'),
           'ITF - 2 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'ITF-3.png', TBarcodeFormat.ITF);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('12345678900098'),
           'ITF - 3 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'ITF-4.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('32145678900098'),
           'ITF - 4 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'ITF-5.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('77745678900093'),
           'ITF - 5 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

procedure TZXingLazarusTest.AllCodeEAN13();
var
  aScanresult: TReadResult;
  success:boolean;
begin
   try
      success := Decode(aScanResult,'ean13.gif', TBarcodeFormat.EAN_13);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('1234567890128'),
           'Code EAN13 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'EAN13-2-big-hidden in bottom.png', TBarcodeFormat.EAN_13);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('1234567890128'),
           'Code EAN13 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'EAN13-2-big-hidden in top.png', TBarcodeFormat.EAN_13);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('1234567890128'),
           'Code EAN13 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

procedure TZXingLazarusTest.AllCodeEAN8;
var
  aScanresult: TReadResult;
  success:boolean;
begin
   try
      success := Decode(aScanResult,'ean8.png', TBarcodeFormat.EAN_8);
      if success then begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('12345670'),
           'Code EAN8 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'EAN8-big-hidden in top.png', TBarcodeFormat.EAN_8);
      if success then begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('12345670'),
           'Code EAN8 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'EAN8-big-hidden in bottom.png', TBarcodeFormat.EAN_8);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('12345670'),
           'Code EAN8 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'EAN8 12345670.png', TBarcodeFormat.EAN_8);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('12345670'),
           'Code EAN8 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;

end;

procedure TZXingLazarusTest.AutoTypes;
var
  aScanresult: TReadResult;
  success:boolean;
begin
   try
      success := Decode(aScanResult,'Code128.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code93-1.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('THIS IS CODE93'),
           'Code 93 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code128.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('1234567'),
           'Code 128 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'q4.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
           'QR code result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'q2.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
           'QR code result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code93-1.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('THIS IS CODE93'),
           'Code 93 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'Code93-1.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('THIS IS CODE93'),
           'Code 93 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'q2.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('http://DataGenetics.com'),
           'QR code result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'ITF-2.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('04601234567893'),
           'ITF - 2 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'ITF-3.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('12345678900098'),
           'ITF - 3 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'dmc7.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' Nil result ');
        IsTrue(aScanresult.Text.Equals('DataMatrix'),
           'DataMatrix code result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upca.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('123456789012'),
           'upca result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'upce.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('01234565'), 'upce result Text Incorrect: '
           + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

      success := Decode(aScanResult,'EAN13-2-big-hidden in bottom.png', TBarcodeFormat.Auto);
      if success then
      begin
        IsNotNull(aScanresult, ' nil result ');
        IsTrue(aScanresult.Text.Equals('1234567890128'),
           'Code EAN13 - 1 result Text Incorrect: ' + aScanresult.Text);
        FreeAndNil(aScanresult);
      end;

   finally
      FreeAndNil(aScanresult);
   end;
end;

/// /////////////////////////////////////////////////////////////////////////////
/// / Helpers below                                                         /////
/// /////////////////////////////////////////////////////////////////////////////

function TZXingLazarusTest.Decode(out aResult:TReadResult; const Filename: String;
         const CodeFormat: TBarcodeFormat; const additionalHints: THints = nil): boolean;
var
   bmp: TBitmap;
   ScanManager: TScanManager;
   ResultPoint:IResultPoint;
begin
   result:=false;
   bmp := GetImage(Filename);
   if Assigned(bmp) then
   try
      result:=true;
      ScanManager := TScanManager.Create(CodeFormat, additionalHints);
      aResult := ScanManager.Scan(bmp);
      {$ifdef GUI}
      if Assigned(aMemo) then
      begin
        if Assigned(aResult) then
        begin
          aMemo.Lines.Append(aResult.text);
          aMemo.Invalidate;
          {$ifdef Debug}
          if (aResult.rawBytes=nil) AND ((aResult.BarcodeFormat=TBarcodeFormat.DATA_MATRIX) OR (aResult.BarcodeFormat=TBarcodeFormat.QR_CODE))  then
          {$else}
          if false then
          {$endif}
          bmp.Canvas.Brush.Color := clRed else
          begin
            bmp.Canvas.Brush.Color := clLime;
          end;
          bmp.Canvas.Brush.Style := bsSolid;
          bmp.Canvas.Pen.Width   := 1;
          bmp.Canvas.Pen.Color   := clBlack;
          for ResultPoint in aResult.ResultPoints do
          bmp.Canvas.Ellipse(TRect.Create(Round(ResultPoint.x - 5),
                                             Round(ResultPoint.y - 5),
                                             Round(ResultPoint.x + 5),
                                             Round(ResultPoint.y + 5)));
        end;
        if Assigned(aImage) then
        begin
          aImage.Picture.Bitmap.Assign(bmp);
          aImage.Invalidate;
        end;
      end;
      Application.ProcessMessages;
      sleep(100);
      {$endif}

   finally
      FreeAndNil(bmp);
      FreeAndNil(ScanManager);
   end;
end;

{$ifdef GUI}
constructor TZXingLazarusTest.Create(Image:TImage;Memo:TMemo);
begin
  inherited Create;
  aImage:=Image;
  aMemo:=Memo;
end;
{$endif}

function TZXingLazarusTest.GetImage(Filename: string): TBitmap;
var
  img: TImage;
  //fpimg:TFPMemoryImage;
  fs: string;
begin
  result:=nil;
  img := TImage.Create(nil);
  //fpimg:=TFPMemoryImage.create(0,0);
  //fpimg.UsePalette:=false;
  try
    {$ifdef Windows}
    fs := ExtractFileDir(ParamStr(0)) + '\..\..\..\UnitTest\Images\' + Filename;
    {$else}
    fs := ExtractFileDir(ParamStr(0)) + '/../../../UnitTest/Images/' + Filename;
    {$endif}
    if NOT FileExists(fs) then
    begin
      {$ifdef Windows}
      fs := ExtractFileDir(ParamStr(0)) + '\Images\' + Filename;
      {$else}
      fs := ExtractFileDir(ParamStr(0)) + '/Images/' + Filename;
      {$endif}
    end;
    if FileExists(fs) then
    begin
      try
        //fpimg.LoadFromFile(fs);
        img.Picture.LoadFromFile(fs);
        result := TBitmap.Create;
        //result.PixelFormat := pf32bit;
        result.Assign(img.Picture.Bitmap);
      except
        on E:FPImageException do
        begin
          {$ifndef GUI}
          writeln('Please note: FPC could not handle/load file '+ExtractFileName(fs)+'.');
          writeln('Tests with this file will be skipped.');
          FreeAndNil(result);
          {$endif}
        end;
      end;
    end;
  finally
    img.Free;
    //fpimg.Free;
  end;
end;

procedure TZXingLazarusTest.IsNull(AObject: TObject; const AMessage: string);
begin
  TAssert.AssertNull(AMessage,AObject);
end;
procedure TZXingLazarusTest.IsNotNull(AObject: TObject; const AMessage: string);
begin
  TAssert.AssertNotNull(AMessage,AObject);
end;
procedure TZXingLazarusTest.IsTrue(ACondition: boolean; const AMessage: string);
begin
  TAssert.AssertTrue(AMessage,ACondition);
end;
procedure TZXingLazarusTest.AreEqual(Expected, Actual: string; something:boolean);
begin
  TAssert.AssertEquals(Expected, Actual);
end;
procedure TZXingLazarusTest.Contains(HayStack,Needle: string; something:boolean);
var
  aCondition:boolean;
begin
  aCondition:=(Pos(Needle,HayStack)>0);
  TAssert.AssertTrue(ACondition);
end;

initialization
  RegisterTest(TZXingLazarusTest);

end.
