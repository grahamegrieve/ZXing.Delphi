{Hint: save all files to location: C:\Users\Alfred\Documents\GitHub\ZXing.Delphi\Lazarus\AndroidOrig\assets\LamwZXing\jni }
unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, And_jni, intentmanager,
  activitylauncher;
  
type

  { TAndroidModule1 }

  TAndroidModule1 = class(jForm)
    jActivityLauncher1: jActivityLauncher;
    jButton1: jButton;
    jIntentManager1: jIntentManager;
    jListView1: jListView;
    procedure AndroidModule1ActivityResult(Sender: TObject;
      requestCode: integer; resultCode: TAndroidResult; intentData: jObject);
    procedure jButton1Click(Sender: TObject);
  private
    {private declarations}
    aBarCode:string;
  public
    {public declarations}
  end;

var
  AndroidModule1: TAndroidModule1;

implementation
  
{$R *.lfm}
  

{ TAndroidModule1 }

procedure TAndroidModule1.AndroidModule1ActivityResult(Sender: TObject;
  requestCode: integer; resultCode: TAndroidResult; intentData: jObject);
begin
  if (requestCode=2) then  //user def code... for barcode scanned
  begin
    if (resultCode=RESULT_OK) AND (Assigned(intentData)) then  //ok
    begin
      // this is important : setclass !!!!
      jIntentManager1.SetClass('com.google.zxing.client.android', 'CaptureActivity');
      aBarCode:=jIntentManager1.GetExtraString(intentData, 'SCAN_RESULT');
      jListView1.Add(aBarCode);
      jListView1.SetItemChecked(jListView1.Count-1, True);
      jListView1.SmoothScrollToPosition(jListView1.Count-1);
    end else ShowMessage('Fail/cancel to scan....');
  end;
end;

procedure TAndroidModule1.jButton1Click(Sender: TObject);
var
  aIntent:jObject;
begin
  jIntentManager1.SetClass('com.google.zxing.client.android', 'CaptureActivity');
  jIntentManager1.SetAction('com.google.zxing.client.android.SCAN');
  jIntentManager1.PutExtraString('PROMPT_MESSAGE', 'Scanning for machine barcode');
  jIntentManager1.PutExtraString('SAVE_HISTORY', 'false');
  aIntent:=jIntentManager1.GetIntent();
  jActivityLauncher1.StartActivityForResult(aIntent, 2);
end;

end.
