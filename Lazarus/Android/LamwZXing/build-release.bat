set Path=%PATH%;C:\Users\Public\Documents\Embarcadero\Studio\17.0\PlatformSDKs\apache-ant-1.10.1\bin
set JAVA_HOME=C:\Program Files\Java\jdk1.8.0_25
cd C:\Users\Alfred\Documents\GitHub\ZXing.Delphi\Lazarus\Android\LamwZXing\
call ant clean release
if errorlevel 1 pause
