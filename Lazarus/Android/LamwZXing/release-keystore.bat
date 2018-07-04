set JAVA_HOME=
set PATH=%JAVA_HOME%\bin;%PATH%
set JAVA_TOOL_OPTIONS=-Duser.language=en
keytool -genkey -v -keystore LamwZXing-release.keystore -alias lamwzxingaliaskey -keyalg RSA -keysize 2048 -validity 10000 < C:\Users\Alfred\Documents\GitHub\ZXing.Delphi\Lazarus\AndroidOrig\assets\LamwZXing\keytool_input.txt
