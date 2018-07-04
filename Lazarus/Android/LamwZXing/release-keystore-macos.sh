export JAVA_HOME=${/usr/libexec/java_home}
export PATH=${JAVA_HOME}/bin:$PATH
keytool -genkey -v -keystore LamwZXing-release.keystore -alias lamwzxingaliaskey -keyalg RSA -keysize 2048 -validity 10000 < /Users/Alfred/Documents/GitHub/ZXing.Delphi/Lazarus/AndroidOrig/assets/LamwZXing/lamwzxingkeytool_input.txt
