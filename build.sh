build_number=${1?enter a build number}

rm -rf nextstep/Emacs.app
make install

rm -f nextstep/Emacs.app/Contents/MacOS/bin/*
rm -f nextstep/Emacs.app/Contents/MacOS/libexec/*

cp "IRC Channel Control.icns" "nextstep/Emacs.app/Contents/Resources/Emacs.icns"
xattr -cr nextstep/Emacs.app

sed 's|<string>26.0.50</string>|<string>'$build_number'</string>|g;
s|<string>org.gnu.Emacs</string>|<string>ca.busfactor1.irc-channel-control</string>|g;
s|<string>Emacs 26.0.50 Copyright (C) 2017 Free Software Foundation, Inc.</string>|<string>BusFactor1 Inc - 2017 based on Emacs 26.0.50 Copyright (C) 2017 Free Software Foundation, Inc.</string>|g;
s|^</dict>|<key>LSApplicationCategoryType</key><string\>public.app-category.social-networking</string>\
<key>LSMinimumSystemVersion</key><string>10.12</string></dict>|'  "nextstep/Emacs.app/Contents/Info.plist" > "nextstep/Emacs.app/Contents/Info.plist,2"
mv "nextstep/Emacs.app/Contents/Info.plist,2" "nextstep/Emacs.app/Contents/Info.plist"

sed '/CFBundleShortVersionString/d;/NSHumanReadableCopyright/d' nextstep/Emacs.app/Contents/Resources/English.lproj/InfoPlist.strings > nextstep/Emacs.app/Contents/Resources/English.lproj/InfoPlist.strings,2
mv nextstep/Emacs.app/Contents/Resources/English.lproj/InfoPlist.strings,2 nextstep/Emacs.app/Contents/Resources/English.lproj/InfoPlist.strings

