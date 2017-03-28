version=${1?enter a version}
appname="Emacs Lisp Programming Environment (ELPE).app"

rm -rf nextstep/"${appname}"
make -j17 install


cp -a nextstep/Emacs.app/ nextstep/"${appname}"/

cp "ELPE.icns" "nextstep/${appname}/Contents/Resources/Emacs.icns"

cp nextstep/Cocoa/Emacs.base/Contents/Info.plist nextstep/"${appname}"/Contents/Info.plist
gsed -i "s|ELPE_VERSION|$version|" nextstep/"${appname}"/Contents/Info.plist

xattr -cr nextstep/"${appname}"

rm -f nextstep/"${appname}"/Contents/MacOS/bin/*
rm -f nextstep/"${appname}"/Contents/MacOS/libexec/*
