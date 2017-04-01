version=$(tail -1 version.current)
build_number=$(tail -1 build-number.current)

#version=${1?enter a version}
#build_number=${2?enter a build number}

appname="Emacs Lisp Programming Environment (ELPE).app"

rm -rf nextstep/"${appname}"
make -j17 install


cp -a nextstep/Emacs.app/ nextstep/"${appname}"/

cp "ELPE.icns" "nextstep/${appname}/Contents/Resources/Emacs.icns"

cp Info.plist nextstep/"${appname}"/Contents/Info.plist

inplace_sed () {
    expr="$1"
    file="$2"

    sed -e "$expr" "$file" > "$file",2 && mv "$file",2 "$file"
}


inplace_sed "s|ELPE_IDENTIFIER|ca.busfactor1.emacs|" "nextstep/${appname}/Contents/Info.plist"
inplace_sed  "s|ELPE_VERSION|$version|" "nextstep/${appname}/Contents/Info.plist"
inplace_sed "s|BUILD_NUMBER|$build_number|" "nextstep/${appname}/Contents/Info.plist"

rm -f nextstep/"${appname}"/Contents/Resources/English.lproj/InfoPlist.strings

xattr -cr nextstep/"${appname}"

rm -f nextstep/"${appname}"/Contents/MacOS/bin/*
rm -f nextstep/"${appname}"/Contents/MacOS/libexec/*

cp urbit-0.4.3/bin/urbit nextstep/"${appname}"/Contents/MacOS
cp urbit-0.4.3/urbit.pill nextstep/"${appname}"/Contents/MacOS
