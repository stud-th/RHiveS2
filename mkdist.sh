#!/bin/sh
PNAME=RHiveS2
SWD=`pwd`
echo "Removing previous dist ..."
rm -rf /tmp/${PNAME}
echo "Copying package base ..."
cp -r ../${PNAME} /tmp

cd /tmp/${PNAME}

echo "Removing CVS and backup stuff ..."
find . -name CVS -o -name .svn | xargs rm -rf
find . -name \*~ | xargs rm -f

jtarget=1.4
jver=`java -version 2>&1 | sed -n 's:.*java version "::p' | sed 's:".*::'`
if test -z "$jver"; then
    echo "*** WARNING: cannot derermine Java version!"
else
    if echo "$jver" | grep ^9 > /dev/null; then
	echo "NOTE: Java 1.9 detected, changing target to 1.6"
	jtarget=1.6
    fi
fi

mkdir -p inst/java
javac -d inst/java -source $jtarget -target $jtarget java/*.java || exit 1
(cd inst/java; jar fvc RJDBC.jar info; rm -rf info)

echo "Updating version ..."
VER=`./version`
echo "$PNAME version ${VER}"
#cat DESCRIPTION| sed "s/Version:.*/Version: ${VER}/" > d
#mv d DESCRIPTION

rm -f "/tmp/${PNAME}/mkdist" "/tmp/${PNAME}/version" "/tmp/${PNAME}/test.sh"

echo "Creating package ..."
cd ..
R CMD build "${PNAME}"
cd ${SWD}
cp /tmp/${PNAME}_${VER}.tar.gz ..
rm -rf /tmp/${PNAME}
echo "Done."
ls -l ../${PNAME}_${VER}.tar.gz
