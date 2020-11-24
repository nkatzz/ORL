#!/usr/bin/env bash

# For the script to work properly pay attention to version numbers.
# version in the commonSettings var of TRAILBuild.scala (e.g version := "0.1-SNAPSHOT")
# must be the same as the version in number in ../version.sbt

source logger.sh

version=`/bin/grep "^version[ ][ ]*in[ ][ ]*ThisBuild[ ][ ]*:=[ ][ ]*" "../version.sbt" | sed 's/version[ ][ ]*in[ ][ ]*ThisBuild[ ][ ]*:=[ ][ ]*\"\(.*\)\"/\1/g'`
log_info "Building TRAIL ${version} ..."

if [ -d ../target/universal ]; then
    rm -r ../target/universal
fi

cd ../
sbt dist
cd target/universal
unzip -q trail-${version}-SNAPSHOT.zip




