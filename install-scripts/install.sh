#!/usr/bin/env bash

# Link logger
source logger.sh

# Installing useful packages
log_info "Installing various packages necessary for Clingo 5.4.0."
sudo apt-get install bison re2c scons gcc libtbb-dev python2.7-dev lua5.2-dev wget

log_info "Installing Visual Studio (also necessary for Clingo 5.4.0.)"
#sudo apt update
sudo apt install software-properties-common apt-transport-https wget
wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main"
#sudo apt update
sudo apt install code

# Check if external dependencies directory exists
if [ ! -d ../dependencies ]; then
    log_info "Dependencies directory not found. Creating ..."
    mkdir ../dependencies
    cd ../dependencies

    # Installing clingo
    log_info "Installing clingo 5.4.0"
    mkdir clingo
    cd clingo
    mkdir build
    b="$PWD/build"
    wget https://github.com/potassco/clingo/archive/v5.4.0.tar.gz
    tar -zxf v5.4.0.tar.gz
    rm v5.4.0.tar.gz
    cd clingo-5.4.0
    cmake -H$PWD -B$b -DCMAKE_BUILD_TYPE=Release
    cmake --build $b
    cd ../..

    # Installing LoMRF
    log_info "Installing LoMRF."
    wget https://github.com/anskarl/LoMRF/archive/develop.zip
    wget http://users.iit.demokritos.gr/~nkatz/oled/lpsolve55.tar.xz
    unzip develop
    tar xf lpsolve55.tar.xz
    cd LoMRF-develop
    sbt +publishLocal
    cd ..
    rm develop.zip
    rm lpsolve55.tar.xz
    cd ..
else
    log_warn "tools directory exists! Moving on."
fi

# Get version from 'version.sbt'
version=`/bin/grep "^version[ ][ ]*in[ ][ ]*ThisBuild[ ][ ]*:=[ ][ ]*" "../version.sbt" | sed 's/version[ ][ ]*in[ ][ ]*ThisBuild[ ][ ]*:=[ ][ ]*\"\(.*\)\"/\1/g'`

log_info "Building ORL ${version} ..."
cd ..
sbt assembly
#mv target/scala-2.11/oled-${version}.jar .
log_info "Done building ORL. The jar is located at: `pwd`/target/scala-2.12/orl-${version}-SNAPSHOT.jar"
