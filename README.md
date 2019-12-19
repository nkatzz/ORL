## Refactoring code

Refactoring old code for learning to simplify and keep the basics as linrary for other projects. 

## Installation

You'll need to have Scala 2.12 with SBT (Scala Build Tool), JDK 8, and Python 2.7 installed on your machine.

* Clone or download the source code:
  * `git clone https://github.com/nkatzz/OLED.git`
* `cd OLED/install-scripts`
* `./install.sh`
* Update your `PATH`, `PYTHONPATH` and `LD_LIBRARY_PATH`. Append the following lines to your `~/.profile` (or `~/.bashrc`) file:
  * `LD_LIBRARY_PATH=<PATH TO CLONED SOURCE>/OLED/external_dependencies/lpsolve55:$LD_LIBRARY_PATH`  
  * `export LD_LIBRARY_PATH`
  * `export PATH=$PATH:<PATH TO CLONED SOURCE>/OLED/external_dependencies/LoMRF/target/universal/LoMRF-0.5.5-SNAPSHOT/bin:<PATH TO CLONED SOURCE>/OLED/external_dependencies/clingo/clingo-4.5.4-source/build/release`
  * `export PYTHONPATH=$PYTHONPATH:<PATH TO CLONED SOURCE>/OLED/external_dependencies/clingo/clingo-4.5.4-source/build/release/python`

## Data

Detailed instructions on how to perform a test run with ``OLED`` are provided in the manual. Please refer to the manual for details on the data and the learning task we'll use for this test run. In sort:

* Install MongoDB.
* Download some data and some background knowledge at some location: 
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/caviar-data.zip`
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/bk.zip`
* `unzip caviar-data.zip`
* `unzip bk.zip`
* `cd caviar-data`
* Import the data into Mongo:
   * `mongoimport --db caviar-train --collection examples --file caviar-train.json`
   * `mongoimport --db caviar-test --collection examples --file caviar-test.json`
* Make sure everything is ok (after axecuting the `show dbs` command you should see the newly-created dbs 'caviar-train' and 'caviar test'):
   * `mongo`
   * `show dbs`
