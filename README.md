# TeLL
Online Temporal Logical Learning


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

## Test Run

Detailed instructions on how to perform a test run with ``OLED`` are provided in the manual. Please refer to the manual for details on the data and the learning task we'll use for this test run. In sort:

* Install MongoDB.
* Download some data and some background knowledge at some location: 
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/caviar-data.zip`
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/caviar-bk.zip`
* `unzip caviar-data.zip`
* `unzip caviar-bk.zip`
* `cd caviar-data`
* Import the data into Mongo:
   * `mongoimport --db caviar-train --collection examples --file caviar-train.json`
   * `mongoimport --db caviar-test --collection examples --file caviar-test.json`
* Make sure everything is ok (after axecuting the `show dbs` command you should see the newly-created dbs 'caviar-train' and 'caviar test'):
   * `mongo`
   * `show dbs`
* Run ``OLED``:
   <!--
   * `java -cp oled.jar app.runners.OLEDDefaultRunner \`  <br/>
     ` --inpath=/oledhome/caviar-bk \` <br/>
     `--delta=0.00001 \` <br/>
     `--prune=0.8 \` <br/>
     `--target=meeting \` <br/>
     `--train=caviar-train \` <br/>
     `--saveto=/oledhome/theory.lp`
   -->
   * `java -cp oled-0.1.jar app.runners.OLEDDefaultRunner --inpath=<PATH TO THE caviar-bk FOLDER> --delta=0.00001 --prune=0.8 --train=caviar-train --test=caviar-test --saveto=<PATH TO SOME LOCATION>/theory.lp`
* The above command learns a theory from `caviar-train` and evaluates it on `caviar-test`. The theory and the evaluation statistics are printed on screen once larning terminates. The learnt theory is also written in `theory.lp`. To assess the quality of any theory (either learnt or hand-crafted) on a test set you can use the following command (here we're just assumming that we're evaluating the learnt theory in `theory.lp` on the downloaded test set `caviat-test`):   
   <!--
   * `java -cp oled.jar app.runners.OLEDDefaultRunner \`  <br/>
         ` --inpath=/oledhome/caviar-bk \` <br/>
         `--target=meeting \` <br/>
         `--test=caviar-test \` <br/>
         `--evalth=/home/nkatz/oledhome/theory.lp`
   -->
   * `java -cp oled-0.1.jar app.runners.OLEDDefaultRunner --inpath=<PATH TO THE caviar-bk FOLDER> --test=caviar-test --evalth=<PATH TO theory.lp FILE>` 
* You may see all available cmd args with `java -cp oled-0.1.jar -help`

