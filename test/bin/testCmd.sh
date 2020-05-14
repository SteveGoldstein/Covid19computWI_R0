#!/bin/bash

## 4 test cases:  Usage test/bin/testCmd.sh test/model| test/output
dir=$1

### defaults
i=0;
Rscript --vanilla bin/computeWI_R_0.R -outFile $dir/fromFile.$i.csv -inFile test/input/sw.csv  1> $dir/fromFile.$i.out 2> $dir/fromFile.$i.err &

#### only current R0  
i=1;
Rscript --vanilla bin/computeWI_R_0.R  -current TRUE -outFile $dir/fromFile.$i.csv -inFile test/input/sw.csv  1> $dir/fromFile.$i.out 2> $dir/fromFile.$i.err &

#### exclude state  
i=2;
Rscript --vanilla bin/computeWI_R_0.R -aggregateLabel none -outFile $dir/fromFile.$i.csv -inFile test/input/sw.csv  1> $dir/fromFile.$i.out 2> $dir/fromFile.$i.err &


#### only current R0; exclude state  
i=3;
Rscript --vanilla bin/computeWI_R_0.R -aggregateLabel none -current TRUE -outFile $dir/fromFile.$i.csv -inFile test/input/sw.csv  1> $dir/fromFile.$i.out 2> $dir/fromFile.$i.err &

#### only current R0; include state;  -verbose and -plotFile 
i=4;
Rscript --vanilla bin/computeWI_R_0.R -aggregateLabel Wisconsin -current TRUE -outFile $dir/fromFile.$i.csv -inFile test/input/sw.csv  -verbose TRUE -plotFile $dir/fromFile.$i.pdf 1> $dir/fromFile.$i.out 2> $dir/fromFile.$i.err &

