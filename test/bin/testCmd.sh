#!/bin/bash

## 4 test cases:  Usage test/bin/testCmd.sh test/model| test/output
dir=$1

### defaults
i=0;
Rscript --vanilla bin/computeWI_R_0.R -outFile $dir/test.$i.csv -inFile test/input/sw.csv  1> $dir/test.$i.out 2> $dir/test.$i.err &

#### only current R0  
i=1;
Rscript --vanilla bin/computeWI_R_0.R  -current TRUE -outFile $dir/test.$i.csv -inFile test/input/sw.csv  1> $dir/test.$i.out 2> $dir/test.$i.err &

#### exclude state  
i=2;
Rscript --vanilla bin/computeWI_R_0.R -aggregateLabel none -outFile $dir/test.$i.csv -inFile test/input/sw.csv  1> $dir/test.$i.out 2> $dir/test.$i.err &


#### only current R0; exclude state  
i=3;
Rscript --vanilla bin/computeWI_R_0.R -aggregateLabel none -current TRUE -outFile $dir/test.$i.csv -inFile test/input/sw.csv  1> $dir/test.$i.out 2> $dir/test.$i.err &

#### only current R0; include state;  -verbose and -plotFile 
i=4;
Rscript --vanilla bin/computeWI_R_0.R -aggregateLabel Wisconsin -current TRUE -outFile $dir/test.$i.csv -inFile test/input/sw.csv  -verbose TRUE -plotFile $dir/test.$i.pdf 1> $dir/test.$i.out 2> $dir/test.$i.err &

