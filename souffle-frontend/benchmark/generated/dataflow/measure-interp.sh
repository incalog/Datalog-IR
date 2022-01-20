
#!/bin/bash

cd $1
for i in {1..10}
do 
    time souffle analysis.dl;
done
