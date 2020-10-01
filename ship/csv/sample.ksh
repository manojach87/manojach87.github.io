#pid=$$
#from=$1
#to=$2
#mv $2 $2.$pid
#head -1 $from 
#cat $from | awk 'BEGIN {srand()} !/^$/ { if (rand() <= .01) print $0}' 
(head -1 data_mod.org.csv && cat data_mod.org.csv | awk 'BEGIN {srand()} !/^$/ { if (rand() <= .04444) print $0}') > data_mod.csv


