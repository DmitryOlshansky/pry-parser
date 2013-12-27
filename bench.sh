#!/bin/sh
for _ in 1 2 3 4 5 
do
echo "buffer-range"
./dgrep "relay=([0-9a-zA-Z\-\.]+[0-9a-zA-Z]+)[\.\,]*\s"  sample.txt
echo "std-c-by-line"
./relay_hosts sample.txt
done