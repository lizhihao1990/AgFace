#!/bin/bash

cd ~/AgFace/2015/Campbell_logger/logger_data
sed 's/SYS3/SYS7/g' SYS7_Hourly.dat > tmp
rm SYS7_Hourly.dat
mv tmp SYS7_Hourly.dat

