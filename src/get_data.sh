#!/bin/bash
cd ../data
wget https://s3.amazonaws.com/tripdata/201501-citibike-tripdata.zip
# wget https://s3.amazonaws.com/tripdata/201506-citibike-tripdata.zip
# wget https://s3.amazonaws.com/tripdata/201510-citibike-tripdata.zip
 for z in *-citibike-tripdata.zip; do unzip $z; done
