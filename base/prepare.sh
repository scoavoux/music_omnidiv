#!/bin/bash

# Reproducible way to unzip all data. Takes forever.
# expect bzip2 warnings "Can't guess original name for temp"

tar -xf DataSet_D_Orange.tar
cd orange

# for each folder
for d in *.bz2
do
  # move to folder
  cd $d
  # concatenate all data files
  cat *.bz2 > temp
  # remove parts
  rm -f *.bz2
  # unzip data
  bzip2 -d temp
  # moves unzipped file to parent
  # directory and rename to .csv
  c="${d%.*}"
  mv temp.out "../$c.csv"
  cd ..
  # remove folder
  rm -rf $d
done

echo "all done"
