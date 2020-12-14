#!/usr/bin/env bash
# prepare dir
rm -rf data/*
mkdir -p data

# download data
wget -O data/cmc.csv https://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data
wget -O data/wine.csv https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv
wget -O data/occupancy.zip https://archive.ics.uci.edu/ml/machine-learning-databases/00357/occupancy_data.zip
wget -O data/zoo.csv https://archive.ics.uci.edu/ml/machine-learning-databases/zoo/zoo.data
wget -O data/diabetes.csv http://nrvis.com/data/mldata/pima-indians-diabetes.csv

# prepare data
sed -i '1d' data/wine.csv
sed -i 's/;/,/g' data/wine.csv

unzip data/occupancy.zip -d data/
sed -i '1d' data/*.txt
cat data/*.txt >> data/occupancy.csv
rm data/*.txt
rm data/occupancy.zip
cut -d, -f1-2 --complement data/occupancy.csv >> data/occupancy_temp.csv
mv data/occupancy_temp.csv data/occupancy.csv

cut -d, -f1 --complement data/zoo.csv >> data/zoo_temp.csv
mv data/zoo_temp.csv data/zoo.csv
