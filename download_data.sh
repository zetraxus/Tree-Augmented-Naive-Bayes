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
cat data/*.txt >> data/occupancy.csv
sed -i '1d' data/occupancy.csv
rm data/*.txt
rm data/occupancy.zip
