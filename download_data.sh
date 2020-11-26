rm -rf data/*
mkdir data/raw
wget -O data/raw/cmc.data https://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data
wget -O data/raw/wine.csv https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv
wget -O data/raw/occupancy.zip https://archive.ics.uci.edu/ml/machine-learning-databases/00357/occupancy_data.zip
wget -O data/raw/zoo.data https://archive.ics.uci.edu/ml/machine-learning-databases/zoo/zoo.data
wget -O data/raw/diabetes.csv http://nrvis.com/data/mldata/pima-indians-diabetes.csv
