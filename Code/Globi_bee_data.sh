################################## 
# Author: Katja C. Seltmann
# Email: enicospilus@gmail
# Date: November, 2020
# Purpose: Download data from https://www.globalbioticinteractions.org/ and select rows based on taxonomy or word match
# Data: https://doi.org/10.5281/zenodo.3950590
# GloBI citation: Jorrit H. Poelen, James D. Simons and Chris J. Mungall. (2014). Global Biotic Interactions: An open infrastructure to share and analyze species-interaction datasets. Ecological Informatics. https://doi.org/10.1016/j.ecoinf.2014.08.005.
# Run in command line from globi_tritrophic_networks/Code folder: sh Globi_bee_data.sh
################################## 

################################## 
#PARTS:
# 1. Manually download data from Zenodo. This is a version of the live database. https://doi.org/10.5281/zenodo.3950590
# 2. Using grep, move all records with bee families to a new file. Bee families are Andrenidae, Apidae, Colletidae, Halictidae, Megachilidae, Melittidae, Stenotritidae creating a *_data.tsv file for each family
# 3. Sort unique rows creating a *_data_unique.tsv file for each family
################################## 

echo Creating headers
head -1 ../Data/interactions.tsv > ../Data/raw_globi_data/Andrenidae_data.tsv
head -1 ../Data/interactions.tsv > ../Data/raw_globi_data/Apidae_data.tsv
head -1 ../Data/interactions.tsv > ../Data/raw_globi_data/Colletidae_data.tsv
head -1 ../Data/interactions.tsv > ../Data/raw_globi_data/Halictidae_data.tsv
head -1 ../Data/interactions.tsv > ../Data/raw_globi_data/Megachilidae_data.tsv
head -1 ../Data/interactions.tsv > ../Data/raw_globi_data/Melittidae_data.tsv
head -1 ../Data/interactions.tsv > ../Data/raw_globi_data/Stenotritidae_data.tsv

#find all Andrenidae and write one file with all data and a second file only with unique records

echo Finding all Andrenidae
cat ../Data/interactions.tsv | grep -w "Andrenidae" >> ../Data/raw_globi_data/Andrenidae_data.tsv
wc -l ../Data/raw_globi_data/Andrenidae_data.tsv

echo Sorting unique records
sort -r ../Data/raw_globi_data/Andrenidae_data.tsv | uniq > ../Data/raw_globi_data/Andrenidae_data_unique.tsv
wc -l ../Data/raw_globi_data/Andrenidae_data_unique.tsv
#####################################
#find all Apidae and write one file with all data and a second file only with unique records

echo Finding all Apidae
cat ../Data/interactions.tsv | grep -w "Apidae" >> ../Data/raw_globi_data/Apidae_data.tsv
wc -l ../Data/raw_globi_data/Apidae_data.tsv

echo Sorting unique records
sort -r ../Data/raw_globi_data/Apidae_data.tsv | uniq > ../Data/raw_globi_data/Apidae_data_unique.tsv
wc -l ../Data/raw_globi_data/Apidae_data_unique.tsv
#####################################
#find all Colletidae and write one file with all data and a second file only with unique records

echo Finding all Colletidae
cat ../Data/interactions.tsv | grep -w "Colletidae" >> ../Data/raw_globi_data/Colletidae_data.tsv
wc -l ../Data/raw_globi_data/Colletidae_data.tsv

echo Sorting unique records
sort -r ../Data/raw_globi_data/Colletidae_data.tsv | uniq > ../Data/raw_globi_data/Colletidae_data_unique.tsv
wc -l ../Data/raw_globi_data/Colletidae_data_unique.tsv
#####################################
#find all Halictidae and write one file with all data and a second file only with unique records

echo Finding all Halictidae
cat ../Data/interactions.tsv | grep -w "Halictidae" >> ../Data/raw_globi_data/Halictidae_data.tsv
wc -l ../Data/raw_globi_data/Halictidae_data.tsv

echo Sorting unique records
sort -r ../Data/raw_globi_data/Halictidae_data.tsv | uniq > ../Data/raw_globi_data/Halictidae_data_unique.tsv
wc -l ../Data/raw_globi_data/Halictidae_data_unique.tsv
#####################################
#find all Megachilidae and write one file with all data and a second file only with unique records

echo Finding all Megachilidae
cat ../Data/interactions.tsv | grep -w "Megachilidae" >> ../Data/raw_globi_data/Megachilidae_data.tsv
wc -l ../Data/raw_globi_data/Megachilidae_data.tsv

echo Sorting unique records
sort -r ../Data/raw_globi_data/Megachilidae_data.tsv | uniq >../Data/raw_globi_data/Megachilidae_data_unique.tsv
wc -l ../Data/raw_globi_data/Megachilidae_data_unique.tsv
#####################################
#find all Melittidae and write one file with all data and a second file only with unique records

echo Finding all Melittidae
cat ../Data/interactions.tsv | grep -w "Melittidae" >> ../Data/raw_globi_data/Melittidae_data.tsv
wc -l ../Data/raw_globi_data/Melittidae_data.tsv

echo Sorting unique records
sort -r ../Data/raw_globi_data/Melittidae_data.tsv | uniq > ../Data/raw_globi_data/Melittidae_data_unique.tsv
wc -l ../Data/raw_globi_data/Melittidae_data_unique.tsv
#####################################
#find all Stenotritidae and write one file with all data and a second file only with unique records

echo Finding all Stenotritidae
cat ../Data/interactions.tsv | grep -w "Stenotritidae" >> ../Data/raw_globi_data/Stenotritidae_data.tsv
wc -l ../Data/raw_globi_data/Stenotritidae_data.tsv

echo Sorting unique records
sort -r ../Data/raw_globi_data/Stenotritidae_data.tsv | uniq > ../Data/raw_globi_data/Stenotritidae_data_unique.tsv
wc -l ../Data/raw_globi_data/Stenotritidae_data_unique.tsv

#####################################
#create one large bee file
cat ../Data/raw_globi_data/*unique.tsv >> ../Data/raw_globi_data/all_bee_data.txt
sort -r ../Data/raw_globi_data/all_bee_data.txt | uniq > ../Data/raw_globi_data/all_bee_data_unique.txt
#remove unsorted file
rm ../Data/raw_globi_data/all_bee_data.txt
wc -l ../Data/raw_globi_data/all_bee_data_unique.txt


#get list of unique interaction types
cat ../Data/raw_globi_data/all_bee_data_unique.txt | cut -f 35 | sort -u > ../Data/raw_globi_data/interaction_types.txt

