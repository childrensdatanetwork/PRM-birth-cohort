# PRM-birth-cohort

Following the steps below can guide you through cross validation of the CA PRM birth model using your birth records and CPS records. 

### Step 1: Birth data prep
Select the birth variables and change the format as presented below.

<img src="https://user-images.githubusercontent.com/17417861/164547439-83e5ede5-2bbd-4cd6-a19a-9aa29bbe5ab5.png" width="700">

### Step 2: Link CPS records
Link your CPS records to this birth dataset at the child level. You need the first placement start date ('PE_S_DT' in "%Y%m%d" format) and a child's birth date ('BIRTH_DT').

### Step 2
Download all the files in the same folder.
Then, run 'PRM_coding_features_cleaned_Dec2021.R'. This creates predicting and target features.
Don't forget to change 'setwd("working directory")' to your current working directory.

### Step 3
Run 'scoreNewDatasetCAModel_Mar2022.R' to generate prediction scores for each child. 
