# PRM-birth-cohort

Following the steps below can guide you through cross validation of the CA PRM birth model using your birth records and CPS records. 

### Step 1: Birth data prep
Prepare birth data by constructing those variables below.

<img src="https://user-images.githubusercontent.com/17417861/167277252-54d372e3-6c8e-489b-8f26-f70fec52ddf4.png" width="700">


### Step 2: Link CPS records
Link your CPS records to this birth dataset at the child level. You need the first placement start date ('PE_S_DT' in "%Y%m%d" format) and a child's birth date ('BIRTH_DT').

Additionally, create the below variables from CPS records.
(1) investigated (yes / no)
(2) date of first ever investigation
(3) substantiated (yes / no)
(4) date of first ever substantiation
(5) placed in foster care (yes / no)
(6) date of first ever placement in foster care
(7) date of first ever referral if possible

### Step 2
Download all the files in the same folder.
Then, run 'PRM_coding_features_cleaned_Dec2021.R'. This creates predicting and target features.
Don't forget to change 'setwd("working directory")' to your current working directory.

### Step 3
Run 'scoreNewDatasetCAModel_Mar2022.R' to generate prediction scores for each child. Again, change 'setwd("working directory")' to your current working directory.
