# ML_WB_longitudinal_CBCL_PGS_LGM

**Summary of the project:**

Combining multiple modalities of data for the prediction of wellbeing in machine learning models might lead to a more accurate prediction. This project aims to investigate the usefulness of longitudinal features of a specific aspect of an individual’s life history, namely childhood psychopathology, for machine learning-based prediction of adult wellbeing. Features derived from longitudinal trajectories of childhood psychopathology will be compared to polygenic risk scores for a variety of phenotypes and to cross-sectional features of childhood psychopathology ignoring the longitudinal aspect. It is expected that longitudinal features will be of high feature importance. 

Authors: Leitritz, D; Pool, R.; Ligthart, L.; Bartels, M.; Pelt, D.

Department of Biological Psychology; Vrije Universiteit Amsterdam; Amsterdam,. Noord-Holland; The Netherlands
July 2024


### Content repository
This repository contains all documents relevant to the analysis and is organized as follows:

- data contains only proof-of-concept calculations and otherwise falls under the gitignore
- doc contains all documentation of the study including legal documents and the pre-registration, furthermore visualizations based on the results of the analyses
- scripts contains all R, Python, Mplus and bash scripts that were used for the analysis

#### Important coding and variable information: 
- CBCL questions: 
    - -1 == missing
    - 0 = not true;
    - 1 = somewhat true;
    - 2 = very true
     (items *q13m5*; *q11m5* and *q4m5* were recoded to align with the coding)

- all questions have specific ending per age:
    - age 3 = **m3**
    - age 5 = **m5**
    - age 7 = **m7**
    - age 10 = **m10**
    - age 12 = **m12**
    - age 14 = **ysr14**
    - age 16 = **ysr16**

- sex: 3 unique codes
    -  -9 == known or suspected transgender;
    -  -2 == missing inconsistent data;
    -  -1 == missing no data;
    -  1 == male;
    -  2 == female*

- twzyg: zygosity twin 1 vs twin 2; 6 unique codes
    -  1 == MZM;
    -  2 == DZM;
    -  3 == MZF;
    -  4 == DZF;
    -  5 == DOSmf (dizygotic and different sex, participant is male);
    -  6 == DOSfm (dizygotic and different sex, participant is female)

- ea4fa_agg: father's educational attainment - 4 levels:
    -  -1 == missing;
    -  1 == lager onderwijs;
    -  2 == lager beroepsonderwijs (lbo) / middelbaar algemeen onderwijs (lavo, mavo);
    -  3 == middelbaar beroepsonderwijs (mbo) / voorgezet algemeen onderwijs (havo, vwo);
    -  4 == hoger beroepsonderwijs (hbo), wetenschappelijk onderwijs (wo)

- ea4mo_agg: mother's educational attainment - 4 levels:
    -  -1 == missing;
    -  1 == lager onderwijs;
    -  2 == lager beroepsonderwijs (lbo) / middelbaar algemeen onderwijs (lavo, mavo);
    -  3 == middelbaar beroepsonderwijs (mbo) / voorgezet algemeen onderwijs (havo, vwo);
    -  4 == hoger beroepsonderwijs (hbo), wetenschappelijk onderwijs (wo)

- invd8: date of completion wave 8 (**invd = invuldate**), can be used for age calculation and time lag

- invjrm3: Year of filling in survey wave 3 (by mother) (**invjrm = invuljaar moeder**)


#### Content scripts
The project contains the following areas of coding and analysis:
- Preprocessing / filtering / train-test-splitting
- Latent growth / longitudinal modeling
- Preparation and running machine learning models
- assessment of model stability
- model evaluation

(The following section is constantly being updated!)
Scripts contain the following operations (in the following order):
- 01 - 01_participantIDs.R
- 02 - 02_data_exploration.R
- 03 - 03_covariates.R
- 04 - 04_data_cleaning_filtering1.R
- 05 - 05_initial_split_training_test_data.R
- 06 - 06_longitudinal_features_no_LGM.R
- 06a - 06_a_autocorrelation.R
- 06b - 06_b_merge_nonLGM.R
- 07 - 07_LGM.R
- 08 - *old: 08a_ML_model_A.R (not needed anymore)* will be changed to **08_PCA_PRS_NTR.R**
- 09 - 09_0_run_bootstrap_stability.R
- (09a - Revision run bootstrap with further feature space shrinkage!) **Also add this at all other bootstrapping model steps**
- 10 - 10_inspection_bootstrap_stability_model_0.R
- 11 - *no filename yet* (run bootstrap stability model A)
- 12 - *no filename yet* (inspection bootstrap stability model A)
- 13 - *no filename yet, tentative depending on discussion if new training test split* (new training-test split, also bootstrap for PGS data, includes removing the genetic outliers!)
- 14 - *no filename yet* (run bootstrap stability model B: CBS only)
- 15 - *no filename yet* (inspect bootstrap stability model B)
- 16 - *no filename yet* (run bootstrap stability mode C: CBCL features + non-LGM CBCL features + PGS)
- ***16 - no filename yet* (this is still somewhat tricky, run bootstrap with LGM features, but when calculating these, only do it once with initial test set? Still to be discussed once Mplus code is completely ready, add to documentation!)***
- 17 - *no filename yet* (run bootstrap stability model D: raw CBCL scores + non-LGM features + LGM features, use same split as in 05_initial_split_training_test_data.R)
- 18 - *no filename yet* (inspect bootstrap stability model D)
- 19 - *no filename yet* (run bootstrap stability model E: all features)
- 20 - *no filename yet* (inspect bootstrap stability model E)
- 21 onwards: ML analyses, feature importances; alternative, after all stability checks are done!


#### Data availability
Requests for obtaining the raw data need to be sent directly to the NTR (https://ntr-data-request.psy.vu.nl/). A data request needs to
state for which purposes the data are requested and which safety measures will be taken. Additionally, an analysis or replication plan
needs to be appended to the data request.
