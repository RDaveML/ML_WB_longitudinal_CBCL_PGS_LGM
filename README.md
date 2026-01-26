# ML_WB_longitudinal_CBCL_PGS_LGM

**Summary of the project:**

Combining multiple modalities of data for the prediction of wellbeing in machine learning models might lead to a more accurate prediction. This project investigated the usefulness of longitudinal features of a specific aspect of an individual’s life history, namely childhood psychopathology, for machine learning-based prediction of adult wellbeing. Features derived from longitudinal trajectories of childhood psychopathology (age 3 - 16) are compared to polygenic risk scores for a variety of phenotypes and to cross-sectional features of childhood psychopathology.

As of January 2026, paper is handed in for publication

Authors: Leitritz, D; Pool, R.; Ligthart, L.; Bartels, M.; Pelt, D.

Department of Biological Psychology; Vrije Universiteit Amsterdam; Amsterdam, Noord-Holland; The Netherlands
July 2024 - January 2026

### Working with the repository
A) Make new R project and sync with repository
    1) Create a new R project 
    2) Select version control -> Git
    3) Insert repository link
    4) Project structure is retained for own fork

B) Pull entire folder directly from gitHub and use file ML_WB_longitudinal_CBCL_PGS_LGM.Rproj
for a new R project



### Content repository
This repository contains all documents relevant to the analysis and is organized as follows:

- doc contains all documentation of the study including legal documents and the pre-registration
- scripts contains all R and bash scripts that were used for the analysis
- the scripts have an order (indicated by the numeric prefix) which needs to be adhered to when replicating the analysis in order to obtain all required intermediate objects
- visualizations can be found in the visualization folder

*No data are placed in this repository*

### Content and organization of scripts
- scripts 01 - 05 contain raw data preparation and cleaning and creation of intial train / test split on the full sample (N = 5,087)
- scripts 06a -c contain calculation of longitudinal summary statistics
- script 07 carries out latent growth modelling for all CBCL items included calling Mplus
- script 08 does preprocessing of genetic data to obtain polygenic scores
- scripts 09 - 11 run bootstrapped machine learning models (09), inspect the stability of the bootstrapped models (10) and create stacked ensemble models and calculate bootstrapped model performance measures (11) for variable set A (Only raw CBCL variables + covariates)
- script 12 creates a new train / test split for all variable sets with genetic data (N = 2,656)
- scripts 13 - 15 execute the same steps as script 09-11 for variables set B (PGS + genetic covariates)
- scripts 16 - 18 for variables set C (raw CBCL items + PGS + covariates)
- script 19 merges derived longitudinal variables with other parts of the data to create variable sets D and E
- scripts 20 - 22 - execute the same steps as script 09-11 for variable set D (raw CBCL items + longitudinal variables + covariates)
- scripts 23 - 25 - execute the same steps as script 09-11 for variable set E (raw CBCL items + longitudinal variables + PGS + covariates)
- script 26 inspects sample demographics
- script 27 compares ML model performances between sets and algorithms
- script 28 calculates variables importances (SHAP values) for all models
- script 29 runs a confounder analysis
- script 30 detects multivariate outliers calculating the Minimum covariance determinant (MCD)
- scripts 31 - 36 re-run the original model training for variable sets A - E with a reduced sample (MCD outliers removed, no bootstrapping)
- scripts 36 - 40 run the stacked ensemble modelling and model performance for the re-ran models for variable sets A - E
- script 41 re-runs the model comparison with the re-ran models
- script 42  calculates for every measure of model performance if confidence intervals overlap between original analysis and sensitivity analysis (MCDc outliers removed before ML)
- script 43 re-runs the variable importance calculation for the sensitivity analysis
- non-numbered scripts contain plotting and formatting code for the publication of the results (no calculations or statistical analyses)
- Tables coded and presented are in most cases limited to showing most important results, code for full tables and figures (e.g. variable importance, pairwise model comparison, SHAP plots) are available upon request from the first author

Note that bootstrapping models, stability tests and performance measure calculation have separate scripts per variable set because of the large computational costs associated with bootstrapping
To execute the bootstrapping, bash scripts that loop ML training over all bootstrapped datasets need to be run


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
    -  2 == female

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



#### Data availability
*Being part of a national prospective cohort study (NTR), (a) our data cannot be made publicly available for privacy reasons but are available for legitimate researchers via their data access procedure (https://tweelingenregister.vu.nl/information_for_researchers/working-with-ntr-data) and (b) our sample will, due to the longitudinal data collection procedures, partly overlap with previous publications.*
