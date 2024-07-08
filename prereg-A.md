# Form A for preregistering predictive models: Problem Definition
**Do not** edit or remove the questions. Add your answers below each question. 

Answers can be updated by updating this file and checking in to git. 

## A.1: Research question
**Q. What problem are you studying?**
Answer: Can the prediction of wellbeing by means of ML models trained on multimodal data be improved by including features that take into account genetic information and longitudinal changes in childhood psychopathology?

## A.2 Dependent variable
**Q. What is the main outcome of interest and how is it measured?**
Answer: The outcome wellbeing is operationalized by means of the Cantril ladder (CL; Cantril, 1965) which asks participants to rate their life on a scale from 1 to 10 where 1 indicates the worst and 10 the best possible life. The outcome will be treated as a continuous variable.

## A.3 Independent variable
**Q. What features will you use to predict this outcome?**
Answer: Features will be derived from multiple sources. Genetic information will be included by means of Polygenic scores (PGSs) of wellbeing and for a wide range of traits that all have been related to mental health in previous studies or that are readily available in the study data will be included. Concretely, 61 PGS that were used in a previous study on the subject by Pelt et al. (2023) will be included e.g., PGS for psychopathology and wellbeing, but also BMI and educational attainment). Phenotype information will include items assessing childhood psychopathology. From the raw phenotype variables, statistical features will be calculated, also such taking into account longitudinal changes. Growth mixture models or, if not feasible, latent class growth analysis or linear multilevel models will be calculated to obtain as additional features:
	- probabilities of group membership regarding longitudinal trajectories (in case of growth mixture modeling and latent class growth analysis)
	- group-level random intercepts (in case of growth mixture modeling and latent class growth analysis)
	- group-level random slopes (in case of growth mixture modeling and latent class growth analysis)
	- individual-level random intercepts (in case of growth mixture modeling)
	- individual-level random slopes (in case of growth mixture modeling)
	- individual linear random intercept (in case of linear multilevel modeling)
	- individual random slope (in case of linear multilevel modeling)

These features will be obtained from all childhood psychopathology items where feasible. Additional features that inform about longitudinal change, such as the mean and SD over time, the nth-order autocorrelation (including correlations with the maximum lag) and the Root mean square of successive differences (RMSSD) over all available time points will be calculated. Demographic (sex, socio-economic status of the family) and study specific (time lag between the last longitudinal assessment and the outcome assessment) covariates will also be part of the feature space. Before model training, highly correlated (0.95), near-zero variance (cutoff:  (most frequent value)/(second most frequent value) =  95/5 ), highly collinear features (VIF > 10) and linear combinations of features will be removed to ensure model stability. Elastic net regression will be used for feature selection since a large number of predictors can be expected. 


## A.4 Training data
**Q. How was the training data constructed?**
Answer: The sample will consist of subjects who gave consent for storing their genetic, phenotype and other data in the Netherlands twin register (NTR) database. The NTR is a large-scale genetic and phenotypic database, established by the Department of Biological Psychology, Vrije Universiteit Amsterdam more than 30 years ago (Ligthart et al., 2019). Every two/three years, longitudinal survey data about lifestyle, personality, psychopathology, and wellbeing in twins and their families are collected. The NTR sample is a population-wide, non-clinical sample. The NTR distinguishes between the Young NTR (YNTR) and the Adult NTR (ANTR). The YNTR comprises of children rated by their parents and teachers, and adolescents providing self-reports; standardized surveys are send out at different ages. Since this study focuses on the association of childhood psychopathology and genetics with wellbeing, only the measures from the YNTR and more specific only those assessing childhood psychopathology and participant’s genetics will be used alongside demographic variables. Socio-economic status, operationalized by educational attainment in the family, and the time lag between the last measurement of the YNTR and the first measure of the ANTR with the outcome variable will be included in the models as covariates. The to be predicted outcome variable wellbeing will be taken from the ANTR. Concretely, the first answer of participants to the Cantril ladder in an ANTR survey will be taken as the outcome.

Data from subjects who were at age 3 when they were first examined and 16 when they were last examined will be included to assess the specific predictive value of symptoms of childhood psychopathology their longitudinal changes. A random part of 80% of the obtained sample will constitute the training data for this study. The split will not be completely random as families will be kept together in order to prevent overfitting that could occur when they are separated between the data subsets. This is because relatives are more similar to each other compared to other participants.


## A.5 Test data
**Q. How will the test dataset be constructed? Will new data be collected, or will the test set be synthetically constructed?**
Answer: The remaining 20 % of the obtained sample will constitute the test set for this study

## A.6 Data transformations
**Q. Will you filter or transform the data from its raw form in any way?**
Answer: Participants and features with more than 50% missing values on the raw variables will be excluded. Regarding missing values, the growth mixture modeling and latent class growth analysis will make use of Full-information maximum likelihood (FIML). Remaining missing values for the raw phenotype data will be imputed using k-nearest neighbors imputation, which is a relatively simple, non-parametric multivariate imputation method and has been shown to consistently outperform other imputation methods in the context of numerical data (Jadhav et al., 2019). Furthermore, outliers detected by means of the top 5 % of distance based on the Minimum Covariance determinant (MCD) will be excluded. Continuous variables will first be standardized and then normalized to take a value ranging from 0-1. Categorical variables will be one-hot encoded. 

## A.7 Metrics
**Q. How will you measure the success of your model in predicting the outcome? If you are using a measure such as accuracy that requires thresholding of a continuous prediction, please specify the threshold(s) you will use.**
Answer: Model performance will be based on the RMSE and R² value of the model in question on the test set. Comparison between models will be based on 95% bootstrapped confidence intervals. Wilcoxon signed rank tests will inform whether there is a significant difference in the predictions between the models. A conservative p-value of .005 will be used. Feature importance will be assessed by calculating Shapley Additive exPlanation (SHAP) values and permutation importances (PIs). 

## A.8 Baselines
**Q. Will you compare your method to other baselines? If so, which ones?**
Answer: Models without the inclusion of features that encode longitudinal trajectories in childhood psychopathology and models with genetic information only will serve as the baseline for comparison.

## A.9 Anything else
**Is there anything else you would like to pre-register before training and validation?**
Answer: The team uses an already existing dataset and thus has no influence on the sample size. A recent method proposed by Riley and Collins (2023) however can be used to examine whether a given sample size and a given number of predictors ML models are trained on lead to stable predictions. This Bootstrap assessment of prediction stability will be included in the ML pipeline of this study intending to exploratively examine it’s applicability for ML models with longitudinal data. If unstable models emerge from the stability check, the modeling procedure will be revised by means of shrinking down the predictor space. An iterative process will be carried out where at each iteration the 10% of the original full predictor set before model training with the lowest variance will be removed, resulting in a maximum of 9 revision rounds. Furthermore, post-hoc correction for the mentioned demographic and study-specific covariates will be applied. We anticipate that the PGS might show very high correlations with each other. If this should be the case, we will raise the removal threshold of r > 0.95 for the correlation based removal of machine learning features to r > 0.99.   
