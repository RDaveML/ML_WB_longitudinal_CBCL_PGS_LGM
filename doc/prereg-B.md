# Form B for preregistering predictive models: Model details
**Do not** edit or remove the questions. Add your answers below each question. 

Answers can be updated by updating this file and checking in to git. 

## B.1 Prediction method
**Q. What prediction method(s) are you using?**
Answer: Random forest regression, support vector regression, XGBoost, stacked ensemble model based on the previously named models

## B.2 Model training
**Q. How did you train the model(s) you're using? Please specify details such as cross-validation, resulting random seeds used, resulting hyperparameters, computational budget, etc.**
Answer: Random seeds, 10-fold cross-validation, the following hypertuning parameters will be tuned using Bayesian optimization of the hypertuning parameters.
Elastic net regression (for feature selection): 
-	alpha (mixing parameter between ridge and lasso regression)
-	lambda (shrinkage parameter)

Random forest:
-	Number of trees
-	Maximum number of features to consider at each split
-	Maximum depth of a tree
-	Minimum number of samples required to split a node 
-	Minimum number of samples required at each leaf node
Support vector regression:
-	C parameter (penalty for each misclassified datapoint)
-	Kernel function (transformation method to allow linear separation of data points)
-	Gamma parameter (similarity radius)

XGBoost:
  Tree-specific:
    -	Number of trees
    -	Maximum depth of a tree
    -	Minimum sum of instance weight required to create new node in a tree
    -	Percentage of cases (rows) used for each tree construction
    -	Percentage of predictors (columns) used for each tree construction
  Learning task-specific (controlling the overall behavior and the learning process of the model):
    -	Learning rate eta (step size shrinkage used in updates to prevent overfitting)
    -	Gamma (minimum loss reduction required to make a further partition on a leaf node of the tree
    -	Lambda (L2 regularization term on weights)
    -	Alpha (L1 regularization term on weights)


## B.3 Accessed test data
**Q. Have you in any way previously accessed the test data?**
Answer: As of the date of submission, the data exist and have been accessed for other studies in the field, though no analysis has been conducted related to the current research plan (including calculation of summary statistics).

## B.4 Anything else
**Q. Are there any secondary or exploratory analyses you'd like to pre-register?**
Answer: Feature importance will be examined by means of Shapley Additive exPlanation (SHAP) values and permutation importances (PIs). Though not being central to the analysis, sex, socio-economic status and the time lag between end YNTR – first ANTR survey will be included as covariates in the predictor space and will be kept in the models even if the feature selection by elastic net would discard them as they are supposed to be accounted for in the final ML models.

## B.5 Plan adjustments
**Q. Are there any changes in your process as compared to your previous answers that you'd like to report?**
Answer: (To be filled in during coding phase)
