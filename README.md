* **Background:** for Chinese Statistical Modeling Competition in 2017
* **Theme:** A data-mining system of methods for binary classification based on imbalanced data – research on bank telemarketing campaign
* **Dataset:** http://archive.ics.uci.edu/ml/datasets/Bank+Marketing
* **Overview:**
  * Data Pre-Processing: handling missing values, one-hot encoding, training set, cross-validation set, test set
  * Data Visualization: numeric variable, categorical variable
  * Feature Selection: `RFE, Boruta, XGBoost`
  * Data Resampling: undersampling `NCL, Tomek Link Removal, K-means Clustering`, oversampling `Bootstrap, SMOTE`
  * Classifier: `KNN, LDA, Naive Bayes, Logistic Regression, ID3 Decision Tree, C50 Decision Tree, Random Forest, Neural Network,
     MLP, SVM, multi-SVM, LightGBM`
  * Ensemble Learning: `XGBoost-Stacking`
  * Numeric Evaluation: `Accuracy = 0.88, Precision = 0.49, Recall = 0.86, F-Score = 0.70`
