# Predicting Voting Behaviour in the 2016 US Election

Can pre-election survey answers predict how someone voted? Using 13,160 respondents from the 2016 Cooperative Congressional Election Study, a logistic regression model predicted Democrat or Republican votes with **91.6% accuracy**, compared with a 52.7% baseline.

## Key findings
- **Past vote dominates.** How someone voted in 2012 was by far the strongest predictor.
- **Education and student status matter, but less.** Higher education and being a student were linked to voting Democrat; home ownership and having children to voting Republican.
- **Party switchers are the main source of error.** Lower-income 2016 Republicans were the most likely to have voted Democrat in 2012, and these were the voters the model most often got wrong.

## Models compared
| Model | Accuracy | Sensitivity | Specificity | AUC |
|---|---|---|---|---|
| Logistic regression | 0.916 | 0.943 | 0.885 | 0.933 |
| Random forest | 0.916 | 0.943 | 0.886 | 0.930 |
| kNN | 0.858 | 0.912 | 0.799 | 0.908 |

Logistic regression was chosen because it matched random forest's accuracy while being simpler and easier to interpret.

## Full report
[Read the full analysis](analysis.md)

## Methods
Data cleaning, exploratory analysis, an 80/20 train-test split, 5-fold cross-validation for tuning, and evaluation on accuracy, sensitivity, specificity and AUC. Built in R with tidyverse and tidymodels.

## About this project
This began as a university assignment and was rebuilt and extended independently. It uses the full cleaned dataset rather than a 3,000-person sample, keeps 2012 non-voters as their own group, and re-examines variables the original version removed.

## Data
The data comes from the [Cooperative Election Study](https://cces.gov.harvard.edu/) and isn't included in this repository.
