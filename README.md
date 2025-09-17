# Hajj & Umrah Crowd Management Analysis

## Overview
Analyzed Kaggle dataset (10K records) on pilgrim behaviors during Hajj/Umrah to predict fatigue, classify health conditions, and assess satisfaction. Tackled challenges like overcrowding, weather impacts, and safety using R.

## Skills Demonstrated
- EDA: Boxplots, bar/pie charts (ggplot2), contingency tables, descriptive stats (psych::describe).
- Modeling: Binary logistic regression (stepwise selection, ROC/AUC, optimal cutoff via Youden/cutpointr).
- Classification: KNN (k=3 optimal via error loop, one-hot encoding, caret confusion matrices).
- Evaluation: McFadden R², sensitivity/specificity trade-offs for imbalanced data.

## Key Findings
- High crowd density correlates with elevated fatigue (OR from logistic: [add coeff, e.g., 2.1 for high density]).
- KNN accuracy: ~75% for health condition; satisfaction tied to wait times/transport.
- Viz Insight: Walking transport = 40% of modes, but highest stress in rainy weather.

## Files
- `hajj_crowd_analysis.R`: Full pipeline (load, EDA, logistic, KNN).

## Report
- (https://drive.google.com/file/d/1pmTBS8s4wH9l8wZMmno5s7wWEZ9PfAcI/view?usp=drive_link)

## Dataset
- (https://www.kaggle.com/datasets/ziya07/hajj-and-umrah-crowd-management-dataset).
