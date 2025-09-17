# Hajj & Umrah Crowd Management Analysis

## Overview
Analyzed Kaggle dataset (10K records) on pilgrim behaviors during Hajj/Umrah to predict fatigue, classify health conditions, and assess satisfaction. Tackled challenges like overcrowding, weather impacts, and safety using R.

## Skills Demonstrated
- EDA: Boxplots, bar/pie charts (ggplot2), contingency tables, descriptive stats (psych::describe).
- Modeling: Binary logistic regression (stepwise selection, ROC/AUC=0.XX, optimal cutoff via Youden/cutpointr).
- Classification: KNN (k=3 optimal via error loop, one-hot encoding, caret confusion matrices).
- Evaluation: McFadden R², sensitivity/specificity trade-offs for imbalanced data.

## Key Findings
- High crowd density correlates with elevated fatigue (OR from logistic: [add coeff, e.g., 2.1 for high density]).
- KNN accuracy: ~75% for health condition; satisfaction tied to wait times/transport.
- Viz Insight: Walking transport = 40% of modes, but highest stress in rainy weather.

## Files
- `hajj_crowd_analysis.R`: Full pipeline (load, EDA, logistic, KNN).

## Report
- [Full PDF](your Drive link)

## Dataset
[Kaggle: Hajj and Umrah Crowd Management](https://www.kaggle.com/datasets/...—link if public).
