# Factors Affecting Exam Performance

## Overview

This project investigates factors associated with exam performance using a synthetic dataset. The analysis explores how variables such as hours studied, hours of sleep, attendance, extracurricular involvement, and gender relate to exam scores.

## Objectives

* Explore patterns and relationships among variables using graphical representations and summary statistics.
* Examine correlations and use multiple linear regression to investigate factors associated with exam scores.
* Conduct hypothesis tests and statistical inference to evaluate relationships in the data.
* Assess model assumptions and explore transformations, variable selection, interaction effects, and model selection.

## Methods

The analysis was conducted in R and included:

* Exploratory data analysis, data visualization, and descriptive statistics
* Correlation analysis and multiple linear regression
* Hypothesis testing for overall model significance and individual predictors
* ANOVA comparisons of full and reduced regression models
* Forward and backward stepwise variable selection
* Interaction-term testing and model comparison using AIC, BIC, and adjusted R-squared
* Regression diagnostics, multicollinearity assessment, and attempted response-variable transformations

## Tools

* **Programming language:** R
* **Statistical techniques:** Exploratory data analysis, regression modeling, statistical inference, variable selection, interaction analysis, and model diagnostics

## Dataset

The project uses the *Student Performance Factors* dataset from Kaggle, containing 6,607 student records. The data is synthetic and should not be interpreted as direct evidence of real-world student behavior.

The report focuses on five predictors: hours studied, attendance, hours of sleep, extracurricular participation, and gender. Exam score is the dependent variable.

## Key Findings

* **Predictor significance:** Hypothesis testing and variable selection indicated that sleep and gender did not significantly contribute to the full regression model.
* **Interaction effects:** Testing interaction terms identified relationships between study hours and attendance, as well as study hours and extracurricular participation. The study hours × attendance interaction was more significant and was retained in the final model.
* **Practical implications:** Within this synthetic dataset, the findings suggest that studying, attendance, and extracurricular participation may be relevant to exam performance. These relationships would need validation using real-world data before informing decisions.

## Limitations

* **Synthetic data:** The dataset may not accurately represent real student behavior or exam performance, limiting the generalizability of the findings.
* **Predictor scope:** The analysis focused on five predictors, while other measured or unmeasured factors may also influence exam scores.
* **Model assumptions:** The final model did not satisfy the normality assumption, limiting confidence in some aspects of statistical inference.
* **Outliers:** The presence of outliers may have influenced the model estimates and results.

## Files

* `exam_performance_analysis.R` — R script containing the data analysis, visualizations, regression models, hypothesis tests, and model diagnostics.
* `StudentPerformanceFactors.csv` — Synthetic student performance dataset used in the analysis.
* `Factors-Affecting-Exam-Performance.Rproj` — RStudio project file for opening the project in RStudio.
