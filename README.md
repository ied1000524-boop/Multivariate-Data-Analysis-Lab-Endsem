# Groundwater Quality Analysis Using Multivariate Statistics

## Project Overview
This project analyzes groundwater quality data using multivariate statistical techniques in R. It examines the chemical characteristics of groundwater samples and compares water quality across two aquifer types: **Alluvial** and **Hard-rock**.

## Dataset
The dataset contains groundwater samples with the following variables:

- **Sample_ID** – Sample identification
- **Aquifer** – Aquifer type
- **pH** – Acidity/alkalinity
- **TDS** – Total Dissolved Solids
- **Ca** – Calcium
- **Mg** – Magnesium
- **Cl** – Chloride
- **NO3** – Nitrate
- **F** – Fluoride

## Methodology

The analysis includes:

1. Data cleaning and preprocessing
2. Descriptive statistics
3. Outlier detection using boxplots
4. Correlation analysis and heatmap
5. Univariate normality tests
6. Multivariate normality using Mardia's test
7. Hotelling's T² test for comparing aquifer groups
8. Principal Component Analysis (PCA)
9. Linear Discriminant Analysis (LDA)
10. Classification accuracy and confusion matrix

## Tools & Packages
- R and RStudio
- `MVN`
- `MASS`
- `corrplot`
- `ggplot2`

## Project Objective
The main objective is to understand groundwater quality patterns, identify relationships among chemical parameters, compare aquifer groups, and classify groundwater samples using multivariate statistical methods.

## Repository Structure
```text
Groundwater-Quality-Analysis/
│
├── data/
│   └── groundwater_clean.csv
│
├── plots/
│   ├── boxplots.png
│   ├── correlation_heatmap.png
│   ├── qq_plots.png
│   └── scree_plot.png
│
├── groundwater_analysis.R
└── README.md
```

## Conclusion
The project demonstrates how multivariate statistical methods can be applied to groundwater data to explore chemical relationships, compare aquifer characteristics, and classify samples.

**Language:** R  
**Environment:** RStudio  
**Domain:** Environmental Statistics | Multivariate Data Analysis
