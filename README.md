# Purchasing-Power-Parity-PPP-Analysis

# Overview
This project examines the principle of purchasing power parity (PPP), which states that over long periods, exchange rate changes should offset differences in inflation rates between two countries. In an efficient international economy, exchange rates should give each currency the same purchasing power in its own economy. While PPP may not hold exactly, it serves as a benchmark for expected exchange rate levels.

This analysis applies a simple linear regression model to evaluate whether the purchasing power parity theory holds true for a given dataset of countries.

# Data
The dataset used in this project is provided by Dr. Jeffrey Simonoff from New York University and is read into R using the read.table command. It contains the following columns:

Country: Name of the country
Inflation.difference: Difference in inflation rate between two countries
Exchange.rate.change: Change in the exchange rate between two countries
Developed: Indicator variable (1 = Developed country, 0 = Developing country)

# Methodology
# Data Preparation:

Read the dataset into R and inspect it.
Handle outliers (such as Brazil) by adding them back into the dataset if necessary.
Re-label the ‘Developed’ column to differentiate between developed and developing countries.
Exploratory Data Analysis:

Use scatter plots to evaluate the linear relationship between inflation differences and exchange rate changes.
Generate box plots to compare exchange rate changes between developed and developing countries.
Linear Regression Analysis:

Fit a linear regression model to determine the relationship between inflation differences and exchange rate changes.
Perform hypothesis testing to evaluate the coefficients of the linear regression model and test the PPP theory.
Advanced Analysis:

Perform a partial F-test for the slope coefficient.
Generate confidence and prediction intervals for new observations.
Analyze residuals to assess the assumptions of the linear regression model.
Outlier Analysis:

Examine the impact of outliers (e.g., Brazil, Indonesia) on the regression results.
Repeat the analysis by omitting outliers to understand their influence on the validity of the PPP theory.
Country-Specific Analysis:

Split the dataset into developed and developing countries.
Perform separate linear regression analyses for each subset.
Identify differences in the PPP theory's validity between developed and developing countries.
# Findings
# Overall Results:

Support for the PPP theory is mixed.
There is some evidence that inflation differences are balanced by exchange rate changes in developed countries, but the case is weaker for developing countries.
Outlier Impact:

Outliers like Brazil and Indonesia significantly affect the regression results, particularly in developing countries.
Country-Specific Analysis:

In developed countries, changes in inflation differences are more likely to be balanced by exchange rate changes.
In developing countries, the relationship is less consistent, with notable deviations due to unusual economic or political conditions.
# Acknowledgments
This analysis was inspired by a dataset made available by Dr. Jeffrey Simonoff from New York University.
