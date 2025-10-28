# 👨‍⚖️ Gender Discrimination Lawsuit Analysis

![R](https://img.shields.io/badge/R-4.0%2B-276DC3?style=for-the-badge&logo=r)
![License](https://img.shields.io/badge/License-MIT-green?style=for-the-badge)
![Status](https://img.shields.io/badge/Status-Complete-success?style=for-the-badge)
![Data Analysis](https://img.shields.io/badge/Type-Statistical%20Analysis-blue?style=for-the-badge)

---

## 📋 Overview

A comprehensive statistical analysis investigating gender discrimination allegations in university faculty salaries. This project employs **linear regression modeling**, **hypothesis testing**, and **data visualization** to uncover the true drivers of salary disparities beyond gender.

**Key Question:** Is gender a significant factor in determining faculty salary, or are other variables responsible?

---

## 🔍 Key Findings

| Finding | Insight |
|---------|---------|
| **Gender Not Significant** | Linear regression shows gender is NOT statistically significant (p=0.218) in salary determination |
| **Real Drivers** | Rank, certification, experience, and clinical emphasis are the primary salary determinants |
| **Career Progression** | Female professors advance FASTER than male counterparts with equal experience |
| **Pay Gap Closing** | Female average salaries rose 15% faster than male salaries (1994-1995) |
| **Department Variation** | Biochemistry & Molecular Biology departments show females earning MORE than males |
| **Structural Differences** | Clinical specialties (higher-paying) are male-dominated; Research tracks are female-dominated (Pipeline Effect) |

---

## 📊 Analysis Highlights

### Regression Analysis
- **Full Model:** All variables included (Gender p-value: 0.218 - NOT significant)
- **Best-Fit Model:** Gender removed, improved AIC & R² values
- **Conclusion:** Differences are structural, not discriminatory

### Data Visualizations (15 figures)
1. Gender distribution across ranks and departments
2. Average salary by gender and rank
3. Regression coefficients and significance levels
4. Predicted vs actual salary performance
5. Experience vs academic rank relationship
6. Certification impact on salary
7. Department-specific salary disparities
8. Year-over-year salary trends
9. Publication rate vs salary correlation
10. Experience distribution analysis
11. Certification rate by gender
12. Salary distribution by certification status
13. Rank distribution by clinical emphasis
14. Average salary by clinical track

---

## 🛠️ Technologies & Libraries

```r
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(tidyr)      # Data tidying
library(data.table) # High-performance data analysis
library(scales)     # Scale formatting
library(car)        # VIF calculations & regression diagnostics
```

---

## 📁 Project Structure

```
Gender-Discrimination-Lawsuit-Analysis/
├── lawsuit.r                                    # Main analysis code
├── Gender Discrimination Lawsuit Report.pdf    # Detailed findings report
├── lawsuit.csv                                 # Dataset
└── README.md                                   # This file
```

---

## 🎯 Statistical Methods

### Linear Regression Model
```
Sal95 ~ Gender + Experience + Rank + Certification + PublicationRate + Clinical
```

**Model Diagnostics:**
- VIF check for multicollinearity
- AIC comparison (full model vs best-fit)
- R² = 0.70 (model explains 70% of variance)
- P-value analysis for each predictor

### Stepwise Regression
- Direction: Both (forward & backward)
- Criterion: AIC
- Result: Gender removed as non-significant variable

---

## 💡 Case Law Support

**Presseisen v. Swarthmore College (1977)**
- Defendants successfully argued that sex was not the significant factor in salary disparity
- Regression analysis showing rank as the true driver supported their case
- This analysis follows similar statistical methodology

---

## 📈 Key Visualizations

### Coefficient Chart (Full Model)
Shows impact of each variable on salary with significance indicators

### Predicted vs Actual Salary
Scatter plot with regression line demonstrates model accuracy (R² = 0.70)

### Experience vs Rank Trajectory
Female professors achieve the same rank with LESS experience than males

### Department Salary Analysis
Faceted visualization showing salary disparities (or lack thereof) across 6 departments

---

## 👥 Team Members

| Name | 
|------|
| Mohammed Sadiq Gulamellis  | 
| Muhammad Ilham Bin Irwan  | 
| Peh Cheng Han Alvin  | 
| Wang Jianning  | 
| Ong Rou Jie |

---

## 🚀 How to Run

1. **Clone the repository**
   ```bash
   git clone https://github.com/Justinnnn0313/Gender-Discrimination-Lawsuit-Analysis.git
   cd Gender-Discrimination-Lawsuit-Analysis
   ```

2. **Install required packages** (if not already installed)
   ```r
   install.packages(c("dplyr", "ggplot2", "tidyr", "data.table", "scales", "car"))
   ```

3. **Run the analysis**
   ```r
   source("lawsuit.r")
   ```

4. **View the report**
   - Open `Gender Discrimination Lawsuit Report.pdf` for detailed findings

---

## 📊 Verdict

**The data shows FAIRNESS.** Salaries and promotions are based on qualifications (experience, rank, certification, clinical emphasis), NOT gender. The apparent pay gap is explained by:

- **Different career paths:** Clinical vs Research tracks
- **Different promotion timelines:** Research depends on publication accumulation
- **Pipeline effect:** Women entered research tracks later, affecting average salary
- **Clinical dominance:** Higher-paying clinical specialties are male-heavy

---

## 📝 Citation

If you use this analysis in your work, please cite:

```
Gender Discrimination Lawsuit Analysis (2024)
Authors: Sadiq, Ilham, Alvin, Jianning, Rou Jie
Repository: https://github.com/Justinnnn0313/Gender-Discrimination-Lawsuit-Analysis
```

---

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

---

## 📧 Contact

For questions or collaboration inquiries, please reach out through GitHub Issues.

---

**Last Updated:** October 2025  
**Status:** ✅ Analysis Complete | 📊 Ready for Peer Review
