library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(scales)
library(readr)
library(car)

setwd('/Users/sadiq/Documents/02 University/02 Modules/BC2406 Analytics I Visual & Predictive Techniques/Course Materials/Graded Team Assignment - Gender Discrimination Lawsuit')
lawsuit.dt<-fread('lawsuit.csv')

#Figure 1: Gender Distribution Across Ranks and Departments
gender_dist <- lawsuit.dt %>%
  group_by(Dept, Rank, Gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(Dept, Rank, Gender = c(0, 1), fill = list(count = 0)) %>%
  group_by(Dept, Rank) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(
    Rank_f = factor(Rank, levels = c(1, 2, 3),
                    labels = c("Assistant", "Associate", "Full Professor")),
    Dept_f = factor(Dept, levels = 1:6,
                    labels = c("Biochemistry/Molecular Biology", "Physiology",
                               "Genetics", "Pediatrics", "Medicine", "Surgery"))
  )

ggplot(gender_dist,
       aes(x = "", y = prop, fill = factor(Gender))) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = ifelse(count > 0,
                               paste0(count, " (", scales::percent(prop, accuracy = 1), ")"),
                               "")),
            position = position_stack(vjust = 0.5),
            size = 3, show.legend = FALSE) +
  coord_polar(theta = "y") +
  facet_grid(Rank_f ~ Dept_f) +
  labs(title = "Gender Distribution Across Ranks and Departments",
       fill = "Gender") +
  scale_fill_manual(values = c(`0` = "#f4a7b9", `1` = "#1f5cff"),
                    labels = c(`0` = "Female", `1` = "Male")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(size = 10))


#Figure 2: Average Salary by Gender and Academic Rank
salary_summary <- lawsuit.dt %>%
  mutate(avg_salary = (Sal94 + Sal95) / 2) %>%
  group_by(Rank, Gender) %>%
  summarise(
    mean_salary = mean(avg_salary, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Rank = factor(Rank, levels = c(1, 2, 3),
                  labels = c("Assistant", "Associate", "Full Professor")),
    Gender = factor(Gender, levels = c(0, 1),
                    labels = c("Female", "Male"))
  )

ggplot(salary_summary, aes(x = Rank, y = mean_salary, fill = Gender)) +
  geom_col(position = "dodge") +
  labs(title = "Average Salary by Gender and Academic Rank",
       x = "Academic Rank", y = "Average Salary") +
  scale_y_continuous(labels = label_dollar(scale = 1, accuracy = 1)) +
  scale_fill_manual(values = c("Female" = "#f4a7b9", "Male" = "#1f5cff")) +
  theme_minimal()
#Figure 3: Linear Regression - Full Model Summary and Coefficient Chart
lawsuit.dt[, GenderLabel := ifelse(Gender == 1, "Male", "Female")]
lawsuit.dt[, RankLabel := factor(Rank, levels = 1:3, labels = c("Assistant", "Associate", "Full"))]
# full model with VIF & AIC check
print("Running Full model Regression...")
full_model <- lm(Sal95 ~ GenderLabel + Exper + Rank + Cert + Prate + Clin, data = lawsuit.dt)


# rmb VIF > 5 is a common threshold for concern!!

print("Summary of the Full Model")
print(summary(full_model))
vif_results <- vif(full_model)
print("VIF Values for Full Model:")
print(vif_results)
print("AIC of Full Model:")
print(AIC(full_model))


# Use stepwise regression (based on AIC) to find the best-fit model
print("Running Stepwise Regression...")
best_fit_model <- step(full_model, direction = "both", trace = 0) 

# Print the summary of the final, optimized model
print("Summary of the Best-Fit Model")
print(summary(best_fit_model))
print("VIF Values for Best-Fit Model:")
print(vif(best_fit_model))
print("AIC of Best-Fit Model:")
print(AIC(best_fit_model))


#==============Linear Regreesion Visualisations============#

#Setting up plot
full_model_summary <- summary(full_model)
coef_data <- as.data.table(full_model_summary$coefficients, keep.rownames = "Variable")
setnames(coef_data, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"), c("Coefficient", "StdError", "T_Value", "P_Value"))


coef_data[Variable == '(Intercept)', Variable := 'Intercept']
coef_data[Variable == 'GenderLabelMale', Variable := 'Gender (Male)']
coef_data[, Significant := P_Value < 0.05]

coef_data[, Variable := factor(Variable, levels = coef_data[order(-Coefficient)]$Variable)]

# plotting full model coefficients
p_full_plot <- ggplot(coef_data[Variable != 'Intercept'], aes(x = Variable, y = Coefficient, fill = Significant)) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = paste0("$", scales::comma(round(Coefficient, 0)), 
                       "\np = ", ifelse(P_Value < 0.001, "< 0.001", 
                                        format(round(P_Value, 3), nsmall = 3)))),
    vjust = ifelse(coef_data[Variable != 'Intercept']$Coefficient >= 0, -0.4, 1.4),
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"),
    labels = c("Not Significant", "Significant")
  ) +
  
  scale_y_continuous(
    labels = scales::dollar_format(), 
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  labs(
    title = "Full Model: Salary Predictor Coefficients",
    subtitle = "Gender is not statistically significant after controlling for other factors",
    x = "Predictor Variable", 
    y = "Impact on Salary ($)",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_full_plot)


#Figure 4: Linear Regression - Best Model Summary and Coefficient Chart
#optimized model coeff plot

best_fit_summary <- summary(best_fit_model)
coef_data <- as.data.table(best_fit_summary$coefficients, keep.rownames = "Variable")
setnames(coef_data, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"), c("Coefficient", "StdError", "T_Value", "P_Value"))

coef_data[Variable == '(Intercept)', Variable := 'Intercept']
coef_data[Variable == 'GenderLabelMale', Variable := 'Gender (Male)']
coef_data[, Significant := P_Value < 0.05]

coef_data[, Variable := factor(Variable, levels = coef_data[order(-Coefficient)]$Variable)]

p_final_plot <- ggplot(coef_data[Variable != 'Intercept'], aes(x = Variable, y = Coefficient, fill = Significant)) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = paste0("$", scales::comma(round(Coefficient, 0)), 
                       "\np = ", ifelse(P_Value < 0.001, "< 0.001", 
                                        format(round(P_Value, 3), nsmall = 3)))),
    vjust = ifelse(coef_data[Variable != 'Intercept']$Coefficient >= 0, -0.4, 1.4),
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"),
    labels = c("Not Significant", "Significant")
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(), 
    expand = expansion(mult = c(0.15, 0.15)) # More space at top and bottom
  ) +
  labs(
    title = "Best fit model: Salary Predictor Coefficients",
    subtitle = "Gender removed, produced a better AIC & simillar R-sqaured value",
    x = "Predictor Variable", 
    y = "Impact on Salary ($)",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_final_plot)

#Figure 5: Predicted vs Actual Salary (Best Fit Model)
# Visualise linear regression of optimized model with scatter and best fit line

lawsuit.dt[, Predicted := predict(best_fit_model)]
lawsuit.dt[, Residuals := residuals(best_fit_model)]
lawsuit.dt[, Standardized_Residuals := scale(Residuals)[,1]]


p1x <- ggplot(lawsuit.dt, aes(x = Predicted, y = Sal95)) +
  geom_point(aes(color = GenderLabel), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "red", linewilawsuit.dth = 1) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_manual(values = c("Female" = "#f4a7b9", "Male" = "#1f5cff")) +
  labs(title = "Predicted vs Actual Salary (Best Fit Model)",
       subtitle = paste0("R² = ", round(summary(best_fit_model)$adj.r.squared, 3), 
                         " - Model explains 70% of variance"),
       x = "Predicted Salary ($)", 
       y = "Actual Salary ($)",
       color = "Gender") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(p1x)

#Figure 6: Relationship between Experience and Academic Rank


#summary statistics
experience_summary <- lawsuit.dt[, .(
  AvgExperience = mean(Exper),
  MedianExperience = median(Exper),
  Count = .N
), by = GenderLabel]

print("Experience Summary by Gender:")
print(experience_summary)

# Relationship between Experience and Academic Rank
p_exprank_line <- ggplot(lawsuit.dt, aes(x = Exper, y = Rank, color = GenderLabel)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) + # se=FALSE hides the grey bounds
  labs(title = "Relationship between Experience and Academic Rank",
       x = "Experience",
       y = "Academic Rank (1=Assistant, 2=Associate, 3=Full)",
       color = "Gender") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Female" = "#f4a7b9", "Male" = "#1f5cff"))

print(p_exprank_line)

#Figure 7: Average Experience Required fo Each Academic Rank
# rank/experience statistics 
rank_experience <- lawsuit.dt[, .(
  MedianExp = as.numeric(median(Exper)),      # numeric conversion
  MeanExp = as.numeric(mean(Exper)),           
  Q25 = as.numeric(quantile(Exper, 0.25)),   
  Q75 = as.numeric(quantile(Exper, 0.75)),    
  Count = as.integer(.N)                      
), by = .(RankLabel, GenderLabel)]

print("Experience Requirements by Rank and Gender:")
print(rank_experience[order(RankLabel, GenderLabel)])

# visualization of it
p_rank_exp <- ggplot(rank_experience, aes(x = RankLabel, y = MeanExp, fill = GenderLabel)) +
  geom_col(position = position_dodge(0.8), alpha = 0.8) +
  geom_errorbar(aes(ymin = Q25, ymax = Q75), 
                position = position_dodge(0.8), wilawsuit.dth = 0.2) +
  labs(title = "Average Experience Required for Each Academic Rank",
       x = "Academic Rank", 
       y = "Average Years of Experience", 
       fill = "Gender") +
  scale_fill_manual(values = c("Female" = "#f4a7b9", "Male" = "#1f5cff")) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(p_rank_exp)

#Figure 8: Gender Salary Disparities Across Ranks and Departments

lawsuit.dt$DepartmentLabel = factor(lawsuit.dt$Dept,
                                    levels = 1:6, 
                                    labels = c("Biochemistry/Molecular Biology",
                                               "Physiology",
                                               "Genetics",
                                               "Pediatrics",
                                               "Medicine",
                                               "Surgery"))

#Add gender column
lawsuit.dt$GenderLabel = factor(lawsuit.dt$Gender, 
                                levels = c(0, 1),
                                labels = c("Female", "Male"))


#Add Rank Column
lawsuit.dt$RankLabel = factor(lawsuit.dt$Rank,
                              levels = c(1,2,3),
                              labels = c("Assistant", "Associate", "Full Professor"))

chart1 = ggplot(lawsuit.dt, aes(x = DepartmentLabel, y = Sal95, fill = GenderLabel)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.9), width = 0.7) +
  facet_wrap(~RankLabel, nrow = 1, scales = "fixed") + 
  scale_fill_manual(values = c("Female" = "#f4a7b9", "Male" = "#1f5cff"), name = "Gender") +
  scale_y_continuous(
    limits = c(0, 400000),
    breaks = seq(0, 400000, by = 50000),
    labels = scales::label_dollar()
  ) +
  labs(title = "Average Salary by 1995", x = NULL, y = NULL) + 
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(chart1)

#Figure 9: Average Salary Increment by Gender
dt_chart2 = as.data.table(lawsuit.dt)[, .(Sal94 = mean(Sal94), Sal95 = mean(Sal95)), by = GenderLabel]

#change dt_chart2 to Long

dt_chart2long = melt(dt_chart2, id.vars = "GenderLabel", variable.name = "Year", value.name = "Salary")

#Create axis range 
female_min = floor(min(dt_chart2long[GenderLabel == "Female", Salary])/1000) * 1000
female_max = ceiling(max(dt_chart2long[GenderLabel == "Female", Salary])/1000) * 1000
male_min = floor(min(dt_chart2long[GenderLabel == "Male", Salary])/ 1000) * 1000
male_max = ceiling(max(dt_chart2long[GenderLabel == "Male", Salary])/ 1000) * 1000
a = (female_max - female_min)/(male_max - male_min)
b = female_min - a * male_min

#plot chart2
chart2 = ggplot(dt_chart2long, aes(x = factor(Year, 
                                              levels = c("Sal94", "Sal95"),
                                              labels = c("Average Salary in 1994", "Average Salary in 1995")),
                                   y = ifelse(GenderLabel == "Male", a * Salary + b, Salary),
                                   color = GenderLabel,
                                   group = GenderLabel)) + 
  geom_line(linewidth = 0.9) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("Male" = "#f4a7b9", "Female" = "#1f5cff"), 
                     name = "Gender",
                     guide = "none") + 
  scale_y_continuous(name = NULL, 
                     limits = c(female_min, female_max), 
                     breaks = pretty(c(female_min, female_max), n = 5),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~(.-b)/a,
                                         name = NULL,
                                         breaks = pretty(c(male_min, male_max), n = 5),
                                         labels = scales::comma)) +
  labs(x = NULL, title = NULL) + 
  annotate("segment", x = 1.6, xend = 1.7, y = female_max, 
           yend = female_max, color = "#f4a7b9", linewidth = 1) +
  annotate("text", x = 1.72, y = female_max, label = "Female", hjust = 0, size = 3.5) + 
  annotate("segment", x = 1.6, xend = 1.7, y = female_max - (female_max - female_min) * 0.1, 
           yend = female_max - (female_max - female_min) * 0.1, color = "#1f5cff", linewidth = 1) +
  annotate("text", x = 1.72, y = female_max - (female_max - female_min) * 0.1, 
           label = "Male", hjust = 0, size = 3.5) + 
  theme_minimal(base_size = 11)

print(chart2)

#Figure 10: Salary vs Publication Rate
ggplot(lawsuit.dt,
       aes(x = Prate, y = Sal95,
           color = factor(Gender, levels = c(1,0), labels = c("Male","Female")))) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("Male" = "#1f5cff", "Female" = "#f4a7b9")) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Salary vs Publication Rate by Gender",
       x = "Publication Rate (Prate)",
       y = "Salary (USD)",
       color = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#Figure 11: Distribution of Experience
hist_data <- lawsuit.dt %>%
  mutate(Gender_label = factor(Gender, levels = c(1,0), labels = c("Male","Female"))) %>%
  group_by(Exper, Gender_label) %>%
  summarise(count = n(), .groups = "drop")

plot_exper_hist <- ggplot(hist_data, aes(x = Exper, y = count, fill = Gender_label)) +
  geom_col(alpha = 0.7, position = "identity", width = 0.8) +  # Overlapping bars
  scale_fill_manual(values = c("Male" = "#1f5cff", "Female" = "#f4a7b9")) +
  labs(title = "Distribution of Experience by Gender",
       x = "Years of Experience",
       y = "Count",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "top")

print(plot_exper_hist)

#Figure 12: Certification Rate
plot_cert <- lawsuit.dt %>%
  group_by(Gender, Cert) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Gender) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  
  ggplot(
    aes(x = factor(Cert), y = Percentage, 
        fill = factor(Gender, levels =c(1,0), labels =c("Male","Female")))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("Male" = "#1f5cff", "Female" = "#f4a7b9")) +
  labs(title = "Certification Rate by Gender",
       x = "Certified (0 = No, 1 = Yes)",
       y = "Percentage (%)",
      fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(plot_cert)

#Figure 13: Salary Distribution by Gender, Rank and Board Certification
lawsuit2.dt <- lawsuit.dt %>%
  mutate(
    Gender = ifelse(Gender == 1, "Male", "Female"),
    Cert   = ifelse(Cert == 1, "Board Certified", "Not Certified"),
    Clin = factor(Clin, levels = c(1, 0), labels = c("Clinical", "Research")),
    Rank   = case_when(
      Rank == 1 ~ "Assistant",
      Rank == 2 ~ "Associate",
      Rank == 3 ~ "Full"
    )
  )
ggplot(lawsuit2.dt, aes(x = Rank, y = Sal95, fill = Gender)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
  facet_wrap(~Cert) +
  scale_fill_manual(values = c("Male" = "#1f5cff", "Female" = "#f4a7b9")) +
  labs(
    title = "Salary Distribution by Gender, Rank, and Board Certification",
    x = "Academic Rank",
    y = "Salary (1995)",
    fill = "Gender"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Figure 14: Rank Distribution by Gender and Clinical Emphasis
ggplot(lawsuit2.dt, aes(x = Rank, fill = Gender)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Clin) +
  labs(
    title = "Rank Distribution by Gender and Clinical Emphasis",
    x = "Academic Rank",
    y = "Count"
  ) +
  scale_fill_manual(values = c("Male" = "#1f5cff", "Female" = "#f4a7b9")) +
  theme_minimal()

#Figure 15: Average Salary by Gender, Rank, and Clinical Emphasis
ggplot(lawsuit2.dt, aes(x = Rank, y = Sal95, fill = Gender)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  facet_wrap(~Clin) +
  labs(
    title = "Average Salary by Gender, Rank, and Clinical Emphasis",
    x = "Academic Rank",
    y = "Average Salary"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_manual(values = c("Male" = "#1f5cff", "Female" = "#f4a7b9")) +
  theme_minimal()
