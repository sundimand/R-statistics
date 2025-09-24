# ======= seminr =============

# Reference
# https://link.springer.com/book/10.1007/978-3-030-80519-7
# data Corporate: https://www.pls-sem.net/downloads/pls-sem-using-r-a-workbook/
# Hair, J. F., Risher, J. J., Sarstedt, M., & Ringle, C. M. (2019). When to use and how to report the results of PLS-SEM. European Business Review, 31(1), 2–24.
# Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. Journal of Marketing Research, 18(1), 39–50.
# Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. Journal of the Academy of Marketing Science, 43(1), 115–135.
# Cohen, J. (1988). Statistical power analysis for the behavioral sciences. Mahwah, NJ: Lawrence Erlbaum.

# Langkah utama untuk analisis PLS-SEM yaitu:
# A. Persiapan ekosistem, data dan model,
# B. Measurement Model Assessment (periksa indikator ke variabel),
# C. Structural Model Assessment (periksa antar variabel),
# D. Plot gambar hasil analisis seminr

# A. Persiapan ekosistem, data dan model,

# a1. instal library seminr jika belum ada
# install.packages("seminr")

# Make the SEMinR library ready to use
library("seminr")

# a2. Load the corporate reputation data
corp_rep_data <- read.csv(file = "Corporate Reputation Data.csv", header = TRUE, sep = ";")

# Show the first several rows of the corporate reputation data
head(corp_rep_data)

# a3. Create measurement model 
# gambar model di link https://github.com/sundimand/R-statistics/blob/7ed605778c6d8034db5360942c6eaddb0917fbd6/corporate%20case%20Large.jpeg
simple_mm <- constructs(
composite("COMP", multi_items("comp_", 1:3)), 
composite("LIKE", multi_items("like_", 1:3)), 
composite("CUSA", single_item("cusa")), 
composite("CUSL", multi_items("cusl_", 1:3)))

# a4. Create structural model 
simple_sm <- relationships(
paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")), 
paths(from = c("CUSA"), to = c("CUSL")))

# a5. Estimate the model
corp_rep_simple_model <- estimate_pls(data = corp_rep_data,
  measurement_model = simple_mm, 
  structural_model = simple_sm, 
  inner_weights = path_weighting, 
  missing = mean_replacement, 
  missing_value = "-99")

#atau
# Estimate the model with default settings
corp_rep_simple_model <- estimate_pls(data = corp_rep_data, 
    measurement_model = simple_mm,
    structural_model = simple_sm,
    missing_value = "-99")

# a6. Summarize the model results
summary_simple_corp_rep <- summary(corp_rep_simple_model)


# B. Measurement Models Assessment (periksa indikator ke variabel),

# Reflective Measurement Models assessment procedure dengan langkah dibawah.
# Sementara untuk Formatif Measurement Model, maka dianggap bahwa:
# indikator setiap variabel menjadi satu variabel dalam struktural model,
# dan dilakukan structural models assestment tersendiri.

# b1. Indicator Reliability,
# Iterations to converge 
summary_simple_corp_rep$iterations

# Inspect the indicator loadings
# All indicator loadings of the reflectively measured constructs 
# COMP, CUSL, and LIKE are well above the threshold value of 0.708 
# (Hair, Risher, Sarstedt, & Ringle, 2019),
# which suggests sufficient levels of indicator reliability. 
summary_simple_corp_rep$loadings

# Inspect the indicator reliability
# The indicator comp_2 (loading, 0.798) has the smallest 
# indicator-explained variance with a value of 0.638
# (= 0.7982), while the indicator cusl_2 (loading, 0.917) has 
# the highest explained variance, with a value of 0.841 (= 0.9172) – 
# both values are well above the threshold value of 0.5.
summary_simple_corp_rep$loadings^2

# b2. Internal consistency reliability,
# Inspect the construct reliability metrics 
# Alpha, rhoC, and rhoA should exceed 0.7 

# b3. Convergent validity,
# while AVE should exceed 0.5
summary_simple_corp_rep$reliability

# Plot the reliabilities of constructs
plot(summary_simple_corp_rep$reliability)

# b4. Discriminant validity

# SEMinR offers several approaches to assess whether the construct measures 
# empirically demonstrate discriminant validity. 

# b41. According to the Fornell–Larcker criterion (Fornell & Larcker, 1981),
# The square root of the AVE of each construct should be higher than the 
# construct’s highest correlation with any other construct in the model 
# (this notion is identical to comparing the AVE with the squared correlations 
# between the constructs). These results can be outputted by inspecting 
# the summary_corp_rep object and validity element for the fl_criteria: 
# the reflectively measured construct COMP has a value of 0.825 for the square root
# of its AVE, which needs to be compared with all correlation values in the column
# of COMP (i.e., 0.645, 0.436, and 0.450), also for others variables.

# Table of the FL criteria
summary_simple_corp_rep$validity$fl_criteria

# b42. The primary criterion for discriminant validity assessment is the HTMT criterion
# For conceptually similar constructs, HTMT <0.90
# For conceptually different constructs, HTMT <0.85
# Test if the HTMT is significantly lower than the threshold value
# HTMT criterion
# all HTMT values are clearly lower than the more conservative
# threshold value of 0.85 (Henseler et al., 2015)
summary_simple_corp_rep$validity$htmt

# In addition to examining the HTMT values, researchers should test whether
# the HTMT values are significantly different from 1 or a lower threshold, such as
# 0.9 or even 0.85. This analysis requires computing bootstrap confidence intervals 
# obtained by running the bootstrapping procedure. To do so, use the boot-
# strap_model() function and assign the output to an object, such as boot_corp_rep.
# Bootstrap the model
boot_corp_rep <- bootstrap_model(seminr_model = corp_rep_simple_model, nboot = 1000)
sum_boot_corp_rep <- summary(boot_corp_rep, alpha = 0.10)

# Extract the bootstrapped HTMT
# the confidence intervals’ upper boundaries (5% CI and 95% CI), in our example, 
# are always lower than the threshold value of 0.90
sum_boot_corp_rep$bootstrapped_HTMT

# C. Structural Models Assessment (periksa antar variabel)

# b1. Assess Collinearity Issues of the Structural Model
# Critical collinearity issues likely occur if VIF ≥ 5
# Collinearity issues are usually uncritical if VIF = 3–5
# Collinearity is not a problematic issue if VIF < 3
# Inspect the structural model collinearity VIF
summary_simple_corp_rep$vif_antecedents

# c2. Assess the Significance and Relevance of the Structural Model Relationships

# Inspect the structural paths
sum_boot_corp_rep$bootstrapped_paths

# Inspect the total effects
sum_boot_corp_rep$bootstrapped_total_paths


# c3. Assess the Model’s Explanatory Power
# the model’s explanatory power by analyzing the R2 of the endogenous constructs 
# and the f2 effect size of the predictor constructs.

# Inspect the model RSquares
# R2 values of 0.75, 0.50, and 0.25 are considered substantial, moderate, and weak. 
# However, R2 values have to be interpreted in the context of the model 
# and its complexity. Excessive R2 values indicate that the model overfits the data
summary_simple_corp_rep$paths

# Inspect the effect sizes
# General guidelines for assessing ƒ2 suggest values of 0.02, 0.15, and 0.35 
# represent small, medium, and large effect sizes, respectively (Cohen, 1988).
summary_simple_corp_rep$fSquare

# c4. Assess the Model’s Predictive Power
# jika hanya menguji kausal model langkah ini tidak wajib

# c5. Model Comparisons
# jika hanya menguji kausal model langkah ini tidak wajib

# Inspect the bootstrapped structural paths 
sum_boot_corp_rep$bootstrapped_paths

# Inspect the bootstrapped indicator loadings 
sum_boot_corp_rep$bootstrapped_loadings

# D. Output model PLS SEM
# plot(boot_corp_rep, title = "Bootstrapped Model")
# save_plot("corporatePLSSEMa.png")

# d1. Pastikan paket terpasang jika belum ada
# install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# d2. Buat atau tangkap graph HTMLwidget (grViz)
graph <- plot(boot_corp_rep, title = "Bootstrapped Model")

# d3. Ekspor ke SVG string
svg_graph <- export_svg(graph)

# d4. Render ke PNG 1920×1080 px dan simpan ke file .png
rsvg_png(
  svg    = charToRaw(svg_graph),      # raw vector SVG
  file   = "bootstrap_model_hd.png",  # nama file output
  width  = 1920,                      # lebar piksel
  height = 1080                       # tinggi piksel
)
