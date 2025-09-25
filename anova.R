# ===== Contoh 1b uji Anova Promo

# 1. Load the data kelas
ad_data <- read.csv(file = "data_promo.csv", header = TRUE, sep = ",")

# 2. Ringkasan data
summary(ad_data)
head(ad_data, 10)

# 3. Cek asumsi

# 3a. ANOVA model
fit <- aov(Profit ~ Promotion, data = ad_data)

# 3b. Normalitas residual (Shapiro-Wilk)
norm_test <- shapiro.test(residuals(fit))

# 3c. Homogenitas varians (Levene)
library(car)
levene_test <- leveneTest(Profit ~ Promotion, data = ad_data)

# 4. Ringkasan hasil uji asumsi
list(
  normality  = norm_test,
  homogeneity = levene_test
)

# 5. Uji ANOVA dan post-hoc
anova_res <- fit
anova_summary <- summary(anova_res)
tukey_res    <- TukeyHSD(anova_res, "Promotion")

# 6. Cetak hasil
anova_summary
tukey_res