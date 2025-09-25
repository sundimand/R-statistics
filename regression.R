# ======= regression =========

# 1. Load the data and library
library(car)       # untuk VIF
# install.packages("lmtest") # jika pertama kali
library(lmtest)    # untuk Breusch-Pagan & Durbin-Watson
library(ggplot2)   # untuk plotting tambahan

ad_data <- read.csv(file = "data_kampanye.csv", header = TRUE, sep = ",")
# x1:	Anggaran Iklan Digital Perbulan, Total anggaran iklan yang dikeluarkan untuk kampanye (dalam juta rupiah)
# x2:	Jumlah Konten Dipublikasikan Perbulan, Jumlah total konten (post, video, artikel) yang dipublikasikan selama kampanye
# x3:	Skor Interaksi Audiens, Indeks engagement (like, share, comment, CTR) dari audiens terhadap konten
# y: Skor Efektivitas Kampanye, Skor gabungan yang mencerminkan performa kampanye (misal skala 0–100)

# 2. Ringkasan data
summary(ad_data)
head(ad_data, 10)

# 3. Fit model regresi linear berganda
model <- lm(y ~ x1 + x2 + x3, data = ad_data)

# 4. Uji Asumsi

# 4a.  Normalitas residual (Shapiro-Wilk)
# Memeriksa apakah residual model berdistribusi normal.
# p-value > 0.05 → gagal tolak H₀ → residual dianggap normal.
# W mendekati 1 menunjukkan distribusi residual sangat mendekati normal.
# Kesimpulan: Asumsi normalitas residual terpenuhi.
sw <- shapiro.test(residuals(model))
cat("Shapiro-Wilk W =", round(sw$statistic,4),
    ", p-value =", round(sw$p.value,4), "\n")

# 4b.  Homoskedastisitas (Breusch-Pagan)
# Memeriksa apakah varians residual konstan (homoskedastisitas).
# p-value > 0.05 → gagal tolak H₀ → tidak ada bukti heteroskedastisitas.
# Varians residual dianggap homogen di seluruh nilai prediksi.
# Kesimpulan: Asumsi homoskedastisitas terpenuhi.
bp <- bptest(model)
cat("Breusch-Pagan χ² =", round(bp$statistic,4),
    ", df =", bp$parameter,
    ", p-value =", round(bp$p.value,4), "\n")

# 4c.  Multikolinearitas (VIF)
# Memeriksa apakah variabel independen saling berkorelasi tinggi.
# VIF < 5 (atau < 10) → tidak ada multikolinearitas serius. –
# Nilai mendekati 1 menunjukkan variabel sangat independen satu sama lain.
# Kesimpulan: Tidak ada masalah multikolinearitas.
vifs <- vif(model)
print(data.frame(Variable = names(vifs), VIF = round(vifs,3)))

# 4d.  Autokorelasi residual (Durbin-Watson)
# Memeriksa apakah residual saling berkorelasi (terutama antar waktu).
# DW ≈ 2 → tidak ada autokorelasi. 
# p-value > 0.05 → gagal tolak H₀ → residual bebas autokorelasi.
# Kesimpulan: Asumsi bebas autokorelasi terpenuhi.
dw <- dwtest(model)
cat("Durbin-Watson =", round(dw$statistic,4),
    ", p-value =", round(dw$p.value,4), "\n")

# Plot diagnostik dasar
par(mfrow = c(2,2))
plot(model)

# Plot scatter X vs residual untuk cek linearitas (ada 4 macam grafik)
# Residuals vs Fitted → Cek pola non-linear: jika titik menyebar acak di sekitar garis horizontal, maka linearitas terpenuhi.
# Normal Q-Q → Cek normalitas residual: titik-titik harus mendekati garis diagonal.
# Scale-Location (Spread vs Fitted) → Cek homoskedastisitas: titik menyebar merata, bukan membentuk corong.
# Residuals vs Leverage → Cek pengaruh outlier: titik ekstrem bisa menunjukkan observasi berpengaruh tinggi.
resid <- residuals(model)

# jika titik menyebar acak → hubungan Y dan x1 cukup linear.
ggplot(ad_data, aes(x = x1, y = resid)) +
  geom_point(alpha=0.4) + geom_hline(yintercept=0, color="red") +
  ggtitle("Residuals vs x1")
ggplot(ad_data, aes(x = x2, y = resid)) +
  geom_point(alpha=0.4) + geom_hline(yintercept=0, color="red") +
  ggtitle("Residuals vs x2")
ggplot(ad_data, aes(x = x3, y = resid)) +
  geom_point(alpha=0.4) + geom_hline(yintercept=0, color="red") +
  ggtitle("Residuals vs x3")

# 5. Summary hasil regresi linear berganda
print(summary(model))
