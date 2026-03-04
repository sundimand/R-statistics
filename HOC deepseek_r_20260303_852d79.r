# ============================================================================
# ANALISIS PLS-SEM DENGAN HIGHER ORDER CONSTRUCT (HOC) TWO-STAGE
# Ref: 
# Sarsted et al 2019 (https://www.sciencedirect.com/science/article/abs/pii/S1441358219301223), 
# Hair et al. (2017, 2021)
# https://researchwithfawad.com/index.php/lp-courses/seminr-lecture-series/seminr-package-higher-order-analysis-ref-ref/
# ============================================================================

# 0. PERSIAPAN ----
# ============================================================================
# Install dan load paket yang diperlukan
# install.packages(c("seminr", "readr", "dplyr", "knitr", "kableExtra"))
library(seminr)
library(readr)
library(dplyr)
library(knitr)
library(kableExtra)

# 1. INPUT DATA ----
# ============================================================================
# Baca data dan seleksi indikator
cat("========================================\n")
cat("TAHAP 1: INPUT DATA\n")
cat("========================================\n\n")

# Link repository data https://zenodo.org/records/18850123
dat <- read_csv("dataDisertasifull.csv", show_col_types = FALSE)

dat_use <- dat %>%
  select(
    starts_with("EKSH"), starts_with("EKVI"), starts_with("EKPI"),
    starts_with("GSN"),  starts_with("GPR"),  starts_with("GRC"),
    starts_with("ABCP"), starts_with("ABCM"),
    starts_with("TPKK"), starts_with("TPTK")
  ) %>%
  select(-ends_with("T"), -ends_with("A")) %>%  # buang total/average
  mutate(across(everything(), as.numeric))

cat("Jumlah observasi:", nrow(dat_use), "\n")
cat("Jumlah indikator:", ncol(dat_use), "\n")
cat("Nama indikator:\n")
print(names(dat_use))

# 2. SPESIFIKASI MODEL ----
# ============================================================================
cat("\n\n========================================\n")
cat("TAHAP 2: SPESIFIKASI MODEL\n")
cat("========================================\n\n")

# 2.1 Measurement Model (Outer Model) - SEMUA REFLEKTIF (Mode A)
mm <- constructs(
  # ---- LOWER ORDER CONSTRUCTS (LOC) ----
  # Semua LOC reflektif (mode_A)
  composite("EKSH", multi_items("EKSH", 1:3), weights = mode_A),
  composite("EKVI", multi_items("EKVI", 1:3), weights = mode_A),
  composite("EKPI", multi_items("EKPI", 1:2), weights = mode_A),
  
  composite("GSN",  multi_items("GSN",  1:3), weights = mode_A),
  composite("GPR",  multi_items("GPR",  1:3), weights = mode_A),
  composite("GRC",  multi_items("GRC",  1:2), weights = mode_A),
  
  composite("ABCP", multi_items("ABCP", 1:4), weights = mode_A),
  composite("ABCM", multi_items("ABCM", 1:3), weights = mode_A),
  
  composite("TPKK", multi_items("TPKK", 1:3), weights = mode_A),
  composite("TPTK", multi_items("TPTK", 1:3), weights = mode_A),
  
  # ---- HIGHER ORDER CONSTRUCTS (HOC) ----
  # Two-stage approach (Hair et al., 2017)
  higher_composite("EK", c("EKSH", "EKVI", "EKPI"), method = two_stage, weights = mode_A),
  higher_composite("GX", c("GSN", "GPR", "GRC"),    method = two_stage, weights = mode_A),
  higher_composite("AB", c("ABCP", "ABCM"),        method = two_stage, weights = mode_A),
  higher_composite("TP", c("TPKK", "TPTK"),        method = two_stage, weights = mode_A)
)

# 2.2 Structural Model (Inner Model)
sm <- relationships(
  paths(from = "EK", to = c("AB", "GX", "TP")),
  paths(from = "AB", to = c("GX", "TP")),
  paths(from = "GX", to = "TP")
)

cat("Measurement Model: Semua konstruk reflektif (Mode A)\n")
cat("Structural Model: EK -> AB, GX, TP; AB -> GX, TP; GX -> TP\n")
cat("HOC Method: Two-stage (Hair et al., 2017)\n")

# 3. ESTIMASI MODEL ----
# ============================================================================
cat("\n\n========================================\n")
cat("TAHAP 3: ESTIMASI MODEL PLS-SEM\n")
cat("========================================\n\n")

set.seed(123)  # untuk reproduksibilitas
disertation_model <- estimate_pls(
  data = dat_use,
  measurement_model = mm,
  structural_model = sm,
  missing = mean_replacement  # handle missing values jika ada
)

cat("Model berhasil diestimasi!\n")
cat("Iterasi konvergensi:", disertation_model$iterations, "\n")

# 4. EVALUASI MODEL PENGUKURAN (FIRST STAGE - LOC) ----
# ============================================================================
cat("\n\n")
cat("====================================================================\n")
cat("TAHAP 4: EVALUASI MODEL PENGUKURAN (FIRST STAGE - LOC)\n")
cat("BERDASARKAN HAIR ET AL. (2017, 2021)\n")
cat("====================================================================\n\n")

evaluasi_loc <- function(pls_model) {
  
  # Ambil first stage model
  first_stage <- pls_model$first_stage_model
  if(is.null(first_stage)) {
    cat("First stage model tidak tersedia!\n")
    return(NULL)
  }
  
  # Dapatkan skor dan loading
  loadings_loc <- first_stage$outer_loadings
  scores_loc <- first_stage$construct_scores
  constructs_loc <- colnames(scores_loc)
  
  # Dataframe untuk menyimpan hasil
  hasil_loc <- data.frame(
    Konstruk = character(),
    Indikator = character(),
    Loading = numeric(),
    Cronbach_Alpha = numeric(),
    rho_A = numeric(),
    CR = numeric(),
    AVE = numeric(),
    stringsAsFactors = FALSE
  )
  
  cat("\n--- A. INDIKATOR RELIABILITY (OUTER LOADINGS) ---\n")
  cat("Syarat: Loading > 0.70 (Hair et al., 2017)\n")
  cat("----------------------------------------\n")
  
  for(const in constructs_loc) {
    # Ambil loading untuk konstruk ini
    const_loadings <- loadings_loc[loadings_loc[,const] != 0, const]
    indikator <- names(const_loadings)
    
    for(i in seq_along(const_loadings)) {
      loading_val <- const_loadings[i]
      status <- ifelse(loading_val > 0.70, "✓", ifelse(loading_val > 0.40, "⚠️", "✗"))
      
      cat(sprintf("%-10s %-8s : %8.4f %s\n", 
                  const, indikator[i], loading_val, status))
    }
  }
  
  cat("\n\n--- B. INTERNAL CONSISTENCY RELIABILITY ---\n")
  cat("Syarat: > 0.70 (Hair et al., 2017)\n")
  cat("----------------------------------------\n")
  
  # Hitung reliabilitas untuk setiap LOC
  for(const in constructs_loc) {
    # Ambil loading
    const_loadings <- loadings_loc[loadings_loc[,const] != 0, const]
    
    if(length(const_loadings) > 1) {
      # Hitung korelasi antar indikator untuk Cronbach's Alpha
      data_indikator <- first_stage$data[, names(const_loadings)]
      cor_matrix <- cor(data_indikator, use = "complete.obs")
      
      # Cronbach's Alpha
      k <- length(const_loadings)
      mean_cor <- mean(cor_matrix[lower.tri(cor_matrix)], na.rm = TRUE)
      cronbach_alpha <- (k * mean_cor) / (1 + (k - 1) * mean_cor)
      
      # rho_A (Dijkstra-Henseler, 2015)
      rho_A <- (sum(const_loadings)^2) / (sum(const_loadings)^2 + sum(1 - const_loadings^2))
      
      # Composite Reliability (CR)
      cr <- (sum(const_loadings))^2 / ((sum(const_loadings))^2 + sum(1 - const_loadings^2))
      
      # AVE
      ave <- mean(const_loadings^2)
      
      # Status
      status_alpha <- ifelse(cronbach_alpha > 0.70, "✓", ifelse(cronbach_alpha > 0.60, "⚠️", "✗"))
      status_rhoA <- ifelse(rho_A > 0.70, "✓", ifelse(rho_A > 0.60, "⚠️", "✗"))
      status_cr <- ifelse(cr > 0.70, "✓", ifelse(cr > 0.60, "⚠️", "✗"))
      status_ave <- ifelse(ave > 0.50, "✓", "✗")
      
      cat("\n", const, ":\n", sep="")
      cat(sprintf("  Cronbach's Alpha : %8.4f %s\n", cronbach_alpha, status_alpha))
      cat(sprintf("  rho_A            : %8.4f %s\n", rho_A, status_rhoA))
      cat(sprintf("  Composite Rel.   : %8.4f %s\n", cr, status_cr))
      cat(sprintf("  AVE              : %8.4f %s\n", ave, status_ave))
      
      # Simpan hasil
      for(i in seq_along(const_loadings)) {
        hasil_loc <- rbind(hasil_loc, data.frame(
          Konstruk = const,
          Indikator = names(const_loadings)[i],
          Loading = round(const_loadings[i], 4),
          Cronbach_Alpha = round(cronbach_alpha, 4),
          rho_A = round(rho_A, 4),
          CR = round(cr, 4),
          AVE = round(ave, 4),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  cat("\n\n--- C. DISCRIMINANT VALIDITY (FORNELL-LARCKER) ---\n")
  cat("Syarat: Akar AVE > korelasi antar konstruk (Fornell & Larcker, 1981)\n")
  cat("----------------------------------------\n")
  
  # Hitung matriks korelasi antar LOC
  cor_loc <- cor(scores_loc)
  
  # Hitung akar AVE
  sqrt_ave <- c()
  for(const in constructs_loc) {
    const_loadings <- loadings_loc[loadings_loc[,const] != 0, const]
    sqrt_ave[const] <- sqrt(mean(const_loadings^2))
  }
  
  # Tampilkan Fornell-Larcker
  fl_matrix <- cor_loc
  for(i in 1:length(constructs_loc)) {
    fl_matrix[i,i] <- sqrt_ave[constructs_loc[i]]
  }
  print(round(fl_matrix, 4))
  
  cat("\n\n--- D. HTMT (Heterotrait-Monotrait Ratio) - ESTIMASI TITIK ---\n")
  cat("Syarat awal: HTMT < 0.90 (Henseler et al., 2015)\n")
  cat("Catatan: Ini adalah screening awal, perlu bootstrap untuk konfirmasi\n")
  cat("----------------------------------------\n")
  
  # Hitung HTMT sederhana (non-bootstrap)
  htmt_matrix <- matrix(NA, length(constructs_loc), length(constructs_loc))
  rownames(htmt_matrix) <- constructs_loc
  colnames(htmt_matrix) <- constructs_loc
  diag(htmt_matrix) <- 1
  
  for(i in 1:(length(constructs_loc)-1)) {
    for(j in (i+1):length(constructs_loc)) {
      const_i <- constructs_loc[i]
      const_j <- constructs_loc[j]
      
      # HTMT sederhana (korelasi skor konstruk)
      htmt_val <- abs(cor_loc[i,j])
      htmt_matrix[i,j] <- htmt_val
      htmt_matrix[j,i] <- htmt_val
      
      status <- ifelse(htmt_val < 0.90, "✓", ifelse(htmt_val < 1.0, "⚠️", "✗"))
      cat(sprintf("%-8s <-> %-8s : %8.4f %s\n", 
                  const_i, const_j, htmt_val, status))
    }
  }
  
  cat("\n\n--- E. HTMT BOOTSTRAP - KONFIRMASI VALIDITAS DISKRIMINAN ---\n")
  cat("Dengan confidence interval 95% (Henseler et al., 2015)\n")
  cat("Syarat: CI atas < 0.90 untuk memastikan validitas\n")
  cat("----------------------------------------\n")
  
  # Fungsi HTMT Bootstrap
  htmt_bootstrap_loc <- function(first_stage, constructs, nboot = 500) {
    
    scores_loc <- first_stage$construct_scores
    n_konstruk <- length(constructs)
    
    # Bootstrap
    boot_htmt <- array(NA, dim = c(n_konstruk, n_konstruk, nboot))
    dimnames(boot_htmt) <- list(constructs, constructs, NULL)
    
    # Isi diagonal
    for(b in 1:nboot) {
      for(k in 1:n_konstruk) {
        boot_htmt[k,k,b] <- 1
      }
    }
    
    # Progress bar
    pb <- txtProgressBar(min = 1, max = nboot, style = 3)
    
    for(b in 1:nboot) {
      # Resample skor konstruk
      idx_boot <- sample(1:nrow(scores_loc), nrow(scores_loc), replace = TRUE)
      scores_boot <- scores_loc[idx_boot, ]
      
      # Hitung HTMT untuk sampel bootstrap
      for(i in 1:(n_konstruk-1)) {
        for(j in (i+1):n_konstruk) {
          htmt_val <- abs(cor(scores_boot[,i], scores_boot[,j]))
          boot_htmt[i,j,b] <- htmt_val
          boot_htmt[j,i,b] <- htmt_val
        }
      }
      
      setTxtProgressBar(pb, b)
    }
    close(pb)
    
    # Tampilkan hasil
    cat("\nHASIL HTMT BOOTSTRAP (95% CI):\n")
    
    hasil_htmt_df <- data.frame()
    
    for(i in 1:(n_konstruk-1)) {
      for(j in (i+1):n_konstruk) {
        boot_vals <- boot_htmt[i,j,]
        mean_val <- mean(boot_vals, na.rm = TRUE)
        sd_val <- sd(boot_vals, na.rm = TRUE)
        ci_lower <- quantile(boot_vals, 0.025, na.rm = TRUE)
        ci_upper <- quantile(boot_vals, 0.975, na.rm = TRUE)
        
        # Status berdasarkan CI
        if(ci_upper < 0.90) {
          status <- "✓ VALID (CI < 0.90)"
        } else if(ci_lower > 0.90) {
          status <- "✗ TIDAK VALID (CI > 0.90)"
        } else if(ci_lower < 0.90 && ci_upper > 0.90) {
          status <- "? RAGU-RAGU (CI mencakup 0.90)"
        } else {
          status <- "-"
        }
        
        cat(sprintf("\n%s <-> %s:\n", constructs[i], constructs[j]))
        cat(sprintf("  HTMT = %.4f (SE = %.4f)\n", mean_val, sd_val))
        cat(sprintf("  95%% CI = [%.4f, %.4f]\n", ci_lower, ci_upper))
        cat(sprintf("  Status: %s\n", status))
        
        hasil_htmt_df <- rbind(hasil_htmt_df, data.frame(
          Pasangan = paste(constructs[i], "<->", constructs[j]),
          HTMT = round(mean_val, 4),
          SE = round(sd_val, 4),
          CI_Lower = round(ci_lower, 4),
          CI_Upper = round(ci_upper, 4),
          Status = status
        ))
      }
    }
    
    return(list(
      boot_samples = boot_htmt,
      ringkasan = hasil_htmt_df
    ))
  }
  
  # Jalankan HTMT bootstrap
  hasil_htmt_boot <- htmt_bootstrap_loc(first_stage, constructs_loc, nboot = 500)
  
  # Simpan hasil
  attr(hasil_loc, "htmt_matrix") <- htmt_matrix
  attr(hasil_loc, "htmt_bootstrap") <- hasil_htmt_boot
  
  cat("\n\n--- RINGKASAN VALIDITAS DISKRIMINAN ---\n")
  cat("========================================\n")
  cat("Berdasarkan HTMT bootstrap:\n")
  
  if(all(hasil_htmt_boot$ringkasan$CI_Upper < 0.90)) {
    cat("✓ SEMUA KONSTRUK MEMILIKI VALIDITAS DISKRIMINAN YANG BAIK\n")
  } else if(any(hasil_htmt_boot$ringkasan$CI_Lower > 0.90)) {
    cat("✗ BEBERAPA KONSTRUK BERMASALAH:\n")
    problematic <- hasil_htmt_boot$ringkasan[hasil_htmt_boot$ringkasan$CI_Lower > 0.90, "Pasangan"]
    cat(paste("  -", problematic, collapse = "\n"))
  } else {
    cat("? VALIDITAS DISKRIMINAN RAGU-RAGU, perlu evaluasi lebih lanjut\n")
  }
  
  return(hasil_loc)
}

# Jalankan evaluasi LOC
hasil_evaluasi_loc <- evaluasi_loc(disertation_model)

# 5. EVALUASI MODEL PENGUKURAN (SECOND STAGE - HOC) ----
# ============================================================================
cat("\n\n")
cat("====================================================================\n")
cat("TAHAP 5: EVALUASI MODEL PENGUKURAN (SECOND STAGE - HOC)\n")
cat("BERDASARKAN HAIR ET AL. (2017, 2021)\n")
cat("====================================================================\n\n")

evaluasi_hoc <- function(pls_model) {
  
  # Ambil skor HOC
  scores_hoc <- pls_model$construct_scores
  constructs_hoc <- colnames(scores_hoc)
  
  cat("\n--- A. RELIABILITAS HOC (BERDASARKAN DIMENSI) ---\n")
  cat("HOC reflektif: Dimensi sebagai indikator HOC\n")
  cat("----------------------------------------\n")
  
  # Korelasi antar HOC
  cat("\nKorelasi antar HOC:\n")
  cor_hoc <- cor(scores_hoc)
  print(round(cor_hoc, 4))
  
  cat("\n--- B. VALIDITAS DISKRIMINAN HOC ---\n")
  cat("HTMT antar HOC < 0.90 (Henseler et al., 2015)\n")
  cat("----------------------------------------\n")
  
  for(i in 1:(length(constructs_hoc)-1)) {
    for(j in (i+1):length(constructs_hoc)) {
      htmt_val <- abs(cor_hoc[i,j])
      status <- ifelse(htmt_val < 0.90, "✓", ifelse(htmt_val < 1.0, "⚠️", "✗"))
      cat(sprintf("%-4s <-> %-4s : %8.4f %s\n", 
                  constructs_hoc[i], constructs_hoc[j], htmt_val, status))
    }
  }
  
  cat("\n--- C. R-SQUARED (ENDOGENOUS HOC) ---\n")
  cat("----------------------------------------\n")
  rsq <- pls_model$rSquared
  rsq_df <- data.frame(
    Konstruk = rownames(rsq),
    RSquare = round(rsq[,1], 4),
    Adj_RSquare = round(rsq[,2], 4)
  )
  print(rsq_df)
}

# Jalankan evaluasi HOC
evaluasi_hoc(disertation_model)

# 6. EVALUASI MODEL STRUKTURAL (PATH COEFFICIENTS) ----
# ============================================================================
cat("\n\n")
cat("====================================================================\n")
cat("TAHAP 6: EVALUASI MODEL STRUKTURAL\n")
cat("PATH COEFFICIENTS (SECOND STAGE)\n")
cat("====================================================================\n\n")

cat("PATH COEFFICIENTS:\n")
cat("----------------------------------------\n")
paths <- disertation_model$path_coef
print(round(paths, 4))

# 7. BOOTSTRAP UNTUK PENGUJIAN HIPOTESIS ----
# ============================================================================
cat("\n\n")
cat("====================================================================\n")
cat("TAHAP 7: BOOTSTRAP UNTUK PENGUJIAN HIPOTESIS\n")
cat("(5000 resamples - Hair et al., 2017)\n")
cat("====================================================================\n\n")

# Fungsi untuk bootstrap dan uji hipotesis
bootstrap_hipotesis <- function(pls_model, nboot = 1000) {
  
  cat("Menjalankan bootstrap dengan", nboot, "sampel...\n")
  cat("(Ini mungkin membutuhkan waktu beberapa menit)\n\n")
  
  # Jalankan bootstrap - simpan ke boot_model
  boot_model <- bootstrap_model(pls_model, nboot = nboot)
  
  # Ambil path coefficients dari bootstrap
  boot_paths <- boot_model$boot_paths
  paths <- pls_model$path_coef  # tambahkan ini!
  
  # Dataframe untuk hasil
  hasil_hipotesis <- data.frame(
    Hipotesis = character(),
    Path = character(),
    Original = numeric(),
    Mean_Boot = numeric(),
    SE = numeric(),
    t_stat = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    p_value = numeric(),
    Signifikan = character(),
    stringsAsFactors = FALSE
  )
  
  # Semua jalur dalam model
  jalur <- list(
    c("EK", "AB", "H1: EK -> AB"),
    c("EK", "GX", "H2: EK -> GX"),
    c("EK", "TP", "H3: EK -> TP"),
    c("AB", "GX", "H4: AB -> GX"),
    c("AB", "TP", "H5: AB -> TP"),
    c("GX", "TP", "H6: GX -> TP")
  )
  
  cat("HASIL BOOTSTRAP PATH COEFFICIENTS:\n")
  cat("========================================\n\n")
  
  for(jalur_info in jalur) {
    from <- jalur_info[1]
    to <- jalur_info[2]
    label <- jalur_info[3]
    
    # Nilai original
    original <- paths[from, to]
    
    if(original != 0) {
      # Statistik bootstrap
      boot_vals <- boot_paths[from, to, ]
      mean_boot <- mean(boot_vals, na.rm = TRUE)
      se <- sd(boot_vals, na.rm = TRUE)
      t_stat <- mean_boot / se
      
      # Confidence interval
      ci_lower <- quantile(boot_vals, 0.025, na.rm = TRUE)
      ci_upper <- quantile(boot_vals, 0.975, na.rm = TRUE)
      
      # P-value (two-tailed)
      p_value <- 2 * (1 - pnorm(abs(t_stat)))
      
      # Signifikansi
      if(ci_lower * ci_upper > 0) {
        signif <- "SIGNIFIKAN"
        bintang <- ifelse(p_value < 0.001, "***", 
                         ifelse(p_value < 0.01, "**",
                               ifelse(p_value < 0.05, "*", "ns")))
      } else {
        signif <- "TIDAK SIGNIFIKAN"
        bintang <- "ns"
      }
      
      # Tampilkan
      cat(sprintf("%-15s: %8.4f (t = %6.3f, p = %7.5f) %s\n",
                  label, original, t_stat, p_value, bintang))
      cat(sprintf("             95%% CI = [%8.4f, %8.4f] - %s\n",
                  ci_lower, ci_upper, signif))
      
      # Simpan
      hasil_hipotesis <- rbind(hasil_hipotesis, data.frame(
        Hipotesis = label,
        Path = paste(from, "->", to),
        Original = round(original, 4),
        Mean_Boot = round(mean_boot, 4),
        SE = round(se, 4),
        t_stat = round(t_stat, 3),
        CI_lower = round(ci_lower, 4),
        CI_upper = round(ci_upper, 4),
        p_value = round(p_value, 5),
        Signifikan = signif,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  cat("\n\n--- RINGKASAN HIPOTESIS ---\n")
  cat("========================================\n")
  print(hasil_hipotesis[, c("Hipotesis", "Original", "t_stat", "p_value", "Signifikan")])
  
  # RETURN DUA OBJEK: hasil ringkasan DAN boot_model
  return(list(
    ringkasan = hasil_hipotesis,
    boot_model = boot_model
  ))
}

# Jalankan bootstrap (simpan ke list)
hasil_bootstrap <- bootstrap_hipotesis(disertation_model, nboot = 500)

# Ekstrak kedua komponen
hasil_boot <- hasil_bootstrap$ringkasan      # dataframe ringkasan
boot_model <- hasil_bootstrap$boot_model      # objek bootstrap mentah

# Untuk final (5000 sampel):
# hasil_bootstrap <- bootstrap_hipotesis(disertation_model, nboot = 5000)
# hasil_boot <- hasil_bootstrap$ringkasan
# boot_model <- hasil_bootstrap$boot_model

# 8. ANALISIS MEDIASI DENGAN BOOTSTRAP ----
# ============================================================================
cat("\n\n")
cat("====================================================================\n")
cat("TAHAP 8: ANALISIS MEDIASI DENGAN BOOTSTRAP\n")
cat("(Preacher & Hayes, 2008; Hair et al., 2017)\n")
cat("====================================================================\n\n")

analisis_mediasi_bootstrap <- function(pls_model, boot_model) {
  
  # Validasi input
  if(!"boot_paths" %in% names(boot_model)) {
    stop("Error: boot_model harus dari bootstrap_model(), bukan hasil ringkasan!")
  }
  
  boot_paths <- boot_model$boot_paths
  n_samples <- dim(boot_paths)[3]
  
  cat("Jumlah sampel bootstrap:", n_samples, "\n\n")
  
  # Jalur mediasi
  mediasi_list <- list(
    list(name = "EK -> AB -> TP", a = c("EK", "AB"), b = c("AB", "TP"), c = c("EK", "TP")),
    list(name = "EK -> GX -> TP", a = c("EK", "GX"), b = c("GX", "TP"), c = c("EK", "TP")),
    list(name = "AB -> GX -> TP", a = c("AB", "GX"), b = c("GX", "TP"), c = c("AB", "TP"))
  )
  
  hasil_mediasi <- data.frame()
  
  for(m in mediasi_list) {
    cat("\n", m$name, "\n")
    cat("----------------------------------------\n")
    
    # Validasi jalur ada
    if(!all(m$a %in% dimnames(boot_paths)[[1]]) || 
       !all(m$b %in% dimnames(boot_paths)[[1]])) {
      cat("Warning: Jalur tidak ditemukan, dilewati\n")
      next
    }
    
    # Hitung indirect effect untuk setiap sampel
    indirect_effects <- numeric(n_samples)
    direct_effects <- numeric(n_samples)
    
    for(i in 1:n_samples) {
      path_a <- boot_paths[m$a[1], m$a[2], i]
      path_b <- boot_paths[m$b[1], m$b[2], i]
      indirect_effects[i] <- path_a * path_b
      direct_effects[i] <- boot_paths[m$c[1], m$c[2], i]
    }
    
    # Statistik indirect effect
    ie_mean <- mean(indirect_effects, na.rm = TRUE)
    ie_sd <- sd(indirect_effects, na.rm = TRUE)
    ie_t <- ie_mean / ie_sd
    ie_ci <- quantile(indirect_effects, c(0.025, 0.975), na.rm = TRUE)
    
    # Statistik direct effect
    de_mean <- mean(direct_effects, na.rm = TRUE)
    de_sd <- sd(direct_effects, na.rm = TRUE)
    de_ci <- quantile(direct_effects, c(0.025, 0.975), na.rm = TRUE)
    
    # Total effect
    total_mean <- de_mean + ie_mean
    
    # VAF
    if(total_mean != 0) {
      vaf <- (abs(ie_mean) / abs(total_mean)) * 100
    } else {
      vaf <- NA
    }
    
    # Signifikansi mediasi
    if(ie_ci[1] * ie_ci[2] > 0) {
      mediasi_sig <- "SIGNIFIKAN"
      jenis <- ifelse(abs(de_mean) > 0 & de_ci[1] * de_ci[2] > 0, 
                      "Mediasi Parsial", "Mediasi Penuh")
    } else {
      mediasi_sig <- "TIDAK SIGNIFIKAN"
      jenis <- "Tidak Ada Mediasi"
    }
    
    # Tampilkan
    cat(sprintf("Indirect Effect : %8.4f (SE = %6.4f, t = %6.3f)\n", ie_mean, ie_sd, ie_t))
    cat(sprintf("95%% CI          : [%8.4f, %8.4f]\n", ie_ci[1], ie_ci[2]))
    cat(sprintf("Direct Effect   : %8.4f\n", de_mean))
    cat(sprintf("Total Effect    : %8.4f\n", total_mean))
    cat(sprintf("VAF             : %6.2f%%\n", vaf))
    cat(sprintf("Kesimpulan      : %s (%s)\n", mediasi_sig, jenis))
    
    # Simpan
    hasil_mediasi <- rbind(hasil_mediasi, data.frame(
      Mediasi = m$name,
      Indirect = round(ie_mean, 4),         
      SE = round(ie_sd, 4),
      t_stat = round(ie_t, 3),
      CI_lower = round(ie_ci[1], 4),
      CI_upper = round(ie_ci[2], 4),
      Direct = round(de_mean, 4),
      Total = round(total_mean, 4),
      VAF = round(vaf, 2),
      Signifikansi = mediasi_sig,
      Jenis = jenis
    ))
  }
  
  return(hasil_mediasi)
}

# Jalankan analisis mediasi (GUNAKAN boot_model, BUKAN hasil_boot)
hasil_mediasi <- analisis_mediasi_bootstrap(disertation_model, boot_model)

# Lihat hasil
print(hasil_mediasi)

# 9. RINGKASAN HASIL ----
# ============================================================================
cat("\n\n")
cat("====================================================================\n")
cat("TAHAP 9: RINGKASAN HASIL ANALISIS\n")
cat("====================================================================\n\n")

# 9.1 Ringkasan Model Pengukuran LOC
cat("\n--- RINGKASAN MODEL PENGUKURAN (LOC) ---\n")
cat("========================================\n")
if(!is.null(hasil_evaluasi_loc)) {
  print(hasil_evaluasi_loc %>% 
          group_by(Konstruk) %>%
          summarise(
            Jumlah_Indikator = n(),
            Mean_Loading = mean(Loading),
            Cronbach_Alpha = first(Cronbach_Alpha),
            rho_A = first(rho_A),
            CR = first(CR),
            AVE = first(AVE)
          ) %>%
          mutate(across(where(is.numeric), round, 4)))
}

# 9.2 Ringkasan Hipotesis
cat("\n\n--- RINGKASAN PENGUJIAN HIPOTESIS ---\n")
cat("========================================\n")
print(hasil_boot[, c("Hipotesis", "Original", "t_stat", "p_value", "Signifikan")])

# 9.3 Ringkasan Mediasi
cat("\n\n--- RINGKASAN ANALISIS MEDIASI ---\n")
cat("========================================\n")
print(hasil_mediasi[, c("Mediasi", "Indirect", "VAF", "Jenis")])

# 10. EKSPOR HASIL (OPSIONAL) ----
# ============================================================================
# Simpan hasil ke file CSV
write.csv(hasil_boot, "hasil_hipotesis.csv", row.names = FALSE)
write.csv(hasil_mediasi, "hasil_mediasi.csv", row.names = FALSE)

cat("\n\n")
cat("====================================================================\n")
cat("ANALISIS SELESAI\n")
cat("Hasil disimpan dalam:\n")
cat("- hasil_hipotesis.csv\n")
cat("- hasil_mediasi.csv\n")
cat("====================================================================\n")


# ============================================================================
# 11. PLOT PATH DIAGRAM ----

plot(disertation_model, 
     title = "Model Struktural PLS-SEM dengan HOC\n(EK, AB, GX, TP)",
     show = "paths",
     digits = 3,
     cex = 1.2)

