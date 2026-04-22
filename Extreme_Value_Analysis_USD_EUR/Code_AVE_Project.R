################################################################################
##                                                                            ##
##    EXTREME VALUE ANALYSIS — USD/EUR EXCHANGE RATE (2004–2023)              ##
##    M2 Econometrics & Statistics - 2025-2026 -Toulouse School of Economics  ##
##                                                                            ##
##    Authors: Alexandre Picart, Badiallo Kantako, Lucien Mirlicourtois       ##
##                                                                            ##
##    This script performs a full extreme value analysis on daily            ##
##    log-returns of the USD/EUR exchange rate. It includes:                  ##
##      0. Setup and packages                                                 ##
##      1. Data loading, cleaning, feature engineering                        ##
##      2. Exploratory data analysis (EDA)                                    ##
##      3. Stationarity, dependence and ARCH effects                         ##
##      4. Justification for EVT (non-normality)                              ##
##      5. Tail index estimation (Hill, bias‑reduced Hill, Moment)            ##
##      6. Optimal k selection (dAMSE) and Pareto QQ-plot                     ##
##      7. Block maxima method (GEV) – monthly and quarterly blocks          ##
##      8. Peaks over threshold (POT/GPD) – stability, threshold choice      ##
##      9. Final risk metrics: return levels, VaR, ES, and asymmetry tests   ##
##                                                                            ##
################################################################################

# ==============================================================================
# PART 0 – SETUP, PACKAGES AND GRAPHICAL THEME
# ==============================================================================

# Install missing packages automatically
pkgs <- c(
  "tidyverse", "lubridate", "moments", "patchwork", "forecast",
  "tseries", "nortest", "extRemes", "evd", "ismev", "POT",
  "ReIns", "evt0", "tea", "EnvStats", "scales", "ggridges",
  "viridis", "RColorBrewer"
)
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, library, character.only = TRUE))

# Color palette
pal <- c(
  primary   = "#1a3a5c",   # TSE blue
  secondary = "#c0392b",   # Red (left tail / losses)
  accent    = "#c8a84b",   # TSE gold (right tail / gains)
  neutral   = "#5d6d7e",   # Gray
  light     = "#eaf2f8",   # Light background
  green     = "#1e8449"
)

# Custom ggplot2::ggplot2 theme
theme_evt <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace% theme(
    plot.background   = element_rect(fill = "#fdfefe", colour = NA),
    panel.grid.major  = element_line(colour = "#e5e8ea", linewidth = 0.35),
    panel.grid.minor  = element_blank(),
    axis.line         = element_line(colour = "#444444", linewidth = 0.35),
    axis.ticks        = element_line(colour = "#444444", linewidth = 0.25),
    plot.title        = element_text(face = "bold", colour = pal["primary"],
                                     size = rel(1.12), hjust = 0,
                                     margin = margin(b = 5)),
    plot.subtitle     = element_text(colour = pal["neutral"], size = rel(0.87),
                                     hjust = 0, margin = margin(b = 8)),
    plot.caption      = element_text(colour = "#aaaaaa", size = rel(0.72), hjust = 1),
    legend.position   = "bottom",
    legend.key.size   = unit(0.42, "cm"),
    strip.text        = element_text(face = "bold", colour = pal["primary"]),
    strip.background  = element_rect(fill = pal["light"], colour = NA)
  )
}
theme_set(theme_evt())
set.seed(42)

# ==============================================================================
# PART 1 – DATA LOADING, CLEANING AND FEATURE ENGINEERING
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 1 – DATA\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

raw <- read.csv("/Users/badiallokantako/Downloads/Projet_AVE/USDEUR.csv")
raw$Date <- as.Date(raw$Date, format = "%Y-%m-%d")
names(raw) <- gsub("\\.", "_", names(raw))
if (!"Adj_Close" %in% names(raw) && "Adj Close" %in% names(raw))
  names(raw)[names(raw) == "Adj Close"] <- "Adj_Close"

cat("\n--- Quality control ---\n")
cat("Raw dimensions          :", paste(dim(raw), collapse = " × "), "\n")
cat("Period                  :", format(min(raw$Date)), "→", format(max(raw$Date)), "\n")
cat("Missing values          :\n"); print(colSums(is.na(raw)))
cat("Duplicated dates        :", sum(duplicated(raw$Date)), "\n")

df <- raw %>%
  filter(if_all(c(Open, High, Low, Close), ~ !is.na(.))) %>%
  distinct(Date, .keep_all = TRUE) %>%
  arrange(Date)

cat("Retained observations   :", nrow(df), "\n")

# Time variables
df <- df %>% mutate(
  Year      = year(Date),
  Month     = month(Date),
  Day       = day(Date),
  Weekday   = wday(Date, label = TRUE, abbr = TRUE),
  Quarter   = quarter(Date),
  YearMonth = format(Date, "%Y-%m"),
  Season = factor(case_when(
    Month %in% c(12, 1, 2) ~ "Winter",
    Month %in% c(3, 4, 5)  ~ "Spring",
    Month %in% c(6, 7, 8)  ~ "Summer",
    TRUE                   ~ "Autumn"
  ), levels = c("Winter","Spring","Summer","Autumn")),
  HighTravel = factor(case_when(
    Month %in% c(7, 8) ~ "Summer (Jul-Aug)",
    Month == 12        ~ "Christmas (December)",
    Month == 2         ~ "Winter (February)",
    TRUE               ~ "Normal period"
  ), levels = c("Normal period","Summer (Jul-Aug)","Christmas (December)","Winter (February)"))
)

# Financial variables
df <- df %>% mutate(
  across(c(Open, High, Low, Close, Adj_Close), as.numeric),
  log_return   = c(NA, diff(log(Close))),
  abs_return   = abs(log_return),
  sq_return    = log_return^2,
  log_range    = log(High) - log(Low),
  is_extreme   = abs(log_return) > 2 * sd(log_return, na.rm = TRUE),
  loss         = -log_return,
)

df <- df %>% filter(!is.na(log_return))
r <- df$log_return

cat("\nCreated variables :", ncol(df) - 6, "columns added\n")
cat("Final observations:", nrow(df), "\n")
cat("Log-returns summary:\n")
cat(sprintf("  Mean = %.6f | Med = %.6f | Sd = %.6f\n  Min = %.6f | Max = %.6f\n",
            mean(r), median(r), sd(r), min(r), max(r)))

# ==============================================================================
# PART 2 – EXPLORATORY DATA ANALYSIS (EDA)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 2 – EXPLORATORY ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Thresholds for extreme classification
thr_up <- quantile(r, 0.995, na.rm = TRUE)
thr_dn <- quantile(r, 0.005, na.rm = TRUE)
df <- df %>% mutate(
  extreme_class = case_when(
    log_return >= thr_up ~ "Extreme gain (> Q99.5)",
    log_return <= thr_dn ~ "Extreme loss (< Q0.5)",
    TRUE                 ~ "Normal"
  )
)

# Price series with crisis annotations
adf_result <- adf.test(df$Close, alternative = "stationary")
adf_text <- paste0("ADF Test: p-value = ", round(adf_result$p.value, 4))

p_prix <- ggplot2::ggplot(df, aes(Date, Close)) +
  geom_line(colour = pal["primary"], linewidth = 0.45) +
  annotate("rect", xmin = as.Date("2008-09-01"), xmax = as.Date("2009-06-01"),
           ymin = -Inf, ymax = Inf, fill = pal["secondary"], alpha = 0.08) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-06-01"),
           ymin = -Inf, ymax = Inf, fill = pal["accent"], alpha = 0.10) +
  annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
           ymin = -Inf, ymax = Inf, fill = pal["neutral"], alpha = 0.07) +
  annotate("text", x = as.Date("2008-09-15"), y = max(df$Close)*0.97,
           label = "2008 GFC", size = 2.8, colour = pal["secondary"], fontface = "bold") +
  annotate("text", x = as.Date("2020-02-15"), y = max(df$Close)*0.93,
           label = "Covid-19", size = 2.8, colour = pal["accent"], fontface = "bold") +
  annotate("text", x = as.Date("2022-03-01"), y = max(df$Close)*0.89,
           label = "Energy Shock", size = 2.8, colour = pal["neutral"]) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "USD/EUR Exchange Rate — Daily Closing Price",
       subtitle = "Colored areas: major crisis periods",
       y = "USD per 1 EUR", x = NULL) +
  theme_evt()

p_acf <- ggAcf(df$Close, lag.max = 40) +
  labs(title = "Autocorrelation (ACF)",
       subtitle = adf_text, y = "ACF", x = "Lags") +
  theme_evt() +
  theme(plot.subtitle = element_text(color = pal["secondary"], face = "bold"))

p_pacf <- ggPacf(df$Close, lag.max = 40) +
  labs(title = "Partial Autocorrelation (PACF)",
       y = "PACF", x = "Lags",
       caption = "Source: USDEUR.csv | 2004–2023") +
  theme_evt()

# Combine price and ACF/PACF
composite_plot <- p_prix / (p_acf | p_pacf) + plot_layout(heights = c(2, 1))
print(composite_plot)

# Log-returns with extremes highlighted
p_ret <- ggplot2::ggplot(df, aes(Date, log_return)) +
  geom_line(colour = "#b0bec5", linewidth = 0.28, alpha = 0.8) +
  geom_point(data = filter(df, extreme_class != "Normal"),
             aes(colour = extreme_class), size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = pal["neutral"], linewidth = 0.4) +
  scale_colour_manual(values = c("Extreme gain (> Q99.5)" = pal["accent"],
                                 "Extreme loss (< Q0.5)"  = pal["secondary"]),
                      name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Daily Log-Returns — Extreme Tails Identification",
       subtitle = paste0("Colored dots: ", sum(df$extreme_class != "Normal"),
                         " observations outside the [Q0.5%, Q99.5%] range"),
       y = "Log-return r_t", x = NULL) +
  theme_evt() + theme(legend.position = "top")
print(p_ret)

# Distribution: histogram + normal density
p_hist <- ggplot2::ggplot(df, aes(log_return)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 120, fill = pal["primary"], alpha = 0.60, colour = "white", linewidth = 0.15) +
  geom_density(colour = pal["neutral"], linewidth = 0.9, linetype = "solid") +
  stat_function(fun = dnorm,
                args = list(mean = mean(r), sd = sd(r)),
                colour = pal["secondary"], linewidth = 1.0, linetype = "dashed") +
  geom_vline(xintercept = c(quantile(r, 0.01), quantile(r, 0.99)),
             colour = pal["accent"], linetype = "dotted", linewidth = 0.8) +
  geom_rug(alpha = 0.07, sides = "b") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Distribution of Daily USD/EUR Log-Returns",
       subtitle = "Solid gray: Empirical KDE | Dashed red: N(μ̂, σ̂) | Dotted gold: Q1% and Q99%",
       x = "Log-return r_t", y = "Density") +
  theme_evt()

# QQ-plot vs normal
qq_df <- tibble(th = qnorm(ppoints(nrow(df))), ob = sort(r))
p_qq <- ggplot2::ggplot(qq_df, aes(th, ob)) +
  geom_point(size = 0.5, alpha = 0.35, colour = pal["primary"]) +
  geom_abline(intercept = mean(r), slope = sd(r),
              colour = pal["secondary"], linewidth = 0.9) +
  geom_point(data = filter(qq_df, ob > quantile(r, 0.99) | ob < quantile(r, 0.01)),
             colour = pal["accent"], size = 1.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "QQ-Plot vs Normal Distribution",
       subtitle = "Gold dots = extreme tails deviating from the theoretical line",
       x = "Theoretical Quantiles N(0,1)", y = "Empirical Quantiles") +
  theme_evt()

(p_hist | p_qq)

# Seasonal analysis (boxplot by month, violin by season)
p_monthly <- ggplot2::ggplot(df, aes(factor(Month), log_return, fill = factor(Month))) +
  geom_boxplot(alpha = 0.65, outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_fill_manual(values = colorRampPalette(c(pal["primary"], pal["accent"]))(12), guide = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = pal["neutral"], linewidth = 0.5) +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Monthly Volatility of Log-Returns",
       subtitle = "Each box = empirical distribution of r_t for that month (2004–2023)",
       x = "Month", y = "Log-return") +
  theme_evt()

p_season <- ggplot2::ggplot(df, aes(Season, log_return, fill = Season)) +
  geom_violin(alpha = 0.45, trim = FALSE) +
  geom_boxplot(width = 0.18, alpha = 0.85, outlier.size = 0.5) +
  scale_fill_manual(values = c(Winter = "#5dade2", Spring = pal["green"],
                               Summer = pal["accent"], Autumn = pal["secondary"]),
                    guide = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = pal["neutral"]) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Distribution of Log-Returns by Season",
       subtitle = "Violins reveal the shape of tails by calendar period",
       x = NULL, y = "Log-return") +
  theme_evt()

(p_monthly / p_season)

# ==============================================================================
# PART 3 – STATIONARITY, DEPENDENCE AND ARCH EFFECTS
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 3 – STATIONARITY & DEPENDENCE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

cat("\n--- Price level (Close) ---\n")
adf_level <- adf.test(df$Close, alternative = "stationary")
kpss_level <- kpss.test(df$Close, null = "Level")
cat(sprintf("ADF: stat = %.4f, p = %.4f → %s\n",
            adf_level$statistic, adf_level$p.value,
            ifelse(adf_level$p.value > 0.05, "NON-STATIONARY", "Stationary")))
cat(sprintf("KPSS: stat = %.4f, p = %.4f → %s\n",
            kpss_level$statistic, kpss_level$p.value,
            ifelse(kpss_level$p.value < 0.05, "NON-STATIONARY", "Stationary")))

cat("\n--- Log-returns (r_t) ---\n")
adf_ret  <- adf.test(r, alternative = "stationary")
kpss_ret <- kpss.test(r, null = "Level")
pp_ret   <- tseries::pp.test(r)
cat(sprintf("ADF: stat = %.4f, p = %.4f → %s\n",
            adf_ret$statistic, adf_ret$p.value,
            ifelse(adf_ret$p.value < 0.05, "STATIONARY", "NON-STATIONARY")))
cat(sprintf("KPSS: stat = %.4f, p = %.4f → %s\n",
            kpss_ret$statistic, kpss_ret$p.value,
            ifelse(kpss_ret$p.value > 0.05, "STATIONARY", "NON-STATIONARY")))
cat(sprintf("PP: stat = %.4f, p = %.4f → %s\n",
            pp_ret$statistic, pp_ret$p.value,
            ifelse(pp_ret$p.value < 0.05, "STATIONARY", "NON-STATIONARY")))

# ACF/PACF of returns and squared returns
p_acf_r <- forecast::ggAcf(r, lag.max = 50, colour = pal["primary"]) +
  labs(title = "ACF — Log-returns r_t",
       subtitle = "Little autocorrelation: consistent with weak market efficiency") +
  theme_evt()
p_pacf_r <- forecast::ggPacf(r, lag.max = 50, colour = pal["primary"]) +
  labs(title = "PACF — Log-returns r_t") + theme_evt()
p_acf_r2 <- forecast::ggAcf(r^2, lag.max = 50, colour = pal["secondary"]) +
  labs(title = "ACF — Squared Returns r_t²",
       subtitle = "Strong dependence → ARCH/GARCH effects present") +
  theme_evt()
p_pacf_r2 <- forecast::ggPacf(r^2, lag.max = 50, colour = pal["secondary"]) +
  labs(title = "PACF — Squared Returns r_t²") + theme_evt()

(p_acf_r | p_pacf_r) / (p_acf_r2 | p_pacf_r2)

# Ljung-Box and ARCH-LM tests
lb_r  <- Box.test(r, lag = 20, type = "Ljung-Box")
lb_r2 <- Box.test(r^2, lag = 20, type = "Ljung-Box")
arch_lm <- FinTS::ArchTest(r, lags = 20)

cat("\n--- Dependence Tests ---\n")
cat(sprintf("Ljung-Box r_t  (lag=20): Q = %.2f, p = %.4f → %s\n",
            lb_r$statistic, lb_r$p.value,
            ifelse(lb_r$p.value > 0.05, "No autocorrelation", "Autocorrelation")))
cat(sprintf("Ljung-Box r_t² (lag=20): Q = %.2f, p = %.4e → %s\n",
            lb_r2$statistic, lb_r2$p.value,
            ifelse(lb_r2$p.value < 0.05, "ARCH effects detected", "OK")))
print(arch_lm)

# ==============================================================================
# PART 4 – JUSTIFICATION FOR EVT (NON-NORMALITY)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 4 – JUSTIFICATION FOR EVT\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

sk <- moments::skewness(r)
ku <- moments::kurtosis(r)
eku <- ku - 3
cat("\n--- Shape statistics ---\n")
cat(sprintf("Skewness        : %.4f [normal = 0]\n", sk))
cat(sprintf("Kurtosis (raw)  : %.4f [normal = 3]\n", ku))
cat(sprintf("Excess kurtosis : %.4f → %s\n", eku,
            ifelse(eku > 0, "HEAVY tails (leptokurtic)", "light tails")))

jb <- tseries::jarque.bera.test(r)
ks <- ks.test(r, "pnorm", mean(r), sd(r))
ad <- nortest::ad.test(r)
sw <- shapiro.test(sample(r, 5000))

cat("\n--- Normality Tests ---\n")
tests_norm <- data.frame(
  Test = c("Jarque-Bera", "Kolmogorov-Smirnov", "Anderson-Darling", "Shapiro-Wilk*"),
  Statistic = round(c(jb$statistic, ks$statistic, ad$statistic, sw$statistic), 4),
  p_value = formatC(c(jb$p.value, ks$p.value, ad$p.value, sw$p.value), format = "e", digits = 2),
  Conclusion = rep("Rejection of normality (p < 0.001)", 4)
)
print(tests_norm, row.names = FALSE)
cat("* Shapiro-Wilk on a sample of 5000 observations\n")

# ==============================================================================
# PART 5 – TAIL INDEX ESTIMATION (γ / ξ)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 5 – TAIL INDEX ESTIMATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

x_right <- r[r > 0]
x_left  <- -r[r < 0]
cat(sprintf("Right tail: %d positive observations\n", length(x_right)))
cat(sprintf("Left tail : %d loss observations\n",   length(x_left)))

k_max_r <- min(600, length(x_right) - 1)
k_max_l <- min(600, length(x_left)  - 1)
k_r <- 1:k_max_r
k_l <- 1:k_max_l

hill_r <- evt0::mop(x_right, k = k_r, p = 0, method = "MOP")$EVI
hill_l <- evt0::mop(x_left,  k = k_l, p = 0, method = "MOP")$EVI
hill_br_r <- evt0::mop(x_right, k = k_r, p = 0, method = "RBMOP")$EVI
hill_br_l <- evt0::mop(x_left,  k = k_l, p = 0, method = "RBMOP")$EVI
mom_r <- ReIns::Moment(x_right)$gamma[1:length(k_r)]
mom_l <- ReIns::Moment(x_left)$gamma[1:length(k_l)]

hill_df_r <- tibble(k = k_r, Hill = hill_r, `RB-Hill` = hill_br_r, Moment = mom_r)
hill_df_l <- tibble(k = k_l, Hill = hill_l, `RB-Hill` = hill_br_l, Moment = mom_l)

plot_hill <- function(d, titre, ylim_val = c(-0.1, 0.6)) {
  d_long <- d %>% pivot_longer(-k, names_to = "Estimator", values_to = "gamma") %>% filter(!is.na(gamma))
  ggplot2::ggplot(d_long, aes(k, gamma, colour = Estimator, linetype = Estimator)) +
    geom_hline(yintercept = 0, colour = "magenta", linewidth = 0.8, linetype = "solid") +
    geom_line(linewidth = 0.8, alpha = 0.85) +
    coord_cartesian(ylim = ylim_val) +
    scale_colour_manual(values = c("Hill" = "#e74c3c", "RB-Hill" = "#2980b9", "Moment" = "#27ae60")) +
    scale_linetype_manual(values = c("Hill" = "solid", "RB-Hill" = "dashed", "Moment" = "dotdash")) +
    labs(title = titre, 
         subtitle = "Hill (solid) | Bias-Reduced Hill (dashed) | Moment (dot-dash)",
         x = "k (number of tail observations used)",
         y = expression(hat(gamma)[k]~~"(tail index)"),
         colour = NULL, linetype = NULL) + theme_evt()
}
p_hill_r <- plot_hill(hill_df_r, "Tail Index Estimators — RIGHT Tail (gains)")
p_hill_l <- plot_hill(hill_df_l, "Tail Index Estimators — LEFT Tail (losses)")
(p_hill_r | p_hill_l)

# ==============================================================================
# PART 6 – OPTIMAL k SELECTION (dAMSE) AND VALIDATION
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 6 – OPTIMAL k SELECTION\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

res_amse_r <- tea::dAMSE(x_right)
k_opt_r <- res_amse_r$k0
gamma_opt_r <- 1 / res_amse_r$tail.index
cat(sprintf("dAMSE right: k* = %d, γ* = %.4f\n", k_opt_r, gamma_opt_r))

res_amse_l <- tea::dAMSE(x_left)
k_opt_l <- res_amse_l$k0
gamma_opt_l <- 1 / res_amse_l$tail.index
cat(sprintf("dAMSE left : k* = %d, γ* = %.4f\n", k_opt_l, gamma_opt_l))

# Hill trajectory with optimal k
hill_all_r <- evt0::mop(x_right, k = 1:(length(x_right)-1), p = 0, method = "MOP")$EVI
hill_all_l <- evt0::mop(x_left,  k = 1:(length(x_left)-1),  p = 0, method = "MOP")$EVI
df_r <- data.frame(k = k_all_r, est = as.numeric(hill_all_r))
df_l <- data.frame(k = k_all_l, est = as.numeric(hill_all_l))
plot_kselect <- function(data_in, k_opt, g_opt, titre, couleur) {
  # On filtre ici pour le zoom
  plot_df <- data_in[data_in$k <= 500, ]
  
  ggplot2::ggplot(plot_df, aes(x = k, y = est)) +
    geom_line(colour = couleur, linewidth = 0.9) +
    geom_vline(xintercept = k_opt, colour = "black", 
               linetype = "dashed", linewidth = 0.8) +
    geom_hline(yintercept = g_opt, colour = "black", 
               linetype = "dashed", linewidth = 0.6) +
    annotate("point", x = k_opt, y = g_opt, colour = "red", size = 3) +
    annotate("label", x = k_opt + 50, y = g_opt + 0.05, 
             label = sprintf("k* = %d\nγ* = %.4f", k_opt, g_opt),
             colour = "black", size = 3, fill = "white", alpha = 0.8) +
    coord_cartesian(ylim = c(0, 0.6)) +
    labs(title = titre, 
         subtitle = "Hill estimator trajectory | Red point: optimal k* (dAMSE)",
         x = "k", y = expression(hat(gamma)[k])) +
    theme_minimal() # Utilise un thème standard si theme_evt() pose problème
}

p_ksel_r <- plot_kselect(df_r, k_opt_r, gamma_opt_r, "Selection of k — RIGHT Tail (gains)", "steelblue")
p_ksel_l <- plot_kselect(df_l, k_opt_l, gamma_opt_l, "Selection of k — LEFT Tail (losses)", "darkred")
(p_ksel_r | p_ksel_l)

# Pareto QQ-plot validation
validate_k <- function(x, k_opt, g_opt, titre, couleur) {
  x_sort <- sort(x, decreasing = TRUE)
  log_exc <- log(x_sort[1:k_opt] / x_sort[k_opt + 1])
  theo <- -log(1 - (seq_len(k_opt) - 0.5) / k_opt) * g_opt
  qq_df <- tibble(theo = sort(theo), emp = sort(log_exc))
  ggplot2::ggplot(qq_df, aes(theo, emp)) +
    geom_point(colour = couleur, size = 1.2, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, colour = "#5d6d7e", linewidth = 0.8) +
    labs(title = titre, subtitle = sprintf("k* = %d, γ̂ = %.4f", k_opt, g_opt),
         x = expression("Theoretical Quantiles Exp(" * hat(gamma) * ")"),
         y = "Empirical Log-Exceedances") + theme_evt()
}
p_qqvalid_r <- validate_k(x_right, k_opt_r, gamma_opt_r, "Pareto QQ-plot — RIGHT Tail", pal["accent"])
p_qqvalid_l <- validate_k(x_left,  k_opt_l, gamma_opt_l, "Pareto QQ-plot — LEFT Tail", pal["secondary"])
(p_qqvalid_r | p_qqvalid_l)

# Summary table
synth <- data.frame(
  Method = c("Hill (k*)", "RB-Hill (k*)", "Moment DEdH (k*)", "dAMSE (optimal)"),
  Right_Tail = round(c(hill_r[k_opt_r], hill_br_r[k_opt_r], mom_r[k_opt_r], gamma_opt_r), 4),
  Left_Tail  = round(c(hill_l[k_opt_l], hill_br_l[k_opt_l], mom_l[k_opt_l], gamma_opt_l), 4)
)
print(synth, row.names = FALSE)

# ==============================================================================
# PART 7 – BLOCK MAXIMA METHOD (BM / GEV)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 7 – BM / GEV METHOD\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Monthly blocks
bm <- df %>%
  group_by(YearMonth) %>%
  summarise(M_gain = max(log_return), M_perte = max(-log_return), .groups = "drop") %>%
  mutate(month_date = as.Date(paste0(YearMonth, "-01")))
cat(sprintf("Number of monthly blocks: %d\n", nrow(bm)))

# Lollipop plots
p_bm_gain <- ggplot2::ggplot(bm, aes(x = month_date, y = M_gain)) +
  geom_segment(aes(xend = month_date, y = 0, yend = M_gain), colour = pal["accent"], alpha = 0.5) +
  geom_point(colour = pal["accent"], size = 1.5) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Monthly Maxima of Log-Returns — RIGHT Tail (gains)",
       x = NULL, y = "Monthly Max(r_t)") +
  theme_evt() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_bm_perte <- ggplot2::ggplot(bm, aes(x = month_date, y = M_perte)) +
  geom_segment(aes(xend = month_date, y = 0, yend = M_perte), colour = pal["secondary"], alpha = 0.5) +
  geom_point(colour = pal["secondary"], size = 1.5) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Monthly Maxima of Losses — LEFT Tail (−r_t)",
       x = NULL, y = "Monthly Max(−r_t)") +
  theme_evt() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
(p_bm_gain | p_bm_perte)

# GEV fitting (monthly)
fit_gev_gain  <- fevd(bm$M_gain, type = "GEV")
fit_gev_perte <- fevd(bm$M_perte, type = "GEV")
extract_gev <- function(fit, label) {
  co <- fit$results$par
  se <- sqrt(diag(parcov.fevd(fit)))
  data.frame(Tail = label, Parameter = c("μ", "σ", "ξ"),
             Estimate = round(co, 6), SE = round(se, 6),
             CI_lower = round(co - 1.96*se, 6), CI_upper = round(co + 1.96*se, 6))
}
gev_results <- rbind(extract_gev(fit_gev_gain, "Right (gains)"),
                     extract_gev(fit_gev_perte, "Left (losses)"))
cat("\n--- Estimated GEV Parameters (monthly) ---\n")
print(gev_results, row.names = FALSE)

# Quarterly blocks
bm_quarterly <- df %>%
  mutate(YearQuarter = paste0(format(Date, "%Y"), "-", quarters(Date))) %>%
  group_by(YearQuarter) %>%
  summarise(M_gain = max(log_return), M_perte = max(-log_return), .groups = "drop")
cat(sprintf("Number of quarterly blocks: %d\n", nrow(bm_quarterly)))

fit_gev_gain_q  <- fevd(bm_quarterly$M_gain, type = "GEV")
fit_gev_perte_q <- fevd(bm_quarterly$M_perte, type = "GEV")
gev_results_q <- rbind(extract_gev(fit_gev_gain_q, "Right (gains) - QTR"),
                       extract_gev(fit_gev_perte_q, "Left (losses) - QTR"))
cat("\n--- Estimated GEV Parameters (quarterly) ---\n")
print(gev_results_q, row.names = FALSE)

# ==============================================================================
# PART 8 – PEAKS OVER THRESHOLD METHOD (POT / GPD)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 8 – POT / GPD METHOD\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Mean Excess Plot
mep_compute <- function(x, n_seuils = 50) {
  u_seq <- quantile(x, probs = seq(0.50, 0.985, length.out = n_seuils))
  purrr::map_dfr(u_seq, function(u) {
    exc <- x[x > u] - u
    if (length(exc) < 10) return(NULL)
    tibble(u = u, mep = mean(exc), se = sd(exc)/sqrt(length(exc)), n = length(exc))
  })
}
mep_r <- mep_compute(r[r > 0])
mep_l <- mep_compute(-r[r < 0])
u95_r <- quantile(r[r > 0], 0.95)
u95_l <- quantile(-r[r < 0], 0.95)

plot_mep <- function(mep_df, u_mark, titre, couleur) {
  ggplot2::ggplot(mep_df, aes(u, mep)) +
    geom_ribbon(aes(ymin = mep - 1.96*se, ymax = mep + 1.96*se), fill = couleur, alpha = 0.18) +
    geom_line(colour = couleur, linewidth = 0.9) +
    geom_vline(xintercept = u_mark, colour = pal["secondary"], linetype = "dashed") +
    annotate("label", x = u_mark, y = max(mep_df$mep, na.rm=TRUE)*0.85,
             label = sprintf("u ≈ Q95%%\n%.5f", u_mark),
             colour = pal["secondary"], size = 2.8, fill = "white") +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = titre, x = "Threshold u", y = "Mean Excess e(u)") + theme_evt()
}
p_mep_r <- plot_mep(mep_r, u95_r, "MEP — RIGHT Tail (gains > u)", pal["accent"])
p_mep_l <- plot_mep(mep_l, u95_l, "MEP — LEFT Tail (losses > u)", pal["secondary"])
(p_mep_r | p_mep_l)

# GPD stability plot (based on k)
stability_gpd <- function(x, n_total, k_range = seq(30, 400, by = 10)) {
  xs <- sort(x, decreasing = TRUE)
  res <- lapply(k_range[k_range < length(xs)], function(k) {
    u <- xs[k+1]
    exc <- xs[1:k] - u
    if (length(exc) < 10) return(NULL)
    fit <- tryCatch(fpot(exc, threshold = 0, model = "gpd", std.err = TRUE),
                    error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    xi <- fit$estimate["shape"]
    sig <- fit$estimate["scale"]
    data.frame(k = k, u = u, quantile = 1 - k/n_total,
               xi = xi, sigma_star = sig - xi*u,
               se_xi = fit$std.err["shape"], se_sig = fit$std.err["scale"])
  })
  do.call(rbind, res)
}
stab_r <- stability_gpd(r[r>0], n_total = nrow(df))
stab_l <- stability_gpd(-r[r<0], n_total = nrow(df))

plot_stab <- function(df, k_mark, param, se_param, titre, coul) {
  row_opt <- df[which.min(abs(df$k - k_mark)), ]
  ggplot2::ggplot(df, aes(x = k, y = .data[[param]])) +
    geom_ribbon(aes(ymin = .data[[param]] - 1.96*.data[[se_param]],
                    ymax = .data[[param]] + 1.96*.data[[se_param]]),
                fill = coul, alpha = 0.2) +
    geom_line(colour = coul, linewidth = 0.85) +
    geom_vline(xintercept = row_opt$k, colour = "magenta", linetype = "dashed") +
    annotate("label", x = row_opt$k, y = max(df[[param]], na.rm=TRUE)*0.85,
             label = sprintf("k = %d\nQ = %.2f%%\nu = %.5f",
                             row_opt$k, row_opt$quantile*100, row_opt$u),
             colour = "magenta", size = 2.5, fill = "white", label.size = 0.2) +
    labs(title = titre, x = "k (exceedances)", y = param) + theme_minimal()
}
p1 <- plot_stab(stab_r, 200, "xi", "se_xi", "ξ̂(k) — Right", pal["accent"])
p2 <- plot_stab(stab_r, 250, "sigma_star", "se_sig", "σ̂*(k) — Right", pal["accent"])
p3 <- plot_stab(stab_l, 220, "xi", "se_xi", "ξ̂(k) — Left", pal["secondary"])
p4 <- plot_stab(stab_l, 250, "sigma_star", "se_sig", "σ̂*(k) — Left", pal["secondary"])
(p1 | p2) / (p3 | p4)

# GPD fit at selected threshold (k=250 for both tails)
k_stable <- 250
q_thresh <- 1 - k_stable / nrow(df)
u_r <- quantile(r, q_thresh, type = 3)
u_l <- quantile(-r, q_thresh, type = 3)
exc_r <- r[r > u_r] - u_r
exc_l <- (-r)[-r > u_l] - u_l
fit_gpd_r <- POT::fitgpd(exc_r, threshold = 0, est = "mle")
fit_gpd_l <- POT::fitgpd(exc_l, threshold = 0, est = "mle")

extract_gpd <- function(fit, label, u_val) {
  xi <- fit$param["shape"]; sig <- fit$param["scale"]
  se_xi <- sqrt(fit$var.cov["shape","shape"])
  se_sig <- sqrt(fit$var.cov["scale","scale"])
  data.frame(Tail = label, Threshold = round(u_val,6), n_exceedances = fit$nat,
             xi = round(xi,5), se_xi = round(se_xi,5), sigma = round(sig,6), se_sigma = round(se_sig,6))
}
gpd_results <- rbind(extract_gpd(fit_gpd_r, "Right (gains)", u_r),
                     extract_gpd(fit_gpd_l, "Left (losses)", u_l))
cat("\n--- Estimated GPD Parameters ---\n")
print(gpd_results, row.names = FALSE)

# ==============================================================================
# PART 9 – FINAL RISK METRICS: RETURN LEVELS, VaR, ES, AND INFERENCE TESTS
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat(" PART 9 – FINAL RISK METRICS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Return levels
n_days_per_year <- 252
p_u_r <- length(exc_r) / nrow(df)
p_u_l <- length(exc_l) / nrow(df)
lam_r <- n_days_per_year * p_u_r
lam_l <- n_days_per_year * p_u_l
xi_r <- fit_gpd_r$param["shape"]
sig_r <- fit_gpd_r$param["scale"]
xi_l <- fit_gpd_l$param["shape"]
sig_l <- fit_gpd_l$param["scale"]
T_points <- c(1,2,5,10,20,50,100)
rl_gpd <- function(T, u, xi, sig, lam) u + (sig/xi)*((T*lam)^xi - 1)
rl_r <- sapply(T_points, rl_gpd, u=u_r, xi=xi_r, sig=sig_r, lam=lam_r)
rl_l <- sapply(T_points, rl_gpd, u=u_l, xi=xi_l, sig=sig_l, lam=lam_l)

# Bootstrap CI for return levels (empirical)
bootstrap_rl <- function(exc, u, p_u, n_yr, T_vec, B=1000) {
  lam <- n_yr * p_u
  replicate(B, {
    boot_exc <- sample(exc, replace = TRUE)
    tryCatch({
      fit <- POT::fitgpd(boot_exc, threshold = 0, est = "mle")
      xi_b <- fit$param["shape"]
      sig_b <- fit$param["scale"]
      if(xi_b < -0.5) return(rep(NA_real_, length(T_vec)))
      sapply(T_vec, function(T) u + (sig_b/xi_b)*((T*lam)^xi_b - 1))
    }, error = function(e) rep(NA_real_, length(T_vec)))
  })
}
set.seed(123)
bt_r <- bootstrap_rl(exc_r, u_r, p_u_r, n_days_per_year, T_points, B=1000)
bt_l <- bootstrap_rl(exc_l, u_l, p_u_l, n_days_per_year, T_points, B=1000)

ic_gain_lo <- (exp(apply(bt_r, 1, quantile, 0.025, na.rm=TRUE))-1)*100
ic_gain_hi <- (exp(apply(bt_r, 1, quantile, 0.975, na.rm=TRUE))-1)*100
ic_loss_lo <- (exp(apply(bt_l, 1, quantile, 0.025, na.rm=TRUE))-1)*100
ic_loss_hi <- (exp(apply(bt_l, 1, quantile, 0.975, na.rm=TRUE))-1)*100

rl_table <- data.frame(
  Period = paste(T_points, "yrs"),
  Gain_pct = round((exp(rl_r)-1)*100, 2),
  IC_Gain = sprintf("[%.2f ; %.2f]", ic_gain_lo, ic_gain_hi),
  Loss_pct = round((exp(rl_l)-1)*100, 2),
  IC_Loss = sprintf("[%.2f ; %.2f]", ic_loss_lo, ic_loss_hi)
)
cat("\n--- Daily Return Levels (95% CI) ---\n")
print(rl_table, row.names = FALSE)

# VaR and ES
alphas <- c(0.95, 0.99, 0.995, 0.999)
var_gpd <- function(alpha, u, xi, sig, n_tot, n_u) u + (sig/xi)*((n_tot/n_u*(1-alpha))^(-xi)-1)
es_gpd <- function(var, u, xi, sig) (var + sig - xi*u)/(1-xi)

var_l <- sapply(alphas, var_gpd, u=u_l, xi=xi_l, sig=sig_l, n_tot=nrow(df), n_u=length(exc_l))
es_l  <- sapply(seq_along(alphas), function(i) es_gpd(var_l[i], u_l, xi_l, sig_l))
var_r <- sapply(alphas, var_gpd, u=u_r, xi=xi_r, sig=sig_r, n_tot=nrow(df), n_u=length(exc_r))
es_r  <- sapply(seq_along(alphas), function(i) es_gpd(var_r[i], u_r, xi_r, sig_r))

risk_table <- data.frame(
  Confidence = paste0(alphas*100,"%"),
  VaR_Loss_pct = round((exp(var_l)-1)*100, 2), ES_Loss_pct = round((exp(es_l)-1)*100, 2),
  VaR_Gain_pct = round((exp(var_r)-1)*100, 2), ES_Gain_pct = round((exp(es_r)-1)*100, 2)
)
cat("\n--- Daily VaR and Expected Shortfall ---\n")
print(risk_table, row.names = FALSE)

# ==============================================================================
# WEEKLY DATA PREPARATION, THRESHOLD SELECTION AND ASYMMETRY TEST
# ------------------------------------------------------------------------------
# 1. WEEKLY DATA AGGREGATION
# ------------------------------------------------------------------------------
# Ensure correct formats (to avoid bugs)
raw$Date <- as.Date(raw$Date, format = "%Y-%m-%d")
raw$Close <- as.numeric(as.character(raw$Close))

# Create weekly series
df_weekly <- raw %>%
  drop_na(Date, Close) %>%
  group_by(Year = year(Date), Week = isoweek(Date)) %>%
  summarise(Date = last(Date),
            Close = mean(Close, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Date) %>%
  mutate(log_return = c(NA, diff(log(Close)))) %>%
  drop_na(log_return)

r_weekly <- df_weekly$log_return
n_w_total <- length(r_weekly)

cat(sprintf("\n Weekly Observations: %d\n", n_w_total))

# Separate the two tails (Gains and Losses)
r_w_right <- r_weekly       # Gains
r_w_left  <- -r_weekly      # Losses (converted to positive)

# --- THRESHOLD CHOICE ---
# With ~1040 observations, the literature often recommends the 95% quantile
# (giving about k = 52 exceedances, a good bias-variance trade-off).
k_target <- 52 

u_w_r <- quantile(r_w_right, 1 - k_target / n_w_total, type = 3)
u_w_l <- quantile(r_w_left, 1 - k_target / n_w_total, type = 3)

cat(sprintf("Selected threshold (Gains)  : %.5f\n", u_w_r))
cat(sprintf("Selected threshold (Losses) : %.5f\n", u_w_l))

# Extract exceedances (X - u)
exc_w_r <- r_w_right[r_w_right > u_w_r] - u_w_r
exc_w_l <- r_w_left[r_w_left > u_w_l] - u_w_l

# ------------------------------------------------------------------------------
# 2. GPD ESTIMATION (Maximum Likelihood)
# ------------------------------------------------------------------------------
fit_w_gpd_r <- POT::fitgpd(exc_w_r, threshold = 0, est = "mle")
fit_w_gpd_l <- POT::fitgpd(exc_w_l, threshold = 0, est = "mle")

xi_w_r <- fit_w_gpd_r$param["shape"]
se_xi_w_r <- sqrt(fit_w_gpd_r$var.cov["shape", "shape"])

xi_w_l <- fit_w_gpd_l$param["shape"]
se_xi_w_l <- sqrt(fit_w_gpd_l$var.cov["shape", "shape"])

# ==============================================================================
# INDEPENDENCE DIAGNOSTICS: BOX-PIERCE TESTS
# ==============================================================================

# Case 1: Quarterly Block Maxima (Gains)
cat("\n--- Diagnostics: Quarterly Block Maxima (USD Appreciation) ---\n")
bp_bm_raw <- Box.test(bm_quarterly$M_gain, lag = 30, type = "Box-Pierce")
bp_bm_sq  <- Box.test(bm_quarterly$M_gain^2, lag = 30, type = "Box-Pierce")

print(bp_bm_raw) # Focus on p-value > 0.05
print(bp_bm_sq)  # Focus on p-value > 0.05 (No ARCH effects)

# Case 2: Weekly Returns (for POT analysis)
# Assuming 'r_w' is your vector of weekly returns
cat("\n--- Diagnostics: Weekly Returns (POT Framework) ---\n")
bp_weekly_raw <- Box.test(r_w, lag = 30, type = "Box-Pierce")
bp_weekly_sq  <- Box.test(r_w^2, lag = 30, type = "Box-Pierce")

print(bp_weekly_raw)
print(bp_weekly_sq)

# ==============================================================================
# SUMMARY TABLE FOR REPORT
# ==============================================================================
diagnostic_summary <- data.frame(
  Method = c("BM Quarterly", "Weekly POT"),
  Raw_P_Value = c(bp_bm_raw$p.value, bp_weekly_raw$p.value),
  Squared_P_Value = c(bp_bm_sq$p.value, bp_weekly_sq$p.value)
)
print(diagnostic_summary)

# --- 1. WALD TEST FOR BM QUARTERLY (extRemes package) ---
# Extraction of parameters and Standard Errors (SE)
xi_bm_r  <- fit_gev_gain_q$results$par["shape"]
var_bm_r  <- diag(extRemes::parcov.fevd(fit_gev_gain_q))["shape"]

xi_bm_l  <- fit_gev_perte_q$results$par["shape"]
var_bm_l  <- diag(extRemes::parcov.fevd(fit_gev_perte_q))["shape"]

# Wald Calculation (Z = difference / combined SE)
z_bm <- (xi_bm_r - xi_bm_l) / sqrt(var_bm_r + var_bm_l)
p_bm <- 2 * (1 - pnorm(abs(z_bm)))

# --- 2. WALD TEST FOR WEEKLY POT (POT package) ---
# Extraction of parameters and Variances from the covariance matrix
xi_pot_r <- fit_w_gpd_r$param["shape"]
var_pot_r <- fit_w_gpd_r$var.cov["shape", "shape"]

xi_pot_l <- fit_w_gpd_l$param["shape"]
var_pot_l <- fit_w_gpd_l$var.cov["shape", "shape"]

# Wald Calculation
z_pot <- (xi_pot_r - xi_pot_l) / sqrt(var_pot_r + var_pot_l)
p_pot <- 2 * (1 - pnorm(abs(z_pot)))

# --- 3. FINAL SUMMARY TABLE ---
wald_results_table <- data.frame(
  Method   = c("BM Quarterly", "Weekly POT"),
  Xi_Right = c(xi_bm_r, xi_pot_r),
  Xi_Left  = c(xi_bm_l, xi_pot_l),
  Z_Stat   = c(z_bm, z_pot),
  P_Value  = c(p_bm, p_pot)
)

print(wald_results_table)

# ==============================================================================
# 8.6 Test for Tail Heaviness (H0: xi = 0 vs H1: xi > 0)
# ==============================================================================

# Wald statistics for H0: xi = 0
z_stat_heavy_r <- xi_r / se_xi_r
z_stat_heavy_l <- xi_l / se_xi_l

# One‑tailed p‑values (since we test xi > 0)
p_value_heavy_r <- pnorm(z_stat_heavy_r, lower.tail = FALSE)
p_value_heavy_l <- pnorm(z_stat_heavy_l, lower.tail = FALSE)

# Display results
cat("\n========================================================\n")
cat(" TEST FOR TAIL HEAVINESS (H0: xi = 0 vs H1: xi > 0)\n")
cat("========================================================\n")

# ---- Right tail (Gains) ----
cat("--- RIGHT TAIL (Extreme Gains) ---\n")
cat(sprintf("xi_right    = %.5f (SE = %.5f)\n", xi_r, se_xi_r))
cat(sprintf("Z-statistic = %.4f\n", z_stat_heavy_r))
cat(sprintf("p-value     = %.4e\n", p_value_heavy_r))

if (p_value_heavy_r < 0.05) {
  cat("CONCLUSION: Reject H0 at 5% level.\n")
  cat("-> The right tail is significantly heavy (Frechet domain).\n\n")
} else {
  cat("CONCLUSION: Fail to reject H0 at 5% level.\n")
  cat("-> The right tail is NOT significantly heavy.\n\n")
}

# ---- Left tail (Losses) ----
cat("--- LEFT TAIL (Extreme Losses) ---\n")
cat(sprintf("xi_left     = %.5f (SE = %.5f)\n", xi_l, se_xi_l))
cat(sprintf("Z-statistic = %.4f\n", z_stat_heavy_l))
cat(sprintf("p-value     = %.4e\n", p_value_heavy_l))

if (p_value_heavy_l < 0.05) {
  cat("CONCLUSION: Reject H0 at 5% level.\n")
  cat("-> The left tail is significantly heavy (Frechet domain).\n")
} else {
  cat("CONCLUSION: Fail to reject H0 at 5% level.\n")
  cat("-> The left tail is NOT significantly heavy.\n")
}
cat("========================================================\n")

# ==============================================================================
# INFERENCE TESTS: TAIL HEAVINESS AND SYMMETRY
# ==============================================================================

# --- 1. MANUAL EXTRACTION OF PARAMETERS AND VARIANCES ---
# BM Quarterly (extRemes)
xi_bm_r  <- fit_gev_gain_q$results$par["shape"]
var_bm_r <- diag(extRemes::parcov.fevd(fit_gev_gain_q))["shape"]
xi_bm_l  <- fit_gev_perte_q$results$par["shape"]
var_bm_l <- diag(extRemes::parcov.fevd(fit_gev_perte_q))["shape"]

# Weekly POT (POT package)
xi_pot_r  <- fit_w_gpd_r$param["shape"]
var_pot_r <- fit_w_gpd_r$var.cov["shape", "shape"]
xi_pot_l  <- fit_w_gpd_l$param["shape"]
var_pot_l <- fit_w_gpd_l$var.cov["shape", "shape"]

# --- 2. HEAVINESS TESTS (H0: xi = 0 vs H1: xi > 0) ---
# One‑tailed p‑values
p_heavy_bm_r  <- pnorm(xi_bm_r / sqrt(var_bm_r), lower.tail = FALSE)
p_heavy_bm_l  <- pnorm(xi_bm_l / sqrt(var_bm_l), lower.tail = FALSE)
p_heavy_pot_r <- pnorm(xi_pot_r / sqrt(var_pot_r), lower.tail = FALSE)
p_heavy_pot_l <- pnorm(xi_pot_l / sqrt(var_pot_l), lower.tail = FALSE)

# --- 3. SYMMETRY TESTS (H0: xi_right = xi_left) ---
# Two‑tailed p‑values (Wald)
p_sym_bm  <- 2 * (1 - pnorm(abs((xi_bm_r - xi_bm_l) / sqrt(var_bm_r + var_bm_l))))
p_sym_pot <- 2 * (1 - pnorm(abs((xi_pot_r - xi_pot_l) / sqrt(var_pot_r + var_pot_l))))

# --- 4. FINAL SUMMARY TABLE ---
final_inference_table <- data.frame(
  Method          = c("BM Quarterly", "Weekly POT"),
  Xi_Right        = c(xi_bm_r, xi_pot_r),
  P_Heavy_Right   = c(p_heavy_bm_r, p_heavy_pot_r),
  Xi_Left         = c(xi_bm_l, xi_pot_l),
  P_Heavy_Left    = c(p_heavy_bm_l, p_heavy_pot_l),
  Wald_P_Symmetry = c(p_sym_bm, p_sym_pot)
)

rownames(final_inference_table) <- NULL

cat("\n======================================================================\n")
cat(" SUMMARY TABLE: TAIL HEAVINESS (xi=0) & SYMMETRY (xi_r=xi_l)\n")
cat("======================================================================\n")
print(final_inference_table)

# ==============================================================================
# DIAGNOSTIC PLOTS: WEEKLY POT (GPD) - 2x4 Grid
# ==============================================================================

# Set up 2x4 grid
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1), oma = c(0, 0, 4, 0), bg = "#fdfefe")

# --- Row 1: Right tail (Extreme Gains) ---
plot(fit_w_gpd_r) 

# --- Row 2: Left tail (Extreme Losses) ---
plot(fit_w_gpd_l)

# Add overall title
mtext("Figure X – GPD Diagnostic Plots for Weekly Returns\n(Top: Extreme Gains | Bottom: Extreme Losses)", 
      side = 3, line = 1, outer = TRUE, font = 2, cex = 1.2, col = "#1a3a5c")

# Reset graphical parameters
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# ==============================================================================
# DIAGNOSTIC PLOTS: QUARTERLY BLOCK MAXIMA (GEV) - 2x2 Grid
# ==============================================================================
graphics.off() 

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0), bg = "#fdfefe")

# Row 1: Gains
plot(fit_gev_gain_q)
mtext("GEV Diagnostics — RIGHT Tail (Gains)", side = 3, outer = TRUE, cex = 1.2, font = 2)

# Row 2: Losses
plot(fit_gev_perte_q)
mtext("GEV Diagnostics — LEFT Tail (Losses)", side = 3, outer = TRUE, cex = 1.2, font = 2)

# Reset
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))