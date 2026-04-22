library("sf")
library("data.table")
library("leaflet")
library("ggplot2")
library("ggrepel")
library("patchwork")
library("scales")
library("viridis")
library("RColorBrewer")
library("shiny")
library("broom")
library("betareg")          # for beta regression (market shares)
library("lmtest")           # for robust standard errors
library("sandwich")         # for clustering
library("dplyr")            # for data manipulation


CLR_NAVY   <- "#1a237e"
CLR_BLUE   <- "#3949ab"
CLR_ORANGE <- "#e65100"
CLR_GREEN  <- "#2e7d32"
CLR_RED    <- "#c62828"
CLR_GRAY   <- "#546e7a"

theme_geo <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = CLR_NAVY, size = 14),
    plot.subtitle    = element_text(color = CLR_GRAY, size = 10),
    plot.caption     = element_text(color = CLR_GRAY, size = 8, face = "italic"),
    axis.title       = element_text(color = CLR_GRAY, size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#eceff1"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


work_dir <- "/data/cg2026/mbappe67"
project_dir <- "/data/cg2026/shared/data/project"

setwd(work_dir)

wgs84_projection <- "+proj=longlat +datum=WGS84"
lambert_projection <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

mapbox_token <- "https://api.mapbox.com/styles/v1/alejandrolara/ck67ldttc0gra1iqgod7xztan/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYWxlamFuZHJvbGFyYSIsImEiOiJjaXR6dWl6NTMwZzRmMm5xdmgzZjR0c3RxIn0.noQKxbNL91sbRINcedbMgA"

###########################
# Task 1.1 - Load data
##########################"

load(file.path(project_dir, "client_shops_sf.RData"))
load(file.path(project_dir, "client_customers_sf.RData"))
load(file.path(project_dir, "siren_competitors.RData"))
load(file.path(project_dir, "market_potential.RData"))

iris_combined <- readRDS(file.path(project_dir, "iris_combined.Rds"))
pois_sf <- readRDS(file.path(project_dir, "pois_sf.RDS"))

iris_light <- iris_combined[, c("IRIS", "P21_PMEN")]

siren_competitors <- st_as_sf(
  x = siren_competitors,
  coords = c("longitude", "latitude"),
  crs = st_crs(wgs84_projection)
)

shops_with_iris <- st_join(client_shops, iris_light[, "IRIS"], join = st_intersects)
customers_with_iris <- st_join(client_customers, iris_light[, "IRIS"], join = st_intersects)
competitors_with_iris <- st_join(siren_competitors, iris_light[, "IRIS"], join = st_intersects)



# Task 1.2 - Shop level
shop_level <- data.table(customers_with_iris)[
  !is.na(shop_id) & !is.na(IRIS),
  .(
    sales = sum(sales, na.rm = TRUE),
    n_customers = .N
  ),
  by = shop_id
]

shop_iris <- data.table(st_drop_geometry(shops_with_iris))[
  !is.na(IRIS),
  .(shop_id, IRIS)
]

competitors_iris <- data.table(competitors_with_iris)[
  !is.na(IRIS),
  .(n_competitors = .N),
  by = IRIS
]

poi_iris <- data.table(pois_sf)[
  !is.na(IRIS),
  .(n_poi = .N),
  by = IRIS
]

shop_level <- merge(shop_level, shop_iris, by = "shop_id", all.x = TRUE)
shop_level <- merge(shop_level, competitors_iris, by = "IRIS", all.x = TRUE)
shop_level <- merge(shop_level, poi_iris, by = "IRIS", all.x = TRUE)

shop_level[is.na(n_competitors), n_competitors := 0]
shop_level[is.na(n_poi), n_poi := 0]
shop_level[, index := sales / mean(sales)]

shop_level <- shop_level[order(-sales)]
shop_level

# Task 1.3 - IRIS level
iris_level <- data.table(customers_with_iris)[
  !is.na(IRIS),
  .(
    sales = sum(sales, na.rm = TRUE),
    n_customers = .N
  ),
  by = IRIS
]

iris_level <- merge(iris_level, competitors_iris, by = "IRIS", all.x = TRUE)
iris_level <- merge(iris_level, poi_iris, by = "IRIS", all.x = TRUE)
iris_level <- merge(iris_level, market_potential, by = "IRIS", all.x = TRUE)

iris_level[is.na(n_competitors), n_competitors := 0]
iris_level[is.na(n_poi), n_poi := 0]

iris_map <- merge(
  iris_light[, "IRIS"],
  iris_level,
  by = "IRIS",
  all.x = FALSE
)

iris_map <- iris_map[!is.na(iris_map$sales), ]

iris_level[order(-sales), .(IRIS, sales, n_customers, n_competitors, n_poi, mp)][1:20]

pal <- colorNumeric("YlOrRd", domain = iris_map$sales, na.color = "transparent")

leaflet() %>%
  addTiles(
    urlTemplate = mapbox_token,
    attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
  ) %>%
  addPolygons(
    data = iris_map,
    fillColor = ~pal(sales),
    fillOpacity = 0.6,
    color = "black",
    weight = 1,
    label = paste(
      "IRIS: ", iris_map$IRIS,
      "<br>Sales: ", round(iris_map$sales, 0)
    )
  ) %>%
  addMarkers(data = client_shops, label = client_shops$shop_id)

# Task 1.4 - Pair level
pair_level <- data.table(customers_with_iris)[
  !is.na(IRIS) & !is.na(shop_id),
  .(
    sales = sum(sales, na.rm = TRUE),
    n_customers = .N
  ),
  by = .(IRIS, shop_id)
]

shops_lambert <- st_transform(client_shops, lambert_projection)
iris_centroids <- st_centroid(iris_light)
iris_centroids_lambert <- st_transform(iris_centroids, lambert_projection)

shops_coords <- st_coordinates(shops_lambert)
iris_coords <- st_coordinates(iris_centroids_lambert)

shops_dt <- data.table(
  shop_id = shops_lambert$shop_id,
  x_shop = shops_coords[, 1],
  y_shop = shops_coords[, 2]
)

iris_dt <- data.table(
  IRIS = iris_centroids_lambert$IRIS,
  x_iris = iris_coords[, 1],
  y_iris = iris_coords[, 2]
)

pair_level <- merge(pair_level, shops_dt, by = "shop_id", all.x = TRUE)
pair_level <- merge(pair_level, iris_dt, by = "IRIS", all.x = TRUE)

pair_level[, distance_km := sqrt((x_shop - x_iris)^2 + (y_shop - y_iris)^2) / 1000]

pair_level
#graphics.off()
plot(
  pair_level[sales > 0]$distance_km,
  pair_level[sales > 0]$sales,
  pch = 16,
  col = rgb(0, 0, 1, 0.25),
  xlab = "Distance (km)",
  ylab = "Sales",
  main = "Sales vs Distance"
)

#Principal Trading Areas

# Task 2.1 - Build market_zones from pair_level
market_zones <- copy(pair_level)

market_zones <- market_zones[order(shop_id, -sales)]

market_zones[, shop_total_sales := sum(sales, na.rm = TRUE), by = shop_id]
market_zones[, sales_share := sales / shop_total_sales, by = shop_id]
market_zones[, sales_cum := cumsum(sales_share), by = shop_id]
market_zones[, pta_80 := sales_cum <= 0.80, by = shop_id]

market_zones[, first_outside_80 := !pta_80 & shift(pta_80, fill = TRUE), by = shop_id]
market_zones[, pta_80 := pta_80 | first_outside_80]

#verif sur le shop 1 :

market_zones[shop_id == "01", .(
  total_sales = sum(sales),
  pta_sales = sum(sales[pta_80 == TRUE]),
  pta_share = sum(sales[pta_80 == TRUE]) / sum(sales)
)]


# Task 2.2 - Create graphs and maps to illustrate trading areas

sub_shop_id <- "01"

sub_shop <- client_shops[client_shops$shop_id == sub_shop_id, ]
sub_pta <- market_zones[shop_id == sub_shop_id & pta_80 == TRUE]

sub_pta_sf <- merge(
  iris_light,
  sub_pta,
  by = "IRIS",
  all.x = FALSE
)

# local filter around the shop
sub_shop_lambert <- st_transform(sub_shop, lambert_projection)
sub_pta_lambert <- st_transform(sub_pta_sf, lambert_projection)

buffer_30km <- st_buffer(sub_shop_lambert, dist = 30000)

inside_buffer <- st_intersects(sub_pta_lambert, buffer_30km, sparse = FALSE)[, 1]
sub_pta_local <- sub_pta_lambert[inside_buffer, ]

# back to WGS84 for leaflet
sub_pta_local <- st_transform(sub_pta_local, wgs84_projection)
sub_shop_wgs84 <- st_transform(sub_shop_lambert, wgs84_projection)

bb <- as.list(st_bbox(sub_pta_local))

leaflet() %>%
  addTiles(
    urlTemplate = mapbox_token,
    attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
  ) %>%
  addPolygons(
    data = sub_pta_local,
    fillColor = "orange",
    fillOpacity = 0.5,
    color = "black",
    weight = 1,
    label = paste(
      "IRIS: ", sub_pta_local$IRIS,
      "<br>Sales: ", round(sub_pta_local$sales, 0),
      "<br>Share: ", round(100 * sub_pta_local$sales_share, 1), "%",
      "<br>Cumulative: ", round(100 * sub_pta_local$sales_cum, 1), "%"
    ),
    labelOptions = labelOptions(
      noHide = FALSE,
      direction = "top",
      offset = c(0, 0),
      textOnly = TRUE,
      style = list(
        "color" = "rgba(0,0,0,1)",
        "font-family" = "Arial Black",
        "font-style" = "bold",
        "box-shadow" = "0px 0px rgba(0,0,0,0.25)",
        "font-size" = "12px",
        "background-color" = "rgba(255,255,255,0.7)",
        "border-color" = "rgba(0,0,0,0)"
      )
    )
  ) %>%
  addMarkers(data = sub_shop_wgs84, label = sub_shop_wgs84$shop_id) %>%
  fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)



sub_data <- market_zones[shop_id == sub_shop_id][order(-sales)]
sub_data[, rank := 1:.N]

cut_rank <- sub_data[pta_80 == TRUE, max(rank)]

plot(
  sub_data$rank,
  sub_data$sales_cum,
  type = "l",
  lwd = 2,
  xlab = "Ranked IRIS",
  ylab = "Cumulative sales share",
  main = paste("Principal trading area - Shop", sub_shop_id)
)

abline(h = 0.80, col = "red", lty = 2)
abline(v = cut_rank, col = "blue", lty = 2)

text(
  x = cut_rank,
  y = 0.1,
  labels = paste("PTA cutoff =", cut_rank, "IRIS"),
  pos = 4,
  cex = 0.8
)


# Model Sales Volumes and Market Shares

################################################################################
# 1. DATA PREPARATION (enriched with socio-economic proxies)
################################################################################

model_data <- copy(pair_level)
model_data <- model_data[sales >= 0]

# Population from iris_combined (column is P21_PMEN)
iris_pop <- data.table(
  IRIS       = iris_combined$IRIS,
  population = iris_combined$P21_PMEN
)
model_data <- merge(model_data, iris_pop, by = "IRIS", all.x = TRUE)

# Shop-level POI attractivity
shop_poi <- shop_level[, .(shop_id, shop_n_poi = n_poi)]
model_data <- merge(model_data, shop_poi, by = "shop_id", all.x = TRUE)

# Market potential and IRIS-level competition
iris_vars <- iris_level[, .(IRIS, mp, n_competitors_iris = n_competitors)]
model_data <- merge(model_data, iris_vars, by = "IRIS", all.x = TRUE)

# --- Socio-economic proxies from iris_combined (no external data) ---
iris_socio <- st_drop_geometry(iris_combined) %>%
    select(
    IRIS,
    C21_PMEN, C21_PMEN_CS5, C21_PMEN_CS6,
    P21_POP1529, P21_POP6074, P21_POP75P, P21_PMEN,
    P21_MAISON, P21_APPART,
    P21_NSCOL15P_SUP2, P21_POP15P
  ) %>%
    mutate(
    prop_cadres   = C21_PMEN_CS5 / C21_PMEN,
    prop_ouvriers = C21_PMEN_CS6 / C21_PMEN,
    prop_jeunes   = P21_POP1529 / P21_PMEN,
    prop_seniors  = (P21_POP6074 + P21_POP75P) / P21_PMEN,
    prop_maisons  = P21_MAISON / (P21_MAISON + P21_APPART),
    prop_diplomes = P21_NSCOL15P_SUP2 / P21_POP15P
  ) %>%
    mutate(across(starts_with("prop"), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
   select(IRIS, prop_cadres, prop_jeunes, prop_maisons, prop_diplomes)

model_data <- merge(model_data, iris_socio, by = "IRIS", all.x = TRUE)

# --- Competitors within 2km of each shop ---
shops_lambert  <- st_transform(client_shops, lambert_projection)
compet_lambert <- st_transform(siren_competitors, lambert_projection)
shops_lambert$n_compet_2km <- lengths(
  st_is_within_distance(shops_lambert, compet_lambert, dist = 2000)
)
shop_compet2 <- data.table(
  shop_id      = shops_lambert$shop_id,
  n_compet_2km = shops_lambert$n_compet_2km
)
model_data <- merge(model_data, shop_compet2, by = "shop_id", all.x = TRUE)

# Fill NAs with 0
model_data[is.na(shop_n_poi),        shop_n_poi        := 0]
model_data[is.na(mp),                mp                := 0]
model_data[is.na(n_competitors_iris), n_competitors_iris := 0]
model_data[is.na(population),        population        := 0]
model_data[is.na(n_compet_2km),      n_compet_2km      := 0]
model_data[is.na(prop_cadres),       prop_cadres       := 0]
model_data[is.na(prop_jeunes),       prop_jeunes       := 0]
model_data[is.na(prop_maisons),      prop_maisons      := 0]
model_data[is.na(prop_diplomes),     prop_diplomes     := 0]

# Keep pairs with positive sales, positive distance, and valid market potential
model_data <- model_data[sales > 0 & distance_km > 0 & !is.na(mp) & mp > 0]

# Market share: sales of the pair / total sales in the IRIS (across all shops)
model_data[, total_iris := sum(sales), by = IRIS]
model_data[, share := sales / total_iris]
model_data[is.na(share), share := 0]
model_data[, share := pmin(pmax(share, 1e-6), 1 - 1e-6)]  # bound away from 0 and 1

# Log transformations
model_data[, log_sales       := log(sales)]
model_data[, log_distance    := log(distance_km)]
model_data[, log_mp          := log(mp + 1)]
model_data[, log_compet_iris := log(n_competitors_iris + 1)]
model_data[, log_compet_2km  := log(n_compet_2km + 1)]
model_data[, log_poi         := log(shop_n_poi + 1)]
model_data[, log_pop         := log(population + 1)]
model_data[, logit_share     := log(share / (1 - share))]

model_data <- model_data[is.finite(logit_share)]

cat("Clean modelling dataset:", nrow(model_data), "pairs\n")


################################################################################
# 2. MODELS FOR SALES VOLUMES (enriched with socio-economic proxies)
################################################################################

# V1: Quasi-Poisson
mod_v1 <- glm(
  sales ~ log_distance + log_poi + log_compet_2km + log_mp + log_pop +
    prop_cadres + prop_jeunes + prop_maisons,
  data   = model_data,
  family = quasipoisson(link = "log")
)
cat("\n========== MODEL V1: QUASI-POISSON (enriched) ==========\n")
print(summary(mod_v1))

# V2: Log-linear OLS — main model for predictions
mod_v2 <- lm(
  log_sales ~ log_distance + log_poi + log_compet_2km + log_mp + log_pop +
    prop_cadres + prop_jeunes + prop_maisons,
  data = model_data
)
cat("\n========== MODEL V2: LOG-LINEAR OLS (enriched) ==========\n")
print(summary(mod_v2))
coeftest(mod_v2, vcov = vcovCL, cluster = ~ IRIS)
# V3: Log-linear with interaction distance*POI + socio-economic proxies
mod_v3 <- lm(
  log_sales ~ log_distance * log_poi + log_compet_2km + log_mp + log_pop +
    prop_cadres + prop_jeunes + prop_maisons,
  data = model_data
)
cat("\n========== MODEL V3: WITH INTERACTION (enriched) ==========\n")
print(summary(mod_v3))
coeftest(mod_v3, vcov = vcovCL, cluster = ~ IRIS)

################################################################################
# 3. MODELS FOR MARKET SHARES (enriched with socio-economic proxies)
################################################################################

# Keep only IRIS with at least 2 shops
huff_data <- model_data[share > 0 & total_iris > 0]
huff_data[, n_shops_iris := .N, by = IRIS]
huff_data <- huff_data[n_shops_iris >= 2]

cat("\nMarket share dataset:", nrow(huff_data), "pairs\n")

# P1: Logit-linear OLS on market share (Huff)
mod_p1 <- lm(
  logit_share ~ log_distance + log_poi + log_compet_2km + log_pop +
    prop_cadres + prop_jeunes + prop_maisons,
  data = huff_data
)
cat("\n========== MODEL P1: LOGIT SHARE (OLS, enriched) ==========\n")
print(summary(mod_p1))
coeftest(mod_p1, vcov = vcovCL, cluster = ~ IRIS)
# P2: Beta regression
beta_data <- huff_data[share > 0 & share < 1]
mod_p2 <- betareg(
  share ~ log_distance + log_poi + log_compet_2km + log_pop +
    prop_cadres + prop_jeunes + prop_maisons,
  data = beta_data,
  link = "logit"
)
cat("\n========== MODEL P2: BETA REGRESSION (enriched) ==========\n")
print(summary(mod_p2))

# P3: Binomial logit weighted by number of customers
huff_data[, total_cust_iris := sum(n_customers), by = IRIS]
mod_p3 <- glm(
  cbind(n_customers, total_cust_iris - n_customers) ~
    log_distance + log_poi + log_compet_2km + log_pop +
    prop_cadres + prop_jeunes + prop_maisons,
  data   = huff_data,
  family = binomial(link = "logit")
)
cat("\n========== MODEL P3: BINOMIAL LOGIT (weighted, enriched) ==========\n")
print(summary(mod_p3))


################################################################################
# 4. DIAGNOSTIC PLOTS (same as before, using the enriched V2)
################################################################################

# Figure 1: Model fit comparison
comparison <- data.table(
  Model = c("V1: QuasiPoisson", "V2: Log-linear", "V3: Interaction",
            "P1: Logit OLS", "P2: Beta", "P3: Binomial"),
  Fit = c(
    NA,
    summary(mod_v2)$adj.r.squared,
    summary(mod_v3)$adj.r.squared,
    summary(mod_p1)$adj.r.squared,
    as.numeric(mod_p2$pseudo.r.squared),
    NA
  ),
  Type = c("Sales", "Sales", "Sales", "Share", "Share", "Share")
)

p_compare <- ggplot(comparison[!is.na(Fit)],
                    aes(x = reorder(Model, Fit), y = Fit, fill = Type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Fit)), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Sales" = CLR_BLUE, "Share" = CLR_ORANGE)) +
  scale_y_continuous(limits = c(0, 1.1), labels = percent_format()) +
  labs(
    title    = "Model Fit Comparison (enriched models)",
    subtitle = "Adjusted R² / McFadden pseudo-R²",
    x        = NULL,
    y        = "Fit measure"
  ) +
  theme_geo
p_compare
#ggsave("model_comparison_enriched.png", p_compare, width = 9, height = 5, dpi = 180)

# Figure 2: Coefficients of V2 with confidence intervals (enriched)
tidy_v2 <- broom::tidy(mod_v2, conf.int = TRUE)
tidy_v2 <- tidy_v2[tidy_v2$term != "(Intercept)", ]
# Custom labels for readability
tidy_v2$term_lab <- c("Distance", "POIs (shop)", "Competition (2km)",
                      "Market potential", "Population",
                      "Proportion cadres", "Proportion jeunes", "Proportion maisons")

p_coef <- ggplot(tidy_v2,
                 aes(x = reorder(term_lab, estimate), y = estimate,
                     ymin = conf.low, ymax = conf.high,
                     color = p.value < 0.05)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(width = 0.2, linewidth = 1) +
  geom_point(size = 3.5) +
  coord_flip() +
  scale_color_manual(
    values = c("FALSE" = "grey70", "TRUE"  = CLR_BLUE),
    labels = c("p >= 0.05", "p < 0.05"),
    name   = NULL
  ) +
  labs(
    title    = "Coefficients of Model V2 (enriched with socio-economic proxies)",
    subtitle = "Elasticities / semi-elasticities with 95% CI",
    x        = NULL,
    y        = "Coefficient"
  ) +
  theme_geo
p_coef
#ggsave("model_v2_coefficients_enriched.png", p_coef, width = 9, height = 6, dpi = 180)

# Figure 3+4: Residuals and predicted vs observed for enriched V2
model_data[, fitted_v2 := fitted(mod_v2)]
model_data[, resid_v2  := residuals(mod_v2)]

p_resid <- ggplot(model_data, aes(x = fitted_v2, y = resid_v2)) +
  geom_point(alpha = 0.15, color = CLR_BLUE, size = 0.8) +
  geom_hline(yintercept = 0, color = CLR_RED, linewidth = 1) +
  geom_smooth(method = "loess", formula = y ~ x,
              se = FALSE, color = CLR_ORANGE, linewidth = 0.9) +
  labs(
    title = "Residuals vs Fitted (Enriched Model V2)",
    x     = "Fitted log sales",
    y     = "Residuals"
  ) +
  theme_geo
p_resid

p_pred_obs <- ggplot(model_data, aes(x = log_sales, y = fitted_v2)) +
  geom_point(alpha = 0.15, color = CLR_NAVY, size = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              color = CLR_RED, linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Observed vs Predicted (Enriched Model V2)",
    x     = "Observed log sales",
    y     = "Predicted log sales"
  ) +
  theme_geo

p_diag <- (p_resid | p_pred_obs) +
  plot_annotation(
    title = "Diagnostic Plots — Enriched Model V2",
    theme = theme(plot.title = element_text(face = "bold", color = CLR_NAVY))
  )
p_diag
#ggsave("model_v2_diagnostics_enriched.png", p_diag, width = 12, height = 5, dpi = 180)

# Figure 5: Distance elasticity by shop (unchanged)
elasticities <- model_data[, {
  if (.N < 10) return(NULL)
  m <- lm(log_sales ~ log_distance + log_mp, data = .SD)
  list(elasticity = coef(m)[["log_distance"]])
}, by = shop_id]

p_elast <- ggplot(elasticities,
                  aes(x = reorder(shop_id, elasticity), y = elasticity, fill = elasticity)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = mean(elasticities$elasticity, na.rm = TRUE),
             color = CLR_RED, linewidth = 1.2, linetype = "dashed") +
  coord_flip() +
  scale_fill_gradient2(
    low      = CLR_RED,
    mid      = "grey80",
    high     = CLR_BLUE,
    midpoint = mean(elasticities$elasticity, na.rm = TRUE)
  ) +
  labs(
    title    = "Distance Elasticity by Shop",
    subtitle = "Red line = overall average | More negative = stronger local dependence",
    x        = "Shop",
    y        = "Distance elasticity"
  ) +
  theme_geo
p_elast
#ggsave("elasticity_by_shop.png", p_elast, width = 8, height = 6, dpi = 180)

################################################################################
# TASK 4 — EVALUATION OF 10 CANDIDATE LOCATIONS (COMPETITORS)
################################################################################

# ─── 4.1 Selection of 10 candidate locations (one per IRIS) ───────────────────

# Compute uncaptured potential per IRIS (same as before)
uncaptured <- merge(
  market_potential,
  iris_level[, .(IRIS, sales, n_competitors)],
  by = "IRIS", all.x = TRUE
)
uncaptured[is.na(sales), sales := 0]
uncaptured[is.na(n_competitors), n_competitors := 0]
uncaptured[, uncaptured_potential := pmax(mp - sales, 0)]

# IRIS already served by existing shops
iris_already_served <- unique(data.table(st_drop_geometry(shops_with_iris))$IRIS)

# Prepare competitors
competitors_wgs84 <- siren_competitors
competitors_lambert <- st_transform(competitors_wgs84, lambert_projection)
shops_lambert <- st_transform(client_shops, lambert_projection)

# Distance to nearest existing shop (meters)
dist_to_shops <- st_distance(competitors_lambert, shops_lambert)
min_dist_to_shop <- apply(dist_to_shops, 1, min)

# Spatial join to assign IRIS
competitors_with_iris <- st_join(competitors_wgs84, iris_combined[, "IRIS"])
coords <- st_coordinates(competitors_with_iris)

competitors_dt <- data.table(
  SIREN = competitors_with_iris$SIREN,
  NIC = competitors_with_iris$NIC,
  IRIS = competitors_with_iris$IRIS,
  min_dist_to_shop = as.numeric(min_dist_to_shop),
  x_wgs84 = coords[, 1],
  y_wgs84 = coords[, 2]
)

# Merge uncaptured potential
competitors_dt <- merge(competitors_dt, uncaptured[, .(IRIS, uncaptured_potential, mp)], 
                        by = "IRIS", all.x = TRUE)

# Filter eligible competitors
MIN_DIST_KM <- 2
candidates_pool <- competitors_dt[
  !is.na(IRIS) & 
    !(IRIS %in% iris_already_served) & 
    uncaptured_potential > 0 & 
    min_dist_to_shop > (MIN_DIST_KM * 1000)
]

# Keep only the competitor with the highest uncaptured potential per IRIS
candidates_pool <- candidates_pool[order(-uncaptured_potential)]
candidates_pool <- candidates_pool[!duplicated(IRIS)]   # one per IRIS

# Take top 10 distinct IRIS
top10_competitors <- candidates_pool[1:10, ]

# Create sf points
top10_candidates_sf <- st_as_sf(top10_competitors, coords = c("x_wgs84", "y_wgs84"), 
                                crs = wgs84_projection, remove = FALSE)
top10_candidates_sf$candidate_id <- paste0("CAND_", 1:10)

# Lambert coordinates for distance calculations
cands_lambert <- st_transform(top10_candidates_sf, lambert_projection)
cands_coords <- st_coordinates(cands_lambert)

candidates_dt <- data.table(
  candidate_id = top10_candidates_sf$candidate_id,
  x_cand = cands_coords[, 1],
  y_cand = cands_coords[, 2],
  IRIS = top10_competitors$IRIS,
  uncaptured_potential = top10_competitors$uncaptured_potential,
  mp = top10_competitors$mp
)

cat("\n✅ 10 candidate locations selected (one per IRIS, distinct):\n")
print(candidates_dt[, .(candidate_id, x_cand, y_cand, IRIS, mp, uncaptured_potential)])

# ─── 4.2 Predictions using best models (mod_v2 for sales, mod_p1 for market share) ───

# Average characteristics of a typical new shop (from existing shops)
avg_poi <- mean(model_data$shop_n_poi, na.rm = TRUE)
avg_compet_2km <- mean(model_data$n_compet_2km, na.rm = TRUE)

# Prediction function
predict_candidate <- function(cand_row, iris_dt_full, iris_pop_dt, market_pot_dt,
                              model_sales, model_ms, avg_poi, avg_compet_2km) {
  
  pred <- copy(iris_dt_full)
  pred[, x_cand := cand_row$x_cand]
  pred[, y_cand := cand_row$y_cand]
  pred[, distance_km := sqrt((x_iris - x_cand)^2 + (y_iris - y_cand)^2) / 1000]
  
  pred <- merge(pred, market_pot_dt[, .(IRIS, mp)], by = "IRIS", all.x = TRUE)
  pred <- merge(pred, iris_pop_dt[, .(IRIS, population)], by = "IRIS", all.x = TRUE)
  pred <- merge(pred, iris_socio, by = "IRIS", all.x = TRUE)
  
  pred <- pred[!is.na(mp) & mp > 0 & distance_km > 0]
  
  pred[, log_distance   := log(distance_km)]
  pred[, log_poi        := log(avg_poi + 1)]
  pred[, log_compet_2km := log(avg_compet_2km + 1)]
  pred[, log_mp         := log(mp + 1)]
  pred[, log_pop        := log(pmax(population, 1) + 1)]
  pred[is.na(prop_cadres), prop_cadres := 0]
  pred[is.na(prop_jeunes), prop_jeunes := 0]
  pred[is.na(prop_maisons), prop_maisons := 0]
  
  # Predict sales (Model V2)
  pred[, pred_log_sales := predict(model_sales, newdata = as.data.frame(pred))]
  pred[, pred_sales := exp(pred_log_sales)]
  
  # Predict market share (Model P1)
  pred[, pred_logit_ms := predict(model_ms, newdata = as.data.frame(pred))]
  pred[, pred_ms := 1 / (1 + exp(-pred_logit_ms))]
  
  # Build 80% trading area (cumulative sales)
  pred <- pred[order(-pred_sales)]
  pred[, total_pred := sum(pred_sales, na.rm = TRUE)]
  pred[, cum_share := cumsum(pred_sales) / total_pred]
  pred[, pta_80 := cum_share <= 0.80]
  pred[, first_outside := !pta_80 & shift(pta_80, fill = TRUE)]
  pred[, pta_80 := pta_80 | first_outside]
  
  pta <- pred[pta_80 == TRUE]
  
  list(
    candidate_id       = cand_row$candidate_id,
    total_pred_sales   = sum(pta$pred_sales, na.rm = TRUE),
    avg_market_share   = mean(pta$pred_ms, na.rm = TRUE) * 100,
    max_dist_km        = max(pta$distance_km, na.rm = TRUE),
    nb_iris_pta        = nrow(pta),
    pta_iris_codes     = pta$IRIS,
    pred_data          = pred
  )
}

# Prepare IRIS centroids (Lambert projection)
iris_centroids_lambert_all <- st_centroid(st_transform(iris_combined, lambert_projection))
all_coords <- st_coordinates(iris_centroids_lambert_all)
iris_dt_full <- data.table(
  IRIS   = iris_combined$IRIS,
  x_iris = all_coords[, 1],
  y_iris = all_coords[, 2]
)

# Run predictions
cat("\n⏳ Computing predictions for", nrow(candidates_dt), "candidates...\n")
candidate_results <- lapply(1:nrow(candidates_dt), function(i) {
  cat("  Candidate", i, "/", nrow(candidates_dt), "\n")
  predict_candidate(
    cand_row        = candidates_dt[i],
    iris_dt_full    = iris_dt_full,
    iris_pop_dt     = iris_pop,
    market_pot_dt   = market_potential,
    model_sales     = mod_v2,
    model_ms        = mod_p1,
    avg_poi         = avg_poi,
    avg_compet_2km  = avg_compet_2km
  )
})
names(candidate_results) <- candidates_dt$candidate_id
cat("✅ Predictions done\n")

# Create summary table for all candidates
results_summary <- rbindlist(lapply(candidate_results, function(r) {
  data.table(
    candidate_id = r$candidate_id,
    total_pred_sales = round(r$total_pred_sales),
    avg_market_share = round(r$avg_market_share, 2),
    max_dist_km = round(r$max_dist_km, 1),
    nb_iris_pta = r$nb_iris_pta
  )
}))

# Add candidate info (IRIS, potential)
results_summary <- merge(results_summary, 
                         candidates_dt[, .(candidate_id, IRIS, mp, uncaptured_potential)],
                         by = "candidate_id")

# Rank by sales
results_summary <- results_summary[order(-total_pred_sales)]
results_summary[, rank := 1:.N]
results_summary[, is_best := rank <= 3]
results_summary[, is_worst := rank >= (.N - 2)]

cat("\n========== CANDIDATE RANKING ==========\n")
print(results_summary[, .(rank, candidate_id, IRIS, total_pred_sales, avg_market_share, max_dist_km, nb_iris_pta)])

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║     SUMMARY TABLE OF 10 CANDIDATE LOCATIONS (competitors)    ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
print(results_summary[, .(
  rank, candidate_id, IRIS,
  `Predicted sales (€)` = format(total_pred_sales, big.mark = " "),
  `Market share (%)` = avg_market_share,
  `PTA radius (km)` = max_dist_km,
  `Nb IRIS` = nb_iris_pta,
  Status = fcase(is_best, "🏆 TOP", is_worst, "❌ BOTTOM", default = "—")
)])

best_candidate  <- results_summary$candidate_id[1]
worst_candidate <- results_summary$candidate_id[nrow(results_summary)]

cat("\n🏆 BEST  :", best_candidate, 
    "| Predicted sales:", format(results_summary$total_pred_sales[1], big.mark = " "), "€\n")
cat("❌ WORST :", worst_candidate,
    "| Predicted sales:", 
    format(results_summary$total_pred_sales[nrow(results_summary)], big.mark = " "), "€\n")

# ─── 4.4 Trading area polygons for candidates ────────────────────────────────
# Skip the union entirely — just store which IRIS belong to each candidate
candidate_pta_dt <- rbindlist(lapply(candidate_results, function(r) {
  data.table(
    candidate_id     = r$candidate_id,
    IRIS             = r$pta_iris_codes,
    total_pred_sales = r$total_pred_sales,
    avg_market_share = r$avg_market_share
  )
}))

# Merge rank info
candidate_pta_dt <- merge(
  candidate_pta_dt,
  results_summary[, .(candidate_id, rank, is_best, is_worst, max_dist_km)],
  by = "candidate_id"
)

# ─── 4.5 Candidate plots (ranking, scatter, profiles, accumulation curves) ───

# ─── 4.5 Candidate plots (ranking, scatter, profiles, accumulation curves) ───

library(ggplot2)
library(ggrepel)
library(scales)

# ---- Figure 1: Ranking bar chart (predicted sales) ----
results_summary[, label_sales := paste0(round(total_pred_sales / 1000), "k€")]
results_summary[, fill_color := fcase(
  is_best,  CLR_GREEN,
  is_worst, CLR_RED
)]

p_ranking <- ggplot(results_summary,
                    aes(x = reorder(candidate_id, total_pred_sales),
                        y = total_pred_sales / 1000,
                        fill = fill_color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = label_sales), hjust = -0.15, fontface = "bold", 
            size = 3.5, color = CLR_NAVY) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = label_number(suffix = "k€")) +
  labs(title = "Candidate Ranking by Predicted Sales",
       subtitle = "Green = Top 3, Red = Bottom 3",
       x = "Candidate", y = "Predicted sales (k€)",
       caption = "Based on enriched log-linear model (mod_v2)") +
  theme_geo

# ---- Figure 2: Scatter plot – Sales vs Market Share ----
p_scatter <- ggplot(results_summary,
                    aes(x = avg_market_share, y = total_pred_sales / 1000,
                        color = fill_color, label = candidate_id)) +
  geom_point(size = 5, alpha = 0.85) +
  geom_text_repel(fontface = "bold", size = 3.5, color = "grey30",
                  box.padding = 0.4, point.padding = 0.2) +
  scale_y_continuous(labels = label_number(suffix = "k€")) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_color_identity() +  # use pre-defined colors
  labs(title = "Predicted Sales vs Market Share",
       subtitle = "Top‑right quadrant = best candidates",
       x = "Average market share in PTA (%)",
       y = "Predicted sales (k€)",
       caption = "Market share is the average predicted share within the 80% trading area") +
  theme_geo

# ---- Figure 3: Trading area profile (radius vs number of IRIS, size = sales) ----
p_profile <- ggplot(results_summary,
                    aes(x = max_dist_km, y = nb_iris_pta,
                        size = total_pred_sales / 1000, color = fill_color,
                        label = candidate_id)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 3.5, color = "grey30", box.padding = 0.5,
                  show.legend = FALSE) +
  scale_size_continuous(name = "Predicted sales (k€)", range = c(3, 12),
                        labels = label_number(suffix = "k€")) +
  scale_color_identity() +
  labs(title = "Trading Area Profile",
       subtitle = "Ideal: short radius + moderate number of IRIS + large sales",
       x = "PTA radius (km)",
       y = "Number of IRIS in the PTA (80% cumulative sales)",
       caption = "Size of points proportional to predicted sales") +
  theme_geo

# ---- Figure 4: Accumulation curves – Best vs Worst candidate ----
get_cum_curve <- function(cand_id, label) {
  pred <- copy(candidate_results[[cand_id]]$pred_data)
  setorder(pred, -pred_sales)
  pred[, rank := .I]
  pred[, cum_share := cumsum(pred_sales) / sum(pred_sales)]
  pred[, candidate := label]
  return(pred[, .(rank, cum_share, candidate)])
}

best_id <- results_summary[is_best == TRUE]$candidate_id[1]
worst_id <- results_summary[is_worst == TRUE]$candidate_id[3]

curve_data <- rbind(
  get_cum_curve(best_id, paste("Best:", best_id)),
  get_cum_curve(worst_id, paste("Worst:", worst_id))
)

p_cum <- ggplot(curve_data, aes(x = rank, y = cum_share, color = candidate)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_vline(data = curve_data[, .(rank80 = rank[which.min(abs(cum_share - 0.80))]), by = candidate],
             aes(xintercept = rank80, color = candidate), linetype = "dotted", linewidth = 0.8) +
  scale_color_manual(values = c(CLR_GREEN, CLR_RED)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  annotate("text", x = max(curve_data$rank) * 0.1, y = 0.82, 
           label = "80% threshold", color = "grey40", size = 3.5, hjust = 0) +
  labs(title = "Sales Accumulation Curves",
       subtitle = "Best vs Worst candidate – 80% trading area cut‑off",
       x = "IRIS ranked by predicted sales (descending)",
       y = "Cumulative share of predicted sales",
       caption = "Dotted vertical lines indicate the number of IRIS needed to reach 80%") +
  theme_geo

print(p_cum)

# ---- Assemble and save plots ----
plot_candidates_1 <- (p_ranking | p_scatter) +
  plot_annotation(title = "Candidate Performance Analysis",
                  theme = theme(plot.title = element_text(face = "bold", color = CLR_NAVY, size = 16)))

plot_candidates_2 <- (p_profile | p_cum) +
  plot_annotation(title = "Trading Area Characteristics",
                  theme = theme(plot.title = element_text(face = "bold", color = CLR_NAVY, size = 16)))

ggsave("plot_candidates_ranking_scatter.png", plot_candidates_1, width = 14, height = 7, dpi = 180)
ggsave("plot_candidates_profile_accum.png", plot_candidates_2, width = 14, height = 7, dpi = 180)

cat("✅ Candidate plots saved: plot_candidates_ranking_scatter.png and plot_candidates_profile_accum.png\n")


 





# ----- 5.10 Selection criteria explanation (for report) -----
cat("\n========== SELECTION CRITERIA ==========\n")
cat("Candidates were selected based on highest uncaptured market potential (mp - captured_sales)\n")
cat("among IRIS without an existing shop. This identifies areas with unmet demand.\n")
cat("Best candidate:", best_candidate, "- highest predicted sales, compact trading area, high market share.\n")
cat("Worst candidate:", worst_candidate, "- lowest predicted sales, possibly due to low potential or high distance.\n")

# ─── Sauvegarde des objets pour l'application Shiny ───────────────────────────

# Liste des objets à conserver
objects_to_save <- c(
  "candidate_results",   # liste des résultats par candidat (ventes, parts, PTA)
  "results_summary",     # tableau récapitulatif (rank, sales, etc.)
  "top10_candidates_sf", # points des candidats (sf)
  "iris_combined",       # polygones IRIS (sf)
  "wgs84_projection",    # projection WGS84
  "mapbox_token",        # token pour les tuiles Mapbox
  "model_data",          # données utilisées pour les modèles (graphiques)
  "mod_v2",              # modèle de ventes (log‑linéaire)
  "mod_p1",              # modèle de parts de marché (logit OLS)
  "mod_v3",              # modèle avec interaction
  "mod_p2"               # modèle bêta (optionnel, pour le graphique de comparaison)
)

# Sauvegarder dans un fichier
save(list = objects_to_save, file = "shiny_data.RData")

cat("✅ Tous les objets nécessaires pour l'application Shiny ont été sauvegardés dans 'shiny_data.RData'\n") 


###############fichier leger pour la shinyapp



library(data.table)
library(sf)
library(broom)

# =========================
# 0) Vérif objets
# =========================
stopifnot(exists("results_summary"))
stopifnot(exists("top10_candidates_sf"))
stopifnot(exists("candidate_results"))
stopifnot(exists("iris_combined"))

# =========================
# 1) Summary très léger
# =========================
results_summary_app <- as.data.table(results_summary)[, .(
  candidate_id,
  rank,
  IRIS,
  total_pred_sales,
  avg_market_share,
  max_dist_km,
  nb_iris_pta
)]

# best / worst
best_candidate_app  <- results_summary_app[which.min(rank), candidate_id]
worst_candidate_app <- results_summary_app[which.max(rank), candidate_id]

# =========================
# 2) Candidate points légers
# =========================
candidate_points_sf_app <- merge(
  top10_candidates_sf[, c("candidate_id")],
  results_summary_app,
  by = "candidate_id",
  all.x = TRUE
)

candidate_points_sf_app <- candidate_points_sf_app[, c(
  "candidate_id",
  "rank",
  "total_pred_sales",
  "avg_market_share",
  attr(candidate_points_sf_app, "sf_column")
)]

# =========================
# 3) Fonction pour construire une PTA
# =========================
build_pta_sf <- function(cid) {
  res <- candidate_results[[cid]]
  if (is.null(res) || is.null(res$pta_iris_codes)) return(NULL)
  
  zone <- iris_combined[iris_combined$IRIS %in% res$pta_iris_codes, c("IRIS")]
  
  pred_dt <- as.data.table(res$pred_data)
  if ("pta_80" %in% names(pred_dt)) {
    pred_dt <- pred_dt[pta_80 == TRUE]
  }
  pred_dt <- unique(pred_dt[, .(IRIS, pred_sales)])
  
  zone <- merge(zone, pred_dt, by = "IRIS", all.x = TRUE)
  zone$candidate_id <- cid
  
  zone[, c("candidate_id", "IRIS", "pred_sales", attr(zone, "sf_column"))]
}

# =========================
# 4) Garder seulement BEST et WORST
# =========================
best_pta_sf_app  <- build_pta_sf(best_candidate_app)
worst_pta_sf_app <- build_pta_sf(worst_candidate_app)

# =========================
# 5) Simplification géométrique
# =========================
lambert_projection <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"

simplify_sf <- function(x, tol = 80) {
  if (is.null(x) || nrow(x) == 0) return(x)
  x <- st_make_valid(x)
  x <- st_transform(x, lambert_projection)
  x <- st_simplify(x, dTolerance = tol, preserveTopology = TRUE)
  x <- st_transform(x, 4326)
  x
}

candidate_points_sf_app <- st_transform(candidate_points_sf_app, 4326)
best_pta_sf_app  <- simplify_sf(best_pta_sf_app, tol = 80)
worst_pta_sf_app <- simplify_sf(worst_pta_sf_app, tol = 80)

# =========================
# 6) Objets modèles légers
# =========================
model_compare_df_app <- data.table(
  Model = c("V2: Log-linear", "V3: Interaction", "P1: Logit OLS", "P2: Beta"),
  Fit = c(
    if (exists("mod_v2")) summary(mod_v2)$adj.r.squared else NA_real_,
    if (exists("mod_v3")) summary(mod_v3)$adj.r.squared else NA_real_,
    if (exists("mod_p1")) summary(mod_p1)$adj.r.squared else NA_real_,
    if (exists("mod_p2")) as.numeric(mod_p2$pseudo.r.squared) else NA_real_
  ),
  Type = c("Sales", "Sales", "Share", "Share")
)

coef_v2_df_app <- NULL
if (exists("mod_v2")) {
  coef_v2_df_app <- broom::tidy(mod_v2, conf.int = TRUE)[, c(
    "term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"
  )]
}

# =========================
# 7) Save ultra-light
# =========================
save(
  results_summary_app,
  candidate_points_sf_app,
  best_candidate_app,
  worst_candidate_app,
  best_pta_sf_app,
  worst_pta_sf_app,
  model_compare_df_app,
  coef_v2_df_app,
  file = "shiny_data_ultra_light.RData",
  compress = "xz"
)

cat("✅ shiny_data_ultra_light.RData créé\n")
print(file.info("shiny_data_ultra_light.RData")$size / 1024^2)


 


