library(shiny)
library(sf)
library(leaflet)
library(htmltools)
library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------------
# Mapbox
# ------------------------------------------------------------------------------

mapbox_token <- "secret_mapbox_token_here"
# ------------------------------------------------------------------------------
# Load light data
# ------------------------------------------------------------------------------

load("shiny_data_ultra_light.RData")

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

make_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

fmt_int <- function(x) {
  x <- make_numeric(x)
  if (length(x) == 0 || all(is.na(x))) return("NA")
  format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
}

fmt_num <- function(x, digits = 1) {
  x <- make_numeric(x)
  if (length(x) == 0 || all(is.na(x))) return("NA")
  format(round(x, digits), nsmall = digits, big.mark = " ", scientific = FALSE, trim = TRUE)
}

empty_plot <- function(message) {
  plot.new()
  text(0.5, 0.5, message, cex = 1.1, col = "#6b7280")
}

to_wgs84 <- function(x) {
  if (is.null(x)) return(NULL)
  if (!inherits(x, "sf")) return(x)
  if (nrow(x) == 0) return(x)
  
  crs_x <- suppressWarnings(st_crs(x))
  
  if (is.na(crs_x)) {
    st_crs(x) <- 4326
    return(x)
  }
  
  if (!is.na(crs_x$epsg) && crs_x$epsg == 4326) {
    return(x)
  }
  
  suppressWarnings(st_transform(x, 4326))
}

# ------------------------------------------------------------------------------
# Prepare objects
# ------------------------------------------------------------------------------

results_summary_app <- as.data.table(results_summary_app)

num_cols <- c(
  "rank",
  "total_pred_sales",
  "avg_market_share",
  "max_dist_km",
  "nb_iris_pta"
)

for (col in intersect(num_cols, names(results_summary_app))) {
  results_summary_app[[col]] <- make_numeric(results_summary_app[[col]])
}

setorder(results_summary_app, rank)

candidate_points_sf_app <- to_wgs84(candidate_points_sf_app)

coords_all_sf <- st_coordinates(candidate_points_sf_app)

points_df <- data.table::copy(
  as.data.table(st_drop_geometry(candidate_points_sf_app))
)

points_df[, `:=`(
  lon = coords_all_sf[, 1],
  lat = coords_all_sf[, 2]
)]

summary_dt <- data.table::copy(
  results_summary_app[, .(
    candidate_id,
    IRIS,
    max_dist_km,
    nb_iris_pta
  )]
)

points_df <- data.table::copy(
  merge(
    points_df,
    summary_dt,
    by = "candidate_id",
    all.x = TRUE,
    suffixes = c("", "_summary"),
    sort = FALSE
  )
)

data.table::setDT(points_df)

if ("IRIS_summary" %in% names(points_df) && !("IRIS" %in% names(points_df))) {
  data.table::setnames(points_df, "IRIS_summary", "IRIS")
}
if ("max_dist_km_summary" %in% names(points_df) && !("max_dist_km" %in% names(points_df))) {
  data.table::setnames(points_df, "max_dist_km_summary", "max_dist_km")
}
if ("nb_iris_pta_summary" %in% names(points_df) && !("nb_iris_pta" %in% names(points_df))) {
  data.table::setnames(points_df, "nb_iris_pta_summary", "nb_iris_pta")
}

for (col in intersect(c("rank", "total_pred_sales", "avg_market_share", "max_dist_km", "nb_iris_pta", "lon", "lat"), names(points_df))) {
  if (!(col %in% c("IRIS", "candidate_id"))) {
    points_df[[col]] <- make_numeric(points_df[[col]])
  }
}

points_df <- data.table::copy(points_df)

best_row  <- results_summary_app[candidate_id == best_candidate_app]
worst_row <- results_summary_app[candidate_id == worst_candidate_app]

best_point  <- points_df[candidate_id == best_candidate_app]
worst_point <- points_df[candidate_id == worst_candidate_app]

points_df[, popup_html := paste0(
  "<b>", candidate_id, "</b><br>",
  "Rank: ", rank, "<br>",
  "IRIS: ", IRIS, "<br>",
  "Predicted sales: ", fmt_int(total_pred_sales), " €<br>",
  "Avg. market share: ", fmt_num(avg_market_share, 1), "%<br>",
  "PTA radius: ", fmt_num(max_dist_km, 1), " km"
)]

best_popup <- paste0(
  "<b>Best candidate</b><br>",
  best_candidate_app, "<br>",
  "Predicted sales: ", fmt_int(best_row$total_pred_sales), " €<br>",
  "Avg. market share: ", fmt_num(best_row$avg_market_share, 1), "%<br>",
  "PTA radius: ", fmt_num(best_row$max_dist_km, 1), " km"
)

worst_popup <- paste0(
  "<b>Worst candidate</b><br>",
  worst_candidate_app, "<br>",
  "Predicted sales: ", fmt_int(worst_row$total_pred_sales), " €<br>",
  "Avg. market share: ", fmt_num(worst_row$avg_market_share, 1), "%<br>",
  "PTA radius: ", fmt_num(worst_row$max_dist_km, 1), " km"
)

plot_dt <- copy(results_summary_app)
plot_dt[, status := "Other"]
plot_dt[candidate_id == best_candidate_app, status := "Best"]
plot_dt[candidate_id == worst_candidate_app, status := "Worst"]

# ------------------------------------------------------------------------------
# Colors / theme
# ------------------------------------------------------------------------------

clr_navy   <- "#111827"
clr_slate  <- "#374151"
clr_muted  <- "#6b7280"
clr_green  <- "#16a34a"
clr_red    <- "#dc2626"
clr_blue   <- "#2563eb"
clr_orange <- "#f59e0b"
clr_gray   <- "#9ca3af"

theme_geo <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = clr_navy, size = 14),
    plot.subtitle = element_text(color = clr_muted, size = 10),
    axis.title = element_text(color = clr_slate, size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e5e7eb"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f6f8fb;
        font-family: Arial, Helvetica, sans-serif;
      }

      .app-title {
        background: linear-gradient(135deg, #111827, #1f2937);
        color: white;
        padding: 22px 28px;
        border-radius: 18px;
        margin-bottom: 18px;
        box-shadow: 0 8px 20px rgba(0,0,0,0.14);
      }

      .app-title h2 {
        margin: 0;
        font-weight: 700;
      }

      .app-title p {
        margin: 6px 0 0 0;
        color: #d1d5db;
      }

      .info-card, .box-card {
        background: white;
        border-radius: 18px;
        padding: 18px 20px;
        margin-bottom: 18px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.08);
      }

      .info-label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.10em;
        color: #6b7280;
        margin-bottom: 8px;
        font-weight: 700;
      }

      .info-value {
        font-size: 22px;
        font-weight: 700;
        color: #111827;
      }

      .info-sub {
        margin-top: 10px;
        color: #4b5563;
        font-size: 14px;
      }

      .green-accent { border-left: 6px solid #16a34a; }
      .red-accent { border-left: 6px solid #dc2626; }
      .blue-accent { border-left: 6px solid #2563eb; }
      .orange-accent { border-left: 6px solid #f59e0b; }

      .section-title {
        font-size: 18px;
        font-weight: 700;
        color: #111827;
        margin: 4px 0 12px 0;
      }

      .section-sub {
        color: #6b7280;
        margin: -4px 0 12px 0;
        font-size: 14px;
      }

      .leaflet-container {
        border-radius: 14px;
      }

      .action-btn {
        width: 100%;
        margin-bottom: 10px;
        border: none;
        border-radius: 12px;
        padding: 10px 12px;
        font-weight: 700;
      }

      .btn-best  { background: #dcfce7; color: #166534; }
      .btn-worst { background: #fee2e2; color: #991b1b; }
      .btn-both  { background: #dbeafe; color: #1d4ed8; }

      table.rank-table {
        width: 100%;
        border-collapse: collapse;
      }

      table.rank-table th {
        background: #f3f4f6;
        color: #111827;
        text-align: left;
        padding: 10px;
        font-size: 13px;
      }

      table.rank-table td {
        padding: 10px;
        border-top: 1px solid #e5e7eb;
        font-size: 13px;
        color: #374151;
      }

      .badge-best {
        background: #dcfce7;
        color: #166534;
        padding: 4px 8px;
        border-radius: 999px;
        font-weight: 700;
        font-size: 12px;
      }

      .badge-worst {
        background: #fee2e2;
        color: #991b1b;
        padding: 4px 8px;
        border-radius: 999px;
        font-weight: 700;
        font-size: 12px;
      }

      .badge-mid {
        background: #e5e7eb;
        color: #374151;
        padding: 4px 8px;
        border-radius: 999px;
        font-weight: 700;
        font-size: 12px;
      }

      .note-box {
        background: #eff6ff;
        border: 1px solid #bfdbfe;
        border-radius: 14px;
        padding: 12px 14px;
        color: #1e3a8a;
        font-size: 13px;
        margin-bottom: 12px;
      }
    "))
  ),
  
  div(
    class = "app-title",
    h2("Final Project - Geomarketing Store Location Analysis"),
    p("Alexandre Picart - Badiallo Kantako - Dorian Senseby - Lucien Mirlicourtois
")
  ),
  
  tabsetPanel(
    tabPanel(
      "Overview",
      br(),
      
      fluidRow(
        column(
          3,
          div(
            class = "info-card green-accent",
            div(class = "info-label", "Best candidate"),
            div(class = "info-value", best_candidate_app),
            div(class = "info-sub", paste0("Rank #", best_row$rank, " • Sales: ", fmt_int(best_row$total_pred_sales), " €"))
          )
        ),
        column(
          3,
          div(
            class = "info-card red-accent",
            div(class = "info-label", "Worst candidate"),
            div(class = "info-value", worst_candidate_app),
            div(class = "info-sub", paste0("Rank #", worst_row$rank, " • Sales: ", fmt_int(worst_row$total_pred_sales), " €"))
          )
        ),
        column(
          3,
          div(
            class = "info-card blue-accent",
            div(class = "info-label", "Best avg. market share"),
            div(class = "info-value", paste0(fmt_num(best_row$avg_market_share, 1), "%")),
            div(class = "info-sub", paste0("PTA radius: ", fmt_num(best_row$max_dist_km, 1), " km"))
          )
        ),
        column(
          3,
          div(
            class = "info-card orange-accent",
            div(class = "info-label", "Candidates evaluated"),
            div(class = "info-value", nrow(results_summary_app)),
            div(class = "info-sub", paste0("Best vs worst gap: ", fmt_int(best_row$total_pred_sales - worst_row$total_pred_sales), " €"))
          )
        )
      ),
      
      fluidRow(
        column(
          3,
          div(
            class = "box-card",
            div(class = "section-title", "Map controls"),
            actionButton("show_best", "Focus best candidate", class = "action-btn btn-best"),
            actionButton("show_worst", "Focus worst candidate", class = "action-btn btn-worst"),
            actionButton("show_both", "Show both zones", class = "action-btn btn-both"),
            div(
              class = "note-box",
              HTML("<b>Green circle</b> = synthetic best PTA<br><b>Red circle</b> = synthetic worst PTA<br>Radius = PTA radius from the model output")
            )
          )
        ),
        column(
          9,
          div(
            class = "box-card",
            div(class = "section-title", textOutput("map_title")),
            div(class = "section-sub", textOutput("map_subtitle")),
            leafletOutput("bw_map", height = 540)
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "box-card",
            div(class = "section-title", "Ranking of candidate locations"),
            div(class = "section-sub", "Predicted sales by candidate"),
            plotOutput("ranking_plot", height = 360)
          )
        ),
        column(
          6,
          div(
            class = "box-card",
            div(class = "section-title", "Candidate profile"),
            div(class = "section-sub", "Market share vs predicted sales"),
            plotOutput("profile_plot", height = 360)
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "box-card",
            div(class = "section-title", "Ranking table"),
            div(class = "section-sub", "Predicted sales, market share and size of the principal trading area"),
            htmlOutput("ranking_table")
          )
        )
      )
    ),
    
    tabPanel(
      "Models",
      br(),
      fluidRow(
        column(
          6,
          div(
            class = "box-card",
            div(class = "section-title", "Model fit comparison"),
            div(class = "section-sub", "Adjusted R² / pseudo-R²"),
            plotOutput("model_compare_plot", height = 380)
          )
        ),
        column(
          6,
          div(
            class = "box-card",
            div(class = "section-title", "Estimated coefficients — main sales model"),
            div(class = "section-sub", "95% confidence intervals"),
            plotOutput("coef_plot", height = 380)
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  selected_mode <- reactiveVal("both")
  
  observeEvent(input$show_best, {
    selected_mode("best")
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_worst, {
    selected_mode("worst")
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_both, {
    selected_mode("both")
  }, ignoreInit = TRUE)
  
  output$map_title <- renderText({
    mode <- selected_mode()
    
    if (mode == "best") {
      return(paste("Best market zone —", best_candidate_app))
    }
    
    if (mode == "worst") {
      return(paste("Worst market zone —", worst_candidate_app))
    }
    
    "Best and worst market zones"
  })
  
  output$map_subtitle <- renderText({
    mode <- selected_mode()
    
    if (mode == "best") {
      return(paste0(
        "Sales: ", fmt_int(best_row$total_pred_sales), " € • ",
        "Market share: ", fmt_num(best_row$avg_market_share, 1), "% • ",
        "PTA radius: ", fmt_num(best_row$max_dist_km, 1), " km"
      ))
    }
    
    if (mode == "worst") {
      return(paste0(
        "Sales: ", fmt_int(worst_row$total_pred_sales), " € • ",
        "Market share: ", fmt_num(worst_row$avg_market_share, 1), "% • ",
        "PTA radius: ", fmt_num(worst_row$max_dist_km, 1), " km"
      ))
    }
    
    paste0(
      "Best = ", best_candidate_app,
      " | Worst = ", worst_candidate_app,
      " | Use the buttons to switch view"
    )
  })
  
  output$bw_map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = TRUE,
        preferCanvas = TRUE
      )
    ) %>%
      addTiles(
        urlTemplate = mapbox_token,
        attribution = "Mapbox"
      ) %>%
      fitBounds(
        lng1 = min(points_df$lon, na.rm = TRUE),
        lat1 = min(points_df$lat, na.rm = TRUE),
        lng2 = max(points_df$lon, na.rm = TRUE),
        lat2 = max(points_df$lat, na.rm = TRUE)
      )
  })
  
  observe({
    req(nrow(points_df) > 0)
    
    mode <- selected_mode()
    
    proxy <- leafletProxy("bw_map")
    
    proxy %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
    
    other_points <- points_df[!(candidate_id %in% c(best_candidate_app, worst_candidate_app))]
    
    if (!is.null(other_points) && nrow(other_points) > 0) {
      proxy %>%
        addCircleMarkers(
          data = other_points,
          lng = ~lon,
          lat = ~lat,
          radius = 4,
          color = "#6b7280",
          weight = 1,
          fillColor = "#9ca3af",
          fillOpacity = 0.85,
          popup = ~popup_html
        )
    }
    
    if (mode %in% c("best", "both") && nrow(best_point) > 0) {
      proxy %>%
        addCircles(
          lng = best_point$lon[1],
          lat = best_point$lat[1],
          radius = best_row$max_dist_km[1] * 100,
          color = "#166534",
          fillColor = "#22c55e",
          fillOpacity = 0.20,
          weight = 2,
          popup = best_popup
        ) %>%
        addCircleMarkers(
          lng = best_point$lon[1],
          lat = best_point$lat[1],
          radius = 9,
          color = "#111827",
          weight = 2,
          fillColor = "#16a34a",
          fillOpacity = 1,
          popup = best_popup
        )
    }
    
    if (mode %in% c("worst", "both") && nrow(worst_point) > 0) {
      proxy %>%
        addCircles(
          lng = worst_point$lon[1],
          lat = worst_point$lat[1],
          radius = worst_row$max_dist_km[1] * 100,
          color = "#991b1b",
          fillColor = "#f87171",
          fillOpacity = 0.20,
          weight = 2,
          popup = worst_popup
        ) %>%
        addCircleMarkers(
          lng = worst_point$lon[1],
          lat = worst_point$lat[1],
          radius = 9,
          color = "#111827",
          weight = 2,
          fillColor = "#dc2626",
          fillOpacity = 1,
          popup = worst_popup
        )
    }
    
    if (mode == "best" && nrow(best_point) > 0) {
      proxy %>%
        setView(
          lng = best_point$lon[1],
          lat = best_point$lat[1],
          zoom = 6
        ) %>%
        addLegend(
          position = "bottomright",
          colors = "#22c55e",
          labels = paste("Best zone:", best_candidate_app),
          opacity = 0.7,
          title = "Displayed zone"
        )
    } else if (mode == "worst" && nrow(worst_point) > 0) {
      proxy %>%
        setView(
          lng = worst_point$lon[1],
          lat = worst_point$lat[1],
          zoom = 6
        ) %>%
        addLegend(
          position = "bottomright",
          colors = "#f87171",
          labels = paste("Worst zone:", worst_candidate_app),
          opacity = 0.7,
          title = "Displayed zone"
        )
    } else {
      proxy %>%
        fitBounds(
          lng1 = min(points_df$lon, na.rm = TRUE),
          lat1 = min(points_df$lat, na.rm = TRUE),
          lng2 = max(points_df$lon, na.rm = TRUE),
          lat2 = max(points_df$lat, na.rm = TRUE)
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("#22c55e", "#f87171"),
          labels = c(
            paste("Best zone:", best_candidate_app),
            paste("Worst zone:", worst_candidate_app)
          ),
          opacity = 0.7,
          title = "Displayed zones"
        )
    }
  })
  
  output$ranking_table <- renderUI({
    dt <- copy(results_summary_app)
    
    status <- rep("Other", nrow(dt))
    status[dt$candidate_id == best_candidate_app] <- "Best"
    status[dt$candidate_id == worst_candidate_app] <- "Worst"
    
    badge_html <- ifelse(
      status == "Best",
      '<span class="badge-best">BEST</span>',
      ifelse(
        status == "Worst",
        '<span class="badge-worst">WORST</span>',
        '<span class="badge-mid">OTHER</span>'
      )
    )
    
    rows <- paste0(
      "<tr>",
      "<td>", dt$rank, "</td>",
      "<td><b>", dt$candidate_id, "</b></td>",
      "<td>", dt$IRIS, "</td>",
      "<td>", fmt_int(dt$total_pred_sales), " €</td>",
      "<td>", fmt_num(dt$avg_market_share, 1), "%</td>",
      "<td>", fmt_num(dt$max_dist_km, 1), "</td>",
      "<td>", dt$nb_iris_pta, "</td>",
      "<td>", badge_html, "</td>",
      "</tr>",
      collapse = ""
    )
    
    HTML(paste0(
      '<table class="rank-table"><thead><tr>',
      '<th>Rank</th><th>Candidate</th><th>IRIS</th><th>Predicted sales</th><th>Avg. market share</th><th>PTA radius (km)</th><th>Nb IRIS</th><th>Status</th>',
      '</tr></thead><tbody>', rows, '</tbody></table>'
    ))
  })
  
  output$ranking_plot <- renderPlot({
    dt <- copy(plot_dt)
    
    if (nrow(dt) == 0) {
      empty_plot("No candidate data available")
      return(invisible(NULL))
    }
    
    ggplot(
      dt,
      aes(x = reorder(candidate_id, total_pred_sales), y = total_pred_sales / 1e6, fill = status)
    ) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = round(total_pred_sales / 1e6, 2)),
        hjust = -0.15,
        size = 3.6,
        fontface = "bold"
      ) +
      coord_flip() +
      scale_fill_manual(values = c("Best" = clr_green, "Worst" = clr_red, "Other" = clr_gray)) +
      labs(x = NULL, y = "Predicted sales (M€)") +
      theme_geo +
      theme(legend.title = element_blank())
  })
  
  output$profile_plot <- renderPlot({
    dt <- copy(plot_dt)
    
    if (nrow(dt) == 0) {
      empty_plot("No candidate data available")
      return(invisible(NULL))
    }
    
    ggplot(
      dt,
      aes(x = avg_market_share, y = total_pred_sales / 1e6, size = nb_iris_pta, color = status)
    ) +
      geom_point(alpha = 0.9) +
      geom_text(aes(label = candidate_id), nudge_y = 0.05, size = 3.5) +
      scale_color_manual(values = c("Best" = clr_green, "Worst" = clr_red, "Other" = clr_blue)) +
      labs(
        x = "Average market share (%)",
        y = "Predicted sales (M€)",
        size = "Nb IRIS"
      ) +
      theme_geo
  })
  
  output$model_compare_plot <- renderPlot({
    if (!exists("model_compare_df_app") || is.null(model_compare_df_app)) {
      empty_plot("Model summary not available")
      return(invisible(NULL))
    }
    
    plot_mod <- as.data.table(model_compare_df_app)
    plot_mod <- plot_mod[!is.na(Fit)]
    
    if (nrow(plot_mod) == 0) {
      empty_plot("Model summary not available")
      return(invisible(NULL))
    }
    
    ggplot(plot_mod, aes(x = reorder(Model, Fit), y = Fit, fill = Type)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = round(Fit, 3)), hjust = -0.15, fontface = "bold", size = 3.8) +
      coord_flip() +
      scale_fill_manual(values = c("Sales" = clr_blue, "Share" = clr_orange)) +
      labs(x = NULL, y = "Fit metric") +
      theme_geo +
      theme(legend.title = element_blank())
  })
  
  output$coef_plot <- renderPlot({
    if (!exists("coef_v2_df_app") || is.null(coef_v2_df_app) || nrow(coef_v2_df_app) == 0) {
      empty_plot("Coefficient summary not available")
      return(invisible(NULL))
    }
    
    coef_dt <- as.data.table(coef_v2_df_app)
    coef_dt <- coef_dt[term != "(Intercept)"]
    
    if (nrow(coef_dt) == 0) {
      empty_plot("Coefficient summary not available")
      return(invisible(NULL))
    }
    
    pretty_names <- c(
      log_distance = "Distance",
      log_poi = "POIs near shop",
      log_compet_2km = "Competition within 2 km",
      log_mp = "Market potential",
      log_pop = "Population",
      prop_cadres = "Cadres",
      prop_jeunes = "Young people",
      prop_maisons = "Houses"
    )
    
    coef_dt[, term_label := ifelse(term %in% names(pretty_names), pretty_names[term], term)]
    coef_dt[, significant := p.value < 0.05]
    
    ggplot(
      coef_dt,
      aes(x = reorder(term_label, estimate), y = estimate, ymin = conf.low, ymax = conf.high, color = significant)
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = clr_gray) +
      geom_errorbar(width = 0.15, linewidth = 0.9) +
      geom_point(size = 3.2) +
      coord_flip() +
      scale_color_manual(values = c("TRUE" = clr_blue, "FALSE" = clr_gray)) +
      labs(x = NULL, y = "Coefficient") +
      theme_geo +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)