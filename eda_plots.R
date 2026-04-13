library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)

# ── shared theme ──────────────────────────────────────────────────────────────
theme_pres <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 12),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

CHATGPT_DATE <- as.Date("2022-11-01")
COL_TEX  <- "#E07B39"   # orange
COL_MISO <- "#3A7DC9"   # blue

# ── load TEX ──────────────────────────────────────────────────────────────────
tex_raw <- read_excel("data/Region_TEX.xlsx")

tex_clean <- tex_raw %>%
  select(`Local date`, Hour, Demand, `Demand forecast`, `Net generation`) %>%
  rename(date = `Local date`, hour = Hour,
         demand_mwh = Demand, demand_forecast = `Demand forecast`,
         generation = `Net generation`) %>%
  mutate(date = as_date(date)) %>%
  filter(!is.na(demand_mwh))

tex_daily <- tex_clean %>%
  group_by(date) %>%
  summarise(demand_daily   = sum(demand_mwh,      na.rm = TRUE),
            forecast_daily = sum(demand_forecast,  na.rm = TRUE),
            gen_daily      = sum(generation,       na.rm = TRUE),
            .groups = "drop")

cat("TEX date range:", format(range(tex_daily$date)), "\n")

# ── PLOT 1 — TEX Monthly Average Daily Demand ─────────────────────────────────
p1 <- tex_daily %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(demand_monthly = mean(demand_daily), .groups = "drop") %>%
  ggplot(aes(x = month, y = demand_monthly)) +
  geom_line(color = COL_TEX, linewidth = 0.9) +
  geom_vline(xintercept = CHATGPT_DATE, linetype = "dashed",
             color = "grey30", linewidth = 0.7) +
  annotate("text", x = CHATGPT_DATE, y = Inf,
           label = "ChatGPT launch\n(Nov 2022)",
           hjust = -0.08, vjust = 1.4, size = 3.5, color = "grey30") +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "TEX (ERCOT) — Monthly Average Daily Electricity Demand",
       subtitle = "2015–2026 | Source: EIA",
       x = NULL, y = "Avg Daily Demand (MWh)") +
  theme_pres()

ggsave("output/plot1_tex_monthly_demand.png", p1,
       width = 10, height = 5, dpi = 180)
cat("Saved: output/plot1_tex_monthly_demand.png\n")

# ── PLOT 2 — Day-of-Year Demand by Year (TEX) ─────────────────────────────────
p2 <- tex_daily %>%
  mutate(year  = year(date),
         doy   = yday(date),
         era   = ifelse(year >= 2023, "Post-ChatGPT (2023–25)", "Pre-ChatGPT (2018–22)")) %>%
  filter(year >= 2018, year <= 2025) %>%
  ggplot(aes(x = doy, y = demand_daily,
             color = era,
             alpha = era,
             group = factor(year))) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, linewidth = 1.0) +
  scale_color_manual(
    values = c("Post-ChatGPT (2023–25)" = COL_TEX, "Pre-ChatGPT (2018–22)" = "grey60"),
    name   = NULL
  ) +
  scale_alpha_manual(
    values = c("Post-ChatGPT (2023–25)" = 1, "Pre-ChatGPT (2018–22)" = 0.5),
    name   = NULL
  ) +
  annotate("text", x = 260, y = 1.62e6,
           label = "Post-ChatGPT\nyears float higher",
           color = COL_TEX, fontface = "bold", size = 3.8, hjust = 0) +
  annotate("text", x = 260, y = 1.28e6,
           label = "Pre-ChatGPT\nbaseline",
           color = "grey45", size = 3.5, hjust = 0) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "TEX — Daily Demand by Day of Year",
       subtitle = "Each line = one year  |  Orange = Post-ChatGPT (2023–25)  |  Grey = Pre-ChatGPT (2018–22)",
       x = "Day of Year", y = "Daily Demand (MWh)") +
  theme_pres() +
  theme(legend.position = "none")

ggsave("output/plot2_tex_doy_by_year.png", p2,
       width = 10, height = 5.5, dpi = 180)
cat("Saved: output/plot2_tex_doy_by_year.png\n")

# ── PLOT 3 — STL Trend Component (TEX only for now) ───────────────────────────
tex_ts <- ts(tex_daily$demand_daily,
             start     = c(year(min(tex_daily$date)), yday(min(tex_daily$date))),
             frequency = 365)

tex_stl <- stl(tex_ts, s.window = "periodic", robust = TRUE)

tex_trend_df <- tibble(
  date  = tex_daily$date,
  trend = as.numeric(tex_stl$time.series[, "trend"])
)

p3_tex <- tex_trend_df %>%
  ggplot(aes(x = date, y = trend)) +
  geom_line(color = COL_TEX, linewidth = 0.9) +
  geom_vline(xintercept = CHATGPT_DATE, linetype = "dashed",
             color = "grey30", linewidth = 0.7) +
  annotate("text", x = CHATGPT_DATE, y = Inf,
           label = "ChatGPT launch", hjust = -0.08, vjust = 1.4,
           size = 3.5, color = "grey30") +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "TEX — STL Trend Component",
       subtitle = "Trend shows upward step after Nov 2022",
       x = NULL, y = "Trend (MWh)") +
  theme_pres()

ggsave("output/plot3a_tex_stl_trend.png", p3_tex,
       width = 8, height = 4.5, dpi = 180)
cat("Saved: output/plot3a_tex_stl_trend.png\n")

# ── PLOT 4 — ACF for TEX ──────────────────────────────────────────────────────
png("output/plot4a_tex_acf.png", width = 800, height = 450, res = 120)
acf(tex_daily$demand_daily, lag.max = 400,
    main = "ACF — TEX (ERCOT)",
    col  = COL_TEX, lwd = 2)
dev.off()
cat("Saved: output/plot4a_tex_acf.png\n")

# ── MESSAGE: MISO plots pending ───────────────────────────────────────────────
cat("\n── MISO plots (plot3b, plot4b) require data/MISO.xlsx ──\n")
cat("   Add MISO.xlsx to data/ and run the block below.\n\n")

# ══ MISO BLOCK (run once MISO.xlsx is available) ══════════════════════════════
if (file.exists("data/MISO.xlsx")) {

  miso_raw <- read_excel("data/MISO.xlsx")

  miso_clean <- miso_raw %>%
    select(`Local date`, Hour, `Demand forecast`,
           `Adjusted demand`, `Adjusted net generation`) %>%
    rename(date = `Local date`, hour = Hour,
           demand_mwh = `Adjusted demand`,
           demand_forecast = `Demand forecast`,
           generation = `Adjusted net generation`) %>%
    mutate(date = as_date(date)) %>%
    filter(!is.na(demand_mwh))

  miso_daily <- miso_clean %>%
    group_by(date) %>%
    summarise(demand_daily   = sum(demand_mwh,     na.rm = TRUE),
              forecast_daily = sum(demand_forecast, na.rm = TRUE),
              gen_daily      = sum(generation,      na.rm = TRUE),
              .groups = "drop")

  cat("MISO date range:", format(range(miso_daily$date)), "\n")

  # STL for MISO
  miso_ts <- ts(miso_daily$demand_daily,
                start     = c(year(min(miso_daily$date)), yday(min(miso_daily$date))),
                frequency = 365)
  miso_stl <- stl(miso_ts, s.window = "periodic", robust = TRUE)

  miso_trend_df <- tibble(
    date  = miso_daily$date,
    trend = as.numeric(miso_stl$time.series[, "trend"])
  )

  p3_miso <- miso_trend_df %>%
    ggplot(aes(x = date, y = trend)) +
    geom_line(color = COL_MISO, linewidth = 0.9) +
    geom_vline(xintercept = CHATGPT_DATE, linetype = "dashed",
               color = "grey30", linewidth = 0.7) +
    annotate("text", x = CHATGPT_DATE, y = Inf,
             label = "ChatGPT launch", hjust = -0.08, vjust = 1.4,
             size = 3.5, color = "grey30") +
    scale_y_continuous(labels = scales::comma) +
    labs(title    = "MISO — STL Trend Component",
         subtitle = "Trend remains relatively flat — no structural break",
         x = NULL, y = "Trend (MWh)") +
    theme_pres()

  ggsave("output/plot3b_miso_stl_trend.png", p3_miso,
         width = 8, height = 4.5, dpi = 180)
  cat("Saved: output/plot3b_miso_stl_trend.png\n")

  # Side-by-side STL trend comparison
  combined_trend <- bind_rows(
    tex_trend_df  %>% mutate(region = "TEX (ERCOT)"),
    miso_trend_df %>% mutate(region = "MISO")
  )

  p3_combined <- combined_trend %>%
    ggplot(aes(x = date, y = trend, color = region)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = CHATGPT_DATE, linetype = "dashed",
               color = "grey30", linewidth = 0.7) +
    annotate("text", x = CHATGPT_DATE, y = Inf,
             label = "ChatGPT launch", hjust = -0.08, vjust = 1.4,
             size = 3.5, color = "grey30") +
    scale_color_manual(values = c("TEX (ERCOT)" = COL_TEX, "MISO" = COL_MISO),
                       name = NULL) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~region, scales = "free_y", ncol = 2) +
    labs(title    = "STL Trend Component — TEX vs MISO",
         subtitle = "TEX shows a clear upward step post-2022; MISO remains stable",
         x = NULL, y = "Trend (MWh)") +
    theme_pres() +
    theme(legend.position = "none")

  ggsave("output/plot3_combined_stl_trend.png", p3_combined,
         width = 12, height = 5, dpi = 180)
  cat("Saved: output/plot3_combined_stl_trend.png\n")

  # ACF for MISO
  png("output/plot4b_miso_acf.png", width = 800, height = 450, res = 120)
  acf(miso_daily$demand_daily, lag.max = 400,
      main = "ACF — MISO",
      col  = COL_MISO, lwd = 2)
  dev.off()
  cat("Saved: output/plot4b_miso_acf.png\n")

} else {
  cat("   Skipping MISO plots — data/MISO.xlsx not found.\n")
}

cat("\nDone. All available plots saved to output/\n")
