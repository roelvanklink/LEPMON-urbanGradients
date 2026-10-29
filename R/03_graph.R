# Preliminary graphs: macro-moth abundance and species richness per trap-night,
# by city, with 2025 and 2026 kept separate (their seasons differ).
# Requires output/macro_site_night.csv from 02_macros.R.

library(data.table)
library(ggplot2)

out_dir <- "output"
sn <- fread(file.path(out_dir, "macro_site_night.csv"))

long <- melt(sn, id.vars = c("city", "siteID", "year", "night"),
             measure.vars = c("abundance", "richness"), variable.name = "metric")
long[, metric := factor(metric, c("abundance", "richness"),
                        c("Individuals per trap-night", "Species per trap-night"))]

# sites are the replicates: mean per site, then mean +- SE over sites per city
site <- long[, .(value = mean(value), nights = .N), by = .(city, siteID, year, metric)]
city <- site[, .(mean = mean(value), se = sd(value) / sqrt(.N), sites = .N), by = .(city, year, metric)]

ink <- "#52514e"; blue <- "#2a78d6"
theme_lep <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "#e6e5e1", linewidth = 0.3),
        strip.text = element_text(face = "bold", hjust = 0, colour = "#0b0b0b"),
        axis.text = element_text(colour = ink), axis.title = element_text(colour = ink),
        plot.background = element_rect(fill = "#fcfcfb", colour = NA),
        plot.caption = element_text(colour = ink, hjust = 0, size = 8))

p1 <- ggplot(city, aes(city, mean)) +
  geom_point(data = site, aes(y = value), colour = "#9a9993", size = 1.6,
             position = position_jitter(width = 0.12, height = 0, seed = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, linewidth = 0.7, colour = blue) +
  geom_point(colour = blue, size = 3) +
  facet_grid(metric ~ year, scales = "free", space = "free_x", switch = "y") +
  labs(x = NULL, y = NULL,
       caption = paste("Blue: city mean ± SE over sites; grey: site means. Macro-moths only, classification probability ≥ 0.7.",
                       "\n2025 season: May–Nov; 2026 season: Mar–Aug (ongoing). Seasons differ, so compare cities within a year only.")) +
  theme_lep + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, hjust = 0.5))
ggsave(file.path(out_dir, "macro_per_trapnight_by_city.png"), p1, width = 9, height = 6.5, dpi = 200)

# phenology: weekly mean per trap-night, to show how the seasons differ
sn[, week := as.Date(cut(as.Date(night), "week"))]
wk <- sn[, .(abundance = mean(abundance), nights = .N), by = .(city, year, week)][nights >= 3]
# complete the week grid so lines break where a city has no data
grid <- wk[, .(week = seq(min(week), max(week), by = "week")), by = .(city, year)]
wk <- merge(grid, wk, by = c("city", "year", "week"), all.x = TRUE)
wk[, plot_date := as.Date(format(week, "2000-%m-%d"))]
cols <- c(Bonn = "#2a78d6", Freiburg = "#eb6834", Jena = "#1baf7a",
          Leipzig = "#eda100", Ludwigshafen = "#e87ba4", Marburg = "#6250d6")

p2 <- ggplot(wk, aes(plot_date, abundance, colour = city)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ year, ncol = 1) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = NULL, y = "Macro-moth individuals per trap-night (weekly mean)",
       caption = "Weeks with fewer than 3 trap-nights in a city are omitted.") +
  theme_lep + theme(legend.position = "top", panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3))
ggsave(file.path(out_dir, "macro_phenology_by_city.png"), p2, width = 9, height = 6.5, dpi = 200)
