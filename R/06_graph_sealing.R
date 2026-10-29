# Macro-moth abundance and species richness per trap-night along the
# urbanisation gradient, within each city. Site values are means over the
# balanced months (05_balanced_months.R), so sites within a city-year are
# comparable; years have different seasons and are shown as separate lines.

library(data.table)
library(ggplot2)

out_dir <- "output"

site <- fread(file.path(out_dir, "macro_site_balanced.csv"))
seal <- fread("data/site_sealing.csv")[, .(siteID, sealing)]
site <- merge(site, seal, by = "siteID")      # cities without sealing categories drop out

long <- melt(site, id.vars = c("city", "year", "siteID", "sealing", "months", "nights"),
             measure.vars = c("abundance", "richness"), variable.name = "metric")
long[, metric := factor(metric, c("abundance", "richness"),
                        c("Individuals per trap-night", "Species per trap-night"))]
long[, year := factor(year)]

periods <- site[, .(n = .N), by = .(city, year, months)][order(city, year)]
periods[, txt := sprintf("%d: %s (%d sites)", year, gsub("-", "–", months), n)]
periods <- periods[, .(txt = paste0(city[1], " — ", paste(txt, collapse = ", "))), by = city]
caption <- paste0("Site means over months in which all sites of a city were sampled (≥ 5 nights per site-month). ",
                  "Macro-moths only, classification probability ≥ 0.7.\n",
                  "Bonn 80% site (Noeggerathstr) uses the months of the other Bonn sites, with fewer nights (39 in 2025, 17 in 2026).\n",
                  paste(periods$txt, collapse = "\n"))

ink <- "#52514e"
p <- ggplot(long, aes(sealing, value, colour = year)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, stroke = 0.8, fill = "#fcfcfb", shape = 21) +
  facet_grid(metric ~ city, scales = "free_y", switch = "y") +
  scale_colour_manual(values = c("2025" = "#2a78d6", "2026" = "#eb6834"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 80, 20), labels = function(x) paste0(x, "%")) +
  expand_limits(y = 0) +
  labs(x = "Sealed surface (category)", y = NULL, caption = caption) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        strip.text = element_text(face = "bold", colour = "#0b0b0b"),
        strip.placement = "outside",
        axis.text = element_text(colour = ink), axis.title = element_text(colour = ink),
        legend.position = "bottom", legend.justification = "right",
        plot.background = element_rect(fill = "#fcfcfb", colour = NA),
        plot.caption = element_text(colour = ink, hjust = 0, size = 8))

ggsave(file.path(out_dir, "macro_vs_sealing_by_city.png"), p, width = 10, height = 6.5, dpi = 200)
