# Monthly totals along the sealing gradient, per city, for the balanced months
# of 05_balanced_months.R:
#   * individuals: estimated monthly total = mean per sampled night x days in month
#   * species: sample-based rarefaction (nights as sampling units) to the
#     smallest number of nights among the sites of that city-month. Expected
#     richness is computed exactly (Colwell et al. 2012), not by resampling.
# Sites exempt from balancing (05_balanced_months.R) are not used to set the
# rarefaction level; if they have fewer nights, all their nights are used and
# the point is flagged.

library(data.table)
library(ggplot2)

out_dir <- "output"

bal  <- fread(file.path(out_dir, "macro_site_month_balanced.csv"))
sn   <- fread(file.path(out_dir, "macro_site_night.csv"))
obs  <- fread(file.path(out_dir, "macro_observations.csv"))
seal <- fread("data/site_sealing.csv")[, .(siteID, sealing)]

sn[, month := as.integer(format(as.Date(night), "%m"))]
sn <- sn[bal[, .(siteID, year, month)], on = c("siteID", "year", "month")]
sn[, days := c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month]]

# ---- individuals: estimated monthly total -----------------------------------------
ind <- sn[, .(nights = .N, value = mean(abundance) * days[1]), by = .(city, year, siteID, month)]

# ---- species: rarefied monthly richness -----------------------------------------------
inc <- unique(obs[species_level == TRUE, .(siteID, night = as.Date(night), scientificName)])
sn[, night := as.Date(night)]
inc <- inc[sn[, .(siteID, night, year, month)], on = c("siteID", "night"), nomatch = NULL]

rarefy <- function(Q, T, m) {         # Q: nights each species was seen, T: nights, m: target nights
  if (m >= T) return(length(Q))
  sum(1 - exp(lchoose(T - Q, m) - lchoose(T, m)))
}

n_rare <- bal[exempt == FALSE, .(m = min(nights)), by = .(city, year, month)]
Q <- inc[, .(Q = .N), by = .(siteID, year, month, scientificName)]
spp <- merge(bal[, .(city, year, siteID, month, nights, exempt)], n_rare, by = c("city", "year", "month"))
spp[, value := mapply(function(s, y, mo, T, m) rarefy(Q[siteID == s & year == y & month == mo]$Q, T, m),
                      siteID, year, month, nights, m)]
spp[, below := nights < m]           # exempt site with fewer nights than the rarefaction level

# ---- graphs ---------------------------------------------------------------------------------
month_cols <- c(Apr = "#2a78d6", May = "#eb6834", Jun = "#1baf7a", Jul = "#eda100",
                Aug = "#e87ba4", Sep = "#008300", Oct = "#6250d6")
ink <- "#52514e"

plot_monthly <- function(d, ylab, caption, file) {
  d <- merge(d, seal, by = "siteID")
  d[, Month := factor(month.abb[month], names(month_cols))]
  p <- ggplot(d, aes(sealing, value, colour = Month, group = Month)) +
    geom_line(linewidth = 0.7) +
    geom_point(aes(shape = flag), size = 2.4, stroke = 0.8, fill = "#fcfcfb") +
    facet_grid(city ~ year, scales = "free_y") +
    scale_colour_manual(values = month_cols, name = NULL, drop = TRUE,
                        guide = guide_legend(nrow = 1)) +
    scale_shape_manual(values = c(normal = 21, flagged = 4), guide = "none") +
    scale_x_continuous(breaks = seq(0, 80, 20), labels = function(x) paste0(x, "%")) +
    expand_limits(y = 0) +
    labs(x = "Sealed surface (category)", y = ylab, caption = caption) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
          strip.text = element_text(face = "bold", colour = "#0b0b0b"),
          axis.text = element_text(colour = ink), axis.title = element_text(colour = ink),
          legend.position = "bottom", legend.justification = "right",
          plot.background = element_rect(fill = "#fcfcfb", colour = NA),
          plot.caption = element_text(colour = ink, hjust = 0, size = 8))
  ggsave(file.path(out_dir, file), p, width = 8, height = 9, dpi = 200)
}

common <- paste("Months in which all sites of a city were sampled (≥ 5 nights per site-month).",
                "Macro-moths only, classification probability ≥ 0.7.")

ind[, flag := "normal"]
plot_monthly(ind, "Macro-moth individuals per month (estimated)",
             paste0(common, "\nEstimated monthly total = mean per sampled night × days in month. ",
                    "Bonn 80% site uses the months of the other Bonn sites, with fewer nights."),
             "macro_monthly_individuals_vs_sealing.png")

spp[, flag := ifelse(below, "flagged", "normal")]
lv <- n_rare[city %in% merge(spp, seal, by = "siteID")$city]
lv <- lv[order(city, year, month), paste0(month.abb[month], " ", m, collapse = ", "), by = .(city, year)]
lv <- lv[, .(V1 = paste(sprintf("%d: %s", year, V1), collapse = "; ")), by = city]
plot_monthly(spp, "Macro-moth species per month (rarefied)",
             paste0(common, "\nSpecies rarefied to the smallest number of nights among the sites of each city-month.\n",
                    "×: Bonn 80% site has fewer nights than that level; all its nights used.\n",
                    "Rarefaction level (nights per site):\n",
                    paste(sprintf("  %s — %s", lv$city, lv$V1), collapse = "\n")),
             "macro_monthly_species_vs_sealing.png")

fwrite(ind, file.path(out_dir, "macro_site_month_individuals.csv"))
fwrite(spp, file.path(out_dir, "macro_site_month_species_rarefied.csv"))
