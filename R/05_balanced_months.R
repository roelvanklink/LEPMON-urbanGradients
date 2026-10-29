# Balanced within-city comparison: pool nights per site and month, and keep
# only the months in which every retained site of the city has at least
# min_nights sampled nights. Each month is then weighted equally per site.
# Requires output/macro_site_night.csv from 02_macros.R.

library(data.table)

out_dir    <- "output"
min_nights <- 5

# sites dropped before balancing (otherwise they remove most months for their city)
drop <- data.table(siteID = "MR-1", year = 2025, why = "only 15 nights in 2025 (Aug-Oct)")

# sites that do not determine the months, but are included in whatever months
# the other sites of their city define (few nights in some of those months)
exempt <- data.table(siteID = "BN-3", year = 2025:2026, why = "Noeggerathstr (80%): sparse catches, keep with Bonn months")

sn <- fread(file.path(out_dir, "macro_site_night.csv"))
sn[, month := as.integer(format(as.Date(night), "%m"))]
sn <- sn[!drop, on = c("siteID", "year")]

sm <- sn[, .(nights = .N, abundance = mean(abundance), richness = mean(richness)),
         by = .(city, year, siteID, month)]
sm[, exempt := FALSE][exempt, exempt := TRUE, on = c("siteID", "year")]
sm[, n_sites := uniqueN(siteID[!exempt]), by = .(city, year)]
sm[, ok := nights >= min_nights]
months_ok <- sm[exempt == FALSE, .(complete = all(ok) & .N == n_sites[1]), by = .(city, year, month)][complete == TRUE]
bal <- sm[months_ok[, .(city, year, month)], on = c("city", "year", "month")]

site <- bal[, .(months = paste(month.abb[sort(month)], collapse = "-"), n_months = .N,
                nights = sum(nights),
                abundance = mean(abundance), richness = mean(richness)),
            by = .(city, year, siteID)]

fwrite(bal,  file.path(out_dir, "macro_site_month_balanced.csv"))
fwrite(site, file.path(out_dir, "macro_site_balanced.csv"))
print(site[order(city, year, siteID)])
