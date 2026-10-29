# Sampling effort per trap-night for the LEPMON urban gradient cities.
#
# Deployments in the Camtrap DP export are upload sessions, not trap-nights:
# a night can be split over several sessions (e.g. short morning sessions
# ~06:00 UTC), and locationIDs are re-issued over time for the same physical
# trap. We therefore
#   * define a site by its coordinates (clustered within 250 m per city),
#     split by camera where two units ran at the same coordinates simultaneously
#   * assign each session to the night it belongs to (start time - 12 h)
#   * measure effort as hours of operation (union of session intervals)
#
# Media are activity-triggered only, so image counts are not used as effort.

data_dir <- "C:/Dropbox/work/2024 LEPMON/Urban gradients/Urban gradient data 2026-10-27"
out_dir  <- "output"
dir.create(out_dir, showWarnings = FALSE)

min_hours <- 2   # a site-night with fewer operating hours is not counted as sampled

# ---- deployments --------------------------------------------------------------
d <- read.csv(file.path(data_dir, "deployments.csv"), stringsAsFactors = FALSE)
d <- d[grepl("Urban Gradient", d$setupBy), ]
d$city  <- sub(" \\(.*$", "", d$setupBy)
d$start <- as.POSIXct(d$deploymentStart, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
d$end   <- as.POSIXct(d$deploymentEnd,   format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
d$night <- as.Date(d$start - 12 * 3600)
d$year  <- as.integer(format(d$night, "%Y"))

# exclusions
d <- d[!grepl("^YY-XXX", d$locationID), ]                                  # test unit near Wismar
d <- d[!(d$city %in% c("Freiburg", "Ludwigshafen") & d$year == 2025), ]  # one-week pilot runs
d <- d[!d$cameraID %in% c("SN010072", "SN010079"), ]  # LU & MR sites started Aug 2026, 2-4 nights only

# coordinate fix: Bonn SN010040 carries the coordinates of SN010035 in the export,
# but its data are independent; true position from "Device coordinates March 2026.csv"
d[d$cameraID == "SN010040", c("latitude", "longitude")] <- list(50.7491988, 7.0863737)

# ---- sites: cluster coordinates within 250 m per city -------------------------
d$site <- NA_character_
for (cty in unique(d$city)) {
  i  <- which(d$city == cty)
  xy <- unique(d[i, c("latitude", "longitude")])
  km <- cbind((xy$latitude - mean(xy$latitude)) * 111.32,
              (xy$longitude - mean(xy$longitude)) * 111.32 * cos(mean(xy$latitude) * pi / 180))
  cl <- if (nrow(xy) > 1) cutree(hclust(dist(km), "single"), h = 0.25) else 1
  key <- paste(xy$latitude, xy$longitude)
  d$site[i] <- paste0(cty, "_", cl[match(paste(d$latitude[i], d$longitude[i]), key)])
}

# split a site by camera where two cameras ran on the same night
shared <- aggregate(cameraID ~ site + night, d, function(z) length(unique(z)))
multi  <- unique(shared$site[shared$cameraID > 1])
d$site[d$site %in% multi] <- paste0(d$site[d$site %in% multi], "_", d$cameraID[d$site %in% multi])

# readable site codes: city abbreviation + number ordered by latitude
abbr <- c(Bonn = "BN", Freiburg = "FR", Jena = "J", Leipzig = "L", Ludwigshafen = "LU", Marburg = "MR")
s <- aggregate(cbind(latitude, longitude) ~ site + city, d, median)
s <- s[order(s$city, -s$latitude, s$longitude), ]
s$siteID <- paste0(abbr[s$city], "-", ave(seq_len(nrow(s)), s$city, FUN = seq_along))
d$siteID <- s$siteID[match(d$site, s$site)]

# ---- split sessions into nights -----------------------------------------------
# Some sessions run over several nights, and a few have implausible end times
# (e.g. 3 months). Truncate each session at the next session of the same camera,
# then split it into nights. Hours are counted only inside a fixed night window
# (15:00-08:00 UTC, which spans sunset to sunrise in Germany year-round).
d <- d[order(d$cameraID, d$start), ]
nxt <- ave(as.numeric(d$start), d$cameraID, FUN = function(z) c(z[-1], Inf))
d$end <- pmax(pmin(d$end, as.POSIXct(nxt, origin = "1970-01-01", tz = "UTC")), d$start)
d$n_nights_session <- as.integer(as.Date(d$end - 12 * 3600) - d$night) + 1

d <- do.call(rbind, lapply(seq_len(nrow(d)), function(k) {
  x <- d[rep(k, d$n_nights_session[k]), ]
  x$night <- x$night + seq_len(nrow(x)) - 1
  ws <- as.POSIXct(paste(x$night, "15:00:00"), tz = "UTC")
  x$start <- pmax(x$start, ws)
  x$end   <- pmax(pmin(x$end, ws + 17 * 3600), x$start)
  x
}))
d$year <- as.integer(format(d$night, "%Y"))

cat("Sessions spanning >1 night:", sum(!duplicated(d$deploymentID) & d$n_nights_session > 1),
    "; longest:", max(d$n_nights_session), "nights\n")

# ---- effort per site-night ----------------------------------------------------
union_hours <- function(st, en) {
  o <- order(st); st <- st[o]; en <- pmax(en[o], st)
  tot <- 0; cs <- st[1]; ce <- en[1]
  for (k in seq_along(st)[-1]) {
    if (st[k] <= ce) ce <- max(ce, en[k]) else { tot <- tot + as.numeric(ce - cs, units = "hours"); cs <- st[k]; ce <- en[k] }
  }
  tot + as.numeric(ce - cs, units = "hours")
}

sn <- do.call(rbind, lapply(split(d, paste(d$siteID, d$night)), function(x) data.frame(
  city = x$city[1], siteID = x$siteID[1], year = x$year[1], night = x$night[1],
  n_sessions = nrow(x),
  start = min(x$start), end = max(x$end),
  hours = union_hours(x$start, x$end),
  cameras = paste(sort(unique(x$cameraID)), collapse = ";"),
  locationIDs = paste(sort(unique(x$locationID)), collapse = ";"),
  deploymentIDs = paste(x$deploymentID, collapse = ";"))))
sn$sampled <- sn$hours >= min_hours
sn$zero_night <- FALSE

# ---- zero nights ------------------------------------------------------------------
# The export only contains images with detections, so a night without any
# detection has no session. A gap of up to max_zero_gap nights between two
# nights with sessions is taken as true zeros (trap ran, nothing came); longer
# gaps are taken as malfunction and not counted as effort.
max_zero_gap <- 1
zeros <- do.call(rbind, lapply(split(sn, paste(sn$siteID, sn$year)), function(x) {
  nights <- sort(x$night)
  gap <- as.integer(diff(nights)) - 1
  k <- which(gap >= 1 & gap <= max_zero_gap)
  if (!length(k)) return(NULL)
  zn <- do.call(c, lapply(k, function(i) nights[i] + seq_len(gap[i])))
  data.frame(city = x$city[1], siteID = x$siteID[1], year = x$year[1], night = zn,
             n_sessions = 0, start = NA, end = NA, hours = NA,
             cameras = NA, locationIDs = NA, deploymentIDs = NA,
             sampled = TRUE, zero_night = TRUE)
}))
sn <- rbind(sn, zeros)
sn <- sn[order(sn$city, sn$siteID, sn$night), ]
rownames(sn) <- NULL

# ---- summaries ----------------------------------------------------------------
site_tab <- do.call(rbind, lapply(split(sn, paste(sn$siteID, sn$year)), function(x) {
  ok <- x[x$sampled, ]
  span <- as.integer(max(ok$night) - min(ok$night)) + 1
  data.frame(city = x$city[1], siteID = x$siteID[1], year = x$year[1],
             first_night = min(ok$night), last_night = max(ok$night),
             span_nights = span, sampled_nights = nrow(ok),
             coverage = round(nrow(ok) / span, 2),
             zero_nights = sum(x$zero_night),
             short_nights = sum(!x$sampled),
             median_hours = round(median(ok$hours, na.rm = TRUE), 1),
             cameras = paste(na.omit(unique(unlist(strsplit(x$cameras, ";")))), collapse = ";"),
             locationIDs = paste(na.omit(unique(unlist(strsplit(x$locationIDs, ";")))), collapse = ";"))
}))
site_tab <- merge(site_tab, s[, c("siteID", "latitude", "longitude")])
site_tab <- site_tab[order(site_tab$city, site_tab$year, site_tab$siteID), ]

city_tab <- do.call(rbind, lapply(split(site_tab, paste(site_tab$city, site_tab$year)), function(x) data.frame(
  city = x$city[1], year = x$year[1], sites = nrow(x),
  first_night = min(x$first_night), last_night = max(x$last_night),
  sampled_nights = sum(x$sampled_nights), zero_nights = sum(x$zero_nights))))
city_tab <- city_tab[order(city_tab$city, city_tab$year), ]

write.csv(sn,       file.path(out_dir, "effort_site_night.csv"), row.names = FALSE)
write.csv(site_tab, file.path(out_dir, "effort_site_year.csv"),  row.names = FALSE)
write.csv(city_tab, file.path(out_dir, "effort_city_year.csv"),  row.names = FALSE)
