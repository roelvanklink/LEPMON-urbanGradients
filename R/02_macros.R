# Macro-moth abundance and species richness per site-night.
#
# Taxonomy: the Camtrap DP observations carry no higher taxonomy, and the
# export's species_list.csv is incomplete. Family is taken from the
# `higher_taxa` column of the August 2026 per-city exports, then from
# species_list.csv, then by genus. Requires output/effort_site_night.csv
# from 01_effort.R.

library(data.table)

data_dir <- "C:/Dropbox/work/2024 LEPMON/Urban gradients/Urban gradient data 2026-10-27"
old_dir  <- "C:/Dropbox/work/2024 LEPMON/Urban gradients/Urban gradient data 2026-08-26/urban gradient"
out_dir  <- "output"

min_prob <- 0.7   # machine classifications below this are dropped (as in first-run script)

macros <- c("Noctuidae", "Erebidae", "Notodontidae", "Nolidae", "Geometridae",
            "Uraniidae", "Saturniidae", "Sphingidae", "Bombycidae", "Endromididae",
            "Drepanidae", "Lasiocampidae", "Mimallonidae",
            "Hepialidae", "Cossidae", "Limacodidae", "Sesiidae", "Zygaenidae")

# ---- taxonomy -------------------------------------------------------------------
tax <- unique(rbindlist(lapply(list.files(old_dir, "csv$", full.names = TRUE), function(f)
  unique(fread(f, select = c("scientificName", "higher_taxa"))))))
tax <- tax[higher_taxa != "", .(higher_taxa = higher_taxa[1]), by = scientificName]

o <- fread(file.path(data_dir, "observations.csv"),
           select = c("observationID", "deploymentID", "mediaID", "eventStart", "observationType",
                      "scientificName", "count", "classificationMethod", "classificationProbability"))
o <- o[observationType == "animal"]

nm <- data.table(scientificName = unique(o$scientificName))
nm <- merge(nm, tax, all.x = TRUE)
nm[, path := paste(scientificName, higher_taxa, sep = ">")]
nm[, family := sapply(strsplit(path, ">"), function(z) {
  f <- grep("idae$", z, value = TRUE); if (length(f)) f[1] else NA_character_ })]

sp <- fread(file.path(data_dir, "species_list.csv"))
i <- is.na(nm$family)
nm$family[i] <- sp$family[match(nm$scientificName[i], sp$species_name)]

# genus fallback: family of other names with the same first word
nm[, genus := sub(" .*", "", scientificName)]
gfam <- nm[!is.na(family) & !grepl("idae$", scientificName), .(gf = family[1]), by = genus]
i <- is.na(nm$family)
nm$family[i] <- gfam$gf[match(nm$genus[i], gfam$genus)]

nm[, macro := family %in% macros]
nm[, species_level := grepl("^[A-Z][a-z]+ [a-z-]+$", scientificName)]

unresolved <- merge(nm[is.na(family)], o[, .(n = sum(count)), by = scientificName])[order(-n)]
cat("Names without family:", nrow(unresolved), "(", sum(unresolved$n), "individuals; incl. 'moth'/'non-moth')\n")

# ---- macro observations ---------------------------------------------------------
m <- merge(o, nm[macro == TRUE, .(scientificName, family, species_level)], by = "scientificName")
n_before <- sum(m$count)
m <- m[classificationMethod == "human" | is.na(classificationProbability) | classificationProbability >= min_prob]
cat("Macro individuals:", n_before, "; after probability filter:", sum(m$count), "\n")

# ---- assign to site-nights --------------------------------------------------------
# deploymentIDs in the effort table can belong to several nights (multi-night
# sessions), so assign each observation by its own timestamp
eff <- fread(file.path(out_dir, "effort_site_night.csv"))
dep_site <- unique(eff[!is.na(deploymentIDs), .(deploymentID = unlist(strsplit(deploymentIDs, ";"))), by = siteID])
m <- merge(m, dep_site, by = "deploymentID")      # drops excluded deployments
m[, night := as.Date(as.POSIXct(eventStart, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") - 12 * 3600)]

per_night <- m[, .(abundance = sum(count),
                   richness = uniqueN(scientificName[species_level])), by = .(siteID, night)]
eff[, night := as.Date(night)]
sn <- merge(eff[sampled == TRUE, .(city, siteID, year, night, hours, zero_night)],
            per_night, by = c("siteID", "night"), all.x = TRUE)
sn[is.na(abundance), `:=`(abundance = 0L, richness = 0L)]

cat("Macro individuals on sampled nights:", sum(sn$abundance), "of", sum(m$count), "\n")

fwrite(m[, .(observationID, siteID, night, scientificName, family, species_level, count,
             classificationMethod, classificationProbability)], file.path(out_dir, "macro_observations.csv"))
fwrite(sn, file.path(out_dir, "macro_site_night.csv"))
fwrite(unresolved[, .(scientificName, n)], file.path(out_dir, "taxa_without_family.csv"))
