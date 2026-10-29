# Percentage sealed surface around each trap site: mean of the Copernicus
# HRL Imperviousness Density 2018 (10 m) within 100 m and 200 m buffers,
# as in the QGIS project "LEPMON sealed surface measurement.qgz".
# Requires output/effort_site_year.csv from 01_effort.R.

library(terra)

imd_file <- "C:/Dropbox/work/Courses/2025 GIS in R/copernicus sealse surface/Results/IMD_2018_010m_eu_03035_v020/IMD_2018_010m_eu_03035_v020/DATA/IMD_2018_010m_eu_03035_V2_0.tif"
out_dir  <- "output"

imd <- rast(imd_file)

s <- read.csv(file.path(out_dir, "effort_site_year.csv"))
s <- s[!duplicated(s$siteID), c("city", "siteID", "latitude", "longitude")]

pts <- project(vect(s, geom = c("longitude", "latitude"), crs = "EPSG:4326"), crs(imd))

sealing <- function(radius) {
  v <- extract(imd, buffer(pts, radius), fun = function(x) mean(x[x <= 100], na.rm = TRUE))
  round(v[, 2], 1)
}
s$sealed_100m <- sealing(100)
s$sealed_200m <- sealing(200)

s <- s[order(s$city, s$sealed_200m), ]
write.csv(s, file.path(out_dir, "site_sealing.csv"), row.names = FALSE)
print(s, row.names = FALSE)
