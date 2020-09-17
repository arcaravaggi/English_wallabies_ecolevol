setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sp)
library(rgdal)
library(rgeos)
library(sf)
library(png)
library(tmap)
library(spatstat)
library(raster)

wal.dat <- read.csv("../data_raw/wallaby_modern_historic_merged.csv")

# Records by year across modern data excluding escapees
wal.dat2 <- subset(wal.dat, type == "modern")
wal.dat2$source.type <- as.character(wal.dat2$source.type) # Set source.type as character

# Count per source, per year
wal.yr <- wal.dat2 %>% group_by(year, source.type) %>% tally() %>% ungroup() # Count by year
wal.yr <- wal.yr[-c(22), ] # Delete 2019

# Add rows for media in 2008 and records in 2015
med <- c("2008", "media", "0")
rec <- c("2015", "records", "0")
wal.yr <- rbind(wal.yr, med)
wal.yr <- rbind(wal.yr, rec)

wal.yr$year <- as.factor(wal.yr$year)
wal.yr$n <- as.numeric(wal.yr$n)

source('theme_ac1.R')

p.yr <- ggplot(wal.yr, aes(year, n, fill =  source.type)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.75) +
  theme_ac1(base_size_a = 20, base_size_t = 20) +
  labs(x = "Year", y = "Number of records") +
  scale_y_continuous(expand = c(0,0), limits = c(0,15)) + 
  scale_x_discrete(breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017,2018)) +
  scale_fill_manual("Legend", values = c("media" = "black", "records" = "grey")) +
  theme(legend.position = "none")


#ggsave("../figures/20200917_counts_per_year.png", p.yr, height = 6, width = 8, dpi = 600)

# Records by month

wal.mt <- wal.dat %>% filter (type == "modern") %>% group_by(month) %>% tally() # Count by month
wal.mt <- wal.mt[-c(1:2), ] # Delete NA
wal.mt$month <- factor(wal.mt$month)

month.abb[wal.mt$month]


p.mt <- ggplot(wal.mt, aes(month, n)) +
  geom_bar(stat = "identity") +
  theme_ac1(base_size_a =20, base_size_t = 20) +
  labs(x = "Month", y = "Number of records") +
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels = month.abb[wal.mt$month],
                   limits = c(1,2,3,4,5,6,7,8,9,10,11,12))

#ggsave("../figures/20200917_counts_per_month.png", p.mt, height = 6, width = 8, dpi = 600)


# Country shapefiles
wal <- ne_countries(geounit = 'wales', type = "map_units", scale = 10, returnclass = "sp")
sco <- ne_countries(geounit = 'scotland', type = "map_units", scale = 10, returnclass = "sp")
eng <- ne_countries(geounit = 'england', type = "map_units", scale = 10, returnclass = "sp")

uk <- list(wal,sco,eng)

joined = SpatialPolygons(lapply(uk, function(x){x@polygons[[1]]})) # Join polygons
plot(joined)

# Create data for dissolving polygons
# Extract polygon ID's
pid <- sapply(slot(joined, "polygons"), function(x) slot(x, "ID")) 
# Create dataframe with correct rownames
p.df <- data.frame( ID=1:length(joined), row.names = pid)   
# Coerce to SPDF
p <- SpatialPolygonsDataFrame(joined, p.df)
# Add dissolve column
p@data$m <- 1

uk <- gUnaryUnion(p, id = p@data$m) #dissolve
plot(uk)
proj4string(uk) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")

wal.pt <- wal.dat
wal.pt$source.type[wal.pt$source.type %in% c("lerc", "nbn", "yalden") ] <- "records"
wal.pt$source.type[wal.pt$source.type %in% c("media", "social_media", "youtube")] <- "media"

# Function to conditionally delete rows containing NA in given columns
#
# data = data frame
# diredCols = column names
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

wal.pt <- completeFun(wal.pt, "longitude")

# Plot all points with symbols for each type
coordinates(wal.pt) <- ~longitude+latitude
proj4string(wal.pt) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")

# Read Peak District & Chilterns shapefiles
chilterns <- readOGR("../shapefiles", "chilterns_aonb")
peaks <- readOGR("../shapefiles", "peak_district_np")

# Build maps
map_uk <- tm_shape(uk) + tm_polygons()

map_uk1 <- tm_shape(uk) +
  tm_borders() +
  tm_shape(peaks) + 
  tm_fill(col = "red") +
  tm_shape(chilterns) + 
  tm_fill(col = "red") +
  tm_shape(subset(wal.pt, type == "historic")) + 
  tm_dots("indiv.count", size = .15, shape = 21, col = "black") 

map_uk2 <- tm_shape(uk) +
  tm_borders() +
  tm_shape(peaks) + 
  tm_fill(col = "red") +
  tm_shape(chilterns) + 
  tm_fill(col = "red") +
  tm_shape(subset(wal.pt, type %in% c("modern", "escapee"))) + 
  tm_dots("indiv.count", size = .15, shape = 21, col = "black") 

tmap_save(map_uk1, "../figures/20200917_wallabies_historic.png", height = 6, width = 4, dpi = 300)
tmap_save(map_uk2, "../figures/20200917_wallabies_modern.png", height = 6, width = 4, dpi = 300)

# Hotspot

# Duplicate rows according to count values, preserving coordinate columns.
wal.all <- wal.dat2[rep(row.names(wal.dat2), wal.dat2$indiv.count), 3:4]

# Create point pattern dataset
wal.ppd <- ppp(wal.all$longitude, wal.all$latitude, 
               window = owin(c(-13.7, 1.8), c(49.9, 60.9)))

# Create density surface
wal.dens <- density(wal.ppd)
plot(wal.dens, main = "Density plot of sample dataset")

# Convert to a raster and apply thresholds for contextual visualisation
wal.dens_r <- raster(wal.dens)

# Rescale cell values between 0 and 1
rasterRescale<-function(r){
  ((r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min")))
}

wal.dens_r2 <- rasterRescale(wal.dens_r)

kde_25 <- wal.dens_r2
crs(kde_25) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
kde_50 <- wal.dens_r2
crs(kde_50) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
kde_75 <- wal.dens_r2
crs(kde_75) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "

kde_25[kde_25<0.75]=NA
kde_25[kde_25>=0.75]=1
kde_50[kde_50<0.50]=NA
kde_50[kde_50>=0.50]=1
kde_75[kde_75<0.25]=NA
kde_75[kde_75>=0.25]=1

kde_75 = mask(kde_75,uk)
kde_50 = mask(kde_50,uk)
kde_25 = mask(kde_25,uk)

# Build raster map
map_uk3 <-
  tm_shape(kde_75) +
  tm_raster(palette = "lightgrey") +
  tm_shape(kde_50) +
  tm_raster(palette = "grey") +
  tm_shape(kde_25) +
  tm_raster(palette = "darkgrey") +
  tm_shape(uk) +
  tm_borders() +
  tm_shape(peaks) + 
  tm_fill(col = "red") +
  tm_shape(chilterns) + 
  tm_fill(col = "red") 

tmap_save(map_uk3, "../figures/20200917_wallabies_hotspot.png", height = 6, width = 4, dpi = 300)
