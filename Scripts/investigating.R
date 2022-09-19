library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(maps)
library(maptools)
library(sp)
library(raster)
library(R.utils)
library(rgdal)

# Reading in attack data
Attack_df <- read.csv('./Data/shark_attack_df.csv')

# investigating data
dim(Attack_df)
colnames(Attack_df)
head(Attack_df$longitude)
length(which(is.na(Attack_df$longitude)))

# creating spatial points
attacks <- Attack_df[, 1:35]
attacks <- attacks %>% filter(!(is.na(ISAF.case_no)))
which(is.na(attacks$latitude))
attacks_sp1 <- attacks %>% dplyr::select(ISAF.case_no, longitude, latitude)
attacks_sp2 <- attacks %>% dplyr::select(ISAF.2nd.case_no, X2nd.longitude,
                                         X2nd.latitude)
attacks_sp3 <- attacks %>% dplyr::select(X3rd.case_no, X3rd.longitude,
                                         X3rd.latitude)
attacks_sp4 <- attacks %>% dplyr::select(ISAF.4th.case_no, X4th.longitude,
                                         X4th.latitude)
attacks_sp2 <- attacks_sp2 %>% filter(!(is.na(X2nd.longitude)))
attacks_sp3 <- attacks_sp3 %>% filter(!(is.na(X3rd.longitude)))
attacks_sp4 <- attacks_sp4 %>% filter(!(is.na(X4th.longitude)))

coordinates(attacks_sp1) <- ~longitude+latitude
coordinates(attacks_sp2) <- ~X2nd.longitude+X2nd.latitude
coordinates(attacks_sp3) <- ~X3rd.longitude+X3rd.latitude
coordinates(attacks_sp4) <- ~X4th.longitude+X4th.latitude

wanted_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(attacks_sp1) <- wanted_crs
proj4string(attacks_sp2) <- wanted_crs
proj4string(attacks_sp3) <- wanted_crs
proj4string(attacks_sp4) <- wanted_crs

# plotting
plot(attacks_sp1, col = "red")
plot(attacks_sp2, col = "blue", add = T)
plot(attacks_sp3, col = "green", add = T)
plot(attacks_sp4, col = "purple", add = T)
sp::plot(ne_countries(), add = T)

# more investigating
head(attacks)
tail(attacks)

#gunzip(filename = './Data/woa18_8594_t04mn01.csv.gz')
test_temp <- read.csv('./Data/woa18_8594_t04mn01.csv')

#untar('./Data/woa18_8594_t04mn01_shape.tar.gz', exdir = './Data')

# creating a map of the area
t <- map('county', 'florida,volusia', fill = T)
IDs <- sapply(strsplit(t$names, ":"), function(x) x[1])
volusia <- map2SpatialPolygons(t, IDs = IDs, proj4string = CRS(wanted_crs))
plot(volusia)
plot(attacks_sp1, col = "red", add = T)
plot(temp_shape, add = T)
extent(volusia)

# creating raster of environment
temp_shape <- shapefile('./Data/woa18_8594_t04mn01.shp')
temp_shape
proj4string(temp_shape) <- wanted_crs
plot(temp_shape)

world <- map('world', fill = T, plot = F)
IDs <- sapply(strsplit(world$names, ":"), "[", 1L)
cea_crs <- "+proj=cea +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
world_sp <- map2SpatialPolygons(world, IDs = world$names, 
                                proj4string = CRS(cea_crs))
plot(world_sp)

# writing shapefiles
writeOGR(obj <- attacks_sp1, dsn = './Data', layer = 'attacks_sp1',
         driver = 'ESRI Shapefile')
