### MERGE SHAPEFILES
library(rgdal)
library(geojsonio)
library(rmapshaper)
library(sp)
library(sf)
library(spdplyr)
library(yaml)
library(stringi)

# lookups
lookup_iso_name <- readr::read_csv("lookup_iso_name.csv")
lookup_iso2_iso3 <- readr::read_csv("lookup_iso2_iso3.csv")
centroids <- readr::read_csv("world_country_centroids_clean.csv")

# define directories
dir_latam <- "data/Calil et al. (2017)LatinAmerica/LatinAmerica.shp"
dir_caribe <- "data/Calil et al. (2017)Caribbean_Dissolve/Caribbean_Dissolve.shp"
dir_caribe2 <- "data/caribis/caribis.shp"

# load in shapefiles and format data
map_caribe <- sf::read_sf(dir_caribe2, options = "ENCODING=UTF-8") %>% select(id = FIRST_FIPS, name = CNTRY_NAME) %>% 
  mutate(name = recode(name, `Bahamas, The` = "Bahamas"))
map_df_caribe <- map_caribe
st_geometry(map_df_caribe) <- NULL

map_latam <- sf::read_sf(dir_latam, options = "ENCODING=UTF-8") %>% select(id = ISO_2DIGIT, name = CNTRY_NAME)
map_df_latam <- map_latam
st_geometry(map_df_latam) <- NULL

map_temp <- rbind(map_latam, map_caribe)
map_temp_df <- map_temp
st_geometry(map_temp_df) <- NULL

map_with_correct_codes <- map_df %>% 
  left_join(lookup_iso_name) %>% 
  mutate(iso2 = ifelse(name == "Netherlands Antilles", "AN", as.character(iso2)),
         iso3 = ifelse(name == "Netherlands Antilles", "ANT", as.character(iso3))) %>% 
  select(iso2, iso3, name) %>% 
  left_join(centroids)


map <- rbind(map_latam, map_caribe) %>% 
  left_join(map_with_correct_codes) %>% 
  select(id = iso2, name, lat, lon)
map_df <- map
st_geometry(map_df) <- NULL  


###-------------------------------------------------------------
### define 'inst' dir of geodata package
mainDir <- "C:/Users/lena.mangold/Documents/Datasketch/repos/geodata/inst"

# create basename (appears as foldername)
adm0_a3 <- "latamcaribbean"

# get country name
country_name <- "Latam and Caribbean"

# identify adm1_type. Use 'countries' if merging together countries
adm1_type <- "countries"

# create parameters for .yaml file
adm1_name <- tolower(paste0(adm0_a3, "_", adm1_type))
label <- paste0(country_name, " ",adm1_type)
basename <- tolower(paste0(adm0_a3, "-", adm1_type))
scope <- label
geographyName <- "name"
geographyId <- "id"
geoname <- tolower(adm0_a3)

# create list for .yaml file
adm_list <- list("label" = label,
                 "basename" = basename,
                 "scope" = scope,
                 "geographyName" = geographyName,
                 "geographyId" = geographyId)
cn_list <- list()
cn_list[[adm1_name]] <- adm_list

# create CSV
df_geodata <- map_df %>%
  select(id, name, lat, lon)

# convert to topojson
map <- map %>% select(id, name)
map_json <- topojson_json(map, geometry = "polygon", group = "group", encoding="UTF-8")

# prepare directories and filenames
directory_yaml <- file.path(mainDir, "meta")
directory_csv_json <- file.path(mainDir, paste0("geodata/",paste0(geoname)))
filename_yaml <- paste0(geoname,".yaml")
filename_csv <- paste0(basename,".csv")
filename_json <- paste0(basename,".topojson")

## write yaml file (if it doesn't exist)
setwd(directory_yaml)
if(!file.exists(filename_yaml)){write_yaml(cn_list, file = filename_yaml)}

## write csv and topojson files (if they don't exist)
if(!dir.exists(directory_csv_json)){
  dir.create(directory_csv_json, showWarnings = FALSE)
  setwd(directory_csv_json)
  geojson_write(map_json, file = filename_json)
  readr::write_csv(df_geodata, file = filename_csv)
}
