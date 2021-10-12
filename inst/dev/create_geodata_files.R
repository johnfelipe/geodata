###-------------------------------------------------------------
### load packages
library(rgdal)
library(geojsonio)
library(rmapshaper)
library(sp)
library(sf)
library(spdplyr)
library(yaml)
library(stringi)

###-------------------------------------------------------------
### load shapefile (need to download ne_10m_admin_1_states_provinces.zip from natural earth)
shapefile_name <- "ne_10m_admin_1_states_provinces"
rootdir <- "data/"
indir <- paste0(rootdir, "ne_10m_admin_1_states_provinces/") # Defining the directory that contains the shapefile

map <- sf::read_sf(paste0(indir, shapefile_name,".shp"), options = "ENCODING=UTF-8")
map_df <- map
st_geometry(map_df) <- NULL


###-------------------------------------------------------------
### get list of countries with codes
countries <- map_df %>%
  filter(iso_a2 != "-1") %>%
  distinct(iso_a2, adm0_a3) %>%
  arrange(iso_a2) %>%
  filter(!(iso_a2 == "AU" & adm0_a3 == "ATC"))


###-------------------------------------------------------------
### setup to track which countries are being saved
# names_countries_with_div <- c("iso_a2", "adm0_a3", "geoname", "basename")
# countries_with_div <- matrix(ncol=length(names_countries_with_div), nrow=nrow(countries))


###-------------------------------------------------------------
### define 'inst' dir of geodata package
mainDir <- "C:/Users/lena.mangold/Documents/Datasketch/repos/geodata/inst"


###-------------------------------------------------------------
### loop through countries
for(i in 1:nrow(countries)){

cn <- as.character(countries$iso_a2[i])

# subset map
map_i <- map %>%
  filter(iso_a2 == cn)

# subset map dataframe
map_df_i <- map_df %>%
  filter(iso_a2 == cn)

# get adm0 code
adm0_a3 <- countries$adm0_a3[i]

# get country name
country_name <- as.character(map_df_i$admin[1])

# print processing message
print(paste0("Processing files for: ",country_name,". Country ", i, "/", nrow(countries)))

# identify adm1_type. Use 'division' if adm1_type is none or not unique
adm1_type <- map_df_i %>% distinct(type_en) %>% pull()
if(length(adm1_type) > 1){adm1_type = "division"} else if(is.na(adm1_type)){adm1_type = "division"}
adm1_type <- gsub("[^[:alnum:]]", "", tolower(adm1_type))

# create parameters for .yaml file
adm1_name <- tolower(paste0(adm0_a3, "_", adm1_type))
label <- paste0(country_name, " ",adm1_type)
basename <- tolower(paste0(adm0_a3, "-", adm1_type))
scope <- paste0(adm0_a3, "_adm1")
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
df_complete <- map_df_i %>%
  select(lat = latitude, lon = longitude, starts_with("name_"), gn_name, adm1_code, type_en) %>%
  select(-name_alt, -name_local, -name_len) %>%
  mutate(id = 1:nrow(map_df_i))

df_geodata <- df_complete %>%
  mutate(name = gn_name,
         name = tidyr::replace_na(name, "unknown")
         ) %>%
  select(id, name, lat, lon)

# create CSV for shi18ny with translations of division names
# df_shi18ny <- df_complete %>%
#   select(id, adm1_code, gn_name, starts_with("name_"))

# convert to topojson
map_i <- map_i %>% select(id = adm1_code, name = gn_name)
map_i_json <- topojson_json(map_i, geometry = "polygon", group = "group", encoding="UTF-8")

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
  # countries_with_div[i,] <- c(cn, adm0_a3, geoname, basename)
  dir.create(directory_csv_json, showWarnings = FALSE)
  setwd(directory_csv_json)
  geojson_write(map_i_json, file = filename_json)
  readr::write_csv(df_geodata, file = filename_csv)
  }

## write csv file for shi18ny
# write.csv(df_shi18ny, paste0("C:/Users/Lena/Documents/Work/Datasketch/scratch_shi18ny/adm1_divisions_translations/countries/",tolower(adm0_a3),".csv"))

}




# countries_with_div <- data.frame(countries_with_div)
# names(countries_with_div) <- names_countries_with_div
# countries_save <- countries_with_div %>% filter(complete.cases(.))
# write.csv(countries_save, paste0(rootdir, "create_topojson_files/iso2_iso3_lookup.csv"), row.names = F)

