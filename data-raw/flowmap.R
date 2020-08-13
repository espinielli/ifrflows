library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

# from (adapted to include country ISO_A2) Muriel's query
# see first skip line in the file
# cp <- read_csv2("C:/Users/spi/Downloads/city-pairs.csv", skip = 73)
wef <- "2020-01-01"
til <- "2020-08-13"
filename <- str_glue("city-pairs_{wef}_{til}.csv.gz")

if (FALSE) {
  cp <- retrieve_daily_airport_flows(wef, til) %>%
    as_tibble() %>%
    mutate(ENTRY_DAY = as_date(ENTRY_DAY)) %>%
    janitor::clean_names()
  cp %>% write_csv(here::here("data-raw", filename), na = "")
} else {
  cp <- read_csv(here::here("data-raw", filename), na = c(""))
}

# cp %>% filter(entry_day > "2020-07-15") -> cp


country_pair <- cp %>%
  mutate(country_pair = paste0(adep_country_iso_name, ">", ades_country_iso_name)) %>%
  group_by(entry_day, country_pair) %>%
  summarize(
    count = sum(flight),
    source = adep_country_iso_name %>% first() %>% str_to_title(),
    target = ades_country_iso_name %>% first() %>% str_to_title(),
    source_iso2c = adep_country_iso_code %>% first(),
    target_iso2c = ades_country_iso_code %>% first()
  ) %>%
  arrange(desc(count)) %>%
  # remove BOGUS
  filter(!(source_iso2c %in% c("##", "AN", "XF", "YT")) |
           !(target_iso2c %in% c("##", "AN", "XF", "YT")))

countries <- country_pair %>%
  ungroup() %>%
  select(entry_day, source_iso2c, target_iso2c) %>%
  pivot_longer(-entry_day, names_to = "col", values_to = "iso_a2", values_ptypes = list(iso_a2 = character())) %>%
  select(iso_a2) %>%
  distinct() %>%
  filter(!is.na(iso_a2))

# country centroids from https://worldmap.harvard.edu/data/geonode:country_centroids_az8
filename <- "country_centroids_az8.csv"
file <- here::here("data-raw", filename)
fs::file_exists(file)

# avoid to interpret "NA" as NA: it is iso_a2 for Namibia
centroids <- read_csv(file, na = c("")) %>%
  select(name, iso_a3, iso_a2, Longitude, Latitude) %>%
  janitor::clean_names()

# # extra centroids
# extra_centroids <- tribble(
#   ~name,     ~iso_a3, ~iso_a2, ~latitude, ~longitude,
#   "Barbados", "BRB", "BB", 13.17, -59.5525,
#   "Uruguay", "URY", "UY", -32.6005596,-58.0283107,
#   "Philippines", "PHL", "PH", 13, 122,
#   "Saint Lucia", "LCA", "LC", 13.883333, -60.966667,
#   "Guinea", "GIN", "GN", 11, -10,
#   "Puerto Rico", "PRI", "PR",18.2, -66.5,
#   "Rwanda", "RWA", "RW", -1.95, 29.866667
# )
#
# centroids <- centroids %>%
#   bind_rows(extra_centroids)

d <- countries %>%
  left_join(centroids, by = c("iso_a2" = "iso_a2")) %>%
  mutate(
    # Canary Islands
    latitude  = ifelse(iso_a2 == "ES-CN",  28.4398708, latitude),
    longitude = ifelse(iso_a2 == "ES-CN", -16.9743268, longitude),
    # France (avoid French Guinea effect)
    latitude  = ifelse(iso_a2 == "FR",     47.5, latitude),
    longitude = ifelse(iso_a2 == "FR",      3.2, longitude),
    # Norway: way too North
    latitude  = ifelse(iso_a2 == "NO",     60.980820, latitude),
    longitude = ifelse(iso_a2 == "NO",      8.855597, longitude),
    # Serbia and Montenegro
    latitude  = ifelse(iso_a2 == "CS",     44.2215031993, latitude),
    longitude = ifelse(iso_a2 == "CS",     20.7895833363, longitude),
    # Gibraltar
    latitude  = ifelse(iso_a2 == "GI",     36.131667, latitude),
    longitude = ifelse(iso_a2 == "GI",     -5.351667, longitude),
    # Reunion
    latitude  = ifelse(iso_a2 == "RE",    -21.114444, latitude),
    longitude = ifelse(iso_a2 == "RE",     55.5325, longitude),
    # Slovenia
    latitude  = ifelse(iso_a2 == "SI",     46.1155477207, latitude),
    longitude = ifelse(iso_a2 == "SI",     14.8044423776, longitude),
    # USA center it
    latitude  = ifelse(iso_a2 == "US",     38.091737, latitude),
    longitude = ifelse(iso_a2 == "US",     -77.118292, longitude),
    NULL
  ) %>%
  # exclude BOGUS
  filter(!(iso_a2 %in% c("##", "AN", "XF", "YT")))

# arrange for flowmap.blue/in-browser
# locations
locations <- d %>%
  mutate(id = row_number(), lat = latitude, lon = longitude) %>%
  select(id, name, iso_a2, lat, lon) %>%
  mutate(
    name = ifelse(iso_a2 == "CS", "Serbia and Montenegro", name),
    name = ifelse(iso_a2 == "GF", "French Guiana", name),
    # fix French Guiana
    lat  = ifelse(iso_a2 == "GF", 4.100192, lat),
    lon  = ifelse(iso_a2 == "GF", -53.165735, lon),
    # ---
    name = ifelse(iso_a2 == "GI", "Gibraltar", name),
    name = ifelse(iso_a2 == "RE", "Réunion", name),
    # fix Svalbard et al.
    name = ifelse(iso_a2 == "SJ", "Svalbard and Jan Mayen", name),
    lat  = ifelse(iso_a2 == "SJ", 75.5300298, lat),
    lon  = ifelse(iso_a2 == "SJ", 3.2220993, lon),
    # ---
    # fix
    NULL
    )

# flows
flows <- country_pair %>%
  # select(source_iso2c, target_iso2c, count) %>%
  left_join(locations, by = c("source_iso2c" = "iso_a2")) %>%
  rename(origin = id) %>%
  left_join(locations, by = c("target_iso2c" = "iso_a2")) %>%
  rename(dest = id) %>%
  select(origin, dest, count) %>%
  # filter NAs
  filter(!(is.na(origin) | is.na(dest)))


library(googlesheets4)
sheet_id <- gs4_find("daily_country_flows")

locations %>%
  select(-iso_a2) %>%
  sheet_write(sheet_id, sheet = "locations")

threshold <- 1

flows %>%
  # filter on relevant counts
  filter(count >= threshold) %>%
  mutate(sheet_name = entry_day %>% format("%Y-%m-%d")) %>%
  group_walk(~sheet_write(.x %>% select(-sheet_name), sheet_id, sheet = first(.$sheet_name)))

# sheets
flows %>%
  # filter on relevant counts
  filter(count >= threshold) %>%
  mutate(sheet_name = entry_day) %>%
  distinct(entry_day, sheet_name) %>%
  arrange(desc(entry_day)) %>%
  pull(sheet_name) %>% paste(collapse = ",") %>%
  as_tibble() %>%
  range_write(data = ., sheet_id, sheet = "properties", range = "B15:B15", col_names = FALSE)



########################################
#-----Single timeline
sheet_id <- gs4_find("daily_country_flows_timeline")

threshold <- 1

locations %>%
  select(-iso_a2) %>%
  sheet_write(sheet_id, sheet = "locations")


flows %>%
  ungroup() %>%
  # filter on relevant counts
  filter(count >= threshold) %>%
  rename(time = entry_day) %>%
  select(origin, dest, count, time) %>%
  arrange(time, origin, dest, count) %>%
  # distinct(time, origin, dest) %>%
  mutate(time = format(time, "%Y-%m-%d")) %>%
  sheet_write(sheet_id, sheet = "flows")

my_properties <- c(
  "title"                        = "Daily country flight flows",
  "description"                  = "Daily flight flows from/to countries in the EUROCONTROL area",
  "source.name"                  = "EUROCONTROL",
  "source.url"                   = "https://eurocontrol.int",
  "createdBy.name"               = "Aviation Intelligence Unit",
  "createdBy.email"              = "PRU-Support@eurocontrol.int",
  "createdBy.url"                = "https://ansperformance.eu",
  "mapbox.accessToken"           = NA,
  "mapbox.mapStyle"              = NA,
  "map.bbox"                     = NA,  # west, south, east, north
  "colors.scheme"                = "Default",
  "colors.darkMode"              = "no",
  "animate.flows"                = "no",
  "clustering"                   = "yes",
  "flows.sheets"                 = "flows",
  "msg.locationTooltip.incoming" = "Incoming flights",
  "msg.locationTooltip.outgoing" = "Outgoing flights",
  "msg.locationTooltip.internal" = "Internal flights",
  "msg.flowTooltip.numOfTrips"   = "Number of flights",
  "msg.totalCount.allTrips"      = "{0} flights",
  "msg.totalCount.countOfTrips"  = "{0} of {1} flights"
  )

tibble(property=names(my_properties)) %>%
  mutate(value=my_properties[property]) %>%
  write_sheet(sheet_id,"properties")

# # upload flows.csv and locations.csv in a (secret or public) GitHub gist
# # then get the location of the files:
# flows_file <- "https://gist.githubusercontent.com/espinielli/8581282046fb0cf364a1874bd1429801/raw/8184275c555f6d789817f5a159208d1037c59855/flows.csv"
# locations_file <- "https://gist.githubusercontent.com/espinielli/8581282046fb0cf364a1874bd1429801/raw/695eb88e6884d9ab88b35933391681685f0eb6c3/locations.csv"
#
# # the URL where to see the flowmap.blue
# str_glue("http://flowmap.blue/from-url?flows={flows_file}&&locations={locations_file}")
