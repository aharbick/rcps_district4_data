# Make sure you install packages
setup <- function() {
  # Required packages for the code below
  required_packages <- c(
    "sqldf",
    "stringr",
    "gsheet",
    "reticulate",
    "devtools",
    "ggplot2",
    "osmdata",
    "maps",
    "ggrepel",
    "showtext",
    "rvest",
    "ggmap",
    "rjson"
  )
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if (length(missing_packages)) install.packages(missing_packages)
  invisible(lapply(required_packages, require, character.only = TRUE))
}
setup()

config <- fromJSON(file = "~/Sites/rcps_district4_data/.config.json")
pwggmap::register_google(config$GOOGLE_API_KEY)

###########################################################################
#  For using Python's code: https://rstudio.github.io/reticulate/index.html
#  (e.g. matplotlib)
# install.packages("reticulate")
#
#  If you don't have conda installed at all just answer Y when it asks about Miniconda.  Otherwise do this first
# use_condaenv("your_conda_env")
# py_install(packages = "matplotlib")
# plt <- import("matplotlib.pyplot",as="plt")
###########################################################################

# Be able to print large sets
options(max.print=25000)

# Load the election data from our Google Sheets
registered_voters <- gsheet2tbl(config$REGISTERED_VOTERS)
voted_2022 <- gsheet2tbl(config$VOTERS_2022)

# Helper functions
print_voters <- function(voters) {
  print(voters[,c("LAST_NAME", "FIRST_NAME", "ADDRESS_LINE_1", "CITY", "STATE", "ZIP", "EFFECTIVE_DATE", "DOB", "GENDER")])
}

plot_by_decade <- function(result, title) {
  birth_decade = result$birth_decade
  num_voters = result$COUNT
  df <- data.frame(birth_decade=birth_decade, num_voters=num_voters)
  print(ggplot(df) + geom_bar(aes(x=birth_decade, y=num_voters), stat="identity") + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)))
}

plot_pct_by_decade <- function(registered, voted) {
  birth_decade = registered$birth_decade
  pct_voted = voted$COUNT / registered$COUNT
  df <- data.frame(birth_decade=birth_decade, pct_voted)
  print(ggplot(df) +
          geom_bar(aes(x=birth_decade, y=pct_voted), stat="identity") +
          ggtitle("Pct. Voting by Birth Decade") +
          theme(plot.title = element_text(hjust = 0.5)) +
          ylim(c(0,1.0))
        )
}

plot_addresses_on_map <- function(voter_df) {
  addresses = sqldf("SELECT FIRST_NAME||' '||LAST_NAME AS name, MAILING_ADDRESS_LINE_1||', '||MAILING_CITY||', '||MAILING_STATE AS addr FROM voter_df")
  lat_longs <- addresses %>% geocode(addr, method = 'osm', lat = latitude , long = longitude)
  
  ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
    borders("state") + geom_point() +
    ggrepel::geom_label_repel(aes(label = name)) +
    theme_void()
}

# This function and other mapping stuff heavily borrowed from: http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
get_basemap <- function() {
  font_add_google(name = "Lato", family = "lato") # add custom fonts
  showtext_auto()
  
  bb <- getbb("Rockingham County, Virginia")
  river <- bb %>% opq() %>% add_osm_feature(key = "waterway", value = "river") %>% osmdata_sf()
  railway <- bb %>% opq() %>% add_osm_feature(key = "railway", value="rail") %>% osmdata_sf()
  big_streets <- bb %>% opq() %>% add_osm_feature(key = "highway", value = c("motorway", "primary", "motorway_link", "primary_link")) %>% osmdata_sf()
  med_streets <- bb %>% opq() %>% add_osm_feature(key = "highway", value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>% osmdata_sf()
  small_streets <- bb %>% opq() %>% add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) %>% osmdata_sf()
  return (ggplot() +
    geom_sf(data = river$osm_lines,
            inherit.aes = FALSE,
            color = "steelblue",
            size = .8,
            alpha = .6) +
    geom_sf(data = railway$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .2,
            linetype="dotdash",
            alpha = .5) +
    geom_sf(data = med_streets$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .3,
            alpha = .5) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#666666",
            size = .2,
            alpha = .3) +
    geom_sf(data = big_streets$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .8,
            alpha = .6) +
    coord_sf(xlim = c(-79.06232906341346, -78.92705644977025), 
             ylim = c(38.40629612339261, 38.3296421229381),
             expand = FALSE) +
    theme_void() + # get rid of background color, grid lines, etc.
    theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
          plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
    labs(title = "ROCKINGHAM COUNTY DISTRICT 4"))
}

# Summary Statistics
# By GENDER
print(sqldf("SELECT COUNT(*) AS COUNT, GENDER FROM voted_2022 GROUP BY GENDER"))

# By DECADE
voted_2022_by_birth_decade <- sqldf("SELECT COUNT(*) AS COUNT, SUBSTRING(DOB, -4, 3) || '0' AS birth_decade FROM voted_2022 GROUP BY birth_decade ORDER BY birth_decade")
registered_by_birth_decade <- sqldf("SELECT COUNT(*) AS COUNT, SUBSTRING(DOB, -4, 3) || '0' AS birth_decade FROM registered_voters GROUP BY birth_decade ORDER BY birth_decade")

# Probably a better way to trim off a couple outliers
voted_2022_by_birth_decade <- sqldf("SELECT * FROM voted_2022_by_birth_decade WHERE birth_decade BETWEEN 1920 AND 2000")
registered_by_birth_decade <- sqldf("SELECT * FROM registered_by_birth_decade WHERE birth_decade BETWEEN 1920 AND 2000")

plot_by_decade(registered_by_birth_decade, "Registered Voters by Birth Decade")
plot_by_decade(voted_2022_by_birth_decade, "2022 Voters by Birth Decade")

# Votes with Gender "U"
u_gender_voters = sqldf("SELECT * FROM voted_2022 WHERE GENDER = 'U'")
print_voters(u_gender_voters)

# Registered voters who didn't vote in 2022
registered_didnt_vote <- sqldf("SELECT * FROM registered_voters WHERE IDENTIFICATION_NUMBER NOT IN (SELECT IDENTIFICATION_NUMBER FROM voted_2022)")
recent_registered_didnt_vote <- sqldf("SELECT * From registered_didnt_vote WHERE EFFECTIVE_DATE > '2021-01-01'")
print_voters(recent_registered_didnt_vote)

# Voter count by precinct
num_registered_by_precinct <- sqldf("SELECT PRECINCTNAME, COUNT(*) AS COUNT FROM registered_voters GROUP BY PRECINCTNAME")
num_voted_by_precinct <- sqldf("SELECT PRECINCTNAME, COUNT(*) AS COUNT FROM voted_2022 GROUP BY PRECINCTNAME")
pct_voting_by_precinct <- data.frame(precinct=num_registered_by_precinct$PRECINCTNAME, pct_voting=(num_voted_by_precinct$COUNT/num_registered_by_precinct$COUNT)*100)
print(num_registered_by_precinct)
print(num_voted_by_precinct)
print(pct_voting_by_precinct)

# Mapping
if (!exists("district4_map")) {
  district4_map <- get_basemap()
  #print(district4_map)
}

# Get the youngest voters
if (!exists("under_25_voters_geocoded")) {
  under_25_voters_geocoded <- mutate_geocode(sqldf("SELECT MAILING_ADDRESS_LINE_1||', '||MAILING_CITY||' '||MAILING_STATE||' ' AS addr FROM voted_2022 WHERE CAST(SUBSTRING(DOB, -4, 4) AS INT) > 1997"), addr)
}

print(
  district4_map +
    geom_point(data=under_25_voters_geocoded, aes(x=lon, y=lat), size = 2, alpha=.8, fill="firebrick4", color="white", pch=21, inherit.aes = F) +
    labs(subtitle = "Voters under 25")
)
  
