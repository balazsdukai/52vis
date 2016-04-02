# ====================
# Question:
# How are the drone sightings distributed around the USA?
# 
# ====================
# References:
# 
# the data challenge:
# http://rud.is/b/2016/03/30/introducing-a-weekly-r-python-js-etc-vis-challenge/
# 
# maps and geocoding:
# http://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html
# ====================

library(readxl)
library(ggplot2)
library(broom)
library(dplyr)
library(ggmap)
library(lubridate)
# get copies of the data locally

URL1 <- "http://www.faa.gov/uas/media/UAS_Sightings_report_21Aug-31Jan.xlsx"
URL2 <- "http://www.faa.gov/uas/media/UASEventsNov2014-Aug2015.xls"

fil1 <- paste0("./data/", basename(URL1))
fil2 <- paste0("./data/", basename(URL2))

if (!file.exists(fil1)) download.file(URL1, fil1)
if (!file.exists(fil2)) download.file(URL2, fil2)

# read it in

xl1 <- read_excel(fil1)
xl2 <- read_excel(fil2)

# merge the data
report <- setNames(bind_rows(xl2[,1:3], arrange(xl1, EventDATETIME)[,c(1,3,4)]), c("timestamp", "city", "state")) %>% 
    mutate(place = paste(city, state, sep = ", ")) %>% 
    select(timestamp, place) 

# geocoding
report <- cbind(report, geocode(report$place, output = "latlona", source = "google"))
report_f <- filter(report, lat > 25 & lat < 50, lon > -125 | lon < -70) %>% 
    mutate(quarter = quarter(timestamp, with_year = TRUE))
usa <- map_data("state")

# export, just in case...
write.csv(report[,1:4], file = "./data/drone_sightings_2014-2016.csv", sep = ",", row.names = F)

# create the plot
sp_minimal  <- theme_void(base_size = 12, base_family = "Verdana") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())

ggplot(report_f) + 
    geom_map(data = usa, aes(x = long, y = lat, map_id = region), map = usa, fill = "white", color = "#808080", size = 0.15) + 
    coord_equal() +
    sp_minimal +
    geom_point(aes(x = lon, y = lat), size = 0.3, color = "#404040") + 
    stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), bins = 4, geom = "polygon") + 
    scale_fill_gradient(low = "#ffeda0", high = "#f03b20") +
    facet_wrap(~quarter) +
    guides(fill="none", alpha="none") +
    ggtitle("Quarterly drone sightings on mainland USA") +
    theme(strip.text.x = element_text(size=8, face="bold"), title = element_text(size = 10,face = "bold")) 

