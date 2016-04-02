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

place <- paste(xl1[1, "LocationCITY"], xl1[1, "LocationSTATE"], sep = ",")
(location <- geocode(place, output = "latlona", source = "google", messaging = T))


usa <- map_data("state")
ggplot()+  geom_map(aes(x = long, y = lat, map_id = region), data = usa, map = usa, fill = "#ffffff", color = "black", size = 0.15)


violent_crimes <- subset(crime, offense != "auto theft" & offense != "theft" & offense != "burglar")
