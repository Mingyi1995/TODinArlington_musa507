setwd("/Users/hemingyi/Documents/musa507")
# install.packages("kableExtra")
install.packages("rmarkdown")
install.packages("kableExtra")

library(tidyverse)
library(tidycensus)
library(sf)
library(FNN)
library(jsonlite)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(raster)
options(scipen=999)
options(tigris_class = "sf")
# FNN is a package that we use for measuring nearest neighbor distance. 
# jsonlite is used to access data via open data APIs. 
# knitr and kableExtra are used to create tables

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 24,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 24,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

census_api_key("270082da749af57396e061417ed72737fec5d025")
 # Arlington VA
indicator = c("P001001","P006002","PCT025050",
              "PCT025009","P053001","H056001",
              "P092001")

tracts00 <- get_decennial(geography = "tract", variables = indicator, year = 2000,
                    state = "VA",county = "Arlington", geometry = TRUE) %>% 
  st_transform(102346)

table(tracts00$variable)

totalPop00 <-
  tracts00 %>%
  filter(variable == "P001001")


ggplot() +
  geom_sf(data = totalPop00, mapping = aes(fill = q5(value))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop00, "value"),
                    name = "Total\nPopluation\n(Quintile Breaks)") +
  labs(title = "Total Population",
       subtitle = "Philadelphia; 2000") +
  mapTheme()

tracts00 <- 
  tracts00 %>%
  dplyr::select(-NAME) %>%
  spread(variable, value)

tracts00 <- 
  tracts00 %>%
  rename(TotalPop = P001001,
         NumberWhites = P006002,
         TotalFemaleBachelors = PCT025050,
         TotalMaleBachelors = PCT025009,
         MedHHInc = P053001,
         MedRent = H056001,
         TotalPoverty = P092001)
view(tracts00)
tracts00$area_sqkm <- st_area(tracts00) 
tracts00$area_sqkm <- as.integer(tracts00$area_sqkm) / 1000000
tracts00$StandPop <- tracts00$TotalPop / tracts00$area_sqkm
tracts00 <- 
  tracts00 %>%
  mutate(percentWhite = ifelse(TotalPop > 0, NumberWhites / TotalPop,0),
         percentBachelors = ifelse(TotalPop > 0, ((TotalFemaleBachelors + TotalMaleBachelors) / TotalPop),0),
         percentPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2000") %>%
  dplyr::select(-NumberWhites,-TotalFemaleBachelors,-TotalMaleBachelors,-TotalPoverty)

tracts17 <- get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                                       "B15001_009E","B19013_001E","B25058_001E",
                                                       "B06012_002E"), year = 2017,
                          state = "VA",county = "Arlington", geometry = TRUE) %>% 
  st_transform(102346)%>%
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B25026_001,
         NumberWhites = B02001_002,
         TotalFemaleBachelors = B15001_050,
         TotalMaleBachelors = B15001_009,
         MedHHInc = B19013_001,
         MedRent = B25058_001,
         TotalPoverty = B06012_002) %>%
  mutate(percentWhite = ifelse(TotalPop > 0, NumberWhites / TotalPop,0),
         percentBachelors = ifelse(TotalPop > 0, ((TotalFemaleBachelors + TotalMaleBachelors) / TotalPop),0),
         percentPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-NumberWhites, -TotalFemaleBachelors, -TotalMaleBachelors, -TotalPoverty) 

allTracts <- 
  rbind(
    as.data.frame(tracts00), 
    as.data.frame(tracts17)) %>%
  st_sf()

table(allTracts$year)
# download metro stations locations from API
# https://gisdata-arlgis.opendata.arcgis.com/datasets/facility-points
# this data set contains many kinds of facilities, we need to filter it
MetroStops <- fromJSON("https://opendata.arcgis.com/datasets/b516a5a71b9b4d67b3f7e26957a923bc_0.geojson")
MetroStops <- as.data.frame(MetroStops$features)
MetroStops <- flatten(MetroStops)
MetroStops <- filter(MetroStops, properties.CODE == 'Metro Station')
names(MetroStops)

coord <- do.call(rbind, MetroStops$geometry.coordinates)
colnames(coord) <- c("x","y")
MetroStops <- cbind(MetroStops[c("properties.NAME","properties.SYMBOL")], coord)

septa.sf <- 
  st_as_sf(MetroStops, coords = c('x','y'), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(tracts00))

ggplot() + 
  geom_sf(data=st_union(tracts00)) +
  geom_sf(data=MetroStops, show.legend = "point", size= 2) +
  scale_colour_manual(name="Quintile\nBreaks") +
  labs(title="Septa Stops",
       subtitle="Arlington, VA",
       caption="`Public Policy Analytics - Figure 2.6") +
  mapTheme()

ggplot() + 
  geom_sf(data=tracts00) +
  geom_sf(data=MetroStops, show.legend = "point", size= 2) +
  scale_colour_manual(name="Quintile\nBreaks") +
  labs(title="Septa Stops",
       subtitle="Arlington, VA",
       caption="`Public Policy Analytics - Figure 2.6") +
  mapTheme()
# coor ESRI:102346 unit is meter.
septaBuffer <- 
  st_buffer(MetroStops, 800) %>%
  mutate(Legend = "Buffer") %>%
  dplyr::select(Legend)

plot(septaBuffer[,1])

septaBufferUnion <- 
  septaBuffer %>%
  st_union() %>%
  st_sf() %>%
  mutate(Legend = "Unioned Buffer") 

septaBuffers <- rbind(septaBuffer,septaBufferUnion)

ggplot() + 
  geom_sf(data=st_union(tracts00), aes()) +
  geom_sf(data=septaBuffers) +
  geom_sf(data=septa.sf, show.legend = "point") +
  facet_wrap(~Legend) +
  scale_colour_manual(values = c("orange","blue"),
                      name="Septa\nLines") +
  labs(title="Septa Stops (Buffered & Unioned), 1/2th Mile",
       subtitle="Arlington, VA",
       caption="Public Policy Analytics - Figure 2.7") +
  mapTheme() +
  theme(strip.text.x = element_text(size = 14))

intersection_tractsAndBuffer <- 
  st_intersection(septaBufferUnion, tracts00) %>%
  dplyr::select(GEOID, TotalPop) %>%
  mutate(Selection_Type = "Intersection")

select_tractsAndBuffer <- 
  tracts00[septaBufferUnion,] %>%
  dplyr::select(GEOID, TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

selectCentroids_tractsAndBuffer <-
  st_centroid(tracts00)[septaBufferUnion,] %>%
  st_set_geometry(NULL) %>%
  left_join(tracts00) %>%
  st_sf() %>%
  dplyr::select(GEOID, TotalPop) %>%
  mutate(Selection_Type = "Select by centroids")

allSelections <- 
  rbind(intersection_tractsAndBuffer,
        select_tractsAndBuffer,
        selectCentroids_tractsAndBuffer)

ggplot() + 
  geom_sf(data = st_union(tracts00), aes()) +
  geom_sf(data = allSelections, aes(fill = q5(TotalPop))) +
  facet_wrap(~Selection_Type) +
  scale_fill_manual(values = palette5,
                    labels = qBr(allSelections, "TotalPop"),
                    name = "Total\nPopluation\n(Quintile Breaks)") + 
  labs(title="Total population within 1/2mi. of subways",
       subtitle="3 Spatial selection techniques; Arlington, VA",
       caption="Public Policy Analytics - Figure 2.8") +
  mapTheme()  

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[septaBufferUnion,] %>%
      st_set_geometry(NULL) %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD_Group = "TOD"),
    
    st_centroid(allTracts)[septaBufferUnion, op = st_disjoint] %>%
      st_set_geometry(NULL) %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD_Group = "Non-TOD"))

ggplot() + 
  geom_sf(data = allTracts.group, aes(fill = TOD_Group)) +
  facet_wrap(~year) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Time/Space groups") +
  mapTheme()

na.omit(allTracts.group) %>%
  mutate(MedRent.inf = ifelse(year == "2000", MedRent * 1.42, MedRent)) %>%
  ggplot() + 
  geom_sf(data = st_union(allTracts)) +
  geom_sf( aes(fill = q5(MedRent.inf), colour = TOD_Group)) +
  facet_wrap(~year) +
  scale_fill_manual(values = palette5,
                    labels = qBr(tracts00, "MedRent"),
                    name = "Rent\n(Quintile Breaks)") + 
  scale_colour_manual(values = c("gray36", "red")) +
  labs(title="Median rent, 2000 - 2017",
       subtitle="Real dollars; Arlington, VA",
       caption="Public Policy Analytics - Figure 2.8") +
  mapTheme()

allTracts.group.Summary <- 
  allTracts.group %>%
  mutate(MedRent = ifelse(year == "2000", MedRent * 1.42, MedRent)) %>%
  group_by(year, TOD_Group) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(percentWhite, na.rm = T),
            Percent_Bachelors = mean(percentBachelors, na.rm = T),
            Percent_Poverty = mean(percentPoverty, na.rm = T)) %>%
  st_set_geometry(NULL)

kable(allTracts.group.Summary,
      caption = "TOD Indicators") %>%
  kable_styling()

allTracts.group.Summary %>%
  unite(year.TOD_Group, year, TOD_Group, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD_Group) %>%
  mutate(Value = round(Value,2)) %>%
  spread(year.TOD_Group, Value) %>%
  kable(caption = "TOD Indicators") %>%
  kable_styling()

allTracts.group.Summary %>%
  gather(Variable, Value, -year, -TOD_Group) %>%
  ggplot(aes(TOD_Group, Value, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme()
# submarket

septa.sf$properties.SYMBOL[septa.sf$properties.SYMBOL == "Metro Blue Line Station"] <- "Metro Yellow/Blue Line Station"

centerCity.Buffer <-
  filter(septa.sf, properties.SYMBOL == "Metro Orange/Blue/Silver Line Station") %>%
  st_buffer(800) %>%
  st_union() %>%
  st_sf() %>%
  mutate(Submarket = "Center City")

OL.Buffer <-
  filter(septa.sf, properties.SYMBOL == "Metro Orange/Silver Line Station") %>%
  st_buffer(800) %>%
  st_union() %>%
  st_sf() %>%
  st_difference(centerCity.Buffer) %>%
  mutate(Submarket = "OL")

YB.Buffer <-
  filter(septa.sf, properties.SYMBOL == "Metro Yellow/Blue Line Station") %>%
  st_buffer(800) %>%
  st_union() %>%
  st_sf() %>%
  st_difference(centerCity.Buffer) %>%
  mutate(Submarket = "YB")

threeBuffers <- 
  rbind(centerCity.Buffer, OL.Buffer, YB.Buffer) %>%
  dplyr::select(Submarket)

ggplot() +
  geom_sf(data = st_union(tracts00)) +
  geom_sf(data = threeBuffers, aes(fill = Submarket)) +
  scale_fill_manual(values = c("orange","green4","blue"),
                    name="Submarket") +
  labs(title = "3 Submarkets") +
  mapTheme()

allTracts.threeBuffers <-
  st_join(st_centroid(allTracts), threeBuffers) %>%
  st_set_geometry(NULL) %>%
  left_join(allTracts) %>%
  mutate(Submarket = replace_na(Submarket, "Non-TOD")) %>%
  st_sf() 

ggplot() + 
  geom_sf(data = allTracts.threeBuffers, aes(fill=Submarket)) +
  scale_fill_manual(values = c("orange","green4","blue","black"),
                    name="Submarket") +
  labs(title = "Tracts by submarkets") +
  mapTheme()

allTracts.threeBuffers %>%
  mutate(MedRent = ifelse(year == "2000", MedRent * 1.42, MedRent)) %>%
  group_by(year, Submarket) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(percentWhite, na.rm = T),
            Percent_Bachelors = mean(percentBachelors, na.rm = T),
            Percent_Poverty = mean(percentPoverty, na.rm = T)) %>%
  st_set_geometry(NULL) %>%
  gather(Variable, Value, -year, -Submarket) %>%
  ggplot(aes(Submarket, Value, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and submarkets") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
