library(readr)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(animation)

library(highcharter)
library(ggmap)
library(ggthemes)
sequake = read_rds("../data/historical_web_data_26112015.rds")



magickPath <- shortPathName("C:\\Program Files\\ImageMagick-7.0.7-Q16\\magick.exe")
ani.options(convert=magickPath)


#1-------------

plot_ly(sequake, x = ~Longitude, y = ~Latitude, z = ~Depth, size = ~Magnitude, color = ~Magnitude)



#2 gganimate won't work-----------
disaster = read_delim("../data/disaster.txt", "\t", escape_double = F, trim_ws = T)
world = map_data("world")

disaster %>% 
  filter(EQ_MAG_MW >= 6) -> dis

ggplot()+
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "light green")+
  geom_point(data = disaster, aes(x = LONGITUDE,
                                      y = LATITUDE,
                                     
                                      color = EQ_PRIMARY,
                                      frame = YEAR), alpha = 0.7) +
  scale_color_gradient(low = "light pink", high = "purple", na.value = "white"
                       ) +
  coord_fixed(1.5) +
  theme_minimal() -> q

ggplotly(q)

gganimate(q, filename = "eq.gif")
#3-------------
iran_earthquake = read_rds("../data/iran_earthquake.rds")


m <- ggplot(iran_earthquake, aes(x = Long, y = Lat)) +
  geom_point(aes(color = Mag)) + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon")


m + geom_point(data = world %>% filter(region == "Iran"), aes(x = long, y = lat), color = "purple")+ 
   xlim(35, 80) + 
  scale_color_gradient(low = "green", high = "blue") +
  theme_minimal() 

#4--------------
disaster %>% 
  filter(COUNTRY == "IRAN") -> iran_disaster
iran_disaster %>% 
  filter(EQ_PRIMARY > 7) -> iran_disaster

#x sal gozashte, p(y|x) ro mikhaym. x mishe yek sal, p(y|1) y <= 5
iran_disaster %>% 
  arrange(desc(YEAR)) %>% 
  head(1) %>% 
  select(YEAR)
  



iran_disaster %>% 
  arrange(YEAR) %>% 
  filter(lead(YEAR) - YEAR >= 1) -> n
n %>%   
filter(lead(YEAR) - YEAR <= 5) %>% 
  nrow() ->  a

round(a / n %>%  nrow(), 3)

#5---------------
disaster %>% 
  group_by(COUNTRY) %>% 
  summarise(sum_death = sum(DEATHS, na.rm = T), mean_death = mean(DEATHS, na.rm = T)) -> country_death

country_death %>% 
  ungroup() %>% 
  mutate(COUNTRY = tolower(COUNTRY)) -> country_death
 
world %>% mutate(COUNTRY = tolower(region)) -> world
full_join(world,country_death) -> death_world

ggplot(data = death_world, aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill = sum_death))  +
  geom_path(color = "grey") + 
  scale_color_gradient(low = "pink", high = "purple", na.value = "white") +
  theme_minimal()

#6----------
disaster %>% 
  select(LONGITUDE, LATITUDE, DEATHS, FOCAL_DEPTH,EQ_PRIMARY) -> disaster_train

model = glm(data = disaster_train, formula = DEATHS ~ LONGITUDE + LATITUDE+ INTENSITY + EQ_PRIMARY, family = "poisson")
model %>% summary()


library(h2o)
h2o.init()
hdisasater = as.h2o(disaster_train)
h2o.glm(y = "DEATHS", x = c("LONGITUDE", "LATITUDE", "INTENSITY","EQ_PRIMARY"), family = "poisson", training_frame = hdisasater) -> hmodel

hmodel


#7-------------
worldwide = read_csv("../data/worldwide.csv")

worldwide %>% 
  mutate(day = as.Date(time)) %>% 
  group_by(place, day, mag, time) %>% 
  summarise() %>% 
  ungroup() -> worldwide_2
  
worldwide %>% 
  mutate(day = as.Date(time)) %>% 
  group_by(place, day, mag, time) %>% 
  summarise() %>% 
  ungroup() %>% 
  group_by(place, day) %>% 
  slice(which.max(mag)) -> prime

colnames(prime) = c("place", "day", "mag_prime", "time_prime")

full_join(prime, worldwide_2) %>% 
  filter(time_prime >= time) %>% 
  mutate(isPrime = ifelse(time == time_prime,0 , mag_prime)) -> pre_prime_earthquake

lm(data = pre_prime_earthquake, formula = isPrime ~ mag) -> pre_prime_model

pre_prime_model %>% summary()

#8----------
aov(data = worldwide, mag ~ depth) %>% summary.aov()

cor.test(worldwide$mag, worldwide$depth)

#9naghes, how to detect haarp? extracting country name---------- 
library(stringr)
str_split_fixed(worldwide$place, ",", 2) %>% as.data.frame() %>% 
  mutate(what = (V2 == "")) %>% 
  mutate(V2 = ifelse(what, as.character(V1), as.character(V2))) %>% 
  select(V2) -> worldwide_country
worldwide$country2 = worldwide_country$V2
coords2country(worldwide %>% select(longitude, latitude)) -> worldwide$country

worldwide %>% filter(!is.na(country)) -> worldwide
#worldwide %>% 
#  mutate(country = ifelse(is.na(country),as.character(country2),as.character(country))) -> worldwide
worldwide %>% 
  ungroup() %>% 
  select(country, mag , time) %>% 
  mutate(year = format(as.Date(time), format="%Y")) -> ww

worldwide$year = ww$year
ww %>% 
  select(year) %>% 
  unique() %>% 
  nrow -> n

worldwide %>% ungroup %>% 
  group_by(country) %>% 
  mutate(total = n()) -> worldwide

worldwide %>%
  group_by(country,format(as.Date(time), format="%Y")) %>% 
  summarise(mag_average = mean(mag)) -> year_country_mag



worldwide %>% 
  group_by(country,format(as.Date(time), format="%Y")) %>% 
  summarise(count = n()) -> year_country_count

year_country_count %>% 
  group_by(country) %>% 
  summarise(number = n(), total = sum(count)) %>% 
  group_by(country) %>% 
  summarise(mean = total / number) %>% 
  arrange(desc(mean)) %>% 
  hchart(hcaes(x = country , y = mean, color = country), type = "column")

colnames(year_country_count) = c("country", "year", "count")
  
aov(data = year_country_count, count ~ year) %>% summary.aov()



#10---------- felan na


#10a----------


#peida kardane gosal haye zamin

worldwide = read_csv("../data/worldwide.csv")
worldwide %>% 
  select(longitude, latitude, mag, magType) -> gosal

ggplot(data = gosal, aes(x=longitude, y=latitude)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "violet", color = "purple") +
  coord_fixed(1.5) +
  theme_minimal()
  
gosal %>% 
  mutate(long = longitude + 200, lat = latitude + 100) %>% 
  mutate(distance = sqrt(long ^ 2 + lat ^ 2)) -> gosal

#body-wave magnitude  -> mb
aov(mag ~ distance, gosal %>% filter(magType == "mb")) %>% 
  summary.aov()
cor.test(gosal %>% filter(magType == "mb") %>% .$mag, gosal %>% filter(magType == "mb") %>% .$distance)


#10b----------

disaster %>% 
  slice(which.max(DEATHS)) %>% 
  select(COUNTRY, LOCATION_NAME, YEAR, DEATHS)
#be tore miangin har sale chand nafar az zelzeleh mimiran
disaster %>% 
group_by(YEAR) %>% 
  summarise(average_death = round(mean(DEATHS, na.rm = T))) %>% 
  filter(average_death != "NaN") -> deaths_each_year
  
hchart(deaths_each_year,hcaes(x = YEAR, y = average_death), type = "line", color = "purple")

#deaths over past 4000 years
deaths_each_year %>% 
  filter(2018 - YEAR <= 4000) -> death_4000

sum(death_4000$average_death)

#har sale chand ta zelezele ghavi rokh mide
disaster %>% 
  filter(EQ_PRIMARY > 7) -> r

r %>% 
  group_by(YEAR) %>% 
  summarise(count = n ()) -> deathly_eq
  
ceiling(mean(deathly_eq$count))  

#10c----------

#ring of fire and Alpide Belt


ggplot(data = r, aes(x=LONGITUDE, y=LATITUDE)) +
  geom_point(color = "orange") +
  geom_polygon(data = world, aes(x = long, y = lat, group = group, fill = group),color = "black") +
  theme_minimal()





#--------------
library(sp)
library(rworldmap)

worldwide %>% select(longitude, latitude) %>% coords2country %>% as.data.frame() %>% View()

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}




