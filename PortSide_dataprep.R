### Load and format PortSide NY Sensor data

# Dyson_raw <- read.csv("Dyson2023-10-21.csv") %>% mutate(
Dyson_raw <- read_csv("Dyson_Dec02.csv", skip=6, col_names = F) 
colnames(Dyson_raw) <- c(
  "DateTime",
  "Pressure",
  "Avg.Wind.Speed",
  "Avg.Wind.Dir",
  "Rain",
  "AQI_adj",
  "PM1",
  "PM2.5",
  "PM10",
  "Temp",
  "Humidity",
  "AQI"
)

numcols <- c(
  "Pressure",
  "Temp",
  "Humidity",
  "Avg.Wind.Speed",
  "Rain",
  "AQI",
  "AQI_adj",
  "PM1",
  "PM2.5",
  "PM10"
)

windir <- read.csv("WindDirTable.csv") #for matching cardinal directions with direction in degrees

Dyson <- Dyson_raw %>% 
  mutate(
    DateTime = parse_date_time(DateTime, "mdy HM", tz = "America/New_York")
  ) %>% 
  filter(DateTime >= "2023-07-11" & DateTime < "2023-12-02") %>%
  mutate(
    across(all_of(numcols), as.numeric)
  ) 

Dyson$degrees <- windir$Degrees[match(Dyson$Avg.Wind.Dir, windir$Direction)]

## aggregate Dyson hourly
aq_list <-c(
  "PM2.5",
  "AQI", 
  "AQI_adj",
  "PM1",
  "PM10"
)
# Round the timestamp to the nearest hour
data <- Dyson %>%
  mutate(hour = floor_date(DateTime, unit = "hour")) %>%
  relocate(DateTime, hour, all_of(aq_list), contains("Rain"))

# Use summarize(across()) to calculate summary statistics for all columns
Dyson_hourly <- data %>%dplyr::select(-DateTime) %>% 
  group_by(hour) %>% dplyr::select(where(is.numeric)) %>%
  summarize(
    across(-contains("Rain"), list(mean = mean)),
    across(contains("Rain"), list(sum=sum))
  ) %>%
  mutate(
    sinWind = sin(degrees_mean),
    cosWind = cos(degrees_mean)
  )%>% rename(
    PM2.5_Dyson = PM2.5_mean
  )

elevated_hours <- Dyson_hourly %>% 
  filter(PM2.5_Dyson > 11) %>% 
  mutate(date = date(hour)) %>% 
  group_by(date) %>% 
  summarize(elevated_hours = n())

Dyson_daily <- Dyson %>% mutate(date = date(DateTime)) %>% dplyr::select(-DateTime) %>% 
  group_by(date)  %>% dplyr::select(where(is.numeric)) %>%
  summarize(
    across(-contains("Rain"), list(mean = mean)),
    across(contains("Rain"), list(sum=sum))
  ) %>%
  mutate(
    sinWind = sin(degrees_mean),
    cosWind = cos(degrees_mean)
  ) %>% rename(
    PM2.5_Dyson = PM2.5_mean
  ) %>% 
  left_join(elevated_hours) %>%
  mutate(
    elevated_hours = if_else(is.na(elevated_hours), 0, elevated_hours)
  )



