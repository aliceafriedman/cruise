### Data Prep
# cruiseShips_Aug <- read_csv("EDC data - Aug 2023.csv",show_col_types = FALSE) %>% 
#   dplyr::mutate(Date = as.Date(Date, format= "%m/%d/%y"))
# 
# ships <- cruiseShips_Aug %>% group_by(Ship) %>%dplyr::select(`Connection`) %>% unique()
# 
# cruiseShips_April <- read_csv("EDC data - April 2023.csv", show_col_types = FALSE) %>% 
#   dplyr::mutate(Date = as.Date(`*Date*`, format= "%m/%d/%Y")) %>% 
#   dplyr::select(-`*Date*`) %>%
#   rename(Ship = `*Vessel*`)
# 
# cruise <- left_join(cruiseShips_April, ships) %>% arrange(Date) %>%dplyr::select(Date, Ship, Connection) %>%
#   mutate(Connection = if_else(Ship=="Crystal Serenity", "No", Connection))
# 
# cruise %>%dplyr::select(Ship, Connection) %>% table()

cruiseRaw <- read_csv("Worldship_all.csv", show_col_types = FALSE) %>% mutate(
  Arrival = if_else(Arrival=="---", "12:00 AM", Arrival),
  Departure = if_else(Departure=="---", "11:59 PM", Departure),
  Arrival = paste(Date, Arrival) |> parse_date_time("m/d/y H:M p"),
  ArrivalHour = hour(Arrival),
  Departure = paste(Date, Departure) |> parse_date_time("m/d/y H:M p"),
  DepartureHour = hour(Departure),
  Date = parse_date_time(Date, "m/d/y"),
  Generator = if_else(Terminal!="BCT", 
                      TRUE, 
                      if_else(
                        Line == "MSC Cruises" | Line == "Marella Cruises", TRUE, FALSE 
                      )),
  Terminal = if_else(is.na(Terminal), "None", Terminal),
  Line = if_else(is.na(Line), "None", Line),
  Line = if_else(Line=="Sliversea Cruises", "Silversea Cruises", Line), #fix typo in raw data
  Line = if_else(Line=="Viking Expeditions Cruises", "Viking Expeditions", Line), #fix typo in raw data
  Line = if_else(Line=="Seabourn Cruise Line", "Seabourn", Line) #fix typo in raw data    
) 

meanArrival = mean(cruiseRaw$ArrivalHour, na.rm = T) |> round(1)
meanDeparture = mean(cruiseRaw$DepartureHour, na.rm = T) |> round(0)

cruise <- cruiseRaw %>% mutate(
  Arrival = if_else(
    is.na(Arrival), 
    Date + hours(meanArrival), 
    Arrival),
  Departure = if_else(
    is.na(Departure), 
    Date + hours(meanDeparture), 
    Departure),
)

# Create an hourly sequence for the entire period
start_date <- min(cruise$Arrival)
end_date <- max(cruise$Departure)
hourly_sequence <- seq(from = start_date, to = end_date, by = "hour")

# Create an empty data frame to store the hourly ship presence
hourly_ship_presence <- data.frame(Hour = hourly_sequence)

# Initialize the Ship_Presence column with FALSE
hourly_ship_presence$Ship_Presence <- FALSE

# Iterate over the original data and mark presence in the hourly data frame
for (i in 1:nrow(cruise)) {
  arrival <- cruise$Arrival[i]
  departure <- cruise$Departure[i]
  hourly_ship_presence$Ship_Presence <- hourly_ship_presence$Ship_Presence |
    (hourly_sequence >= arrival & hourly_sequence < departure)
}

hourly_ship_presence$Date <- date(hourly_ship_presence$Hour)

cruise_ships_hourly <- full_join(hourly_ship_presence, cruise, by="Date", relationship = "many-to-many") %>% 
  mutate(
  Ship_Presence = if_else(is.na(Ship_Presence), FALSE, Ship_Presence),
  Ship = if_else(Ship_Presence==F, "None", Ship),
  Line = if_else(Ship_Presence==F, "None", Line),
  Terminal = if_else(Ship_Presence==F, "None", Terminal),
  Generator = if_else(Ship_Presence==F, F, Generator),)

cruise_ships_daily <- cruise_ships_hourly |> dplyr::filter(hour(Hour)==12)
