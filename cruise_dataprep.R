### Cruise Data Prep

cruise <- read_csv("Worldship_all.csv", show_col_types = FALSE) %>% filter(!is.na(Date)) %>%
  mutate(
  Arrival = case_match(Arrival, c("---", "N/A", NA, "AM") ~ "12:00 AM", .default = Arrival),
  Departure = case_match(Departure, c("---", "N/A", NA, "Early AM", "PM", "AM") ~ "11:59 PM", .default = Departure),
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
  Line = if_else(Line=="Sliversea Cruises", "Silversea Cruises", Line), #fix typo in raw data
  Line = if_else(Line=="Viking Expeditions Cruises", "Viking Expeditions", Line), #fix typo in raw data
  Line = if_else(Line=="Seabourn Cruise Line", "Seabourn", Line) #fix typo in raw data    
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
  Generator = if_else(Ship_Presence==F, F, Generator),
  ) %>%
  dplyr::select(-contains(c("Arrival", "Departure")))

wide_ships <- pivot_wider(
  cruise_ships_hourly, id_cols=c("Hour", "Date"), names_from = c("Ship"), values_from = Ship_Presence, names_prefix = "Ship",values_fn = sum,   values_fill = 0,
  )
wide_terminals <- pivot_wider(
  cruise_ships_hourly, id_cols=c("Hour", "Date"), names_from = c("Terminal"), names_prefix = "Terminal", values_from = Ship_Presence, values_fn = sum,   values_fill = 0,
  )
wide_lines <- pivot_wider(
  cruise_ships_hourly, id_cols=c("Hour", "Date"), names_from = c("Line"), names_prefix = "Line", values_from = Ship_Presence, values_fn = sum,   values_fill = 0,
  )
wide_generators <- pivot_wider(
  cruise_ships_hourly, id_cols=c("Hour", "Date"), names_from = c("Generator"), values_from = Generator, values_fn = sum,   values_fill = 0,
  ) %>% 
  rename(Generator = `TRUE`) %>%
  dplyr::select(Hour, Date, Generator)
  
cruise_ships_hourly_wide <- left_join(wide_ships, wide_terminals) |> left_join(wide_lines) |> left_join(wide_generators) |> filter(Date > "2023/07/10")

cruise_ships_daily_wide <- cruise_ships_hourly_wide |> group_by (Date) |>
  summarise(
  across(
    contains(c("Ship", "Generator", "Terminal", "Line")),
    ~ max(.x)
  )
)
