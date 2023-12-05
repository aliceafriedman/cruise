### Data pull

source("keys.R")

# Define sensor (ID comes from URL in purpleair maps)
`PortSide Test` <- 145962
`Dikeman St` <- 182235
`Hilo Acres` <- 23619
`Dyberry Creek Farm` <- 21311
`Pier 83` <- 185775

## Define start dates
start_dates <- c(
  "2023-07-03",
  "2023-07-17",
  "2023-07-31",
  "2023-08-14",
  "2023-08-28",
  "2023-09-11",
  "2023-09-25",
  "2023-10-09",
  "2023-10-23",
  "2023-11-06",
  "2023-11-20"
)

# Define list of fields
fields_list <- c("temperature", 
                 "humidity", 
                 "pm2.5_alt")

## Define functions
format_timestamp <- function(date){
  return (paste0(as.character(date),"T00%3A00%3A00%2B00%3A00"))
}

add_days <- function(day1){
  day1 <- ymd(day1, tz = "UTC")
  nextdate <- day1 + days(14)
  nexttime <- format_timestamp(nextdate)
  return(nexttime)
}

get_purple <- function(sensor, startday=day1, fields=fields_list){
  
  # Combine the fields into a single string with commas and URL-encode if needed
  fields_to_include <- paste(fields, collapse = "%2C%20")
  
  #encode and format startday
  startDate <- format_timestamp(startday)
  # Calculate end day 14 days later
  endDate <- add_days(startday)
  # Amend the URL with the combined fields
  url <- paste0(
    "https://api.purpleair.com/v1/",
    "sensors/",
    sensor,
    "/history?start_timestamp=",   
    startDate,
    "&end_timstamp=",
    endDate,
    "&average=60&fields=", #thisdplyr::selects the hourly average
    fields_to_include
  )
  
  print(url)
  
  # Define headers
  headers = httr::add_headers(`X-API-Key` = api_key)
  
  # Send a GET request with custom headers
  response <- GET(url, headers)
  
  # Parse the JSON response
  data <- jsonlite::fromJSON(content(response, "text"))
  
  # Access the data you want (temperature, humidity, pm2.5_alt) & convert to data frame
  df <- data$data %>% as.data.frame()
  
  # Extract the column names from the JSON response and set them as column names in the data frame
  colnames(df) <- data$fields
  
  return(df)
}

purple_loop <- function(sensor, n=length(start_dates), fields=fields_list, startday="2023-07-31"){
  
  dfs <- list()
  
  for(i in 1:n){
    dfs[[i]] <- get_purple(sensor, startday = start_dates[i])
    Sys.sleep(1)
  }  
  combined <- do.call(rbind, dfs)
  
  # before joining, have to use lubridate to subtract 4 hours from UTC <- this is done above!!!
  df <- combined %>%
    mutate(
      local_datetime = ymd_hms(time_stamp) - hours(4), #adjust timezone
      Date = local_datetime %>% as.Date()
    ) %>%
    mutate_at(fields, as.numeric)
  
  return (df)
}


Dyberry_pull <- purple_loop(`Dyberry Creek Farm`)
#Pier83_pull <- purple_loop(`Pier 83`)
DikemanSt_pull <- purple_loop(`Dikeman St`)

prep <- function(df){
  df <- df %>% 
    arrange(local_datetime) %>%
    filter(Date > lubridate::as_date("2023-07-10"))%>% # filter to start July 10 with Dyson 
    mutate(
      PM2.5_corrected = (pm2.5_alt* 0.524 - 0.0862 * humidity + 5.75), # Correction factor (national study)
    ) %>%
    mutate(across(all_of(fields_list),as.numeric)) }

Dyberry <- prep(Dyberry_pull)
Dyberry |> write_csv("Dyberry.csv")

Dikeman <- prep(DikemanSt_pull)
Dikeman |> write_csv("Dikeman.csv")
