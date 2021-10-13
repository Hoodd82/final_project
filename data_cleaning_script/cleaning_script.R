# load_libraries ----------------------------------------------------------

pacman::p_load(
  here,        
  janitor,    
  lubridate,  
  tidyverse
)

# read_in_datasets --------------------------------------------------------

airlines <- read_csv(here("raw_data/airlines.csv"))
airports <- read_csv(here("raw_data/airports.csv"))
flights <- read_csv(here("raw_data/flights.csv"))
aircraft <- read_csv(here("raw_data/planes.csv"))
weather <- read_csv(here("raw_data/weather.csv"))

# data_cleaning -----------------------------------------------------------

weather <- weather %>% 
  select(-c(temp, dewp, humid, precip, pressure, 
            year, month, day, hour)) # drop variables where proportion of NAs is greater than 90%

airports <- airports %>% 
  select(-c(tz, dst, tzone)) # to avoid irregularities due to changes in timezones, New York local time will be the reference

aircraft <- aircraft %>%
  mutate(year = na_if(year, 0)) %>% # avoid issues with aircraft with year equal zero
  mutate(aircraft_age = as.numeric(format(Sys.Date(), "%Y")) - year) %>% # create aircraft age variable
  select(-c(engines, speed, year, type, engine)) # drop variables which will not be used in the final analysis

datetime_fn <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
} # using modulus arithmetic we can pull the hour and minutes from the departure and arrival variables

flights <- flights %>% 
  mutate(
    dep_time = datetime_fn(year, month, day, dep_time),
    arr_time = datetime_fn(year, month, day, arr_time),
    sched_dep_time = datetime_fn(year, month, day, sched_dep_time),
    sched_arr_time = datetime_fn(year, month, day, sched_arr_time)
  ) %>% # use the datetime_fn to create new departure and arrival variables
  drop_na() %>% # drop NAs since these observations will not be useful in the analysis
  mutate(origin_name = case_when(
    origin == "EWR" ~ "Newark Liberty Int. Airport",
    origin == "JFK" ~ "John F. Kennedy Int. Airport",
    origin == "LGA" ~ "LaGuardia Airport"
  ), .after = "origin") %>% # create origin_name for airport names, easier to do this rather than performing an additional join
  select(-c(year, month, day, hour, minute))  # drop variables which are no longer required

# first_join --------------------------------------------------------------

flights <- flights %>% 
  left_join(airlines, by = "carrier") %>% # join with airline dataset
  left_join(aircraft, by = "tailnum") %>% # join with aircraft dataset
  left_join(airports, by = c("dest" = "faa")) # join with the airport dataset

# flights_weather_join ----------------------------------------------------

# the following function takes an input of the flights and weather datasets, then subsets them based on origin (this is performed to avoid any conflicts which might arise when a flight departs at the same time_hour from different airports when weather is joined with flights), joins the datasets and combines them using bind_rows

flights_weather_join_fn <- function(dataset1, dataset2){
  # subset flights
  ewr_flights <- dataset1 %>% 
    filter(origin == "EWR")
  jfk_flights <- dataset1 %>% 
    filter(origin == "JFK")
  lga_flights <- dataset1 %>% 
    filter(origin == "LGA")
  # subset weather
  ewr_weather <- dataset2 %>% 
    filter(origin == "EWR")
  jfk_weather <- dataset2 %>% 
    filter(origin == "JFK")
  lga_weather <- dataset2 %>% 
    filter(origin == "LGA")
  # perform the joins
  ewr_flights_joined <- ewr_flights %>% 
    left_join(ewr_weather, by = "time_hour")
  jfk_flights_joined <- jfk_flights %>% 
    left_join(jfk_weather, by = "time_hour")
  lga_flights_joined <- lga_flights %>% 
    left_join(lga_weather, by = "time_hour")
  # combine the resulting datasets
  flights_joined <- bind_rows(
    ewr_flights_joined, jfk_flights_joined, lga_flights_joined)
  # assign the output to flights
  flights <<- flights
}

flights_weather_join_fn(flights, weather)

# final_clean -------------------------------------------------------------

# final round of cleaning on the joined dataset, the focus here is to reorder the data so the information is in a logical order

   