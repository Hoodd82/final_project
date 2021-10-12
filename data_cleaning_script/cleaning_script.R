# load_libraries ----------------------------------------------------------

pacman::p_load(
  here,        
  janitor,    
  lubridate,  
  tidyverse,
  skimr,
  hms
)

# read_in_datasets --------------------------------------------------------

airlines <- read_csv(here("raw_data/airlines.csv"))
airports <- read_csv(here("raw_data/airports.csv"))
flights <- read_csv(here("raw_data/flights.csv"))
aircraft <- read_csv(here("raw_data/planes.csv"))
weather <- read_csv(here("raw_data/weather.csv"))

# data_cleaning -----------------------------------------------------------

weather <- weather %>% 
  select(-c(temp, dewp, humid, precip, pressure, year, month, day, hour)) # drop variables where proportion of NAs is greater than 90%

airports <- airports %>% 
  select(-c(tz, dst, tzone)) # drop variables which will not be used in the final analysis

aircraft <- aircraft %>%
  mutate(year = na_if(year, 0)) %>% 
  mutate(aircraft_age = as.numeric(format(Sys.Date(), "%Y")) - year) %>% # create aircraft age variable
  select(-c(engines, speed, year, type, engine)) # drop variables which will not be used in the final analysis

flights <- flights %>%
  drop_na() %>% # drop NAs since these observations will not be useful in the analysis
  mutate(depart_date = str_c(year, month, day, sep = "-")) %>% # create departure date
  mutate(across(.cols = c(dep_time, sched_dep_time, arr_time, sched_arr_time), 
                .fns = ~ str_pad(.x, width = 4, pad = 0))) %>% # convert the time variables to hhmm
  mutate(across(.cols = c(dep_time, sched_dep_time, arr_time, sched_arr_time), 
                .fns = ~ str_replace(.x, "^(..)", "\\1:"))) %>% # convert to hh:mm
  mutate(sched_depart_datetime = ymd_hm(paste(depart_date, sched_dep_time))) %>% # create scheduled departure datetime column
  mutate(act_depart_datetime = sched_depart_datetime + dep_delay * 60) %>% # create actual departure date column by adding dep_delay
  mutate(origin_name = case_when(
    origin == "EWR" ~ "Newark Liberty Int. Airport",
    origin == "JFK" ~ "John F. Kennedy Int. Airport",
    origin == "LGA" ~ "LaGuardia Airport"
  ), .after = "origin") %>% # create origin_name variable for airport names
  select(-c(dep_time, sched_dep_time, arr_time, sched_arr_time, arr_delay, depart_date, year, month, day, hour, minute))  # drop variables which are no longer required

# the brief is to investigate departure delays therefore variables relating to arrivals have been dropped - the assumption is that an arrival delay is as a result of a departure delay which falls outwith the scope of the project
# destination information is retain as there may be useful insights around whether some routes suffer from delays more than others

# first_join --------------------------------------------------------------

flights_joined <- flights %>% 
  left_join(airlines, by = "carrier") %>% 
  left_join(aircraft, by = "tailnum") %>% 
  left_join(airports, by = c("dest" = "faa"))

# subset_datatsets_by_airport ---------------------------------------------

# create individual datasets from flights and weather to ensure the correct weather information is joined to the correct airport data, this avoids any conflicts where flights have departed at the same time

ewr_flights <- flights_joined %>% 
  filter(origin == "EWR")
jfk_flights <- flights_joined %>% 
  filter(origin == "JFK")
lga_flights <- flights_joined %>% 
  filter(origin == "LGA")

ewr_weather <- weather %>% 
  filter(origin == "EWR")
jfk_weather <- weather %>% 
  filter(origin == "JFK")
lga_weather <- weather %>% 
  filter(origin == "LGA")

# second_joins ------------------------------------------------------------

# join the subset datasets

ewr_flights_joined <- ewr_flights %>% 
  left_join(ewr_weather, by = "time_hour")
ewr_flights_joined

jfk_flights_joined <- jfk_flights %>% 
  left_join(jfk_weather, by = "time_hour")
jfk_flights_joined

lga_flights_joined <- lga_flights %>% 
  left_join(lga_weather, by = "time_hour")
lga_flights_joined

# bind_datasets -----------------------------------------------------------

flights_joined <- bind_rows(ewr_flights_joined, jfk_flights_joined, lga_flights_joined)

# final_clean -------------------------------------------------------------

# final round of cleaning on the joined dataset, the focus here is to reorder the data so the information is in a logical order

   