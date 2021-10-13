flights_weather_join_fn <- function(dataset1, dataset2){
  
  ewr_flights <- dataset1 %>% 
    filter(origin == "EWR")
  jfk_flights <- dataset1 %>% 
    filter(origin == "JFK")
  lga_flights <- dataset1 %>% 
    filter(origin == "LGA")
  
  ewr_weather <- dataset2 %>% 
    filter(origin == "EWR")
  jfk_weather <- dataset2 %>% 
    filter(origin == "JFK")
  lga_weather <- dataset2 %>% 
    filter(origin == "LGA")
  
  ewr_flights_joined <- ewr_flights %>% 
    left_join(ewr_weather, by = "time_hour")
  jfk_flights_joined <- jfk_flights %>% 
    left_join(jfk_weather, by = "time_hour")
  lga_flights_joined <- lga_flights %>% 
    left_join(lga_weather, by = "time_hour")
  
  flights_joined <- bind_rows(
    ewr_flights_joined, jfk_flights_joined, lga_flights_joined)
  
  flights_joined <<- flights_joined
  
}

flights_weather_join_fn(flights, weather)

weather_filter_fn <- function(dataset){
  ewr_weather <- dataset %>% 
    filter(origin == "EWR")
  jfk_weather <- dataset %>% 
    filter(origin == "JFK")
  lga_weather <- dataset %>% 
    filter(origin == "LGA")
  
  ewr_weather <<- ewr_weather
  jfk_weather <<- jfk_weather
  lga_weather <<- lga_weather
}

weather %>% 
  weather_filter_fn()

join_fn <- function(dataset1 = ewr_flights, dataset2 = ewr_weather, dataset3 = jfk_flights, 
                    dataset4 = jfk_weather, dataset5 = lga_flights, dataset6 = lga_weather){
  ewr_flights_joined <- dataset1 %>% 
    left_join(dataset2, by = "time_hour")
  jfk_flights_joined <- dataset3 %>% 
    left_join(dataset4, by = "time_hour")
  lga_flights_joined <- dataset5 %>% 
    left_join(dataset6, by = "time_hour")
  
  flights_joined <- bind_rows(
    ewr_flights_joined, jfk_flights_joined, lga_flights_joined)
  
  flights_joined <<- flights_joined
}

join_fn()
