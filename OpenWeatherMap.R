install.packages("owmr")
library(owmr)
library(rjson)

Sys.setenv(OWM_API_KEY = '2573f41bfe406d7fe6528f2615d667c1')

#json_file <- "/Users/kieranschubert/Downloads/city.list.json"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))
#json_data %>% as.data.frame %>% head

geneva <- get_current("Geneva, CH", units = "metric") %>%
    owmr_as_tibble()

# other cities named Geneva
geneva_info <- search_city_list("Geneva") %>%
  as.list()

# Weather Forecast
forecast <- get_forecast("Sestri Levante, IT", units = "metric") %>%
  owmr_as_tibble()

forecast

# apply funcs to some columns
funcs <- list(
  temp = round,
  wind_speed = round
)

forecast %<>% parse_columns(funcs)

# do some templating ...
("{{dt_txt}}h {{temp}}°C, {{wind_speed}} m/s" %$$%
    forecast) %>% head(10)

