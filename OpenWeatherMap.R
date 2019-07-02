install.packages("owmr")
library(owmr)

Sys.setenv(OWM_API_KEY = '2573f41bfe406d7fe6528f2615d667c1')

res <- get_current("London", units = "metric") %>%
    owmr_as_tibble() %>% names()

rio <- search_city_list("Rio de Janeiro") %>%
  as.list()

forecast <- get_forecast("London", units = "metric") %>%
  owmr_as_tibble()

# apply funcs to some columns
funcs <- list(
  temp = round,
  wind_speed = round
)
forecast %<>% parse_columns(funcs)

# do some templating ...
("{{dt_txt}}h {{temp}}°C, {{wind_speed}} m/s" %$$%
    forecast) %>% head(10)