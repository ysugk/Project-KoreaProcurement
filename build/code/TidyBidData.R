library(tidyverse)
library(lubridate)

dates <- list.files("~/Tasks/KoreaProcurement/build/temp/awardedConstructStandard",
           pattern = "*.csv",
           recursive = FALSE) %>%
  str_remove(".csv$") %>%
  ymd() %>%
  subset(. >= ymd("2013-12-06") & . < ymd("2014-01-01")) %>%
  str_remove_all("-")

path <- paste0("~/Tasks/KoreaProcurement/build/temp/awardedConstructStandard/", dates, ".csv")

read_and_write <- function(f){
  print(f)
  read_csv(f, col_types = rep("c", 38) %>% paste0(collapse = "")) %>%
    select(-starts_with("fnlsucs")) %>%
    write_csv(f)
}

walk(path, read_and_write)

