library(tidyverse)
library(lubridate)
library(sparklyr)

sc <- spark_connect("local")
bid_conn <- spark_read_csv(sc, name = "bid", "build/input/PPS/auction_bid.csv")

# Import bid key files
keyfiles <- list.files("~/Tasks/KoreaProcurement/build/temp/awardedConstructStandard/key", full.names = TRUE)

key <- keyfiles %>%
  map_dfr(read_csv,
          col_types = rep("c", 19) %>% paste0(collapse = ""))

key %>%
  mutate_at(.vars = vars(sucsflwstlmtrt, presmptprce, rsrvtnprce, bssamt),
            .funs = as.numeric) %>%
  mutate(opendatetime = paste(opengdate, opengtm) %>% ymd_hm()) %>%
  select(bidntceno, bidntceord,
         bsnsdivnm:bidwinrdcsnmthdnm,
         ntceinsttcd, dmndinsttcd,
         sucsflwstlmtrt:bssamt,
         opendatetime, opengrsltdivnm) %>%
  drop_na(presmptprce) %>%
  filter(presmptprce != 0) %>%
  
  write_rds(path = "build/input/PPS/auction_key.rds",
            compress = "gz")


# Import demand institutions and supply institutions
dmndinst <- read_csv(file = "~/Tasks/KoreaProcurement/build/output/demandInst.csv",
                     col_types = rep("c", 28) %>% paste0(collapse = "")) 

write_rds(dmndinst,
          path = "build/input/PPS/dmndinst.rds",
          compress = "gz")

supplyinst <- read_csv(file = "~/Tasks/KoreaProcurement/build/output/supplyInst.csv")

write_rds(supplyinst, 
          path = "build/input/PPS/supplyinst.rds",
          compress = "gz")

# Regulations
municipal <- tribble(
  ~date, ~municipal,
  19950706, 2e+09,
  19970101, 3e+09,
  20031212, 5e+09,
  20060102, 7e+09,
  20090316, 10e+09
)

national <- tribble(
  ~date, ~national,
  19950706, 2e+09,
  19970101, 3e+09,
  20031212, 5e+09,
  20090305, 7.6e+09,
  20110101, 9.5e+09,
  20130101, 8.7e+09,
  20150101, 8.2e+09,
  20170101, 8e+09,
  20190101, 7.8e+09
)

regional_threshold <- full_join(municipal, national, by = "date") %>%
  mutate(date = ymd(date)) %>%
  arrange(date) %>%
  fill(municipal, national, .direction = "down")

write_rds(regional_threshold,
          path = "build/input/PPS/regional_threshold.rds",
          compress = "gz")



# Import winning bid information
bidfiles <- list.files("~/Tasks/KoreaProcurement/build/temp/awardedConstructStandard/bid", full.names = TRUE)

winbid <- bidfiles %>%
  map_dfr(function(file){
    read_csv(file, col_types = rep("c", 21) %>% paste0(collapse = "")) %>%
      select(bidntceno, bidntceord, bidprccorpbizrno, bidprcamt, bidprcrt, sucsfyn) %>%
      filter(sucsfyn == "Y") %>%
      mutate_at(.vars = vars(bidprcamt, bidprcrt),
                .funs = as.numeric)
  })

write_rds(winbid,
          path = "build/input/PPS/auction_winbid.rds",
          compress = "gz")

# Import N of participants
bidfiles <- list.files("~/Tasks/KoreaProcurement/build/temp/awardedConstructStandard/bid", full.names = TRUE)

Nbid <- bidfiles %>%
  map_dfr(function(file){
    read_csv(file, col_types = rep("c", 21) %>% paste0(collapse = "")) %>%
      count(bidntceno, bidntceord)
  })

Nbid %>%
  rename("Nbid" = n) %>%
  write_rds(path = "build/input/PPS/auction_nbid.rds",
            compress = "gz")


