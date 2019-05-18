library(tidyverse)
library(lubridate)
library(ggthemes)
library(lfe)
library(broom)

auction_key <- read_rds("build/input/PPS/auction_key.rds")
auction_winbid <- read_rds("build/input/PPS/auction_winbid.rds")
dmndinst <- read_rds("build/input/PPS/dmndinst.rds")
regional_threshold <- read_rds("build/input/PPS/regional_threshold.rds")

regional_dmndinst <- dmndinst %>%
  filter(jrsdctnDivNm %in% c("지자체", "교육기관", "지방공기업", "지자체 출자출연기관"))

regional_auction_key <- semi_join(auction_key, regional_dmndinst, by = c("dmndinsttcd" = "dminsttCd"))
regional_auction_key <- regional_auction_key %>%
  distinct()

regional_auction_data <- inner_join(regional_auction_key, auction_winbid, by = c("bidntceno", "bidntceord"))

regional_auction_data %>%
  filter(opendatetime <= ymd("20090315")) %>%
  mutate(side = if_else(presmptprce < 10e9, "left", "right")) %>%
  ggplot() +
  
  geom_vline(xintercept = 10e9, linetype = "dashed") +
  geom_point(aes(x = presmptprce, y = bidprcamt/presmptprce, color = side), alpha = 0.5) +
  # geom_smooth(aes(x = presmptprce, y= bidprcamt/presmptprce, color = side), method = "loess") +
  stat_summary_bin(aes(x = presmptprce, y = bidprcamt/presmptprce),
                   binwidth = 0.5e9,
                   fun.y = mean,
                   fun.ymax = function(x) quantile(x, 0.75),
                   fun.ymin = function(x) quantile(x, 0.25),
                   geom = "pointrange") +
  
  scale_x_continuous(name = "Presumptive Price (bil KRW)",
                     limit = c(7e9, 13e9), breaks = seq(7e9, 13e9, 0.5e9),
                     labels = function(x) x/1e9) +
  scale_y_continuous(name = "Winning Price/Presumptive Price",
                     limit = c(0.7, 1.1), breaks = seq(0.7, 1.1, 0.1),
                     sec.axis = dup_axis()) +
  scale_color_few(palette = "Dark") +
  
  guides(color = FALSE) +
  theme_few(base_size = 12, base_family = "sans")

national_dmndinst <- dmndinst %>%
  filter(jrsdctnDivNm %in% c("국가기관", "공기업", "준정부기관", "기타공공기관"))

national_auction_key <- semi_join(auction_key, national_dmndinst, by = c("dmndinsttcd" = "dminsttCd")) %>%
  distinct()

national_auction_data <- inner_join(national_auction_key, auction_winbid, by = c("bidntceno", "bidntceord"))

pdf(file = "paper-slides/figure/winprice.pdf", width = 12, height = 9, family = "sans")
national_auction_data %>%
  mutate(side = if_else(presmptprce < 10e9, "left", "right")) %>%
  ggplot() +
  geom_vline(xintercept = 10e9, linetype = "dashed") +
  geom_point(aes(x = presmptprce, y = bidprcamt, color = side), alpha = 0.5) +
  # geom_smooth(aes(x = presmptprce, y= bidprcamt/presmptprce, color = side), method = "loess") +
  stat_summary_bin(aes(x = presmptprce, y = bidprcamt),
                   shape = 15, size = 1,
                   binwidth = 0.25e9,
                   fun.y = median,
                   fun.ymax = function(x) quantile(x, 0.75),
                   fun.ymin = function(x) quantile(x, 0.25),
                   geom = "pointrange") +
  
  scale_x_continuous(name = "Presumptive Price (bil KRW)",
                     breaks = seq(7e9, 13e9, 0.5e9),
                     labels = function(x) x/1e9) +
  scale_y_continuous(name = "Winning Price (bil KRW)",
                     breaks = seq(5e9, 13e9, 1e9),
                     labels = function(x) x/1e9,
                     sec.axis = dup_axis()) +
  scale_color_few(palette = "Medium") +
  
  coord_cartesian(xlim = c(7e9, 13e9),
                  ylim = c(5e9, 13e9),
                  expand = FALSE) +
  
  guides(color = FALSE) +
  theme_few(base_size = 12, base_family = "sans")
dev.off()

national_auction_data %>%
  rowid_to_column() %>%
  mutate(year = year(opendatetime)) %>%
  felm(formula = I(bidprcamt/10e9) ~ poly(presmptprce/10e9, degree = 1, raw = TRUE) + I(presmptprce >= 10e9) | year | 0 | rowid,
       subset = between(presmptprce, 9.5e9, 10.5e9),
       exactDOF = TRUE) %>%
  summary()

national_auction_data %>%
  rowid_to_column() %>%
  mutate(year = year(opendatetime)) %>%
  felm(formula = I(bidprcamt/10e9) ~ poly(presmptprce/10e9, degree = 1, raw = TRUE) + I(presmptprce >= 10e9) | year | 0 | 0,
       subset = between(presmptprce, 9.5e9, 10.5e9)) %>%
  summary(robust = TRUE)


national_auction_data %>%
  rowid_to_column() %>%
  mutate(year = year(opendatetime)) %>%
  felm(formula = I(bidprcamt/10e9) ~ poly(presmptprce/10e9, degree = 1, raw = TRUE) + I(presmptprce >= 10e9) | year | 0 | rowid,
       subset = between(presmptprce, 9.5e9, 10.5e9)) %>%
  stargazer(type = "text")

national_auction_data %>%
  mutate(year = year(opendatetime)) %>%
  felm(formula = I(bidprcamt/10e9) ~ poly(presmptprce/10e9, degree = 3, raw = TRUE) + I(presmptprce >= 10e9) | year + dmndinsttcd | 0 | rowid,
       subset = between(presmptprce, 7e9, 13e9)) %>%
  stargazer(type = "text")


