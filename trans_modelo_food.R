library(readr)
food <- read_csv("global-food-prices-database-wfp-1.csv")
library(naniar)
library(tidyverse)
library(lubridate)
library(quantmod)
options(scipen = 999)


miss_var_summary(food)
unique(food$ID) %>% length()
food$mp_commoditysource<-NULL

food %>% 
  filter(adm0_name  == "Peru") %>% 
  select(mkt_name) %>% .$mkt_name %>% table

food$pt_name %>% table

food$fecha <- paste0(food$mp_year, "-", sprintf("%02d", food$mp_month))

food$fecha <- ymd(paste0(food$fecha, "-01"))
food$mp_month <- NULL
food$mp_year <- NULL

Ubicacion <- food %>% 
  select(adm0_name, adm1_name, mkt_name) %>% 
  distinct() %>% 
  mutate(ID_Ubi = 1001:(1001 + nrow(.) - 1))


food <- merge(food, Ubicacion, by = c("adm0_name", "adm1_name", "mkt_name"), all.x = TRUE) %>% select(-(1:6))

unique(food$cur_name)

ex_rate<- data.frame(
  Moneda = c("AFN", "DZD", "AMD", "AZN", "BDT", "INR", "XOF", "BTN", "BOB", "BIF", "KHR", "XAF", "CVE", "COP", "CDF", "DJF", "DOP", "EGP", "ETB", "GMD", "GEL", "GHS", "GTQ", "GNF", "HTG", "IDR", "IRR", "IQD", "JOD", "KES", "KGS", "LAK", "LBP", "LSL", "LRD", "LYD", "MGA", "MWK", "MRO", "MZN", "MMK", "NPR", "NGN", "PKR", "PEN", "PHP", "RWF", "SLL", "SOS", "SSP", "LKR", "NIS", "SDG", "SZL", "SYP", "TJS", "TRY", "UGX", "UAH", "TZS", "YER", "ZMW", "USD"),
  CambioUSD = c(0.01400, 0.00740, 0.00250, 0.59000, 0.00910, 0.01200, 0.00160, 0.01200, 0.14000, 0.00035, 0.00025, 0.00160, 0.00960, 0.00025, 0.00036, 0.00560, 0.01700, 0.02100, 0.01800, 0.01500, 0.38000, 0.07400, 0.13000, 0.00012, 0.00750, 0.00006, 0.00002, 0.00076, 1.41000, 0.00750, 0.01100, 0.00005, 0.00001, 0.05200, 0.00518, 0.21000, 0.00023, 0.00057, 0.02500, 0.01600, 0.00048, 0.00750, 0.00087, 0.00360, 0.27000, 0.01700, 0.00077, 0.27000, 0.00170, 0.00768, 0.00330, 0.27000, 0.73000, 0.05200, 0.00008, 0.09200, 0.03100, 0.00026, 0.02500, 0.00039, 0.00650, 0.03900, 1)
)


food$price <- food$mp_price * ex_rate$CambioUSD[match(food$cur_name, ex_rate$Moneda)]

food$cur_id <- NULL
food$cur_name <- NULL
food$mp_price <- NULL
food$pt_id <- NULL
food$um_id <- NULL

food$cm_name <- sub(" -.*", "", food$cm_name)

productos <- food %>%
  mutate(variante = str_match(cm_name, "\\((.*?)\\)")[,2]) %>% mutate(variante = str_to_title(variante)) %>% 
  mutate(
    producto = sapply(strsplit(cm_name, "\\("), `[`, 1)) %>% select(cm_id, producto, variante) %>% distinct() %>% rename(ID_prod = cm_id) %>% arrange (ID_prod)

food <- food %>% 
  rename(ID_prod = cm_id)

food %>% 
  arrange(ID_prod,cm_name)

food$cm_name<- NULL



food <- food %>% rename(unidades = um_name)
food <- food %>% rename(precio = price)
food <- food %>% rename(medio_compra = pt_name)


Ubicacion %>% rename(Pais = adm0_name , Ciudad = adm1_name , Region = mkt_name ) -> Ubicacion
food$ID <- 1:nrow(food)

write_csv(file = "fact_precio.csv", food)
write_csv(file = "ubicacion_food.csv", Ubicacion)
write_csv(file = "productos_food.csv", productos)
