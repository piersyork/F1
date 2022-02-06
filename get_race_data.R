options(box.path = getwd())
box::use(dplyr[...],
         magrittr[...],
         functions/scrape[...],
         readr[write_csv])

get_standings()
get_standings(drivers = FALSE)
get_latest_race()

f1 <- get_races(2010:2020)

f1 <- as_tibble(f1) %>% 
  mutate(winner = if_else(f_result == 1, 1, 0),
         winner = if_else(is.na(winner), 0, winner))

write_csv(f1, "Data/f1.csv")

f1 %>% 
  filter(winner == 1) %>% 
  count(Driver) %>% 
  arrange(n)


f1_2021 <- get_races(2021)

readr::write_csv(f1_2021, "Data/f1_2021.csv")

f1_2021$Car %>% unique()
f1_2021 %>% 
  filter(Car == "Ferrari") %$%
  table(Car, f_result)
f1_2021 %>% 
  filter(Car == "Ferrari") 


get_latest_race()



