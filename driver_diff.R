box::use(dplyr[...],
         readr[read_csv, cols, col_character, col_double, col_date],
         functions/scrape[...],
         ggplot2[...])

f1 <- read_csv("Data/f1.csv", col_types = cols(
  .default = col_character(),
  f_result = col_double(),
  points = col_double(),
  Laps = col_double(),
  date = col_date(format = ""),
  quali_result = col_double(),
  quali_laps = col_double(),
  fp3_result = col_double(),
  fp3_laps = col_double(),
  fp2_result = col_double(),
  fp2_laps = col_double(),
  fp1_result = col_double(),
  fp1_laps = col_double(),
  year = col_double()
))

f1_2021 <- read_csv("Data/f1_2021.csv", col_types = cols(
  .default = col_character(),
  f_result = col_double(),
  points = col_double(),
  Laps = col_double(),
  date = col_date(format = ""),
  quali_result = col_double(),
  quali_laps = col_double(),
  fp3_result = col_double(),
  fp3_laps = col_double(),
  fp2_result = col_double(),
  fp2_laps = col_double(),
  fp1_result = col_double(),
  fp1_laps = col_double(),
  year = col_double()
))

f1 <- f1 %>% 
  bind_rows(f1_2021)

yearly_diff <- f1 %>% 
  group_by(Driver, year, Car) %>% 
  summarise(avg_result = mean(f_result, na.rm = TRUE)) %>% 
  arrange(Car, year) %>% 
  group_by(Car, year) %>% 
  mutate(first_driv = ifelse(avg_result == max(avg_result), 1, 0)) %>% 
  group_by(Car, first_driv, year) %>% 
  summarise(Driver, avg_result = mean(avg_result)) %>% 
  distinct() %>% 
  arrange(Car, year) %>% 
  group_by(Car, year) %>% 
  mutate(diff = avg_result - lag(avg_result)) %>% 
  select(Car, year, diff) %>% 
  na.omit() %>% 
  ungroup()
  
yearly_diff$Car %>% unique()
yearly_diff %>% 
  filter(grepl("^Red Bull", Car)) %>%
  group_by(year, Car) %>% 
  summarise(diff = mean(diff)) %>% 
  # filter(year > 2014) %>% 
  ggplot(aes(factor(year), diff)) +
  geom_col(fill = "#2745a5") +
  # scale_x_continuous(limits = c(2010, 2021)) +
  labs(title = "Difference between average race finish \nfor fist and second driver",
       x = "", y = "") +
  theme_minimal() +
  theme(plot.margin = margin(0.2, 1, 0.2, 0.2, unit = "cm")) #+
  # facet_wrap(~Car)
  






