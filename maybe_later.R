prices %>% filter(commodity %in% c("Eggs (local)", "Tomatoes (local)")) %>%  
  filter(pricetype == "Retail") %>%  
  group_by(date, admin1, commodity) %>% 
  summarise(price = mean(price))


# Ok, this is a pretty big red flag -- eggs and tomatoes had the same price until 2021-10-15
# Who do you ask in wfp? 


prices %>% 
  filter(pricetype == "Retail") %>% 
  select(commodity, admin1, admin2, date, price, market, year) %>% 
  rbind(mau %>% select(-filename)) %>% 
  filter(year >= 2021 & price > 20) %>%  
  group_by(date, commodity, admin1, admin2) %>% 
  summarise(price = mean(price)) %>% 
  arrange(date, commodity) %>% 
  group_by(commodity, admin1, admin2) %>% 
  mutate(mom = (price - lag(price)) / lag(price)) %>%  
  filter(!is.na(mom)) %>%
  filter(commodity %in% c("Salt", "Tomatoes (local)", "Rice (low quality)", 
                          "Pulses", "Oil (palm)", "Onions (local)")) %>% 
  mutate(direction = ifelse(mom > 0, "increase", "decrease"), 
         count = 1) %>% 
  group_by(commodity, admin1) %>% 
  mutate(total = sum(count), 
         pc = count / total)