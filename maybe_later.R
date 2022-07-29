prices %>% filter(commodity %in% c("Eggs (local)", "Tomatoes (local)")) %>%  
  filter(pricetype == "Retail") %>%  
  group_by(date, admin1, commodity) %>% 
  summarise(price = mean(price)) %>% 
  filter(date >= "2021-11-01")


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

prices %>% 
  filter(pricetype == "Retail") %>% 
  select(commodity, admin1, admin2, date, price, market, year) %>% 
  rbind(mau %>% select(-filename)) %>% 
  filter(year >= 2020 & date <= "2022-02-01") %>%  
  group_by(commodity) %>% 
  mutate(count = sum(!is.na(commodity))) %>% 
  ungroup() %>% 
  filter(count > 500 & price > 20) %>%
  group_by(date, commodity, admin1) %>% 
  summarise(price = mean(price)) %>% 
  filter(commodity %in% c("Salt", "Tomatoes (local)", "Rice (low quality)", 
                          "Pulses", "Oil (palm)", "Onions (local)")) %>% 
  group_by(commodity, admin1) %>% 
  arrange(date) %>% 
  filter(row_number() == 1 | row_number() == n()) %>%
  mutate(order = c("first", "last")) %>%
  select(-date) %>% 
  pivot_wider(names_from = order, values_from = price) %>% 
  mutate(difference = (last - first) / first, 
         # this changes the anomalous value for salt in Shan East
         difference = ifelse(difference > 2.5, 2.5, difference)) %>%
  ggplot(aes(x = difference, y = fct_rev(admin1), fill = commodity)) + 
  geom_vline(xintercept = 1, lty = 2, colour = "red") +
  geom_col(position = position_dodge(width = .9)) + 
  facet_row(vars(commodity), scales = "free_x", space = "free") + 
  scale_fill_manual(values = c("#4686fbff", "#26A69A", "#30123bff",
                               "#e4460aff", "#e4cf5bff", "#9c179eff")) +
  scale_x_continuous(labels = percent, breaks = seq(-1, 2.5, by = .5)) +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "#212121"),
        axis.text.x = element_text(size = 7)) + 
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "% change in prices since 2020", y = "",  
       title = "Changes in commodity prices, 01/2020 vs 01/2022",
       subtitle = "Prices are MMK per kg, except for palm oil which is MMK per litre; red line indicates a 100% increase",
       caption = "Data sources: WFP and Mercy Corps MAU") 

filter(str_detect(commodity, 
                  "Eggs|Tomato")) %>% 
  arrange(date) %>% 
  select(date, commodity, price, market) %>%
  pivot_wider(names_from = commodity, values_from = price) %>% 
  filter(is.na(Tomatoes)) %>% 
  select(-Tomatoes) %>% 
  mutate(duplicate = ifelse(`Eggs (local)` == `Tomatoes (local)`,
                            TRUE, 
                            FALSE)) %>% 
  write_csv("./data/egg_tomato_duplicates.csv")