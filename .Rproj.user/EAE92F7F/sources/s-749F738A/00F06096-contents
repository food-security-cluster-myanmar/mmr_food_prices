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

## 3. Price shocks and volatility
# I'm not really sure what to do with this section. It feels unfinished

Price volatility, represented below by the standard deviation in prices, has been most pronounced amongst palm oil, pulses and tomatoes, with tomatoes displaying particularly extreme volatility in 2022. 

It is currently unclear whether this is due to its nature as a crop or whether this has been a general trend amongst other fruits and vegetables. Onions, which were one of the few other commodities whose prices have been measured sufficiently, has only displayed price fluctuations similar to tomatoes in Kayah and Shan North.  

Below, volatility is plotted against price. For most commodities, prices



prices %>% 
  filter(pricetype == "Retail" & unit %in% c("KG", "L")) %>% 
  select(commodity, admin1, admin2, date, price, market, year) %>% 
  rbind(mau %>% select(-filename)) %>% 
  filter(year >= 2021) %>%  
  group_by(commodity) %>% 
  mutate(count = sum(!is.na(commodity))) %>% 
  ungroup() %>% 
  filter(price > 20) %>%
  group_by(date, commodity, admin2, admin1) %>% 
  group_by(commodity, year) %>% 
  mutate(count = ifelse(!is.na(commodity), 1, 0)) %>% 
  ungroup() %>% 
  filter(!is.na(count)) %>%
  filter(commodity %in% c("Salt", "Tomatoes (local)", "Rice (low quality)", 
                          "Pulses", "Oil (palm)", "Onions (local)")) %>%
  mutate(com_year = paste0(commodity, " - ", year),
         com_year = fct_relevel(com_year, 
                                c("Salt - 2021", 
                                  "Salt - 2022", 
                                  "Onions (local) - 2021", 
                                  "Onions (local) - 2022", 
                                  "Rice (low quality) - 2021", 
                                  "Rice (low quality) - 2022", 
                                  "Tomatoes (local) - 2021",
                                  "Tomatoes (local) - 2022", 
                                  "Pulses - 2021", 
                                  "Pulses - 2022", 
                                  "Oil (palm) - 2021", 
                                  "Oil (palm) - 2022"))) %>%
  ggplot(aes(y = price, x = com_year, colour = commodity)) +
  geom_jitter(alpha = .05, 
              aes(size = price)) + 
  geom_boxplot(alpha = .5, 
               outlier.alpha = .1,
               size = 1.1) +
  scale_y_log10(labels = comma, 
                breaks = c(0, 100, 300, 600, 1000, 2000, 
                           3000, 6000, 9000)) + 
  scale_colour_manual(values = c("#F564E3", "#00BA38", "#619CFF", 
                                 "#F8766D", "#B79F00", "#00BFC4")) + 
  labs(x = "", 
       y = "Price in MMK", 
       colour = "", 
       title = "Price volatility amongst commonly surveyed commodities", 
       caption = "Data sources: WFP and Mercy Corps MAU") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 60, hjust = 1), 
        strip.text = element_text(size = 10, face = "bold"), 
        strip.background = element_rect(fill = "#212121"))

```