prices %>% filter(commodity %in% c("Eggs (local)", "Tomatoes (local)")) %>%  
  filter(pricetype == "Retail") %>%  
  group_by(date, admin1, commodity) %>% 
  summarise(price = mean(price))


# Ok, this is a pretty big red flag -- eggs and tomatoes had the same price until 2021-10-15
# Who do you ask in wfp? 