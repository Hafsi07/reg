require(tidyverse)
require(readxl)

datco2 <- readxl::read_xlsx("Class_Lab/CFLab01/Data/co2data_countryyear.xlsx", skip = 1)

glimpse(datco2)

datco2 %>% glimpse()

datco2 %>%
  select(other_industry_co2) %>%
  summarise(nn = sum(is.na(other_industry_co2)), n = length(other_industry_co2))

datco2 %>%
  select(country, year, other_industry_co2) %>%
  filter(!is.na(other_industry_co2))

datco2 <- datco2 %>% select(-other_industry_co2)

datco2 %>% select(country, year, population, gdp)

datco2 %>% select(country:gdp)
datco2 %>% select(-(country:gdp))

comb <- datco2 %>%
  select(country:co2, total_ghg, methane, primary_energy_consumption)

numerical_vars <- datco2 %>% select(where(is.numeric))
N_Numerical_vars <- datco2 %>% select(!where(is.numeric))

italian_segment <- datco2 %>% filter(iso_code == "ITA")

datco2 %>%
  filter(iso_code == "ITA" | iso_code == "FRA", between(year, 2000, 2010))

datco2 %>%
  filter(iso_code %in% c("ITA", "LUX", "FRA"), between(year, 2000, 2010)) %>% view()

datco2 %>% group_by(iso_code) %>% summarise(mgdp = mean(gdp))

datco2 %>%
  mutate(gdp_pc = gdp / population) %>%
  group_by(iso_code) %>%
  summarise(mgdp = mean(gdp_pc))

comb %>%
  mutate(gdp_pc = gdp / population) %>%
  ggplot() +
  geom_boxplot(aes(x = iso_code, y = gdp_pc, fill = iso_code)) +
  labs(x = "Country", y = "gdp per capita", fill = "Country") +
  scale_fill_manual(values = c("black", "red", "green")) +
  theme_dark()
