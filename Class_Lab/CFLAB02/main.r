require(tidyverse)
require(magrittr)

cov <- "Data/healthcare-coverage.csv"
spe <- "Data/healthcare-spending.csv"

datc <- read_csv(cov, skip = 2)
dats <- read_csv(spe, skip = 2, n_max = 52)

read_lines(cov, n_max = 10)

datc <- datc %>% slice_tail(n = 52)

spec(datc)
problems(data)

str(datc)

save(datc, dats, file = "Data/practice.rda")

load("Data/practice.rda")

datc %>% view()

datc %>%
    mutate(across(where(is.character),\(x) x[x=='N/a'] <- NA)) %>%
    glimpse

datc2 <- datc %>% mutate(across(where(is.character),\(x) x[x=='N/a'] <- NA))


datc2 %>% mutate(totorsingle=ifelse(type=="Total","Total","Non Total")) %>%
    group_by(Location, year, totorsingle) %>%
    summarise(covsum=sum(coverage, na.rm = T))
