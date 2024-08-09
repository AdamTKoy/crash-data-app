library(tidyverse)

bad_drivers = as_tibble(read.csv("data/bad-drivers.csv"))

bad_drivers = bad_drivers %>%
  rename(Fatalities = Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,
         Speeding = Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,
         Alcohol = Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,
         pct_not_distracted = Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted,
         pct_no_prior = Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents) %>%
  mutate(State = as.factor(State)) %>%
  mutate(Distracted = 100 - pct_not_distracted) %>%
  mutate(Prior = 100 - pct_no_prior) %>%
  select(State, Fatalities, Speeding, Alcohol, Distracted, Prior) %>%
  arrange(desc(State))

write_csv(x = bad_drivers, file = "data/bd.csv")
