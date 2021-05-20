library(tidyverse)
library(readr)

# Cases in BC and Yukon since January 1st, 2021
CasesPHAC <- read_csv("COVID19-eng-may14th.csv") %>% filter (COV_EY == 21 & COV_REG == 5) %>% 
  select(COV_ID, COV_AGR, COV_HSP, COV_DTH) %>% rename (ID = COV_ID, age=COV_AGR, hosp = COV_HSP, death=COV_DTH)

CasesPHACCount <- CasesPHAC %>% group_by(age) %>% tally()

deathPHAC <- CasesPHAC %>% group_by(age) %>% filter(death==1) %>% tally() %>% 
  rename (IFR=n)

# Hospitalization, both ICU and non-ICU
hospPHAC <- CasesPHAC %>% group_by(age) %>% filter(hosp==1 | hosp==2) %>% tally()  %>% 
  rename (IHR=n) 

IFRIHR <- CasesPHACCount %>% left_join(deathPHAC) %>% left_join(hospPHAC)%>% 
  mutate(IFR = IFR/n, IHR = IHR/n) %>% filter(age != 99) %>% 
  mutate (age = case_when(
    age == 1 ~ "0-19",
    age == 2 ~ "20-29",
    age == 3 ~ "30-39",
    age == 4 ~ "40-49",
    age == 5 ~ "50-59",
    age == 6 ~ "60-69",
    age == 7 ~ "70-79",
    age == 8 ~ "80+",
  ))

# Missing IFRs added from BCCDC Report May 12 2021 Week 17
IFRIHR$IFR[1] = 9.559772e-05
IFRIHR$IFR[2] = 3.345265e-05
IFRIHR$IFR[3] = 0.0005741941
IFRIHR$IFR[4] = 0.0009663801

saveRDS(IFRIHR$IFR, "./data/ifr_vec_CAN.rds")
saveRDS(IFRIHR$IHR, "./data/ihr_vec_CAN.rds")