library(Cairo)
source('./analysis/setup.R')

# parameter space
pars <- crossing(R=c(1.15, 1.35, 1.4, 1.5), ve = c(0.60, 0.75, 0.9), vp = 0.80, scen=c(1,2))

# RUN (according to piecewise scenario)
res <- pars %>%  future_pmap_dfr(run_over_scen_2, .progress=TRUE)

############################
# FIGURE 1 (trajectories)
#############################
# Look at trajectories
trajA <- compare_sims(sim1 = filter(res, R==1.35 & scen==1 & ve==0.60), 
                             sim2=filter(res, R==1.35 & scen ==2 & ve==0.60),
                             name1=labels[1], name2=labels[2], startDate=startDate, 
                             textsize = 16)

# trajB <- compare_sims(sim1 = filter(res, R==1.3 & scen==3 & ve==0.75), 
#                              sim2=filter(res, R==1.3 & scen==4 & ve==0.75),
#                              name1=labels[3], name2=labels[4], startDate=startDate, 
#                              textsize = 16)

# Look at number vaccinated
gg_vax <- res %>% 
    filter(R==1.15) %>% # note: all R values will give approx. the same plot
    group_by(type)%>% 
    nest()%>%
    summarize(plot=map(data, display_prop_vax, startDate, type, textsize=16))


fig1a = ggarrange(gg_vax$plot[[1]]+
                    labs(title = "", subtitle = 'A: 80+, 70-79, FLW, 60-69, ...') +
                    scale_color_viridis(discrete = T)+
                    ylab ("prop. vaccinated") +
                    theme_ipsum_rc(grid="XY") + 
                    theme(axis.title.x = element_blank(),text=element_text(size=16))
                    ,
                  gg_vax$plot[[2]]+labs(title = "", subtitle='B: 80+, 70-79, 60-69, FLW, 50-59, ...') +
                    scale_color_viridis(discrete = T)+
                    ylab ("prop. vaccinated") +
                    theme_ipsum_rc(grid="XY"),
                  ncol=2, nrow=1, common.legend=TRUE, legend="bottom")
saveRDS(fig1a, "fig1a.rds")

# Arrange
fig1b = ggarrange(ggarrange(plotlist=trajA, nrow=2, ncol=2, widths = c(1,1,1,1),
                            common.legend=TRUE, legend="bottom"),
           #ggarrange(plotlist=trajB, nrow=1, ncol=4, widths = c(1,1,1,1)),
           nrow=1)

saveRDS(fig1b, "fig1b.rds")
ggsave("figures/fig-trajectories.pdf", width = 45, height = 10, device = cairo_pdf)      

ggarrange(fig1a, fig1b, nrow=2,heights = c(1, 1.6))
ggsave("figures/fig-trajectoriesFull.pdf", width = 15, height = 10, device = cairo_pdf)      


##########################
# FIG 2 (bar plots)
###########################
tsize <- 16

#---SUMMARIZE
res2 <- res %>% 
    group_by(type, R, ve,vp) %>%
    nest() %>%
    summarize(cases=map_dbl(data, total_cases),
              hosp=map_dbl(data, total_hosp,hosp_efficacy=rep(vp, 9)),
              deaths=map_dbl(data, total_deaths),
              long = map_dbl(data, total_long, hosp_efficacy=rep(vp, 9)),
              t_turn = map_dbl(data, time_to_decr),
              cases_turn = map_dbl(data,cases_immunity),
              vax_immunity = map_dbl(data, vax_immunity)
)


saveRDS(res2, "res2.rds")

R_vec <- c(1.15,1.35) # R vals to plot

g1 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=cases, group=type, fill=type))+
  geom_col(position='dodge', alpha=1)+ 
  #scale_fill_brewer(palette = "Dark2")+ 
  facet_wrap(~ R,  ncol=4,labeller=label_both)+
  #scale_color_brewer(palette = "Dark2")+
  theme(text=element_text(size=16))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  labs(x='Efficacy against Infection', y='Infections', fill='Scenario') +
            theme(text=element_text(size=tsize))+
            theme(panel.spacing.x=unit(1.5, "lines"),
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.60,0.75,0.9)) +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum_rc(grid="Y")

g2 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=hosp, group=type, fill=type))+
  geom_col(position='dodge',alpha=1)+ 
  #scale_fill_brewer(palette = "Dark2")+ 
  facet_wrap(~ R,  ncol=4,labeller=label_both)+
  #scale_color_brewer(palette = "Dark2")+
  theme(text=element_text(size=tsize))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  labs(x='Efficacy against Infection', y='Hospitalizations', fill='Scenario') +
            theme(text=element_text(size=16))+theme(panel.spacing.x=unit(1.5, "lines") ,
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.60,0.75,0.9)) + theme_ipsum_rc(grid="Y")

g3 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=deaths, group=type, fill=type))+
  geom_col(position='dodge',alpha=1)+ 
  #scale_fill_brewer(palette = "Dark2")+ 
  facet_wrap(~ R,  ncol=4, labeller=label_both)+
  #scale_color_brewer(palette = "Dark2")+
  theme(text=element_text(size=16))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  labs(x='Efficacy against Infection', y='Deaths', fill='Scenario') +
            theme(text=element_text(size=tsize))+theme(panel.spacing.x=unit(1.5, "lines") ,
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.60,0.75,0.9)) + theme_ipsum_rc(grid="Y")

g4 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=long, group=type, fill=type))+
  geom_col(position='dodge',alpha=1)+ 
  #scale_fill_brewer(palette = "Accent")+ 
  facet_wrap(~ R,  ncol=4,labeller=label_both)+
  #scale_color_brewer(palette = "Accent")+
  theme(text=element_text(size=16))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  
  labs(x='Efficacy against Infection', y='Long COVID', fill='Scenario') +
            theme(text=element_text(size=tsize))+theme(panel.spacing.x=unit(1.5, "lines") ,
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.60,0.75,0.9)) +
  theme_ipsum_rc(grid="Y") 

bars <- ggarrange(g1,g2,g3,g4, ncol=2, nrow=2, common.legend=TRUE, legend='bottom', align='v')

saveRDS(bars, "bars.rds")
ggsave('figures/fig-barplots.pdf', width=14, height=10,  device = cairo_pdf)


### Personal Risk

oo <- compare_sims_data(sim1 = filter(res, R==1.35 & scen==1 & ve==0.60), 
                                               sim2=filter(res, R==1.35 & scen ==2 & ve==0.60),
                                               name1=labels[1], name2=labels[2], startDate=startDate, 
                                               textsize = 16)

 risk60sR1 <- oo %>% filter (age_band == "60-69" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
   select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 611615
 risk60sR1$age <- "60-69"

risk50sR1 <- oo %>% filter (age_band == "50-59" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 709300
risk50sR1$age <- "50-59"

risk40sR1 <- oo %>% filter (age_band == "40-49" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 617410
risk40sR1$age <- "40-49"

risk30sR1 <-oo %>% filter (age_band == "30-39" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 607340
risk30sR1$age <- "30-39"

risk20sR1 <-oo %>% filter (age_band == "20-29" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 590560
risk20sR1$age <- "20-29"

covidRisksR1 <- rbind.data.frame(risk60sR1, risk50sR1, risk40sR1, risk30sR1, risk20sR1) %>% 
              mutate(R0 = 1.35)

ooR2 <- compare_sims_data(sim1 = filter(res, R==1.15 & scen==1 & ve==0.60), 
                          sim2=filter(res, R==1.15 & scen ==2 & ve==0.60),
                          name1=labels[1], name2=labels[2], startDate=startDate, 
                          textsize = 16)

risk60sR2 <- ooR2 %>% filter (age_band == "60-69" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                                date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 611615
risk60sR2$age <- "60-69"

risk50sR2 <- ooR2 %>% filter (age_band == "50-59" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                                date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 709300
risk50sR2$age <- "50-59"

risk40sR2 <- ooR2 %>% filter (age_band == "40-49" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                                date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 617410
risk40sR2$age <- "40-49"

risk30sR2 <-ooR2 %>% filter (age_band == "30-39" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                               date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 607340
risk30sR2$age <- "30-39"

risk20sR2 <-ooR2 %>% filter (age_band == "20-29" & scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ..." & 
                               date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 590560
risk20sR2$age <- "20-29"

covidRisksR2 <- rbind.data.frame(risk60sR2, risk50sR2, risk40sR2, risk30sR2, risk20sR2) %>% 
  mutate(R0 = 1.15)

covidRisk <- rbind(covidRisksR1, covidRisksR2) %>% 
  mutate(`VIPIT-NACI` = 8E-6, `VIPIT-EMA` = 2.73E-6) %>% 
  rename(`COVID-19` = newdeaths)

write_csv(covidRisk, "personalRisk.csv")


####### Validation
#cases <- read_csv('http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv')
cases <- readRDS("./bcCDCCases.RDS")
counts <- cases %>% filter (Reported_Date > ymd("2020-12-01")) %>% 
  group_by(Reported_Date) %>% tally()


observed <- as_tibble(counts) %>% rename(date = Reported_Date) %>%
  mutate(observed = zoo::rollmean(n, k = 7, fill = NA))


predicted <- oo %>% 
  filter(scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ...") %>% 
  group_by(date) %>% summarize(incid = sum(incid)) %>% 
  filter (date < ymd("2021-04-05") & date > ymd("2021-01-02"))

validation <- predicted %>% left_join(observed, by="date")
validation %>% 
  rename (predicted = incid) %>%
  pivot_longer(cols = c(predicted, observed), values_to = "cases") %>%
  ggplot () + geom_line(aes(y=cases, x=date, color=name), size=2) + 
  labs(subtitle = "Predicted and observed COVID-19 cases in BC",
       caption  = "Observed case counts are 7-day rolling averages") + 
  theme_ipsum_rc(grid="Y") + expand_limits (y = 0)


predictedDeath <- oo %>% 
  filter(scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ...") %>% 
  group_by(date) %>% summarize(newdeaths = sum(newdeaths)) %>% 
  filter (date < ymd("2021-04-05") & date > ymd("2021-01-02"))

observedDeath <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv") %>%
  rename (province = "prname") %>% filter (province == "British Columbia") %>%
  select(date, numdeathstoday) %>%  filter (date < ymd("2021-04-05") & date > ymd("2021-01-02")) %>%
  mutate(observedDeaths= zoo::rollmean(numdeathstoday, k = 7, fill = NA))

validationDeath <- predictedDeath %>% left_join(observedDeath, by="date")

validationDeath %>% 
  rename (predictedDeaths = newdeaths) %>%
  pivot_longer(cols = c(predictedDeaths, observedDeaths), values_to = "cases") %>%
  ggplot () + geom_line(aes(y=cases, x=date, color=name), size=2) + 
  labs(subtitle = "Predicted and observed COVID-19 deaths in BC",
       caption  = "Observed death counts are 7-day rolling averages") + 
  theme_ipsum_rc(grid="Y") + expand_limits (y = 0)

age.under.80 <- c("0-9", 
                  "10-19",
                  "20-29",
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70-79")

age.80.plus <- c("80-89", "90+")


countsPerAge <- cases %>% filter (Reported_Date > ymd("2020-12-01")) %>% 
  group_by(Reported_Date, Age_Group) %>% rename (age_band = Age_Group) %>%
  mutate(age_band=recode(age_band,"<10" = "0-9")) %>%
  mutate(age_band  = ifelse(age_band %in% age.under.80, age_band,
                            ifelse(age_band %in% age.80.plus, "80+", 
                                   NA))) %>%
  tally() 


predictedPerAge <- oo %>% 
  filter(scen == "B: 80+, 70-79, 60-69, FLW, 50-59, ...") %>% 
  group_by(date, age_band) %>% summarize(incid = sum(incid)) %>% 
  rename (predicted = incid) %>%
  filter (date < ymd("2021-04-05") & date > ymd("2021-01-02"))

observedPerAge <- as_tibble(countsPerAge) %>% rename(date = Reported_Date) %>%
  mutate(observed7day = zoo::rollmean(n, k = 7, fill = NA))

observedPerAge5daySum <- as_tibble(countsPerAge) %>% rename(date = Reported_Date) %>%
  mutate(observed5daysum = 5* zoo::rollmean(n, k = 5, fill = NA))

observedPerAge5daySum %>% filter(date==ymd("2021-01-01"))


validationPerAge <- predictedPerAge %>% 
  left_join(observedPerAge, by=c("date", "age_band"))
validationPerAge %>% pivot_longer(cols = c(predicted, observed7day), values_to = "cases") %>%
  ggplot () + geom_line(aes(y=cases, x=date, color=name), size = 1.5) + 
  labs(subtitle = "Predicted and observed cases of COVID-19 per age group in BC",
       caption  = "Observed case counts are 7-day rolling averages") + 
  facet_wrap(~age_band) +
  theme_ipsum_rc(grid="Y")

ggsave('figures/fig-validation.pdf', width=14, height=10,  device = cairo_pdf)


#####################
# QALYs and Cost
#####################
#source('./analysis/qalys_cost.R')
