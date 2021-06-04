library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(furrr)
library(cowplot)
library(lubridate)
library(ggpubr)
library(viridis)
library(hrbrthemes)

no_cores <- availableCores() - 1
plan(multicore, workers = no_cores)
theme_set(theme_light())

source('./vaccine-model.R') # main model & simulation funcs
source('./utils.R') 
source('./contact-matrix.R')

# BUILD CONTACT MATRIX
p_ess <- c(0.0,0.0,0.1705,0.2043,0.1675,0.1536,0.1642,0.1069, 0.0) 
age_demo_by_five <- as.matrix(readr::read_csv('./data/Population_Estimates.csv'))
generate_contact_matrix(p_ess, age_demo_by_five) #build

age_demo <- readRDS("./generated-data/age_demographics_essential.rds")
mu_home <- readRDS("./generated-data/mu_home_essential.rds")
mu_work <- readRDS("./generated-data/mu_work_essential.rds")
mu_school <- readRDS("./generated-data/mu_school_essential.rds")
mu_other <-  readRDS("./generated-data/mu_other_essential.rds")

# RATES (10-yr age bins, 0-9,10-19,...,80+)
YLL_vec <- readRDS("./data/yll_vec_CAN.RData")
IFR <- readRDS("./data/ifr_vec_CAN.rds")
# IFR <- #Based on BC CDC Situation Report Week 17, May 12 2021
#        c(0.000280584,
#          0,
#          3.34526E-05,
#          0.000574194,
#          0.00096638,
#          0.003171753,
#          0.01296075,
#          0.058012027,
#          0.168)


IHR <- readRDS("./data/ihr_vec_CAN.rds")
#Based on BC CDC Situation Report Week 17, May 12 2021
# IHR <- 
#    c(0.008698092,
#      0.003915029,
#      0.010537584,
#      0.025223526,
#      0.036976756,
#      0.06181927,
#      0.116095229,
#      0.243721259,
#      0.27
#    )


# Symptom duration lognormal. log sigma=0.8. log  mu are 1.9, 2.2, 2.5, 2.8,
# for ages under 30, 30-39, 40-49, over 50.
# means are 9, 12, 17 and 22
SDUR_vec = c(9,9,9, 12, 17, 22,22,22,22)  # symptom dur Sudre et al and Paul

LCR = c(0.037, 0.037, 0.037, 0.079,0.15,0.252, 0.252, 0.252, 0.252) # from Paul from Sudre
longDUR = c(rep(40.3,3), 42.5, 45.6, rep(49.7,4)) 
HOSPDUR = c(4.692308,3.380952,5.260870,4.304348,7.212598,7.874346,9.092857,12.752475,12.964346)


# ADD ESSENTIAL WORKERS TO IFR 
IFR <- c(IFR, IFR[3:8])


# GLOBAL MODEL PARAMETERS
n <- length(age_demo)
pop_total <- age_demo[n] #pop BC
age_demo <- age_demo[1:n-1]
N_i <-  pop_total*age_demo
num_groups <- length(age_demo)


sero_none <- rep(0, num_groups) # no prior immunity
startDate <- lubridate::ymd("2021-01-01")

I_0 <- c(133.0,
         241.0,
         488.8,
         364.0,
         315.2,
         243.2,
         203.2,
         111.2,
         155.0,
         
         122.2, 
         91.0,  
         78.8,
         60.8,  
         50.8,  
         27.8) # estimated prevalence by age in BC in Jan 2021

u_var <- c(0.48, 0.30, 
           0.40, 0.40, 0.45, 0.44, 0.70, 1.0, 1.0,
           0.40, 0.40, 0.45, 0.44, 0.70, 1.0) # rel. susceptibility (this works better with validation than one in bubar)

# u_var <- c(0.58, 0.34, 0.35, 0.34, 0.46, 0.45, 0.88, 1, 1,
#            0.34, 0.46, 0.45, 0.88, 1, 1) # rel. susceptibility (this works better with validation than one in bubar)

percent_vax <- 1.0 # just a limit we can't exceed

H = c(0.0,0.3,0.3,0.2,0.2,0.2,0.2,0.20,0.20,
      0.3,0.2,0.2,0.2,0.20,0.20)*N_i # Hesitancy. Increased for 80+ to match cases

# H = c(1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1,
#       1)*N_i # Hesitancy. debug test

#H = c(0.0,0.0,0.3,0.2,0.2,0.2,0.01,0.01,0.01,0.3,0.2,0.2,0.2,0.01,0.01)*N_i # Hesitancy
##########################
# DEFINE MAIN STRATEGIES
##########################

labels <- c(#'A: Oldest to Youngest', 
            'A: 80+, 70-79, 60-69, FLW, 50-59, ...', 
            'B: Oldest to Youngest')

strategies <- list(#list(9, c(8,15),c(7,14), c(6,13), c(5,12), c(4,11), c(3,10)), 
                   list(9, 8, 7, 10:15, 6, 5, 4, 3, 2),
                   list(9, c(8,15),c(7,14), c(6,13), c(5,12), c(4,11), c(3,10), 2))


#######################
# DEFINE MAIN SCENARIO
# This is the main piecewise
# scenario. Start with first phase
# of vaccinating 80+ slowly. Then
# move on to a second phase of
# faster vaccination 
########################
run_over_scen_2 = function(R, ve, vp, scen,alpha=0.0){
   T1 <- 70 #extended till April 1st, and adding 70 to 79
   T2 <- 270 - T1
   # Initial stage (vax all 80+)
   R_init <- 1.03
   n <- ( age_demo[9])/T1
   C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                              target_R0=R_init, in_school=TRUE, alpha_factor=alpha)

   df0 <- run_sim_basic(C, I_0=I_0, percent_vax =1.0, strategy=list(9), num_perday=n,
                        v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                        u = u_var, num_days=T1, with_essential=TRUE, H=H) 
   # Final stage
   n <- sum(age_demo[-c(9)])/T2
   C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                              target_R0=R, in_school=TRUE, alpha_factor=alpha)
   df <- run_sim_restart(C, df_0=tail(df0, n=1), percent_vax =1.0, strategy= strategies[[scen]], num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T2, with_essential=TRUE, H=H)
   # combine 
   df$time <- df$time+T1+1
   df <- combine_age_groups(rbind(df0,df))
   # add pars
   df$R <- R
   df$ve <- ve
   df$vp <- vp
   df$type <- labels[[scen]]
   df$scen <- scen
   df$alpha <- alpha
   return(df)}

#######################
# Another scneario, three phases 
# scenario. Start with first phase
# of vaccinating 80+ slowly. Then
# move on to a second phase of faster vaccination for those above 70. 
#  followed by a third phase of reduced transmission in late April
########################
run_over_scen_3 = function(R, ve, vp, scen,alpha=0.0){
   T1 <- 73
   T2 <- 23
   T3 <- 240 - T1 - T2
   # Initial stage (vax all 80+), low R0
   R_init <- 1.05
   n <- (age_demo[9])/T1
   C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                              target_R0=R_init, in_school=TRUE, alpha_factor=alpha)
   
   df0 <- run_sim_basic(C, I_0=I_0, percent_vax =1.0, strategy=list(9), num_perday=n,
                        v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                        u = u_var, num_days=T1, with_essential=TRUE, H=H)
   
   
   # second stage (vax 70+) high R0
   R_surge <- 1.38
   n <- (age_demo[8])/T2
   C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                              target_R0=R_surge, in_school=TRUE, alpha_factor=alpha)
   
   
   df2 <- run_sim_restart(C, df_0=tail(df0, n=1), percent_vax =1.0, strategy= list(8), num_perday=n,
                          v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                          u = u_var, num_days=T2, with_essential=TRUE, H=H)
   df2$time <- df2$time+T1+1
   
   # Final stage
   n <- sum(age_demo[-c(9, 8)])/T3
   C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                              target_R0=R, in_school=TRUE, alpha_factor=alpha)
   
   df <- run_sim_restart(C, df_0=tail(df2, n=1), percent_vax =1.0, strategy= strategies[[scen]], num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T3, with_essential=TRUE, H=H)
   # combine
   df$time <- df$time+T1+T2+2
   df <- combine_age_groups(rbind(df0,df2, df))
   
   # add pars
   df$R <- R
   df$ve <- ve
   df$vp <- vp
   df$type <- labels[[scen]]
   df$scen <- scen
   df$alpha <- alpha
   return(df)}



########################
# Another scenario
# no piecewise
# constant R and rate (n)
########################

run_over_scen = function(R, ve, vp, scen, n, alpha=0.0){
   T <- 270 

   C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
      target_R0=R, in_school=TRUE, alpha_factor=alpha)
    
   df <- run_sim_basic(C, I_0=I_0, percent_vax =1.0, strategy= alt_strategies[[scen]], num_perday=n,
                           v_e = rep(ve, num_groups),v_p=rep(vp, num_groups),
                           u = u_var, num_days=T, with_essential=TRUE, H=H)

   df <- combine_age_groups(df)
   df$R0 <- R
   df$ve <- ve
   df$vp <- vp
   df$type <- alt_labels[[scen]]
   df$scen <- scen
   df$alpha <- alpha
   return(df)}

