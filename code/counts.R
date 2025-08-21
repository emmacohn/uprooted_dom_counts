library(tidyverse)
library(epiextractr)
library(epidatatools)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)
library(openxlsx2)

#set up workbook
wb <- wb_workbook()

# load CPI data from realtalk
# pulling in CPI-U-RS series, and also saving a copy to share
cpi_data <- realtalk::c_cpi_u_annual

# set base year to 2024 
cpi2024 <- cpi_data$c_cpi_u[cpi_data$year==2024]

#load CPS basic data, add domestic workers category
cps_basic <- load_basic(2024,year, month, age, female, wbhao, union, educ, cow1, citistat,
                  statefips, basicwgt, region, selfemp, emp,selfinc, lfstat,occ18,ind17) %>%
  filter(age>=16, emp==1, selfinc !=1,cow1!=8) %>%#selfemp !=1, selfinc !=1
  # Merge annual CPI data to data frame by year
 # left_join(cpi_data, by='year') 
  mutate(
    #orgwgt = orgwgt / (12*n_distinct(year)),
   # realwageotc=wageotc*(cpi2024/cpi_u_rs),
    dom_work = case_when(
      #Maids
      occ18==4230 & ind17 == 9290 & !(cow1 %in% c(7)) ~ 1,
      #childcare nannies
      occ18==4600 & ind17 %in% c(9290,7580) & !(cow1 %in% c(7)) ~ 2,
      #childcare ownhome
      occ18==4600 & ind17 %in% c(8470) & cow1 %in% c(7) ~ 3,
      #dca not agency
      ((occ18 %in% c(3601,3603,3605) & ind17==9290) | (occ18 %in% c(3602) & ind17 %in% c(9290,7580)))  & !(cow1 %in% c(7)) ~ 4,
      #DCA agency
      (occ18 %in% c(3601,3603,3605,3602) & ind17 %in% c(8170,8370))  & !(cow1 %in% c(7)) ~ 5,
      TRUE~0
    ),
    dom_work_ind = case_when(
      dom_work>0 ~ 1,
      TRUE ~ 0
    ),
    native = case_when(
      citistat %in% c(1, 2, 3) ~ 1, 
      TRUE ~ 0
    ),
  )


## domestic (all) by region and state ##
dom_all_geo <- cps_basic |> 
    filter(dom_work_ind == 1) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(statefips, region)) |>
    mutate(region = to_factor(region),
           statefips = to_factor(statefips))

## domestic (all) by demographics (race, gender, race*gender, nativity) by region ##
#find emp by sex
dom_all_sex <- cps_basic |> 
  filter(dom_work_ind == 1) |>
  mutate(female = to_factor(female),
         region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(female, region))

#find emp by race
dom_all_race <- cps_basic |> 
  filter(dom_work_ind == 1) |>
  mutate(wbhao = to_factor(wbhao),
         region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(wbhao, region))

#find emp by race and sex
dom_race_sex <- cps_basic |> 
  filter(dom_work_ind == 1) |>
  mutate(wbhao = to_factor(wbhao),
        female = to_factor(female),
        region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(wbhao, female, region)) |>
  unite(col = "demographic", c(wbhao, female), na.rm = TRUE)

#find emp by nativity
dom_native <- cps_basic |> 
  filter(dom_work_ind == 1) |>
  mutate(region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(citistat, region)) 

## domestic (all) median wage by region and state ##
XXXXXXXXXXXXXXX

## domestic (occs) by region ##
dom_occ_reg <- cps_basic |> 
    filter(dom_work_ind == 1) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(dom_work, region)) |>
    mutate(dom_work = case_when(dom_work == 1 ~ "Maids", 
                            dom_work == 2 ~ "Childcare nannies",
                            dom_work == 3 ~ "Childcare ownhome",
                            dom_work == 4 ~ "DCA not agency",
                            dom_work == 5 ~ "DCA agency"),
          region = to_factor(region))

## ag by region and state ##
ag_geo <- cps_basic |> 
    filter(occ18 == 6050) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(statefips, region)) |>
    mutate(region = to_factor(region),
           statefips = to_factor(statefips))

## ag by demographics by region ##
#find emp by sex
ag_sex <- cps_basic |> 
  filter(occ18 == 6050) |>
  mutate(female = to_factor(female),
         region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(female, region))

#find emp by race
ag_race <- cps_basic |> 
  filter(occ18 == 6050) |>
  mutate(wbhao = to_factor(wbhao),
         region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(wbhao, region))

#find emp by race and sex
ag_race_sex <- cps_basic |> 
  filter(occ18 == 6050) |>
  mutate(wbhao = to_factor(wbhao),
        female = to_factor(female),
        region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(wbhao, female, region)) |>
  unite(col = "demographic", c(wbhao, female), na.rm = TRUE)

#find emp by nativity
ag_native <- cps_basic |> 
  filter(occ18 == 6050) |>
  mutate(region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(citistat, region)) 

## ag wage by region and state ##
XXXXXXXXXXXXXXXXX

## pub sec (all) by region and state ##
pubsec_all_geo <- cps_basic |> 
    filter(cow1 %in% c(1, 2, 3)) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(statefips, region)) |> 
      mutate(region = to_factor(region),
             statefips = to_factor(statefips))

## pub sec (all) by demographics by region ##
#find emp by sex
pubsec_sex <- cps_basic |> 
  filter(cow1 %in% c(1, 2, 3)) |>
  mutate(female = to_factor(female),
         region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(female, region))

#find emp by race
pubsec_race <- cps_basic |> 
  filter(cow1 %in% c(1, 2, 3)) |>
  mutate(wbhao = to_factor(wbhao),
         region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(wbhao, region))

#find emp by race and sex
pubsec_race_sex <- cps_basic |> 
  filter(cow1 %in% c(1, 2, 3)) |>
  mutate(wbhao = to_factor(wbhao),
        female = to_factor(female),
        region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(wbhao, female, region)) |>
  unite(col = "demographic", c(wbhao, female), na.rm = TRUE)

#find emp by nativity
pubsec_native <- cps_basic |> 
  filter(cow1 %in% c(1, 2, 3)) |>
  mutate(region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(citistat, region)) 

## pub sec (all) wage by region and state ##
XXXXXXXXXXXXXXXXX

## pub sec (teachers) by region and state ##
pubsec_ed_geo <- cps_basic |> 
    filter(cow1 %in% c(1, 2, 3)) |>
    mutate(pub_occ = case_when(occ18 %in% c(2300, 2310, 2320, 2330, 2360, 2545) ~ 1,
         TRUE ~ 2),
        pub_occ = case_when(pub_occ == 1 ~ "Teachers", 
                            pub_occ == 2 ~ "Other pub sec"),
        region = to_factor(region),
        statefips = to_factor(statefips)) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(pub_occ, region, state))

## all workers wage by region and state ##
XXXXXXXXXXXXXX