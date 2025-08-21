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

#load CPS basic data, add domestic workers category and native born variable
cps_basic <- load_basic(2024,year, month, age, female, wbhao, union, educ, cow1, citistat,
                  statefips, basicwgt, region, selfemp, emp,selfinc, lfstat,occ18,ind17) %>%
  filter(age>=16, emp==1, selfinc !=1,cow1!=8) %>%#selfemp !=1, selfinc !=1
  mutate(
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

  #load CPS org data, add domestic workers category and native born variable
cps_org <- load_org(2024,year, month, age, female, wbhao, union, educ, cow1, citistat, wage,
                  statefips, orgwgt, region, selfemp, emp,selfinc, lfstat,occ18,ind17) %>%
  filter(age>=16, emp==1, selfinc !=1,cow1!=8) %>%#selfemp !=1, selfinc !=1
    # Merge annual CPI data to data frame by year
  left_join(cpi_data, by='year') %>%
  # inflation adjust wages to 2024$
  mutate(realwage = wage * (cpi2024/c_cpi_u),
  # add domestic workers and native born variables
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

### DOMESTIC WORKERS ###

## domestic (all) by region and state ##
dom_geo <- cps_basic |> 
    filter(dom_work_ind == 1) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(statefips, region)) |>
    mutate(region = to_factor(region),
           statefips = to_factor(statefips))


## domestic (occs) by region ##
dom_occ_geo <- cps_basic |> 
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

wb$add_worksheet(sheet = "Domestic workers count") $
  add_data(x = dom_geo, start_col = 1) $
  add_data(x = dom_occ_geo, start_col = 7)

## domestic (all) by demographics (race, gender, race*gender, nativity) by region ##
#find emp by sex
dom_sex <- cps_basic |> 
  filter(dom_work_ind == 1) |>
  mutate(female = to_factor(female),
         region = to_factor(region)) |>
  summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(female, region))

#find emp by race
dom_race <- cps_basic |> 
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

wb$add_worksheet(sheet = "Domestic workers demographics") $
  add_data(x = dom_race, start_col = 1) $
  add_data(x = dom_sex, start_col = 7) $
  add_data(x = dom_race_sex, start_col = 13) $
  add_data(x = dom_native, start_col = 19)

## domestic (all) median wage by region and state ##
dom_wages <- cps_org |> 
  filter(dom_work_ind == 1) |>
  mutate(region = to_factor(region),
        statefips = to_factor(statefips)) |> 
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(statefips, region))

wb$add_worksheet(sheet = "Domestic workers wages") $
  add_data(x = dom_wages, start_col = 1)

### AGRICULTURE ###

## ag by region and state ##
ag_geo <- cps_basic |> 
    filter(occ18 == 6050) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(statefips, region)) |>
    mutate(region = to_factor(region),
           statefips = to_factor(statefips))

wb$add_worksheet(sheet = "Agricultural workers count") $
  add_data(x = ag_geo, start_col = 1)

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

wb$add_worksheet(sheet = "Ag workers demographics") $
  add_data(x = ag_race, start_col = 1) $
  add_data(x = ag_sex, start_col = 7) $
  add_data(x = ag_race_sex, start_col = 13) $
  add_data(x = ag_native, start_col = 19)

## ag wage by region and state ##
ag_wages <- cps_org |> 
  filter(occ18 == 6050) |>
  mutate(region = to_factor(region),
        statefips = to_factor(statefips)) |> 
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(statefips, region))

wb$add_worksheet(sheet = "Ag workers wages") $
  add_data(x = ag_wages, start_col = 1)

### PUBLIC SECTOR ###

## pub sec (all) by region and state ##
pubsec_all_geo <- cps_basic |> 
    filter(cow1 %in% c(1, 2, 3)) |>
    summarise(total_emp = sum(emp * basicwgt/12, na.rm=TRUE),
        n=n(),
        .by=c(statefips, region)) |> 
      mutate(region = to_factor(region),
             statefips = to_factor(statefips))

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
        .by=c(pub_occ, region, statefips))

wb$add_worksheet(sheet = "Public sector workers count") $
  add_data(x = pubsec_all_geo, start_col = 1) $
  add_data(x = pubsec_ed_geo, start_col = 7)

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

wb$add_worksheet(sheet = "Pub sec workers demographics") $
  add_data(x = pubsec_race, start_col = 1) $
  add_data(x = pubsec_sex, start_col = 7) $
  add_data(x = pubsec_race_sex, start_col = 13) $
  add_data(x = pubsec_native, start_col = 19)

## pub sec (all) wage by region and state ##
pubsec_wages <- cps_org |> 
  filter(cow1 %in% c(1, 2, 3)) |>
  mutate(region = to_factor(region),
        statefips = to_factor(statefips)) |> 
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(statefips, region))

wb$add_worksheet(sheet = "Pub sec workers wages") $
  add_data(x = pubsec_wages, start_col = 1)

### ALL WORKERS ###

## all workers wage by region and state ##
all_wages <- cps_org |> 
  mutate(region = to_factor(region),
        statefips = to_factor(statefips)) |> 
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(statefips, region))

wb$add_worksheet(sheet = "All workers wages") $
  add_data(x = all_wages, start_col = 1)

#save workbook
wb_save(wb, "output/uprooted_counts_wages.xlsx")
