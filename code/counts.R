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
cpi_data <- realtalk::cpi_u_rs_annual

# set base year to 2024
cpi2024 <- cpi_data$cpi_u_rs[cpi_data$year==2024]

cps <- load_basic(2022:2024,year, month, age, female, wbhao, union, educ, cow1, citizen,
                  statefips, basicwgt, region, selfemp, emp,selfinc, lfstat,occ18,ind17) %>%
  filter(age>=16, emp==1, selfinc !=1,cow1!=8) %>%#selfemp !=1, selfinc !=1
  # Merge annual CPI data to data frame by year
 # left_join(cpi_data, by='year') %>%
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
    )
  )

dom_counts <- cps |> 
    filter(dom_work_ind == 1) |>
    summarise(total_emp = sum(emp * basicwgt, na.rm=TRUE),
        n=n(),
        .by=c(dom_work, region)) |>
    mutate(dom_work = case_when(dom_work == 1 ~ "Maids", 
                            dom_work == 2 ~ "Childcare nannies",
                            dom_work == 3 ~ "Childcare ownhome",
                            dom_work == 4 ~ "DCA not agency",
                            dom_work == 5 ~ "DCA agency"),
          region = to_factor(region)) |>
    pivot_wider(id_cols = region, names_from = dom_work, values_from = total_emp)


ag_counts <- cps |> 
    filter(occ18 == 6050) |>
    summarise(total_emp = sum(emp * basicwgt, na.rm=TRUE),
        n=n(),
        .by=c(occ18, region)) |>
    mutate(region = to_factor(region),
       occ18 = to_factor(occ18)) |>
    pivot_wider(id_cols = region, names_from = occ18, values_from = total_emp)


pubsec_counts <- cps |> 
    filter(cow1 %in% c(1, 2, 3)) |>
    mutate(pub_occ = case_when(occ18 %in% c(2300, 2310, 2320, 2330, 2360, 2545) ~ 1,
         TRUE ~ 2),
        pub_occ = case_when(pub_occ == 1 ~ "Teachers", 
                            pub_occ == 2 ~ "Other pub sec"),
        region = to_factor(region)) |>
    summarise(total_emp = sum(emp * basicwgt, na.rm=TRUE),
        n=n(),
        .by=c(pub_occ, region)) |>
        pivot_wider(id_cols = region, names_from = pub_occ, values_from = total_emp)


