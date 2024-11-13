#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Last updated: 27.06.2023
#' function:
#' - load and process MESSAGE-Australia scenario data
#' - load and process IPCC data



try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
library(here)
here::i_am("message-australia-degrowth.Rproj")

# import functions from other files
source("utils.R")
load_pkgs()


# Files: IPCC ====
IPCC.DATA.FILE.R10 <- "C:\\Users\\kikstra\\OneDrive - IIASA\\_Other\\Data\\Scenario data\\Scenario Databases\\AR6_Scenarios_Database_R10_regions_v1.1/AR6_Scenarios_Database_R10_regions_v1.1.csv"

IPCC.META.FILE <- "C:\\Users\\kikstra\\OneDrive - IIASA\\_Other\\Data\\Scenario data\\Scenario Databases\\AR6_Scenarios_Database_R10_regions_v1.1/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
IPCC.META.FILE.SHEET <- "meta_Ch3vetted_withclimate" # Category


# Files: Li ====
MESSAGE.AUSTRALIA.DATA.FILE <- "DegrowthIAM_AU_IAMC_20230528.xlsx"

# OPTIONS ====

normalization.year <- 2020

ipcc.variables.of.interest <- c(
  "Population",
  "GDP|PPP",
  "Final Energy", 
  "Emissions|Kyoto Gases",
  "Consumption",
  "Primary Energy|Wind",
  "Primary Energy|Solar",
  "Primary Energy|Biomass",
  "Primary Energy|Fossil"
)


# preprocess data ====
 
preprocess_data_li <- function(fname){
  # this function puts the data from the MESSAGE runs in a tidy format 
  # also checks if anything is missing or duplicated
  
  # GDP_Finalenergy_Emission
  rd1.raw <- read_excel(
    here("data", "input", fname),
    sheet = "GDP_Finalenergy_Emission"
  ) %>% 
    
    # final energy is duplicated between sheet "GDP_Finalenergy_Emission" and sheet "ACT" - we filter it out here, only keep it in when we read in "ACT" 
    filter(variable!="Final Energy") %>% 
    
    iamc_wide_to_long(upper.to.lower = F) %>%
    rename_li_data()
  
  # checks
  rd1.raw %>% unique_scenario()
  check_scenario_duplicate_missing_li(rd1.raw, expected.scen = 56)
  
  # add population for all scenarios
  rd1.all.scens <- rd1.raw %>% distinct(model,scenario,region) %>% mutate(variable="Population", unit="million ppl")
  rd1.pop.values <- rd1.raw %>% filter(variable=="Population") %>% select(year,value)
  rd1.pop <- left_join(
    rd1.all.scens,
    rd1.pop.values,
    by=character() # to perform cross-join
  )
  
  rd1 <- rd1.raw %>% filter(variable!="Population") %>% 
    bind_rows(rd1.pop)
  
  # Sectoral_demand
  rd2 <- read_excel(
    here("data", "input", fname),
    sheet = "Sectoral_demand"
  ) %>% 
    
    mutate(variable=paste0("Useful Energy|",variable)) %>% 
    
    iamc_wide_to_long(upper.to.lower = F) %>% 
    rename_li_data()
  
  # check
  rd2 %>% unique_scenario()
  check_scenario_duplicate_missing_li(rd2, expected.scen = 56) #  (8 degrowth types * (5 budget + 2 other climate policies))
  
  
  # ACT
  rd3 <- read_excel(
    here("data", "input", fname),
    sheet = "ACT"
    # .name_repair = "minimal" # following https://stackoverflow.com/questions/65072813/r-read-xlsx-tacks-cell-number-to-the-end-of-column-names
  ) %>% 
    
    
    select(-Note) %>% 
    drop_na(ACT) %>% 
    rename(variable=ACT) %>%
    rename(unit=Unit) %>%
    mutate(model = "MESSAGEix-Australia") %>% 
    mutate(region = "AUS") %>% 
    
    pivot_longer(cols = `SSP2-budget7000_2020`:`10 k-budget3000_2100`) %>% 
    
    mutate(scenario = substr(x=name, start = 0, stop = nchar(name)-5)) %>% 
    mutate(year = as.numeric(substr(x=name, start = nchar(name)-3, stop = nchar(name)))) %>%
    select(-name) %>%
    
    rename_li_data()
  
  # check
  rd3 %>% unique_scenario()
  check_scenario_duplicate_missing_li(rd3, expected.scen = 56)
  
  
  # Carbon_price (only for budget) 
  rd4 <- read_excel(
    here("data", "input", fname),
    sheet = "Carbon_price"
  ) %>% 
    rename(unit = `unit/note: this one is not 100% sure, but the unit should be same as in the Globle SSP2`) %>%
    
    iamc_wide_to_long(upper.to.lower = F) %>% 
    rename_li_data()
  
  # check
  rd4 %>% unique_scenario()
  check_scenario_duplicate_missing_li(rd4, expected.scen = 56) # is ok to fail; no data for ren or fossil
  check_scenario_duplicate_missing_li(rd4, expected.scen = 40)
  
  # combine
  d <- rd1 %>% 
    bind_rows(rd2) %>% 
    bind_rows(rd3) %>% 
    bind_rows(rd4) 
  
  # final check
  d %>% unique_scenario()
  check_scenario_duplicate_missing_li(d, expected.scen = 56)
  

  # group, and filter out all-zero variables (which should be treated as not-reported)
  d <- d %>% group_by(model,scenario,region,variable,unit) %>%
    filter(!all(value==0)) %>%
    ungroup()
  
  
  return(d)
    
}


# Load: Li et al. data (Australia) ====
li.data <- preprocess_data_li(fname = MESSAGE.AUSTRALIA.DATA.FILE)


write_delim(
  x = li.data,
  file = here("data", "li.csv"),
  delim = ","
)

li.data.normalised <- li.data %>% 
  mutate(MOD.SCEN=paste0(model,"_",scenario)) %>% 
  normalise_iamc_long(starting.year = normalization.year)

write_delim(
  x = li.data.normalised,
  file = here("data", "li_normalised.csv"),
  delim = ","
)


# Load: IPCC (R10) ====

# meta
ipcc.meta <- load_meta_ar6() %>%  
  select(
    model,
    scenario,
    Category,
    Ssp_family,
    IMP_marker
  ) %>% 
  mutate(SSP=paste0("SSP",as.character(Ssp_family)))

# data
ipcc.data <- NULL
for (v in ipcc.variables.of.interest){
  ipcc.data <- ipcc.data %>% 
    bind_rows(
      load_var_ar6(variable = v,
                   path.ar6.data = IPCC.DATA.FILE.R10) %>% rename_li_data()
    )
}

# filter region and category for IPCC data
# this line is simply to save some space in writing out the processed data
ipcc.data <- ipcc.data %>% 
  filter_by_cat_ar6(cat="C1") %>% 
  filter(region=="R10PAC_OECD") 
  


write_delim(
  x = ipcc.data,
  file = here("data", "ipcc_r10.csv"),
  delim = ","
)

ipcc.data.normalised <- ipcc.data %>% 
  normalise_iamc_long(starting.year = normalization.year)

write_delim(
  x = ipcc.data.normalised,
  file = here("data", "ipcc_r10_normalised.csv"),
  delim = ","
)
