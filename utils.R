# a script hosting utility functions for the DLE data preparation for MESSAGEix
# 07.12.2021, Jarmo Kikstra
load_pkgs <- function() {
  library(vroom) # for reading CSV files
  library(readxl) # for reading Excel files
  library(here) # for consistently using relative paths
  library(ggdist) # for visualisations of spreads and uncertainty beyond boxplots
  library(ggsci) # for some colorschemes
  library(ggthemes) # for ggplot themes (like theme_hc())
  library(patchwork) # for combining ggplots in one figure
  library(lestat) # for invcdf in dle
  library(geomtextpath) # for setu-style lines: https://cran.r-project.org/web/packages/geomtextpath/vignettes/geomtextpath.html
  library(zoo) # for interpolation, using na.approx
  
  library(tidyverse) # load this one last
  
  
  select <- dplyr::select # explicitly say that we mean dplyr's select function whenever we use select (not the one from the MASS library...)
  filter <- dplyr::filter # explicitly say that we mean dplyr's filter function whenever we use filter (not the one from the stats library...)
  mutate <- dplyr::mutate # explicitly say that we mean dplyr's mutate function whenever we use mutate
}

# other loading options:

# option B:
# pacman: https://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library && https://github.com/trinker/pacman
# if (!require("pacman")) install.packages("pacman"); library(pacman)
# p_load(qdap, ggplot2, fakePackage, dplyr, tidyr)


# option C:
# https://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
# install_load <- function (package1, ...)  {   
#   
#   # convert arguments to vector
#   packages <- c(package1, ...)
#   
#   # start loop to determine if each package is installed
#   for(package in packages){
#     
#     # if package is installed locally, load
#     if(package %in% rownames(installed.packages()))
#       do.call('library', list(package))
#     
#     # if package is not installed locally, download, then load
#     else {
#       install.packages(package)
#       do.call("library", list(package))
#     }
#   } 
# }



filter_wildcard_var <- function(df, variable.string) {

  # NB/TOFIX: this function does not work like pyam in that it does not respect that e.g. "*Emissions|CO2" should end with "|CO2", rather it implicitly treats it as "*Emissions|CO2*"

  split.string <- str_split(string = variable.string, pattern = "\\*", n = Inf, simplify = FALSE)[[1]]
  n.split.string <- length(split.string)

  # TODO: find a way to map this to go beyond 2 * characters
  if (n.split.string > 3) {
    message("Maximum wildcards that can be used in one string search is currently set to 2.")
  } else if (n.split.string == 2) {
    df <- df %>%
      filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T))
  } else if (n.split.string == 3) {
    df <- df %>%
      filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T) & grepl(x = Variable, pattern = split.string[3], fixed = T))
  }

  return(df)
}

iamc_wide_to_long <- function(df, upper.to.lower = F) {

  # function assumes all five basic IAMC columns are there, and nothing more

  if (upper.to.lower) {
    df <- df %>%
      rename(
        model = Model,
        scenario = Scenario,
        region = Region,
        variable = Variable,
        unit = Unit
      )
  }

  first.year <- colnames(df)[6] # assumes all five basic IAMC columns are there, and nothing more
  last.year <- colnames(df)[length(colnames(df))]

  df <- df %>%
    pivot_longer(
      cols = first.year:last.year,
      names_to = "year",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    mutate(year = as.numeric(year))

  return(df)
}

normalise_iamc_long <- function(df, starting.year) {
  
  # normalise the values in column "value" to the year "starting.year = 1"
  # - by (model, scenario, region, variable, unit)
  
  df.temp <- df %>% 
    left_join(
      df %>% filter(year==starting.year) %>% rename(value.start=value) %>% 
        select(model, scenario, region, variable, unit, value.start),
      by = c("model", "scenario", "region", "variable", "unit")
    ) %>% 
    mutate(value=value/value.start)
    
  return(
    df.temp %>% select(-value.start)
  )
  
}

to_per_capita <- function(df){
  pop <- df %>% filter(variable=="Population") %>% rename(pop=value) %>% select(model,scenario,region,year,pop)
  df <- df %>% filter(variable!="Population") %>% 
    left_join(pop) %>% 
    mutate(value=value/pop) %>% 
    select(-pop)
  return(df)
}

to_per_gdp <- function(df, vgdp = "GDP (PPP)"){
  gdp <- df %>% filter(variable==vgdp) %>% rename(gdp=value) %>% select(model,scenario,region,year,gdp)
  df <- df %>% filter(variable!=vgdp) %>% 
    left_join(gdp) %>% 
    mutate(value=value/gdp) %>% 
    select(-gdp)
  return(df)
}

rename_li_data <- function(df){
  return(
    df %>% 
      
      # --- part 2023-03-30 --- below
      # net-zero budget data in MeL data - 7Gt - type c
      mutate(scenario=ifelse(scenario=="10 k-budget7000", "SSP2-10k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k-budget7000", "SSP2-20k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k-budget7000", "SSP2-30k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k-budget7000", "SSP2-40k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k-budget7000", "SSP2-50k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k-budget7000", "SSP2-60k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k-budget7000", "SSP2-70k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2-budget7000", "SSP2-baseline-7Gt", scenario)) %>%
      # net-zero budget data in MeL data - 6Gt - type c
      mutate(scenario=ifelse(scenario=="10 k-budget6000", "SSP2-10k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k-budget6000", "SSP2-20k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k-budget6000", "SSP2-30k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k-budget6000", "SSP2-40k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k-budget6000", "SSP2-50k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k-budget6000", "SSP2-60k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k-budget6000", "SSP2-70k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2-budget6000", "SSP2-baseline-6Gt", scenario)) %>%
      # net-zero budget data in MeL data - 5Gt - type c
      mutate(scenario=ifelse(scenario=="10 k-budget5000", "SSP2-10k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k-budget5000", "SSP2-20k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k-budget5000", "SSP2-30k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k-budget5000", "SSP2-40k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k-budget5000", "SSP2-50k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k-budget5000", "SSP2-60k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k-budget5000", "SSP2-70k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2-budget5000", "SSP2-baseline-5Gt", scenario)) %>%
      # net-zero budget data in MeL data - 4Gt - type c
      mutate(scenario=ifelse(scenario=="10 k-budget4000", "SSP2-10k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k-budget4000", "SSP2-20k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k-budget4000", "SSP2-30k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k-budget4000", "SSP2-40k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k-budget4000", "SSP2-50k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k-budget4000", "SSP2-60k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k-budget4000", "SSP2-70k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2-budget4000", "SSP2-baseline-4Gt", scenario)) %>%
      # net-zero budget data in MeL data - 3Gt - type c
      mutate(scenario=ifelse(scenario=="10 k-budget3000", "SSP2-10k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k-budget3000", "SSP2-20k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k-budget3000", "SSP2-30k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k-budget3000", "SSP2-40k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k-budget3000", "SSP2-50k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k-budget3000", "SSP2-60k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k-budget3000", "SSP2-70k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2-budget3000", "SSP2-baseline-3Gt", scenario)) %>%
      
      # type c in MeL data
      mutate(scenario=ifelse(scenario=="10 k-ren", "SSP2-10k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k-ren", "SSP2-20k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k-ren", "SSP2-30k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k-ren", "SSP2-40k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k-ren", "SSP2-50k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k-ren", "SSP2-60k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k-ren", "SSP2-70k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2-ren", "SSP2-baseline-ren", scenario)) %>%
      
      # --- part 2023-03-30 --- above
      
      
      
      
      # --- versions before 2023-03-30 --- below
      
      # type a in MeL data 
      mutate(scenario=ifelse(scenario=="10 k ren", "SSP2-10k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k ren", "SSP2-20k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k ren", "SSP2-30k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k ren", "SSP2-40k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k ren", "SSP2-50k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k ren", "SSP2-60k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k ren", "SSP2-70k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2 ren", "SSP2-baseline-ren", scenario)) %>%
      # type b in MeL data (ACT)
      mutate(scenario=ifelse(scenario=="10_ren", "SSP2-10k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20_ren", "SSP2-20k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30_ren", "SSP2-30k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40_ren", "SSP2-40k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50_ren", "SSP2-50k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60_ren", "SSP2-60k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70_ren", "SSP2-70k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2_ren", "SSP2-baseline-ren", scenario)) %>%
      # type b in MeL data
      mutate(scenario=ifelse(scenario=="'10_ren'", "SSP2-10k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'20_ren'", "SSP2-20k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'30_ren'", "SSP2-30k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'40_ren'", "SSP2-40k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'50_ren'", "SSP2-50k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'60_ren'", "SSP2-60k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'70_ren'", "SSP2-70k-ren", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'SSP2_ren'", "SSP2-baseline-ren", scenario)) %>%
      
      
      
      
      # type a in MeL data
      mutate(scenario=ifelse(scenario=="10 k fossil", "SSP2-10k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k fossil", "SSP2-20k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k fossil", "SSP2-30k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k fossil", "SSP2-40k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k fossil", "SSP2-50k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k fossil", "SSP2-60k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k fossil", "SSP2-70k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2 fossil", "SSP2-baseline-fossil", scenario)) %>% 
      # type b in MeL data
      mutate(scenario=ifelse(scenario=="10_fossil", "SSP2-10k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20_fossil", "SSP2-20k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30_fossil", "SSP2-30k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40_fossil", "SSP2-40k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50_fossil", "SSP2-50k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60_fossil", "SSP2-60k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70_fossil", "SSP2-70k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2_fossil", "SSP2-baseline-fossil", scenario)) %>%
      # type c in MeL data
      mutate(scenario=ifelse(scenario=="'10_fossil'", "SSP2-10k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'20_fossil'", "SSP2-20k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'30_fossil'", "SSP2-30k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'40_fossil'", "SSP2-40k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'50_fossil'", "SSP2-50k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'60_fossil'", "SSP2-60k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'70_fossil'", "SSP2-70k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'SSP2_fossil'", "SSP2-baseline-fossil", scenario)) %>%
      # type d in MeL data
      mutate(scenario=ifelse(scenario=="10 k-fossil", "SSP2-10k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k-fossil", "SSP2-20k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k-fossil", "SSP2-30k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k-fossil", "SSP2-40k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k-fossil", "SSP2-50k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k-fossil", "SSP2-60k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k-fossil", "SSP2-70k-fossil", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2-fossil", "SSP2-baseline-fossil", scenario)) %>% 
      
      
      # full-century data in MeL data
      mutate(scenario=ifelse(scenario=="10 k 1.5", "SSP2-10k-1.5C", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20 k 1.5", "SSP2-20k-1.5C", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30 k 1.5", "SSP2-30k-1.5C", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40 k 1.5", "SSP2-40k-1.5C", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50 k 1.5", "SSP2-50k-1.5C", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60 k 1.5", "SSP2-60k-1.5C", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70 k 1.5", "SSP2-70k-1.5C", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2 1.5", "SSP2-baseline-1.5C", scenario)) %>% 
      # net-zero budget data in MeL data - 7Gt
      mutate(scenario=ifelse(scenario=="'10_budget7000'", "SSP2-10k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'20_budget7000'", "SSP2-20k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'30_budget7000'", "SSP2-30k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'40_budget7000'", "SSP2-40k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'50_budget7000'", "SSP2-50k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'60_budget7000'", "SSP2-60k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'70_budget7000'", "SSP2-70k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'SSP2_budget7000'", "SSP2-baseline-7Gt", scenario)) %>%
      # net-zero budget data in MeL data - 6Gt
      mutate(scenario=ifelse(scenario=="'10_budget6000'", "SSP2-10k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'20_budget6000'", "SSP2-20k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'30_budget6000'", "SSP2-30k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'40_budget6000'", "SSP2-40k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'50_budget6000'", "SSP2-50k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'60_budget6000'", "SSP2-60k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'70_budget6000'", "SSP2-70k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'SSP2_budget6000'", "SSP2-baseline-6Gt", scenario)) %>%
      # net-zero budget data in MeL data - 5Gt
      mutate(scenario=ifelse(scenario=="'10_budget5000'", "SSP2-10k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'20_budget5000'", "SSP2-20k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'30_budget5000'", "SSP2-30k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'40_budget5000'", "SSP2-40k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'50_budget5000'", "SSP2-50k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'60_budget5000'", "SSP2-60k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'70_budget5000'", "SSP2-70k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'SSP2_budget5000'", "SSP2-baseline-5Gt", scenario)) %>%
      # net-zero budget data in MeL data - 4Gt
      mutate(scenario=ifelse(scenario=="'10_budget4000'", "SSP2-10k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'20_budget4000'", "SSP2-20k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'30_budget4000'", "SSP2-30k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'40_budget4000'", "SSP2-40k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'50_budget4000'", "SSP2-50k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'60_budget4000'", "SSP2-60k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'70_budget4000'", "SSP2-70k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'SSP2_budget4000'", "SSP2-baseline-4Gt", scenario)) %>%
      # net-zero budget data in MeL data - 3Gt
      mutate(scenario=ifelse(scenario=="'10_budget3000'", "SSP2-10k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'20_budget3000'", "SSP2-20k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'30_budget3000'", "SSP2-30k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'40_budget3000'", "SSP2-40k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'50_budget3000'", "SSP2-50k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'60_budget3000'", "SSP2-60k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'70_budget3000'", "SSP2-70k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="'SSP2_budget3000'", "SSP2-baseline-3Gt", scenario)) %>%
      # net-zero budget data in MeL data - 7Gt - type b
      mutate(scenario=ifelse(scenario=="10_budget7000", "SSP2-10k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20_budget7000", "SSP2-20k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30_budget7000", "SSP2-30k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40_budget7000", "SSP2-40k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50_budget7000", "SSP2-50k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60_budget7000", "SSP2-60k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70_budget7000", "SSP2-70k-7Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2_budget7000", "SSP2-baseline-7Gt", scenario)) %>%
      # net-zero budget data in MeL data - 6Gt - type b
      mutate(scenario=ifelse(scenario=="10_budget6000", "SSP2-10k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20_budget6000", "SSP2-20k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30_budget6000", "SSP2-30k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40_budget6000", "SSP2-40k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50_budget6000", "SSP2-50k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60_budget6000", "SSP2-60k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70_budget6000", "SSP2-70k-6Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2_budget6000", "SSP2-baseline-6Gt", scenario)) %>%
      # net-zero budget data in MeL data - 5Gt - type b
      mutate(scenario=ifelse(scenario=="10_budget5000", "SSP2-10k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20_budget5000", "SSP2-20k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30_budget5000", "SSP2-30k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40_budget5000", "SSP2-40k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50_budget5000", "SSP2-50k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60_budget5000", "SSP2-60k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70_budget5000", "SSP2-70k-5Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2_budget5000", "SSP2-baseline-5Gt", scenario)) %>%
      # net-zero budget data in MeL data - 4Gt - type b
      mutate(scenario=ifelse(scenario=="10_budget4000", "SSP2-10k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20_budget4000", "SSP2-20k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30_budget4000", "SSP2-30k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40_budget4000", "SSP2-40k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50_budget4000", "SSP2-50k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60_budget4000", "SSP2-60k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70_budget4000", "SSP2-70k-4Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2_budget4000", "SSP2-baseline-4Gt", scenario)) %>%
      # net-zero budget data in MeL data - 3Gt - type b
      mutate(scenario=ifelse(scenario=="10_budget3000", "SSP2-10k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="20_budget3000", "SSP2-20k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="30_budget3000", "SSP2-30k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="40_budget3000", "SSP2-40k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="50_budget3000", "SSP2-50k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="60_budget3000", "SSP2-60k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="70_budget3000", "SSP2-70k-3Gt", scenario)) %>% 
      mutate(scenario=ifelse(scenario=="SSP2_budget3000", "SSP2-baseline-3Gt", scenario)) %>%
      
      
      
      mutate(variable=ifelse(variable=="Emissions|Kyoto Gases", "GHG emissions", variable)) %>% 
      mutate(variable=ifelse(variable=="Final Energy", "Final Energy", variable)) %>% 
      mutate(variable=ifelse(variable=="GDP|PPP", "GDP (PPP)", variable))
  )
}

check_scenario_duplicate_missing_li <- function(df, expected.scen=56){
  if (!(df %>% unique_ms() %>% length() ==  expected.scen)) {
    message(paste0("Not ", as.character(expected.scen), " unique scenarios."))
  } else {
    message("Correct number of unique scenario names.")
  }
  
  if (!(df %>% distinct() %>% pull(value) %>% length() ==  df %>% pull(value) %>% length())) {
    message(paste0("Duplicates exist..."))
    message(
      
      paste(paste0("Number of duplicate rows (based on column 'value'): ",
                   as.character(
                     (df %>% pull(value) %>% length()) - (df %>% distinct() %>% pull(value) %>% length()) 
                   )
                   ))
      )
    # paste duplicate values
    message("The following are duplicate rows ...")
    dups <- df %>% group_by_all() %>% filter(n() > 1)
    message(
      dups %>% paste0("\n")
    )
    message(
      dups %>% unique_ms() %>% paste0("\n")
    )
    message(
      dups %>% unique_variable() %>% paste0("\n")
    )
    
  } else {
    message("No duplicates in data.")
  }
}

add_degrowth_level <- function(df){
  return(
    df %>% 
      mutate(scen=scenario) %>% 
      separate(col = scen, sep = "-", into = c("ssp", "Annual consumption per capita (at utility peak)", "Scenario set"))
  )
}
add_scenario_set_type <- function(df){
  return(
    df %>% 
      mutate(`Climate policy` = ifelse(grepl(x=scenario,pattern="fossil"), "Keep fossil fuels", NA)) %>%
      mutate(`Climate policy` = ifelse(grepl(x=scenario,pattern="ren"), "Expand renewables", `Climate policy`)) %>%
      mutate(`Climate policy` = ifelse(grepl(x=scenario,pattern="1.5C"), "Expand renewables and NETs", `Climate policy`)) %>% # NOT USED since 25 March 2023 anymore. If still used, rethink how to name it. 
      mutate(`Climate policy` = ifelse(grepl(x=scenario,pattern="Gt"), "GHG budget", `Climate policy`)) %>% 
      mutate(`GHG budget` = ifelse(
        grepl(x=scenario,pattern="Gt"), 
        substr(x = scenario, start = nchar(scenario)-2, stop = nchar(scenario)), 
               "No GHG budget"))
  )
}

add_percentile_columns <- function(df, percentiles = c(0.25, 0.5, 0.75), group.cols = c("variable", "year")) {
  p_names <- map_chr(percentiles, ~ paste0("p", .x * 100))
  
  p_funs <- map(percentiles, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)
  
  group.cols <- enquo(group.cols) # need to quote
  
  df.percentiles <- df %>%
    group_by_at(vars(!!group.cols)) %>%
    summarize_at(vars(value), .funs = p_funs)
  
  return(df %>% left_join(df.percentiles))
}

filter_by_cat_ar6 <- function(df, cat){
  m <- load_meta_ar6() %>% select(model,scenario,Category)
  df <- df %>% left_join(m) %>% 
    filter(Category==cat)
  return(df)
}
get_list_vetted_scenarios_ar6_withclimate <- function(lowercase.var.columns = F) {
  meta <- load_meta_ar6(with.lowercase.columns = lowercase.var.columns)
  
  if (lowercase.var.columns) {
    list.mod.scens <- meta %>%
      select(model, scenario) %>%
      mutate(
        MOD.SCEN = paste0(model, "_", scenario)
      ) %>%
      pull(MOD.SCEN) %>%
      unique() %>%
      sort()
  } else if (!lowercase.var.columns) {
    list.mod.scens <- meta %>%
      select(Model, Scenario) %>%
      mutate(
        MOD.SCEN = paste0(Model, "_", Scenario)
      ) %>%
      pull(MOD.SCEN) %>%
      unique() %>%
      sort()
  }
  
  return(list.mod.scens)
}
filter_ar6climate_scenarios <- function(df, lowercase.var.columns = F) {
  if (lowercase.var.columns) {
    return(
      df %>% mutate(
        MOD.SCEN = paste0(model, "_", scenario)
      ) %>%
        filter(MOD.SCEN %in% get_list_vetted_scenarios_ar6_withclimate(lowercase.var.columns)) %>%
        select(-MOD.SCEN)
    )
  } else if (!lowercase.var.columns) {
    return(
      df %>% mutate(
        MOD.SCEN = paste0(Model, "_", Scenario)
      ) %>%
        filter(MOD.SCEN %in% get_list_vetted_scenarios_ar6_withclimate(lowercase.var.columns)) %>%
        select(-MOD.SCEN)
    )
  }
}


load_meta_ar6 <- function(meta.sheet = IPCC.META.FILE.SHEET,
                          with.lowercase.columns = T,
                          path.ar6.meta = IPCC.META.FILE) {
  if (!with.lowercase.columns) {
    return(
      read_excel(
        path.ar6.meta,
        sheet = meta.sheet
      )
    )
  } else if (with.lowercase.columns) {
    return(
      read_excel(
        path.ar6.meta,
        sheet = meta.sheet
      ) %>%
        rename(model = Model, scenario = Scenario)
    )
  }
}
# TODO: complement R10 style function for this one 
load_var_ar6 <- function(variable,
                         keep.only.vetted.climate.data = T,
                         with.lowercase.columns = F,
                         add.cicero = F,
                         path.ar6.data = IPCC.DATA.FILE) {
  
  # This reads in the (very) big EVERYTHING file.
  # To keep the code for this repository simple, we stick to that, even though one could speed up this reproduction code by choosing to load in the standard AR6DB snapshot when the desired variable(s) is not unique to the EVERYTHING file [could check against some variable lists].
  
  if (add.cicero) {
    df <- vroom(
      path.ar6.data
    ) %>% bind_rows(
      vroom(path.ar6.data.extracicero)
    )
  } else {
    df <- vroom(
      path.ar6.data
    )
  }
  
  gc()
  
  
  if (keep.only.vetted.climate.data) {
    df <- df %>%
      filter_ar6climate_scenarios(with.lowercase.columns)
  }
  
  gc()
  
  
  if (grepl(pattern = "\\*", x = variable)) {
    df <- filter_wildcard_var(
      df,
      variable.string = variable
    )
  } else {
    df <- df %>%
      filter(
        Variable == variable
      )
  }
  # note: this function could be extended add the option to pass a list, which would be faster than the wildcard filter
  
  
  gc()
  return(df %>%
           iamc_wide_to_long(upper.to.lower = T))
}

unique_variable <- function(df){
  return(
    df %>% pull(variable) %>% unique() %>% sort()
  )
}
unique_region <- function(df){
  return(
    df %>% pull(region) %>% unique() %>% sort()
  )
}
unique_ms <- function(df){
  return(
    df %>% mutate(`Model-scenario`=paste0(model,"-",scenario)) %>% pull(`Model-scenario`) %>% unique() %>% sort()
  )
}
unique_scenario <- function(df){
  return(
    df %>% pull(`scenario`) %>% unique() %>% sort()
  )
}
mutate_cond = function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
save_ggplot = function(p,f,h=150,w=150,format="png-pdf"){
  if(format=="png-pdf"){
    ggsave(
      plot = p,
      file = paste0(f,".png"), 
      height = h,
      width = w,
      unit = "mm"
    ) 
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = grDevices::cairo_pdf,
      height = h,
      width = w,
      unit = "mm"
    )
  } else if (format=="png") {
    ggsave(
      plot = p,
      file = paste0(f,".png"), 
      height = h,
      width = w,
      unit = "mm"
    ) 
  } else if (format=="pdf") {
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = grDevices::cario_pdf,
      height = h,
      width = w,
      unit = "mm"
    )
  }
}
