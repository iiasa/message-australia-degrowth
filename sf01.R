#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Supplementary Figure 1: AEEI rates
#' 
#' 
#' Last updated: 05.05.2023

source("load_basics_and_version.R")



# INIT ====

# LOAD ====
aeei <- read_excel(
  path = here(
    "data",
    "input",
    "AEEI.xlsx"
  ),
  sheet = "SSP2-budget5000"
)
# PROCESS ====
aeei <- aeei %>% 
  mutate(`Climate policy`="GHG budget") %>%  
  bind_rows(
    read_excel(
      path = here(
        "data",
        "input",
        "AEEI.xlsx"
      ),
      sheet = "SSP2-fossil"
    ) %>% mutate(`Climate policy`="Keep fossil fuels")
  ) %>%  
  bind_rows(
    read_excel(
      path = here(
        "data",
        "input",
        "AEEI.xlsx"
      ),
      sheet = "SSP2-ren"
    ) %>% mutate(`Climate policy`="Expand renewables")
  ) %>% 
  filter(
    Year >= 2020
  )

# PLOT ====
p.aeei <- ggplot(
  aeei,
  aes(
    x=Year,
    y=`AEEI factor`,
    colour=`Climate policy`,
    linetype=`Climate policy`
  )
) +
  facet_wrap(Sector~., ncol=2) +
  geom_line(linewidth=1.2) +
  scale_color_colorblind() +
  theme_classic() +
  theme_hc()

p.aeei

# SAVE ====
save_ggplot(
  p = p.aeei,
  f = here("output", paste0("sf01-",version)),
  h=200, w=200
)





