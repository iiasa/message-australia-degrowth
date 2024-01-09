#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Supplementary Figure 2: FE-to-UE ratios
#' 
#' 
#' Last updated: 08.05.2023

source("load_basics_and_version.R")



# INIT ====

# LOAD ====
li.raw.notnorm <- vroom(here("data", "li.csv")) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type() %>% 
  filter(
    variable %in% c(
      # totals
      "Useful Energy",
      "Final Energy",
      "Secondary Energy",
      "Primary Energy",
      
      # sectoral
      "Useful Energy|Air transport",
      "Useful Energy|Industry feedstock",
      "Useful Energy|Industry thermal",
      "Useful Energy|Industry_specific",
      "Useful Energy|Rail transport and shipping",
      "Useful Energy|Residential-commercial specific",
      "Useful Energy|Residential-commercial thermal",
      "Useful Energy|Road transport",
      
      # population
      "Population"
    )
  ) %>% 
  to_per_capita() %>% 
  mutate(value = value * 1e3,
         unit = "GJ/cap/yr")

# historical data Paul MRPFU ====
# Data sources	
# A	International Energy Agency (2022): World Energy Balances (2022 Edition) UK Data Service. Available at https://dx.doi.org/10.5257/iea/web/2022
# B	"Marshall ZHM, Brockway PE, Aramendia E, Steenwyk P, Relph T, Widjanarko M,et al. A Multi-Regional Primary-Final-Useful (MR-PFU) energy and exergy database v1.0, 1960-2020. 2023. https://doi.org/10.5518/1199 [Dataset]."
# C	World Bank (2023) World Development Indicators: NY.GDP.MKTP.KD Available at https://databank.worldbank.org/selection-of-indicators/id/fc321ecc

hist <- read_excel(
  path = here("data", "input", "2023_03_14_Historical GDP, PFU energy data for Jarmo.xlsx"),
  sheet = "raw data",
  range = "A8:BK14"
) 

# PROCESS ====

fe.to.ue <- li.raw.notnorm %>% filter(
  variable %in% c(
    "Final Energy",
    "Useful Energy"
  ) 
) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(
    `Final to Useful energy conversion` = `Useful Energy` / `Final Energy` 
  )

fe.to.ue.norm <- fe.to.ue %>% 
  select(model, scenario, region, unit, year, `Final to Useful energy conversion`) %>% 
  mutate(variable = "Final to Useful energy conversion") %>% 
  rename(value=`Final to Useful energy conversion`) %>% 
  normalise_iamc_long(starting.year = 2020) %>% 
  rename_li_data() %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type()

hist.fe.to.ue.norm <- hist %>% filter(`Raw energy + GDP Data (Australia)`%in%c("Final-to-useful energy efficiency")) %>% 
  pivot_longer(`1960`:`2019`, names_to = "year", values_to = "value") %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(
    model = "MRPFU",
    scenario = "Historical",
    region = "AUS"
  ) %>% 
  rename(
    variable = `Raw energy + GDP Data (Australia)`,
    unit = `Units`
  ) %>% 
  select(-`Data source`) %>% 
  # by (model, scenario, region, variable, unit)
  normalise_iamc_long(starting.year = 2019) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type()

# PLOT ====

p.fe.to.ue <- ggplot(
  fe.to.ue.norm %>% filter(year<=2100 #,
                           # `Annual consumption per capita (at utility peak)`!="baseline",
                           # `Annual consumption per capita (at utility peak)`!="10k"
  ),
  aes(x=year,y=value, colour=`Annual consumption per capita (at utility peak)`)
) +
  facet_wrap(`Climate policy`~., ncol = 1) +
  
  geom_line(
    data = hist.fe.to.ue.norm %>% mutate(`Climate policy`="Expand renewables") %>% 
      bind_rows(hist.fe.to.ue.norm %>% mutate(`Climate policy`="GHG budget")) %>% 
      bind_rows(hist.fe.to.ue.norm %>% mutate(`Climate policy`="Keep fossil fuels")) %>% 
      filter(year>1990),
    colour="black", 
    linetype="solid",
    linewidth=1.4
  ) +
  
  geom_line(aes(group=scenario),
            linewidth=1.1,
            linetype="dotdash") +
  geom_line(aes(group=scenario),
            data = . %>% filter(`Annual consumption per capita (at utility peak)`=="baseline"),
            linewidth=1.4) +
  scale_color_manual(
    values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8)),
  ) +
  # scale_color_colorblind() +
  ylab("Final to Useful energy conversion") +
  xlab(NULL) +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.background = element_rect(linewidth=0.5, linetype="solid", 
                                         colour ="black")) +
  labs(
    title = "Useful Energy divided by Final Energy",
    subtitle = "How much energy used by end-use technologies is useful?",
    caption = "Values normalised to 2019 (historical) and 2020 (modelled)"
  ) +
  theme_classic() +
  theme_hc()

p.fe.to.ue

# SAVE ====



save_ggplot(p = p.fe.to.ue, 
            f = here("output", paste0("sf02-",version)),
            h=200, w=200)
