#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 3: Primary Energy design of scenarios
#' 
#' 
#' Last updated: 12.05.2023

source("load_basics_and_version.R")



# INIT ====

# LOAD ====
li <- vroom(here("data", "li.csv"))

# PROCESS ====
li.prim <- li %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type() %>%
  filter(
    grepl(x=variable,pattern="Primary"),
    variable %in% c(
      "Primary Energy|Fossil",
      "Primary Energy|Solar",
      "Primary Energy|Wind",
      "Primary Energy|Biomass",
      "Primary Energy|Nuclear",
      "Primary Energy|Hydro",
      "Primary Energy"
    )
  ) %>% 
  mutate(variable=substr(variable, start=nchar("Primary Energy|")+1,stop=nchar(variable))) %>% 
  # filter(
  #   scenario %in% c(
  #     "SSP2-baseline-fossil",
  #     "SSP2-baseline-ren",
  #     "SSP2-baseline-4Gt"
  #   )
  # ) %>% 
  filter(
    `GHG budget` %in% c(
      "4Gt",
      "No GHG budget"
    )
  )
li.prim <- li.prim %>% 
  mutate(`Climate policy` = factor(`Climate policy`, levels=c("Keep fossil fuels", "Expand renewables", "GHG budget"))) %>% 
  filter(
    `Annual consumption per capita (at utility peak)`=="baseline"
  )

# PLOT ====

p.primary <- ggplot(
  data=li.prim %>% filter(!(variable=="")),
  aes(x=year, y=value)
) +
  facet_grid(.~`Climate policy`) + 
  # facet_grid(~scenario) + 
  scale_fill_manual(
    breaks = c("Fossil", "Solar", "Wind", "Biomass", "Nuclear", "Hydro"),
    values = c("grey", "darkgoldenrod1", "aquamarine3", "chartreuse4", "darkorchid1", "blue2")
  ) +
  # scale_fill_wsj() +
  geom_area(
    aes(fill=variable)
  ) +
  geom_textpath(
    data = li.prim %>% filter((variable=="")),
    label = "Total"
  ) +
  labs(title="Primary Energy",
       caption="Using 4GtCO2eq GHG budget.") +
  xlab(NULL) +
  ylab("EJ/yr") +
  scale_y_continuous(expand = c(0,0)) +
  # scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  guides(fill=guide_legend(NULL)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y.right = element_text(angle = 0))

p.primary


# SAVE ====
save_ggplot(p = p.primary, 
            f = here("output", paste0("f03-",version)),
            h=100, w=200)
