#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 4: demand reduction vs baseline
#' 
#' 
#' Last updated: 08.06.2023

source("load_basics_and_version.R")



# INIT ====
# ...

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

# PROCESS ====
# PROCESS: functions ====
rename_demand_category_variables <- function(df){
  return(
    df %>% 
      
      mutate_cond(variable=="Air transport", variable = "Air transport") %>% 
      mutate_cond(variable=="Industry feedstock", variable = "Industry (feedstock)") %>% 
      mutate_cond(variable=="Industry thermal", variable = "Industry (thermal)") %>% 
      mutate_cond(variable=="Industry_specific", variable = "Industry (specific)") %>% 
      mutate_cond(variable=="Rail transport and shipping", variable = "Shipping and rail") %>% 
      mutate_cond(variable=="Residential-commercial specific", variable = "Buildings (specific)") %>% 
      mutate_cond(variable=="Residential-commercial thermal", variable = "Buildings (thermal)") %>% 
      mutate_cond(variable=="Road transport", variable = "Road transport")
  )
}
# PROCESS: data preparation ====
ue.sectoral <- li.raw.notnorm %>% filter(
  variable %in% c(
    "Useful Energy|Air transport",
    "Useful Energy|Industry feedstock",
    "Useful Energy|Industry thermal",
    "Useful Energy|Industry_specific",
    "Useful Energy|Rail transport and shipping",
    "Useful Energy|Residential-commercial specific",
    "Useful Energy|Residential-commercial thermal",
    "Useful Energy|Road transport"
  ) 
) %>% 
  mutate(variable = substr(variable, nchar("Useful Energy|")+1, nchar(variable))) %>% 
  rename_demand_category_variables() %>% 
  rename(detailed.sector=variable) %>% 
  mutate(variable=ifelse(
    grepl(x=detailed.sector,pattern="Industry",fixed=T),
    "Industry",
    ifelse(
      grepl(x=detailed.sector,pattern="Buildings",fixed=T),
      "Buildings",
      detailed.sector
    )
  )) %>% 
  group_by(model,scenario,region,variable,unit,year,ssp,`Annual consumption per capita (at utility peak)`,`Scenario set`,`GHG budget`,`Climate policy`) %>% 
  summarise(value=sum(value)) %>% 
  ungroup()
ue.sectoral.baselines <- ue.sectoral %>% filter(`Annual consumption per capita (at utility peak)`=="baseline")
ue.sectoral.w.baselines <- ue.sectoral %>% 
  left_join(
    ue.sectoral.baselines %>% select(-scenario,-`Annual consumption per capita (at utility peak)`) %>% rename(baseline=value)
  ) %>% 
  mutate(
    `Total change from baseline` = value - baseline,
    `Percentage change from baseline` = (value - baseline)/baseline * 100
  )
ue.sectoral.w.baselines.numeric <- ue.sectoral.w.baselines %>% mutate(`GDP degrowth numeric` = as.numeric(substr(`Annual consumption per capita (at utility peak)`,0,2))) %>% 
  drop_na() # drop NAs which are baselines

# PLOT: functions ====
Create_Viz_Demand_Percentage <- function(yr){
  p.dots.numeric.relationship.percentage.v.paper <- ggplot(
    ue.sectoral.w.baselines.numeric %>% filter(year%in%c(yr)) %>% 
      bind_rows(
        ue.sectoral.w.baselines.numeric %>% filter(year%in%c(yr)) %>% mutate(`Annual consumption per capita (at utility peak)`="All")
      ) %>% 
      # filter(`Annual consumption per capita (at utility peak)`!="baseline") %>% 
      left_join(
        vroom(here("data", "li.csv")) %>% 
          add_degrowth_level() %>% 
          add_scenario_set_type() %>% 
          filter(
            variable %in% c(
              # GDP
              "GDP (PPP)",
              "Consumption",
              # population
              "Population"
            )
          ) %>% 
          to_per_capita() %>% 
          filter(variable=="Consumption") %>% 
          rename(`Consumption (thousand 2005USD)`=value) %>% select(model,scenario,year,`Consumption (thousand 2005USD)`)
          # rename(`GDP per capita (thousand PPP)`=value) %>% select(model,scenario,year,`GDP per capita (thousand PPP)`)
      ) %>% 
      rename_demand_category_variables()
  ) +
    # facet_grid(~variable) +
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_smooth(
      aes(
        # x=`GDP degrowth numeric`,
        # x=`GDP per capita (thousand PPP)`,
        x=`Consumption (thousand 2005USD)`,
        y=`Percentage change from baseline`,
        colour=variable,
        fill=variable,
        # linetype=`Climate policy`
      )
    ) +
    geom_point(
      aes(
        # x=`GDP degrowth numeric`,
        # x=`GDP per capita (thousand PPP)`,
        x=`Consumption (thousand 2005USD)`,
        y=`Percentage change from baseline`,
        colour=variable,
        shape=`Climate policy`
      )
    ) +
    labs(
      subtitle = paste0("Percentage change. Year: ", as.character(yr))
    ) +
    theme_classic() +
    theme_hc() +
    guides(color=guide_legend(""),fill=guide_legend("")) +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    scale_y_continuous(limits = c(-100,20), expand = c(0,0), name = "Change (%)\ncompared to baseline") +
    scale_x_reverse(breaks = seq(10,90,20), limits=c(70,10), name = "Consumption per capita\n(thousand 2005USD)") #+
  # scale_y_continuous(limits = c(-100,5)) + # note: check what is up with a few increases (instability, esp. 30k ren rescom spec)
  # theme(
  #   panel.grid.major.x = element_line("lightgrey"),
  #   panel.grid.minor.x = element_blank(),
  #   panel.grid.major.y = element_blank(),
  #   panel.grid.minor.y = element_blank(),
  # ) 
  
  return(p.dots.numeric.relationship.percentage.v.paper)  
}


# PLOT ====
p.timeseries.demand <- ggplot(
  ue.sectoral.w.baselines %>% filter(year<=2050) %>%  
    rename_demand_category_variables(),
  aes(
    x=year
  )
) +
  facet_grid(~variable) +
  
  geom_line(
    data=. %>% filter(`Annual consumption per capita (at utility peak)`!="baseline"),
    aes(
      y=value,
      group=scenario,
      colour=variable,
    ),
    # colour="lightgrey",
    alpha=0.1,
    linewidth=0.7,
    linetype = "solid"
  ) +
  geom_textpath(
    data=. %>% filter(`Annual consumption per capita (at utility peak)`=="baseline"),
    label="SSP2 baseline",
    aes(
      y=value,
      group=scenario
    ),
    colour="black",
    linetype = "dashed",
    linewidth = 1.5
  ) + 
  theme_classic() +
  theme_hc() +
  ylab("GJ/cap/yr") +
  xlab(NULL) +
  scale_color_colorblind() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    # caption = "Thinner, lighter lines are scenarios with lowered consumption.",
    subtitle = "Energy demand per capita by sector"
  )
p.timeseries.demand
  
p.timeseries.consumption <- ggplot(
  vroom(here("data", "li.csv")) %>% filter(year<=2050) %>% 
    add_degrowth_level() %>% 
    add_scenario_set_type() %>% 
    filter(
      variable %in% c(
        # GDP
        "GDP (PPP)",
        "Consumption",
        # population
        "Population"
      )
    ) %>% 
    to_per_capita() %>% 
    filter(variable=="Consumption"),
  aes(
    x=year
  )
) +
  geom_line(
    data=. %>% filter(!grepl(x=scenario, pattern="baseline", fixed=T)),
    aes(
      y=value,
      group=scenario
    ),
    colour="lightgrey",
    linetype = "solid"
  ) +
  geom_textpath(
    data=. %>% filter(grepl(x=scenario, pattern="baseline", fixed=T)),
    label="SSP2 baseline",
    aes(
      y=value,
      group=scenario
    ),
    linetype = "dashed",
    linewidth = 1.5
  ) + 
  theme_classic() +
  theme_hc() +
  scale_y_continuous(limits = c(10,80)) +
  ylab("Thousand 2005USD/year") +
  xlab(NULL) +
  scale_color_colorblind() +
  theme(legend.position = "none") +
  labs(
    caption = "Thinner, lighter lines are scenarios with lowered consumption.",
    subtitle = "Consumption per capita"
  )
p.timeseries.consumption


p.dots.numeric.relationship.v.paper <- ggplot(
  ue.sectoral.w.baselines.numeric %>% filter(year%in%c(2050)) %>% 
    bind_rows(
      ue.sectoral.w.baselines.numeric %>% filter(year%in%c(2050)) %>% mutate(`Annual consumption per capita (at utility peak)`="All")
    ) %>% 
    # filter(`Annual consumption per capita (at utility peak)`!="baseline") %>% 
    left_join(
      vroom(here("data", "li.csv")) %>% 
        add_degrowth_level() %>% 
        add_scenario_set_type() %>% 
        filter(
          variable %in% c(
            # GDP
            "GDP (PPP)",
            "Consumption",
            # population
            "Population"
          )
        ) %>% 
        to_per_capita() %>% 
        filter(variable=="Consumption") %>% 
        rename(`Consumption (thousand 2005USD)`=value) %>% select(model,scenario,year,`Consumption (thousand 2005USD)`)
      # rename(`GDP per capita (thousand PPP)`=value) %>% select(model,scenario,year,`GDP per capita (thousand PPP)`)
    ) %>%  
    rename_demand_category_variables()
) +
  facet_grid(~variable) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_smooth(
    aes(
      # x=`GDP degrowth numeric`,
      # x=`GDP per capita (thousand PPP)`,
      x=`Consumption (thousand 2005USD)`,
      y=`Total change from baseline`,
      colour=variable,
      fill=variable,
      # linetype=`Climate policy`
    )
  ) +
  geom_point(
    aes(
      # x=`GDP degrowth numeric`,
      # x=`GDP per capita (thousand PPP)`,
      x=`Consumption (thousand 2005USD)`,
      y=`Total change from baseline`,
      colour=variable,
      shape=`Climate policy`
    )
  ) +
  labs(
    # title = "How much is energy demand reduced under different degrowth levels?",
    subtitle = "Absolute change compared to baseline. Year: 2050"
  ) +
  theme_classic() +
  theme_hc() +
  ylab("GJ/cap/yr") +
  scale_x_reverse(breaks = seq(10,80,20)) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  guides(color=guide_legend(""),fill=guide_legend(""))
p.dots.numeric.relationship.v.paper



# SAVE ====


p.demand.paper <- ((p.timeseries.demand | p.timeseries.consumption) + plot_layout(widths = c(5,1))) / 
  ((Create_Viz_Demand_Percentage(yr=2030)+theme(legend.position="none") | Create_Viz_Demand_Percentage(yr=2040)+theme(legend.position="none") | Create_Viz_Demand_Percentage(yr=2050)+theme(legend.position="none")) /
                     p.dots.numeric.relationship.v.paper +
                     theme(legend.position="bottom", legend.box="vertical", legend.margin=margin() )) +
  plot_annotation(tag_levels = "A",
                  title = "How much is useful energy demand reduced under different degrowth levels?")

p.demand.paper

save_ggplot(
  p = p.demand.paper, 
  f = here("output", paste0("f04-",version)),
  h=300, w=300
)
