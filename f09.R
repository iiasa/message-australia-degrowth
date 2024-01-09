#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 9: Carbon prices across scenarios
#' 
#' 
#' Last updated: 27.06.2023

source("load_basics_and_version.R")



# INIT ====
# ...

# LOAD ====
li <- vroom(here("data", "li.csv")) %>%  
  filter(variable%in%c("GHG emissions", "Carbon price")) %>%
  bind_rows(
    vroom(here("data", "li.csv")) %>% 
      filter(variable%in%c("GDP (PPP)", "Population")) %>% to_per_capita()
  ) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type()


# PLOT ====
p.carbonprice.heatmap <- ggplot(
  li %>%  
    mutate(scenario = paste0(model,"-",scenario)) %>% 
    select(scenario,year,variable,value, `GHG budget`, `Climate policy`, `Annual consumption per capita (at utility peak)`) %>% 
    pivot_wider(values_from = value, names_from = variable) %>% 
    filter(
      `Climate policy` == "GHG budget",
      year %in% c(2030,2040,2050)
    ),
  aes(x=`Annual consumption per capita (at utility peak)`,
      y=`GHG budget`)
) +
  facet_grid(year~.) +
  geom_tile(
    data=. %>% filter(`Carbon price`<400),
    aes(fill = `Carbon price`)
  ) +
  geom_tile(
    data=. %>% filter(`Carbon price`>400),
    fill = "darkred"
  ) +
  geom_tile(
    data=tibble(
      `GHG budget`="3Gt",
      `Annual consumption per capita (at utility peak)`=c("40k", "50k", "60k", "70k", "baseline"),
      `Carbon price`=c("Not\nfeasible.")
    ),
    fill = "lightgrey"
  ) +
  geom_tile(
    data=tibble(
      `GHG budget`="2Gt",
      `Annual consumption per capita (at utility peak)`=c("10k", "20k", "30k", "40k", "50k", "60k", "70k", "baseline"),
      `Carbon price`=c("Not\nfeasible.")
    ),
    fill = "lightgrey"
  ) +
  
  theme_minimal() +
  scale_fill_gradient2(name = "Carbon price\n[USD/tCO2]", 
                       # trans = "log",
                       # breaks = c(1,10,50,100,400),#500,1000,2000), 
                       # labels = c(1,10,50,100,400),#,500,1000,2000),
                       low = "lightgrey",
                       # mid = "darkred",
                       # midpoint = 400,
                       high = "darkred"
  ) +
  geom_text(
    data=. %>% filter(`Carbon price`>300),
    aes(
      label=round(`Carbon price`)
    ),
    colour="white"
  ) +
  
  geom_text(
    data = tibble(
      `GHG budget`="3Gt",
      `Annual consumption per capita (at utility peak)`=c("40k", "50k", "60k", "70k", "baseline"),
      `Carbon price`=c("Not\nfeasible.")
    ),
    aes(
      label=`Carbon price`
    ),
    colour="black",
    size=2.4
  ) +
  
  geom_text(
    data = tibble(
      `GHG budget`="2Gt",
      `Annual consumption per capita (at utility peak)`=c("10k", "20k", "30k", "40k", "50k", "60k", "70k", "baseline"),
      `Carbon price`=c("Not\nfeasible.")
    ),
    aes(
      label=`Carbon price`
    ),
    colour="black",
    size=2.4
  ) +
  
  
  
  labs(
    caption = "Numbers given for Carbon Price > 300USD"
  )


p.carbonprice.heatmap


# SAVE ====
save_ggplot(
  p = p.carbonprice.heatmap,
  f = here("output", paste0("f09-",version)),
  h=200, w=200
)
