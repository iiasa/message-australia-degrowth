#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Supplementary Figure 5: scenarios overview
#' 
#' 
#' Last updated: 12.05.2023

source("load_basics_and_version.R")

# TODO:
# - make carbon price logarithmic?

# INIT ====
# ...

# LOAD ====
li.raw <- vroom(here("data", "li.csv"))

# PROCESS ====
li <- li.raw  %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type() %>% 
  filter(
    variable %in% c(
      "Final Energy",
      "GDP (PPP)",
      "GHG emissions",
      "Population",
      "Useful Energy",
      "Secondary Energy",
      "Primary Energy",
      "Carbon price",
      "Consumption"
    )
  ) %>% 
  
  # add solar-wind itself
  bind_rows(
    vroom(here("data", "li.csv")) %>% 
      add_degrowth_level() %>% 
      add_scenario_set_type() %>% 
      filter(
        variable %in% c(
          "Primary Energy|Wind",
          "Primary Energy|Solar"
        )
      ) %>% 
      pivot_wider(names_from = variable, values_from = value) %>% 
      mutate(`Wind and Solar\n(primary energy)` = `Primary Energy|Wind` + `Primary Energy|Solar`) %>% 
      select(-c("Primary Energy|Wind","Primary Energy|Solar")) %>% 
      mutate(variable="Wind and Solar\n(primary energy)") %>% 
      rename(value=`Wind and Solar\n(primary energy)`)
  ) %>% 
  
  
  
  mutate_cond(variable=="Primary Energy", variable = "Energy (primary)") %>% 
  mutate_cond(variable=="Secondary Energy", variable = "Energy (secondary)") %>% 
  mutate_cond(variable=="Final Energy", variable = "Energy (final)") %>% 
  mutate_cond(variable=="Useful Energy", variable = "Energy (useful)") %>% 
  mutate_cond(variable=="Carbon price", variable = "Price of carbon")


# PLOT ====
p.overview.many.lines <- ggplot(
  li %>% 
    filter(variable%in%c("Population", "Price of carbon", "Wind and Solar\n(primary energy)")) %>% 
    bind_rows(
      li %>% 
        filter(!(variable%in%c("Price of carbon", "Wind and Solar\n(primary energy)"))) %>% 
        to_per_capita() %>% 
        mutate(variable=paste0(variable,"\nper capita"))
    ) %>% 
    mutate(value=ifelse(grepl(x=variable,pattern="Energy",fixed=T),value*1e3,value)) %>% 
    filter(year<=2100) %>% 
    mutate_cond(variable=="Consumption\nper capita", variable = paste0(variable,"\n[thousand USD/yr]")) %>% 
    mutate_cond(variable=="Energy (final)\nper capita", variable = paste0(variable,"\n[GJ/cap/yr]")) %>% 
    mutate_cond(variable=="Energy (primary)\nper capita", variable = paste0(variable,"\n[GJ/cap/yr]")) %>% 
    mutate_cond(variable=="Energy (secondary)\nper capita", variable = paste0(variable,"\n[GJ/cap/yr]")) %>% 
    mutate_cond(variable=="Energy (useful)\nper capita", variable = paste0(variable,"\n[GJ/cap/yr]")) %>% 
    mutate_cond(variable=="GDP (PPP)\nper capita", variable = paste0(variable,"\n[thousand USD/yr]")) %>% 
    mutate_cond(variable=="GHG emissions\nper capita", variable = paste0(variable,"\n[tCO2eq]")) %>% 
    mutate_cond(variable=="Population", variable = paste0(variable,"\n[million]")) %>%
    mutate_cond(variable=="Price of carbon", variable = paste0(variable,"\n[USD/tCO2]")) %>% 
    mutate_cond(variable=="Wind and Solar\n(primary energy)", variable = paste0(variable,"\n[EJ/yr]")),
  aes(x=year,y=value, colour=`Climate policy`)
) +
  facet_grid(variable~`Annual consumption per capita (at utility peak)`, scales = "free_y") +
  geom_line(aes(group=scenario)) + #, #alpha=`Annual consumption per capita (at utility peak)`)) +
  geom_point(data=. %>% filter(`Climate policy`=="GHG budget"), aes(group=scenario, shape=`GHG budget`)) + #, #alpha=`Annual consumption per capita (at utility peak)`)) +
  scale_color_colorblind() +
  ylab(NULL) +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y.right = element_text(angle = 0))


p.overview.many.lines


# SAVE ====
save_ggplot(p = p.overview.many.lines, 
            f = here("output", paste0("sf05-",version)),
            h=400, w=300)
