#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 6: Technology change (primary energy)
#' 
#' 
#' Last updated: 30.10.2023

source("load_basics_and_version.R")



# INIT ====
normalization.year <- 2020
budget.choice.highlight <- "4Gt"

upsc.yrs <- c(2020,2030,2040,2050,2060,2070,2080,2090,2100)


vars.tech <- c(
  "Primary Energy|Wind",
  "Primary Energy|Solar",
  "Primary Energy|Biomass",
  "Primary Energy|Fossil"
)

# LOAD ====
li.raw.notnorm <- vroom(here("data", "li.csv")) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type() %>% 
  filter(
    variable %in% vars.tech
  ) %>% 
  filter(
    `Climate policy` == "GHG budget"
  ) 
li.raw.notnorm <- li.raw.notnorm %>% filter(!(variable%in%c("Primary Energy|Wind","Primary Energy|Solar"))) %>% 
  bind_rows(
    li.raw.notnorm %>% filter(variable%in%c("Primary Energy|Wind","Primary Energy|Solar")) %>% 
      pivot_wider(names_from = variable, values_from = value) %>% 
      mutate(`Primary Energy|Wind and Solar` = `Primary Energy|Wind` + `Primary Energy|Solar`) %>% 
      select(-c("Primary Energy|Wind","Primary Energy|Solar")) %>% 
      mutate(variable="Primary Energy|Wind and Solar") %>% 
      rename(value=`Primary Energy|Wind and Solar`)
  )

li.norm <- li.raw.notnorm %>% 
  normalise_iamc_long(starting.year = normalization.year)


# PROCESS ====
baselines <- 
  li.raw.notnorm %>% 
  filter(
    year %in% upsc.yrs,
    `Annual consumption per capita (at utility peak)`=="baseline"
  ) %>% ungroup() %>% 
  select(variable,year,value, `GHG budget`) %>% rename(baseline=value)


baselines.norm <- 
  li.norm %>% 
  filter(
    year %in% upsc.yrs,
    `Annual consumption per capita (at utility peak)`=="baseline"
  ) %>% ungroup() %>% 
  select(variable,year,value, `GHG budget`) %>% rename(baseline=value)

# PLOT ====

# explore wind and solar only
p.scaleup.levels.all.wind.and.solar <- ggplot(li.raw.notnorm %>% 
                                 filter(
                                   year %in% upsc.yrs
                                 ) %>% left_join(baselines) %>% 
                                 filter(
                                   `GHG budget`!="3Gt"
                                 ) %>% 
                                 mutate_cond(variable == "Primary Energy|Biomass", variable = "Biomass") %>% 
                                 mutate_cond(variable == "Primary Energy|Fossil", variable = "Fossil") %>% 
                                 mutate_cond(variable == "Primary Energy|Wind and Solar", variable = "Wind and Solar") %>% 
                                 filter(variable=="Wind and Solar"), 
                               aes(y=value,alpha=`Annual consumption per capita (at utility peak)`)) +
  facet_grid(`GHG budget`~`Annual consumption per capita (at utility peak)`) +
  geom_hline(yintercept = 0, colour="darkgrey") +
  geom_col(
    aes(fill=as.factor(year),
        x=year),
    position = "dodge"
  ) +
  
  # add arrow
  geom_point(
    shape=95, # https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
    size=5,
    aes(
      x=year,
      y=baseline
    )
  ) +
  geom_segment(aes(
    x = year,
    xend = year,
    y = ifelse(abs(baseline-value)<0.3,NA,baseline),
    yend = value
  ),
  size = 0.3, linejoin = "round", lineend = "round", arrow = arrow(length = unit(0.05, "npc"))
  ) +
  
  geom_text(
    size=2,
    aes(
      x=year,
      y=baseline + 0.2,
      label = paste0(as.character(round(((baseline-value)/baseline)*100)),"%") 
    ),
    data = . %>% filter(year%in%c(2030, 2040, 2050, 2060, 2080, 2100))
  ) +
  
  scale_fill_manual(breaks = upsc.yrs, values = c("grey", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue")) +
  scale_alpha_manual(breaks = c("10k","20k","30k","40k","50k","60k","70k","baseline"), values = c(1, 1, 1, 0.4, 1, 1, 1, 1)) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(limits = c(0,9), expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  ylab("EJ/yr") + xlab(NULL) +
  labs(subtitle = paste0("Absolute levels, compared to SSP2 baseline")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.levels.all.wind.and.solar

p.scaleup.growth.all.wind.and.solar <- ggplot(li.norm %>% 
                                                filter(
                                                  year %in% upsc.yrs
                                                ) %>% left_join(baselines.norm) %>% 
                                                filter(
                                                  `GHG budget`!="3Gt"
                                                ) %>% 
                                                mutate_cond(variable == "Primary Energy|Biomass", variable = "Biomass") %>% 
                                                mutate_cond(variable == "Primary Energy|Fossil", variable = "Fossil") %>% 
                                                mutate_cond(variable == "Primary Energy|Wind and Solar", variable = "Wind and Solar") %>% 
                                                filter(variable=="Wind and Solar"), 
                                              aes(y=value,alpha=`Annual consumption per capita (at utility peak)`)) +
  facet_grid(`GHG budget`~`Annual consumption per capita (at utility peak)`) +
  geom_hline(yintercept = 0, colour="darkgrey") +
  
  # add arrow
  # geom_point(
  #   shape=95, # https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
  #   size=5,
  #   aes(
  #     x=year,
  #     y=baseline
  #   )
  # ) +
  geom_segment(aes(
    x = year,
    xend = year,
    y = ifelse(abs(baseline-value)<0.3,NA,baseline),
    yend = value
  ),
  data = .  %>% 
    filter(`Annual consumption per capita (at utility peak)`!="10k",
           year%in%c(2030, 2040, 2050, 2060, 2080, 2100)),
  size = 0.3, linejoin = "round", lineend = "round", arrow = arrow(length = unit(0.05, "npc"))
  ) +
  
  geom_ribbon(
    aes(
      x = year,
      ymin = value,
      ymax = baseline
    ),
    fill = "dodgerblue", alpha = 0.1,
  ) +
  
  geom_line(aes(x=year, y=value), linewidth=2, colour = "dodgerblue", 
            data = . %>% filter(`Annual consumption per capita (at utility peak)`!="10k")) +
  geom_line(aes(x=year, y=baseline), linewidth=2, colour = "darkgrey", 
            data = . %>% filter(`Annual consumption per capita (at utility peak)`!="10k")) +
  geom_textpath(aes(x=year, y=value), linewidth=2, colour = "dodgerblue", label = "Stopping\nGDP growth", size=3,
                data = . %>% filter(`Annual consumption per capita (at utility peak)`=="10k"),
                halign = "left",
                text_smoothing = 50,
                vjust = 0.2,
                hjust = 0.6) +
  geom_textpath(aes(x=year, y=baseline), linewidth=2, colour = "darkgrey",  label = "Continuing\nGDP growth", size=3,
                data = . %>% filter(`Annual consumption per capita (at utility peak)`=="10k"),
                halign = "left",
                text_smoothing = 50,
                vjust = 0.3,
                hjust = 0.6) +
  
  
  
  geom_text(
    size=3,
    aes(
      x=year-2,
      y=baseline + 0.5,
      label = paste0(as.character(round(((baseline-value)/baseline)*100)),"%") 
    ),
    data = . %>% filter(year%in%c(2030, 2040, 2050, 2060, 2080, 2100)) %>% 
      filter(`Annual consumption per capita (at utility peak)`!="10k")
  ) +
  
  
  
  scale_fill_manual(breaks = upsc.yrs, values = c("grey", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue")) +
  scale_alpha_manual(breaks = c("10k","20k","30k","40k","50k","60k","70k","baseline"), values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(limits = c(0,20), expand = c(0,0,0.1,0)) +
  theme_classic() +
  theme_hc() +
  ylab("Times bigger than 2020") + xlab(NULL) +
  labs(subtitle = paste0("Growth compared to 2020")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.growth.all.wind.and.solar



p.scaleup.levels.and.growth.wind.and.solar <- ((p.scaleup.levels.all.wind.and.solar + labs(title = "Wind and Solar (primary energy) under a GHG budget")
                                                ) / p.scaleup.growth.all.wind.and.solar) +
  plot_layout(heights = c(1,1)) +
  plot_annotation(tag_levels = "A")
p.scaleup.levels.and.growth.wind.and.solar



# SAVE ====
save_ggplot(p = p.scaleup.levels.and.growth.wind.and.solar, 
            f = here("output", paste0("sf06-",version)),
            h=450, w=325)
