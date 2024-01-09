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



# PLOT ====
p.scaleup.levels.all <- ggplot(li.raw.notnorm %>% 
                             filter(
                               year %in% upsc.yrs
                             ) %>% left_join(baselines) %>% 
                             filter(
                               `GHG budget`==budget.choice.highlight
                             ) %>% 
                               mutate_cond(variable == "Primary Energy|Biomass", variable = "Biomass") %>% 
                               mutate_cond(variable == "Primary Energy|Fossil", variable = "Fossil") %>% 
                               mutate_cond(variable == "Primary Energy|Wind and Solar", variable = "Wind and Solar"), 
                           aes(y=value,alpha=`Annual consumption per capita (at utility peak)`)) +
  facet_grid(variable~`Annual consumption per capita (at utility peak)`) +
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
  
  scale_fill_manual(breaks = upsc.yrs, values = c("grey", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue")) +
  scale_alpha_manual(breaks = c("10k","20k","30k","40k","50k","60k","70k","baseline"), values = c(1, 1, 1, 0.4, 1, 1, 1, 1)) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(limits = c(0,9), expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  ylab("EJ/yr") + xlab(NULL) +
  labs(subtitle = paste0("Primary Energy: all scenarios,\nwith a ", budget.choice.highlight, "CO2eq carbon budget.")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.levels.all

p.scaleup.levels.40k <- ggplot(li.raw.notnorm %>% 
                             filter(
                               year %in% upsc.yrs
                             ) %>% left_join(baselines) %>% 
                             filter(
                               `GHG budget`==budget.choice.highlight,
                               `Annual consumption per capita (at utility peak)`=="40k"
                             ) %>% 
                               mutate_cond(variable == "Primary Energy|Biomass", variable = "Biomass") %>% 
                               mutate_cond(variable == "Primary Energy|Fossil", variable = "Fossil") %>% 
                               mutate_cond(variable == "Primary Energy|Wind and Solar", variable = "Wind and Solar"), 
                           aes(y=value)) +
  facet_grid(`Annual consumption per capita (at utility peak)`~variable) +
  geom_hline(yintercept = 0, colour="darkgrey") +
  
  geom_segment(aes(
    x = year,
    xend = year,
    y = ifelse(abs(baseline-value)<0.5,NA,baseline-0.2),
    yend = value + 0.1
  ),
  size = 0.3, linejoin = "round", lineend = "round", arrow = arrow(length = unit(0.05, "npc")),
  data = . %>% filter(year%in%c(2030, 2040, 2050, 2060, 2080, 2100))
  ) +
  geom_segment(aes(
    x = year,
    xend = year,
    y = ifelse(abs(baseline-value)<0.5&abs(baseline-value)>0.1,baseline,NA),
    yend = value
  ),
  size = 0.3, linejoin = "round", lineend = "round", arrow = arrow(length = unit(0.02, "npc")),
  data = . %>% filter(year%in%c(2030, 2040, 2050, 2060, 2080, 2100))
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
            data = . %>% filter(variable!="Wind and Solar")) +
  geom_line(aes(x=year, y=baseline), linewidth=2, colour = "darkgrey", 
            data = . %>% filter(variable!="Wind and Solar")) +
  geom_textpath(aes(x=year, y=value), linewidth=2, colour = "dodgerblue", label = "Stopping GDP growth",
                hjust = 0.6,
                text_smoothing = 50,
                data = . %>% filter(variable=="Wind and Solar")) +
  geom_textpath(aes(x=year, y=baseline), linewidth=2, colour = "darkgrey",  label = "Continuing GDP growth",
                hjust = 0.7,
                text_smoothing = 50,
                data = . %>% filter(variable=="Wind and Solar")) +


  geom_text(
    size=3,
    aes(
      x=year,
      y=baseline + 0.5,
      label = ifelse(((baseline-value)/baseline)*100>25,paste0(as.character(round(((baseline-value)/baseline)*100)),"%"),NA) 
    ),
    data = . %>% filter(year%in%c(2030, 2040, 2050, 2060, 2080, 2100))
  ) +
  
  
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(limits = c(0,9), expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  ylab("EJ/yr") + xlab(NULL) +
  labs(title = paste0("Energy supply technology change in net-zero scenario under different growth pathways"),
       subtitle = paste0("Primary Energy: 40k compared to SSP2-baseline, with a ", budget.choice.highlight, "CO2eq carbon budget.")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.levels.40k


p.scaleup.levels <- (p.scaleup.levels.40k / p.scaleup.levels.all) +
  plot_layout(heights = c(1,1)) +
  plot_annotation(tag_levels = "A")
p.scaleup.levels


# NUMBERS: for text ====
# NUMBERS: 2040 fossil reduction
li.raw.notnorm %>% 
  filter(
    year %in% c(2020,2040)
  ) %>% 
  filter(
    `GHG budget`==budget.choice.highlight,
    variable == "Primary Energy|Fossil"
  ) %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(`foss.phaseout`=(1-(`2040`/`2020`))*100) %>% pull(foss.phaseout)

# NUMBERS: 2040 biomass reduction compared to baseline 
li.raw.notnorm %>% left_join(baselines) %>%  
  filter(
    year %in% c(2040)
  ) %>% 
  filter(
    `GHG budget`==budget.choice.highlight,
    `Annual consumption per capita (at utility peak)`=="40k",
    variable == "Primary Energy|Biomass"
  ) %>%  
  mutate(`bio.relief`=(1-(`value`/`baseline`))*100) %>% pull(bio.relief)

# NUMBERS: 2040, 2050, 2060 solar and wind reduction compared to baseline 
li.raw.notnorm %>% left_join(baselines) %>%  
  filter(
    year %in% c(2040, 2050, 2060, 2100)
  ) %>% 
  filter(
    `GHG budget`==budget.choice.highlight,
    `Annual consumption per capita (at utility peak)`=="40k",
    variable == "Primary Energy|Wind and Solar"
  ) %>%  
  mutate(`sw.relief`=(1-(`value`/`baseline`))*100) %>% pull(sw.relief)

# NUMBERS: 2020 to 2030 upscaling of solar and wind in 40k (in figure)
li.norm %>% filter(
  `GHG budget`==budget.choice.highlight,
  `Annual consumption per capita (at utility peak)`=="40k",
  variable == "Primary Energy|Wind and Solar"
) %>% filter(year=="2030") %>% pull(value) %>% round(digits=2)
li.norm %>% filter(
  `GHG budget`==budget.choice.highlight,
  `Annual consumption per capita (at utility peak)`=="baseline",
  variable == "Primary Energy|Wind and Solar"
) %>% filter(year=="2030") %>% pull(value) %>% round(digits=2)

# NUMBERS: 2020 to 2100 upscaling of solar and wind in 40k (in figure)
li.norm %>% filter(
  `GHG budget`==budget.choice.highlight,
  `Annual consumption per capita (at utility peak)`=="40k",
  variable == "Primary Energy|Wind and Solar"
) %>% filter(year=="2100") %>% pull(value) %>% round(digits=2)
li.norm %>% filter(
  `GHG budget`==budget.choice.highlight,
  `Annual consumption per capita (at utility peak)`=="baseline",
  variable == "Primary Energy|Wind and Solar"
) %>% filter(year=="2100") %>% pull(value) %>% round(digits=2)


# # NUMBERS: upscaling of solar and wind in the baseline (to double-check constraint one needs to look at activity data)
# full.upscaling.no.retiring.25p <- tibble(
#   year = seq(2020,2100,10) 
# ) %>% 
#   mutate(
#     constraint.value = 1*1.25^(year-2020)
#   )
# li.raw.notnorm %>% normalise_iamc_long(starting.year = 2020) %>% 
#   filter(variable=="Capacity|Wind and Solar")

# SAVE ====
save_ggplot(p = p.scaleup.levels, 
            f = here("output", paste0("f06-",version)),
            h=300, w=250)
