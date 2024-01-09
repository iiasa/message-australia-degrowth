#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 8: Emissions statistics for C1 and degrowth scenarios
#' 
#' 
#' Last updated: 27.06.2023

source("load_basics_and_version.R")



# INIT ====
# ...

# LOAD ====
li <- vroom(here("data", "li_normalised.csv")) %>%  
  filter(variable%in%c("GHG emissions")) %>%
  bind_rows(
    vroom(here("data", "li_normalised.csv")) %>% 
      filter(variable%in%c("GDP (PPP)", "Population")) %>% to_per_capita()
  ) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type()
ipcc <- vroom(here("data", "ipcc_r10_normalised.csv")) %>% # 69 scenarios
  filter(variable%in%c("GHG emissions")) %>%
  bind_rows(
    vroom(here("data", "ipcc_r10_normalised.csv")) %>% 
      filter(variable%in%c("GDP (PPP)", "Population")) %>% to_per_capita()
  ) %>%
  filter_by_cat_ar6(cat="C1") %>% 
  filter(region=="R10PAC_OECD") %>% 
  rename_li_data() %>% 
  mutate(`Annual consumption per capita (at utility peak)` = NA)


# PROCESS ====
ipcc.stats <- ipcc %>% add_percentile_columns(percentiles = c(0.05,0.25,0.5,0.75,0.95)) %>% distinct(variable,year,p5,p25,p50,p75,p95) %>% 
  filter(year %in% (li %>% pull(year) %>% unique()))



# PLOT ====
yrs.vis.emis <- c(2020,2030,2040,2050,2060)
p.degr.v.climpol <- ggplot(
  mapping=aes(x=year)
) +
  
  geom_ribbon(
    data=ipcc.stats %>% filter(year%in%yrs.vis.emis),
    aes(
      ymin=1-p5,
      ymax=1-p95,
    ),
    fill="lightgrey",
    alpha=0.3
  ) +
  geom_ribbon(
    data=ipcc.stats %>% filter(year%in%yrs.vis.emis),
    aes(
      ymin=1-p25,
      ymax=1-p75,
    ),
    fill="lightgrey",
    alpha=0.5
  ) +
  
  
  geom_line(
    data=li %>% filter(year%in%yrs.vis.emis) %>% mutate(ms=paste0(model,scenario)),
    aes(
      colour=`GHG budget`,
      y=1-value,
      group=ms
    )
  ) +
  geom_point(
    data=li %>% filter(year%in%yrs.vis.emis) %>% mutate(ms=paste0(model,scenario)),
    aes(
      colour=`GHG budget`,
      shape=`Climate policy`,
      y=1-value,
      group=ms
    )
  ) +

  # layout
  facet_grid(variable~`Annual consumption per capita (at utility peak)`) +
  labs(title="GHG per capita emissions reductions",
       subtitle = "100% = net-zero GHGs. 0%=current emissions levels.",
       caption = "Grey shading: IPCC AR6 WG3 C1 interquartile (grey) and 5-95th percentile (lightgrey) ranges for Pacific OECD.",
       x=NULL) +
  theme_classic() +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_color_colorblind() +
  scale_y_reverse(labels = scales::percent) +
  ylab("Relative reduction (compared to 2020)")

p.degr.v.climpol


# SAVE ====
save_ggplot(
  p = p.degr.v.climpol,
  f = here("output", paste0("f08-",version)),
  h=200, w=300
)

