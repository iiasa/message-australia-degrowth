#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 2: scenario setup
#' 
#' 
#' Last updated: 27.06.2023

source("load_basics_and_version.R")



# INIT ====
degrowth.choice <- "40k"

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
  


# NUMBER: UE reduction ====
li %>% to_per_capita() %>% filter(variable=="Energy (useful)", `Annual consumption per capita (at utility peak)`=="10k") %>% mutate(value=value*1e3) %>% 
  filter(grepl(x = `Climate policy`, pattern = "Keep fossil", fixed=T)) %>%
  # filter(grepl(x = `GHG budget`, pattern = "4Gt", fixed=T)) %>%
  filter(year<=2100) %>% normalise_iamc_long(starting.year = 2020) %>% filter(year%in%c(2030,2050)) %>% pull(value,year)
# NUMBER: FE reduction ====
li %>% to_per_capita() %>% filter(variable=="Energy (final)", `Annual consumption per capita (at utility peak)`=="10k") %>% mutate(value=value*1e3) %>% 
  filter(grepl(x = `Climate policy`, pattern = "Keep fossil", fixed=T)) %>%
  # filter(grepl(x = `GHG budget`, pattern = "4Gt", fixed=T)) %>%
  filter(year<=2100) %>% normalise_iamc_long(starting.year = 2020) %>% filter(year%in%c(2030,2050)) %>% pull(value,year)


# NUMBER: GDP/cap reduction (baseline) ====
li %>% to_per_capita() %>% filter(variable=="GDP (PPP)", `Annual consumption per capita (at utility peak)`=="baseline") %>% 
  filter(year%in%c(2020,2040)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(annual.growth.rate= (`2040`/`2020`)^(1/20) - 1 ) %>% summarise(mean(annual.growth.rate), sd(annual.growth.rate))
# NUMBER: GDP/cap reduction (10k) ====
li %>% to_per_capita() %>% filter(variable=="GDP (PPP)", `Annual consumption per capita (at utility peak)`=="10k") %>% 
  filter(year%in%c(2020,2040)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(annual.growth.rate= (`2040`/`2020`)^(1/20) - 1 ) %>% summarise(mean(annual.growth.rate), sd(annual.growth.rate))

# NUMBER: GDP reduction (baseline) ====
li %>% filter(variable=="GDP (PPP)", `Annual consumption per capita (at utility peak)`=="baseline") %>% 
  filter(year%in%c(2020,2040)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(annual.growth.rate= (`2040`/`2020`)^(1/20) - 1 ) %>% summarise(mean(annual.growth.rate), sd(annual.growth.rate))
# NUMBER: GDP reduction (10k) ====
li %>% filter(variable=="GDP (PPP)", `Annual consumption per capita (at utility peak)`=="10k") %>% 
  filter(year%in%c(2020,2040)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(annual.growth.rate= (`2040`/`2020`)^(1/20) - 1 ) %>% summarise(mean(annual.growth.rate), sd(annual.growth.rate))



# PLOT: emissions per capita ====
p.emissions.design <- ggplot(data = li %>% to_per_capita() %>% filter(variable=="GHG emissions") %>% 
                               # filter(grepl(x = `Climate policy`, pattern = "GHG budget", fixed=T)) %>%
                               filter(grepl(x = `Annual consumption per capita (at utility peak)`, pattern = degrowth.choice, fixed=T)|grepl(x = `Annual consumption per capita (at utility peak)`, pattern = "baseline", fixed=T)) %>%
                               filter(year<=2100),
                             mapping = aes(x=year,y=value,group=scenario)
) +
  geom_line(
    data = . %>% filter(!(scenario%in%c(paste0("SSP2-", degrowth.choice, "-fossil"), paste0("SSP2-", degrowth.choice, "-ren"),"SSP2-baseline-fossil", "SSP2-baseline-ren"))),
    aes(colour=`GHG budget`, linetype=`Annual consumption per capita (at utility peak)`),
    linewidth=1.1
  ) +
  geom_textpath(
    data = . %>% filter((scenario%in%c("SSP2-baseline-fossil", "SSP2-baseline-ren"))),
    colour = "#7f3b08",
    size  = 4, 
    aes(label = scenario, linetype=`Annual consumption per capita (at utility peak)`), 
    linewidth = 1.2, hjust = 0.7 # close to 3/4 of the line
  ) +
  geom_textpath(
    data = . %>% filter((scenario%in%c(paste0("SSP2-", degrowth.choice, "-fossil"), paste0("SSP2-", degrowth.choice, "-ren")))),
    colour = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8))[[4]],
    size  = 4, 
    aes(label = scenario, linetype=`Annual consumption per capita (at utility peak)`), 
    linewidth = 1.2, hjust = 0.9 # close to 3/4 of the line
  ) +
  
  
  # demand-side reduction potential
  
  geom_segment(aes(
    x = 2015, 
    xend = 2055,
    y = 19,
    yend = 19
  ),
  linewidth = 1, linejoin = "round", lineend = "round", linetype="dotted"
  ) +
  geom_segment(aes(
    x = 2050, 
    xend = 2050,
    y = 19,
    yend = 19*0.43
  ),
  linewidth = 1.7, linejoin = "round", lineend = "round", arrow = arrow(length = unit(0.05, "npc"))
  ) +
  geom_text(
    x=2052, hjust = 0,
    y=15,
    label="Demand-side\nmitigation potential\n(IEA)"
  ) +
  geom_segment(aes(
    x = 2047.5, 
    xend = 2052.5,
    y = 19*0.43,
    yend = 19*0.43
  ),
  linewidth = 1, linejoin = "round", lineend = "round", linetype="dotted"
  ) +
  
  
  geom_ribbon(
    data=tibble(
      yr = c(2045, 2055),
      low.potential.ipcc = 19*0.6,
      high.potential.ipcc = 19*0.3,
      scenario="ipcc",
      value=-100
    ),
    alpha=0.4,
    aes(x = yr, ymax = low.potential.ipcc,ymin = high.potential.ipcc)
    )+
  geom_text(
    x=2055, hjust = 1,
    y=6.9,
    label="Demand-side\nmitigation potential\n(range IPCC)"
  ) +

  
  
  # scale_color_colorblind() +
  scale_color_manual(breaks = c("3Gt", "4Gt", "5Gt", "6Gt", "7Gt"), values = ggthemes::colorblind_pal()(5)) +
  theme_hc() +
  ylab("GHG emissions per capita (tCO2eq/yr)") + xlab(NULL) +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  labs(title = "Climate policies") +
  guides(colour=guide_legend(nrow=1,byrow=TRUE), label="none", linetype=guide_legend(nrow=1,byrow=TRUE))

p.emissions.design

# PLOT: economics design ====
p.econ.design <- 
  ggplot(data = li %>% to_per_capita() %>% filter(variable%in%c("Consumption", "GDP (PPP)")) %>% 
           filter(grepl(x = `Climate policy`, pattern = "Keep fossil", fixed=T)) %>%
           filter(year<=2100),
         mapping = aes(x=year,y=value,group=scenario,colour=scenario,linetype=variable)
  ) +
  geom_textpath(
    data = . %>% filter(variable=="GDP (PPP)"),
    size  = 2.67, aes(label = paste0(ssp,"-",`Annual consumption per capita (at utility peak)`)), linewidth = 1, hjust = 0.9 # on 3/4 of the line
  ) +
  geom_textpath(
    data = . %>% filter(variable=="Consumption"),
    size  = 4, aes(label = paste0(ssp,"-",`Annual consumption per capita (at utility peak)`)), linewidth = 1.2, hjust = 0.65 # on 3/4 of the line
  ) +
  scale_color_manual(
    values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8)),
    # values = c("#E08214", "#D27712", "#C36D11", "#B5630F", "#A7580D", "#9A4E0B", "#8C450A", "#7F3B08")
    # values = c("lightgrey", "lightgrey", "lightgrey", "#B5630F", "lightgrey", "lightgrey", "lightgrey", "#7F3B08")
  ) +
  
  theme_hc() +
  ylab("Thousand 2005USD/year") + xlab(NULL) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130), limits=c(0,130), expand = c(0,0)) +
  scale_linetype_manual(breaks = c("Consumption", "GDP (PPP)"), values = c("twodash", "dotted")) +
  guides(label=FALSE, colour=FALSE, linetype=guide_legend(title = "Economic variable",)) +
  labs(title = "Consumption and \nGDP (PPP) per capita")
p.econ.design

# PLOT: combine ====
p.design <- ( p.econ.design | p.emissions.design) + plot_layout(
  widths = c(1,1.9)
) +
  plot_annotation(tag_levels = "A",
                  title = "Design of scenario set along consumption and climate policy")
p.design

# SAVE ====
save_ggplot(
  p = p.design,
  f = here("output", paste0("f02-",version)),
  h=250, w=250
)

