#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 5: Comparing final energy reductions to other literature
#' 
#' 
#' Last updated: 08.06.2023

source("load_basics_and_version.R")

# INIT ====
# ...

# LOAD ====
led.raw <- vroom(here("data","input", "LED_R5_OECD_finalenergy.csv"))

li.raw <- vroom(here("data", "li.csv"))




# PROCESS ====
led <- led.raw %>% iamc_wide_to_long(upper.to.lower = F)

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


# feasibility on final energy demand
li.feas.concerns <- li %>% filter(variable=="Energy (final)") %>% 
  left_join(
    li %>% filter(variable=="Energy (final)",year==2020) %>% rename(start.fe=value) %>% select(-year)
  ) %>% 
  # filter(year%in%seq(2020,2100,10)) %>% 
  mutate(
    `Medium concern threshold` = start.fe*(0.9)^((year-2020)/10),
    `High concern threshold` = start.fe*(0.8)^((year-2020)/10)
  )
fe.start <- li %>% filter(variable=="Energy (final)",year==2020,scenario=="SSP2-baseline-ren") %>% pull(value)

# PLOT: Final Energy per capita ====

p.fe <- 
  ggplot(data = li %>% to_per_capita() %>% filter(variable=="Energy (final)") %>% mutate(value=value*1e3) %>% 
           filter(grepl(x = `Climate policy`, pattern = "Expand", fixed=T)) %>%
           # filter(grepl(x = `GHG budget`, pattern = "4Gt", fixed=T)) %>%
           filter(year<=2100),
         mapping = aes(x=year)
  ) +
  
  # feasibility energy demand
  geom_ribbon(
    data = li.feas.concerns %>% select(-variable,-value) %>% pivot_longer(
      `Medium concern threshold`:`High concern threshold`,
      names_to = "variable", values_to = "value"
    ) %>%  
      bind_rows(li %>% filter(variable=="Population")) %>% 
      filter(scenario=="SSP2-baseline-ren") %>%
      to_per_capita() %>% mutate(value=value*1e3, unit="GJ/cap/yr") %>% 
      pivot_wider(names_from = variable, values_from = value), # pick one scenario to avoid overlapping areas
    aes(x=year,ymax=`Medium concern threshold`,ymin=`High concern threshold`),
    alpha=0.3,
    colour=NA,
    fill="grey"
  ) + 
  geom_ribbon(
    data = li.feas.concerns %>% select(-variable,-value) %>% pivot_longer(
      `Medium concern threshold`:`High concern threshold`,
      names_to = "variable", values_to = "value"
    ) %>%  
      bind_rows(li %>% filter(variable=="Population")) %>% 
      filter(scenario=="SSP2-baseline-ren") %>%
      to_per_capita() %>% mutate(value=value*1e3, unit="GJ/cap/yr") %>% 
      pivot_wider(names_from = variable, values_from = value), # pick one scenario to avoid overlapping areas
    aes(x=year,ymax=`High concern threshold`,ymin=0),
    alpha=0.6,
    colour=NA,
    fill="grey"
  ) + 
  geom_textline(
    data = li.feas.concerns %>% select(-variable,-value) %>% pivot_longer(
      `Medium concern threshold`:`High concern threshold`,
      names_to = "variable", values_to = "value"
    ) %>%  
      bind_rows(li %>% filter(variable=="Population")) %>% 
      filter(scenario=="SSP2-baseline-ren") %>%
      to_per_capita() %>% mutate(value=value*1e3, unit="GJ/cap/yr") %>% 
      pivot_wider(names_from = variable, values_from = value), # pick one scenario to avoid overlapping areas
    aes(x=year,y=`Medium concern threshold`),
    alpha=0.6,
    linewidth = 0,
    vjust = 1,
    hjust=0.75,
    colour="grey",
    label="Medium feasibility concern",
    fontface="bold"
  ) +
  geom_line(
    data = li.feas.concerns %>% select(-variable,-value) %>% pivot_longer(
      `Medium concern threshold`:`High concern threshold`,
      names_to = "variable", values_to = "value"
    ) %>%  
      bind_rows(li %>% filter(variable=="Population")) %>% 
      filter(scenario=="SSP2-baseline-ren") %>%
      to_per_capita() %>% mutate(value=value*1e3, unit="GJ/cap/yr") %>% 
      pivot_wider(names_from = variable, values_from = value), # pick one scenario to avoid overlapping areas
    aes(x=year,y=`Medium concern threshold`),
    alpha=0.6,
    colour="grey"
  ) +
  geom_textline(
    data = li.feas.concerns %>% select(-variable,-value) %>% pivot_longer(
      `Medium concern threshold`:`High concern threshold`,
      names_to = "variable", values_to = "value"
    ) %>%  
      bind_rows(li %>% filter(variable=="Population")) %>% 
      filter(scenario=="SSP2-baseline-ren") %>%
      to_per_capita() %>% mutate(value=value*1e3, unit="GJ/cap/yr") %>% 
      pivot_wider(names_from = variable, values_from = value), # pick one scenario to avoid overlapping areas
    aes(x=year,y=`High concern threshold`),
    alpha=1,
    linewidth = 0,
    vjust = 1,
    hjust=0.75,
    colour="grey",
    label="High feasibility concern",
    fontface="bold"
  ) +
  geom_line(
    data = li.feas.concerns %>% select(-variable,-value) %>% pivot_longer(
      `Medium concern threshold`:`High concern threshold`,
      names_to = "variable", values_to = "value"
    ) %>%  
      bind_rows(li %>% filter(variable=="Population")) %>% 
      filter(scenario=="SSP2-baseline-ren") %>%
      to_per_capita() %>% mutate(value=value*1e3, unit="GJ/cap/yr") %>% 
      pivot_wider(names_from = variable, values_from = value), # pick one scenario to avoid overlapping areas
    aes(x=year,y=`High concern threshold`),
    alpha=1,
    colour="grey"
  ) +
  
  
  # scenarios
  geom_textpath(
    size  = 4, aes(label = paste0(ssp,"-",`Annual consumption per capita (at utility peak)`),
                   y=value,group=scenario,colour=scenario), linewidth = 1.2, hjust = 0.6 # on 3/4 of the line
  ) +
  
  # # demand-side potential
  # geom_segment(aes(
  #   x = 2015, 
  #   xend = 2055,
  #   y = 170,
  #   yend = 170
  # ),
  # linewidth = 1, linejoin = "round", lineend = "round", linetype="dotted"
  # ) +
  # geom_segment(aes(
  #   x = 2050, 
  #   xend = 2050,
  #   y = 170,
  #   yend = 170*(1-0.287)
  # ),
  # colour="grey",
  # linewidth = 1, linejoin = "round", lineend = "round", linetype="solid", arrow = arrow(length = unit(0.05, "npc")),
  # ) +
  # geom_segment(aes(
  #   x = 2050, 
  #   xend = 2050,
  #   y = 170,
  #   yend = 170*(1-0.129)
  # ),
  # linewidth = 1, linejoin = "round", lineend = "round", linetype="solid", arrow = arrow(length = unit(0.05, "npc")),
  # ) +
  # geom_text(
  #   x=2050, 
  #   y=220,
  #   label="Demand-side reduction\npotential (IEA)"
  # ) +
  # geom_segment(aes(
  #   x = 2045, 
  #   xend = 2055,
  #   y = 170*(1-0.129),
  #   yend = 170*(1-0.129)
  # ),
  # linewidth = 1, linejoin = "round", lineend = "round", linetype="dotted"
  # ) +
  # geom_segment(aes(
  #   x = 2045, 
  #   xend = 2055,
  #   y = 170*(1-0.287),
  #   yend = 170*(1-0.287)
  # ),
  # linewidth = 1, linejoin = "round", lineend = "round", linetype="dotted"
  # ) +
  
  
  # scale_color_colorblind() +
  scale_color_manual(
    values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8)),
  ) +
  theme_hc() +
  ylab("GJ/cap/yr") + xlab(NULL) +
  scale_y_continuous(limits=c(0,240), expand = c(0,0)) +
  theme(legend.position = "none") +
  labs(subtitle = "MESSAGEix-Australia pathways and feasibility concerns.\nClimate policy: Expand renewables.",
       caption = "Feasibility frontier estimates from\nBrutschin et al. 2021")
p.fe

p.led <- ggplot(data = led %>% to_per_capita() %>% mutate(value=value*1e3, unit="GJ/cap/yr") %>% filter(year>=2020)) +
  
  geom_ribbon(data=. %>% filter(variable %in% c("Final Energy|Industry")) %>%
                mutate(Sector="Industry"),
            aes(x=year,ymin=0,ymax=value,fill=Sector),
            alpha=1) +
  geom_ribbon(data=. %>% filter(variable %in% c("Final Energy|Industry", "Final Energy|Residential and Commercial")) %>% 
                                  pivot_wider(names_from = variable, values_from = value) %>% 
                                  mutate(Sector="Buildings"),
              aes(x=year,ymin=`Final Energy|Industry`,ymax=`Final Energy|Industry`+`Final Energy|Residential and Commercial`,fill=Sector),
              alpha=1) +
  geom_ribbon(data=. %>% filter(variable %in% c("Final Energy|Transportation", "Final Energy|Residential and Commercial","Final Energy|Industry")) %>% 
                                  pivot_wider(names_from = variable, values_from = value) %>% 
                                  mutate(Sector="Transport"),
              aes(x=year,ymin=`Final Energy|Industry`+`Final Energy|Residential and Commercial`,ymax=`Final Energy|Industry`+`Final Energy|Residential and Commercial`+`Final Energy|Transportation`,fill=Sector),
              alpha=1) +
  
  geom_text(data=. %>% filter(variable %in% c("Final Energy|Industry")) %>%
                mutate(Sector="Industry") %>% 
              filter(year==2060),
              aes(x=year,y=mean(c(0,value)),
                  label=Sector),
            # colour="white",
              alpha=1) +
  geom_text(data=. %>% filter(variable %in% c("Final Energy|Industry", "Final Energy|Residential and Commercial")) %>% 
                pivot_wider(names_from = variable, values_from = value) %>% 
                mutate(Sector="Buildings") %>% 
              filter(year==2060),
              aes(x=year,y=mean(c(`Final Energy|Industry`,`Final Energy|Industry`+`Final Energy|Residential and Commercial`)),
                  label=Sector),
            # colour="white",
              alpha=1) +
  geom_text(data=. %>% filter(variable %in% c("Final Energy|Transportation", "Final Energy|Residential and Commercial","Final Energy|Industry")) %>% 
                pivot_wider(names_from = variable, values_from = value) %>% 
                mutate(Sector="Transport") %>% 
              filter(year==2060),
              aes(x=year,y=mean(c(`Final Energy|Industry`+`Final Energy|Residential and Commercial`,`Final Energy|Industry`+`Final Energy|Residential and Commercial`+`Final Energy|Transportation`)),
                  label=Sector),
            # colour="white",
              alpha=1) +
  
  geom_line(data=. %>% filter(variable == "Final Energy"),
            aes(x=year,y=value)) +
  
  # add all scenarios
  geom_textpath(
    data = li %>% to_per_capita() %>% filter(variable=="Energy (final)") %>% mutate(value=value*1e3) %>% 
      filter(grepl(x = `Climate policy`, pattern = "Expand", fixed=T)) %>%
      # filter(grepl(x = `GHG budget`, pattern = "4Gt", fixed=T)) %>%
      filter(year<=2100) %>% 
      filter(`Annual consumption per capita (at utility peak)`!="40k"),
    size  = 4, 
    aes(label = paste0(ssp,"-",`Annual consumption per capita (at utility peak)`),
        y=value,group=scenario,
        x=year), 
    alpha=0.8,
    colour="grey",
    linewidth = 1.2, hjust = 0.85 # on 3/4 of the line
  ) +
  # add 40k scenario
  geom_textpath(
    data = li %>% to_per_capita() %>% filter(variable=="Energy (final)") %>% mutate(value=value*1e3) %>% 
      filter(grepl(x = `Climate policy`, pattern = "Expand", fixed=T)) %>%
      # filter(grepl(x = `GHG budget`, pattern = "4Gt", fixed=T)) %>%
      filter(year<=2100) %>% 
      filter(`Annual consumption per capita (at utility peak)`=="40k"),
    size  = 4, 
    aes(label = paste0(ssp,"-",`Annual consumption per capita (at utility peak)`),
                   y=value,group=scenario,colour=scenario,
                   x=year), 
    linewidth = 1.2, hjust = 0.85 # on 3/4 of the line
  ) +
  
  
  

  theme_hc() +
  ylab("GJ/cap/yr") + xlab(NULL) +
  scale_y_continuous(limits=c(0,240), expand = c(0,0)) +
  scale_fill_manual(breaks=c("Industry","Buildings","Transport"),values=c("#FAE9DA","#FFC6C2","#C3E0DD")) +
  scale_color_manual(
    values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8))[4],
  ) +
  theme(legend.position = "none") +
  labs(subtitle = "Low Energy Demand scenario - R5 OECD region",
       caption = "Data from AR6DB, Byers et al. 2022,\nbased on Grubler et al. 2018")
p.led



p.compare.energy.demand <- (p.fe | p.led) +
  plot_annotation(tag_levels = "A",title = "Final energy per capita compared to other literature (Climate policy: expand renewables)")
p.compare.energy.demand

# SAVE ====
save_ggplot(
  p = p.compare.energy.demand,
  f = here("output", paste0("f05-",version)),
  h=225, w=250
)
