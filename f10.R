#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 10: decoupling rates
#' 
#' 
#' Last updated: 27.06.2023

source("load_basics_and_version.R")



# INIT ====
# ...
IPCC.META.FILE <- "{YOUR-PATH-TO-AR6-SCENARIO-DATA}/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
IPCC.META.FILE.SHEET <- "meta_Ch3vetted_withclimate" # Category

# LOAD ====
li <- vroom(here("data", "li_normalised.csv")) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type() %>% 
  filter(`Climate policy`!="Keep fossil fuels")
ipcc <- vroom(here("data", "ipcc_r10_normalised.csv")) %>% # 69 scenarios
  filter_by_cat_ar6(cat="C1") %>% 
  filter(region=="R10PAC_OECD") %>% 
  filter(variable%in%(li %>% unique_variable())) %>% 
  rename_li_data() %>% 
  mutate(`Annual consumption per capita (at utility peak)` = NA)
hist <- read_excel(
  path = here("data", "input", "2023_03_14_Historical GDP, PFU energy data for Jarmo.xlsx"),
  sheet = "raw data",
  range = "A8:BK14"
) 
hist.ghg <- vroom(
  here("data", "input", "HISTCR_AUS_KYOTOGHGAR4_Mt.csv")
) %>% 
  mutate(value = IPC1 + IPC2 + IPC4 + IPC5 + IPCMAG ) %>% 
  select(year,value)


# PROCESS ====
hist.decadal.rates <- hist %>% pivot_longer(cols = `1960`:`2019`, names_to = "year", values_to = "value") %>% mutate(year=as.numeric(year)) %>% mutate(matchcol=year) %>% rename(start.year=year, value.startyear=value) %>%  
  left_join(
    hist %>% pivot_longer(cols = `1960`:`2019`, names_to = "year", values_to = "value") %>% mutate(year=as.numeric(year)) %>% mutate(matchcol=year-10) %>% rename(end.year=year, value.endyear=value),
    by = c("Raw energy + GDP Data (Australia)", "Units", "Data source", "matchcol")
  ) %>% 
  mutate(
    decadal.change.rate = value.endyear / value.startyear
  ) %>% 
  # select and tidy
  filter(`Raw energy + GDP Data (Australia)`%in%c("Total Final Consumption (final energy)", "GDP $ 2017 constant PPP")) %>% 
  select(`Raw energy + GDP Data (Australia)`,start.year,end.year,decadal.change.rate) %>% 
  pivot_wider(names_from = `Raw energy + GDP Data (Australia)`, values_from = decadal.change.rate)

hist.ghg.decadal.rates <- hist.ghg %>% mutate(matchcol=year) %>% rename(start.year=year, value.startyear=value) %>% 
  left_join(
    hist.ghg %>% mutate(matchcol=year-10) %>% rename(end.year=year, value.endyear=value)
  ) %>% 
  mutate(
    decadal.change.rate = value.endyear / value.startyear
  ) %>% 
  # select and tidy
  select(start.year,end.year,decadal.change.rate) %>% 
  rename(ghg=decadal.change.rate) %>% 
  left_join(hist.decadal.rates)
# use normalised, because of comparison to IPCC region...
timestep.diff <- li %>% bind_rows(ipcc) %>% 
  filter(
    variable %in% c(
      "GDP (PPP)",
      "Final Energy",
      "GHG emissions"
    )
  )


gr.data <- timestep.diff %>% 
  select(-unit) %>%
  filter(
    year %in% c(
      2020, 2030, 2040, 2050
    )
  ) %>% 
  pivot_wider(names_from = c(variable,year), values_from = value) %>% 
  mutate(
    gr.fe.2030 = `Final Energy_2030` / `Final Energy_2020`,
    gr.fe.3040 = `Final Energy_2040` / `Final Energy_2030`,
    gr.fe.4050 = `Final Energy_2050` / `Final Energy_2040`,
    gr.ghg.2030 = `GHG emissions_2030` / `GHG emissions_2020`,
    gr.ghg.3040 = `GHG emissions_2040` / `GHG emissions_2030`,
    gr.ghg.4050 = `GHG emissions_2050` / `GHG emissions_2040`,
    gr.gdp.2030 = `GDP (PPP)_2030` / `GDP (PPP)_2020`,
    gr.gdp.3040 = `GDP (PPP)_2040` / `GDP (PPP)_2030`,
    gr.gdp.4050 = `GDP (PPP)_2050` / `GDP (PPP)_2040`,
    
    
    gr.fe.3050 = `Final Energy_2050` / `Final Energy_2030`,
    gr.ghg.3050 = `GHG emissions_2050` / `GHG emissions_2030`,
    gr.gdp.3050 = `GDP (PPP)_2050` / `GDP (PPP)_2030`
  )


# PLOT ====
# PLOT: functions ====
decoupling_plot_vs_ipcc_pb <- function(data=gr.data, x, y, variable,
                                       xl=c(-0.1,1.5), yl=c(-0.1,1.5)) {
  
  # This shows:
  # (FE[t+1] / FE[t]) / (GDP [t+1] / GDP[t])
  
  ggplot(
    data
  ) +
    
    geom_vline(xintercept = 0, linetype = "dotted") +
    
    geom_abline(slope = 1) +
    geom_abline(slope = 0) +
    
    geom_point(
      data = . %>% filter(region=="R10PAC_OECD"),
      aes(x={{x}}-1,y={{y}}-1), # -1 because it is growth rate based on normalised change 
      colour = "lightgrey"
    ) +
    geom_point(
      data = . %>% filter(region=="AUS"),
      aes(x={{x}}-1,y={{y}}-1, # -1 because it is growth rate based on normalised change
          color=`Annual consumption per capita (at utility peak)`, shape = `Climate policy`),
      size = 2.2
    ) +
    
    geom_text(
      aes(x=0.8,y=-0.3, label="Absolute\ndecoupling"), size=3, fontface = "italic",
    ) +
    
    geom_text(
      aes(x=0.8,y=0.05, label="Decoupled"), size=3
    ) +
    
    geom_text(
      aes(x=0.2,y=0.2+0.05, label="Coupled", hjust=-0.0,vjust=-0.0,angle=55), size=3
    ) +
    
    
    scale_color_manual(
      values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8))
      # colours from: https://loading.io/color/feature/PuOr-10/
    ) +
    theme_classic() +
    
    coord_cartesian(xlim = xl, ylim = yl ) + 
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(labels = scales::percent_format()) 
}
decoupling_plot_vs_ipcc_pb_withistFE <- function(data=gr.data, x, y, variable,
                                                 xl=c(-0.1,1.5), yl=c(-0.1,1.5)) {
  
  # This shows:
  # (FE[t+1] / FE[t]) / (GDP [t+1] / GDP[t])
  
  ggplot(
    data
  ) +
    
    geom_vline(xintercept = 0, linetype = "dotted") +
    
    # ggplot() +
    geom_point(
      data=hist.decadal.rates,
      aes(
        x=`GDP $ 2017 constant PPP`-1,
        y=`Total Final Consumption (final energy)`-1
      ),
      colour="dodgerblue"
    ) +
    
    
    geom_abline(slope = 1) +
    geom_abline(slope = 0) +
    
    geom_point(
      data = . %>% filter(region=="R10PAC_OECD"),
      aes(x={{x}}-1,y={{y}}-1), # -1 because it is growth rate based on normalised change 
      colour = "lightgrey"
    ) +
    geom_point(
      data = . %>% filter(region=="AUS"),
      aes(x={{x}}-1,y={{y}}-1, # -1 because it is growth rate based on normalised change
          color=`Annual consumption per capita (at utility peak)`, shape = `Climate policy`),
      size = 2.2
    ) +
    
    geom_text(
      aes(x=0.8,y=-0.3, label="Absolute\ndecoupling"), size=3, fontface = "italic",
    ) +
    
    geom_text(
      aes(x=0.8,y=0.05, label="Decoupled"), size=3
    ) +
    
    geom_text(
      aes(x=0.2,y=0.2+0.05, label="Coupled", hjust=-0.0,vjust=-0.0,angle=55), size=3
    ) +
    
    
    scale_color_manual(
      values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8))
      # colours from: https://loading.io/color/feature/PuOr-10/
    ) +
    theme_classic() +
    
    coord_cartesian(xlim = xl, ylim = yl ) + 
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(labels = scales::percent_format()) 
}
decoupling_plot_vs_ipcc_pb_withistGHG <- function(data=gr.data, x, y, variable,
                                                  xl=c(-0.1,1.5), yl=c(-0.1,1.5)) {
  
  # This shows:
  # (FE[t+1] / FE[t]) / (GDP [t+1] / GDP[t])
  
  ggplot(
    data
  ) +
    
    geom_vline(xintercept = 0, linetype = "dotted") +
    
    # ggplot() +
    geom_point(
      data=hist.ghg.decadal.rates %>% drop_na(),
      aes(
        x=`GDP $ 2017 constant PPP`-1,
        y=ghg-1
      ),
      colour="dodgerblue"
    ) +
    # geom_density_2d(
    #   data=hist.ghg.decadal.rates %>% drop_na(),
    #   aes(
    #     x=`GDP $ 2017 constant PPP`-1,
    #     y=ghg-1
    #   ),
    #   colour="dodgerblue"
    # ) +
    
    
    geom_abline(slope = 1) +
    geom_abline(slope = 0) +
    
    geom_point(
      data = . %>% filter(region=="R10PAC_OECD"),
      aes(x={{x}}-1,y={{y}}-1), # -1 because it is growth rate based on normalised change 
      colour = "lightgrey"
    ) +
    geom_point(
      data = . %>% filter(region=="AUS"),
      aes(x={{x}}-1,y={{y}}-1, # -1 because it is growth rate based on normalised change
          color=`Annual consumption per capita (at utility peak)`, shape = `Climate policy`),
      size = 2.2
    ) +
    
    geom_text(
      aes(x=0.8,y=-0.3, label="Absolute\ndecoupling"), size=3, fontface = "italic",
    ) +
    
    geom_text(
      aes(x=0.8,y=0.05, label="Decoupled"), size=3
    ) +
    
    geom_text(
      aes(x=0.2,y=0.2+0.05, label="Coupled", hjust=-0.0,vjust=-0.0,angle=55), size=3
    ) +
    
    
    scale_color_manual(
      values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8))
      # colours from: https://loading.io/color/feature/PuOr-10/
    ) +
    theme_classic() +
    
    coord_cartesian(xlim = xl, ylim = yl ) + 
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(labels = scales::percent_format()) 
}
# PLOT: combine plots ====
p.gr <- 
  (
    decoupling_plot_vs_ipcc_pb_withistFE(
      x=gr.gdp.2030,
      y=gr.fe.2030,
      yl=c(-1,0.5),
      xl=c(-1,1)
    ) + xlab("GDP") + ylab("Final Energy") + labs(subtitle = "2020-2030", title = "Final Energy vs GDP") + theme(legend.position = "none") |
    decoupling_plot_vs_ipcc_pb_withistFE(
      x=gr.gdp.3050,
      y=gr.fe.3050,
      yl=c(-1,0.5),
      xl=c(-1,1)
    ) + xlab("GDP") + ylab("Final Energy") + labs(subtitle = "2030-2050") + guides(color=guide_legend(ncol=2,bycol=TRUE))
  ) /
  (
    decoupling_plot_vs_ipcc_pb_withistGHG(
      x=gr.gdp.2030,
      y=gr.ghg.2030,
      yl=c(-1,0.5),
      xl=c(-1,1)
    ) + xlab("GDP") + ylab("GHG emissions") + labs(subtitle = "2020-2030", title = "GHG emissions vs GDP") + theme(legend.position = "none") |
    decoupling_plot_vs_ipcc_pb_withistGHG(
      x=gr.gdp.3050,
      y=gr.ghg.3050,
      yl=c(-1,0.5),
      xl=c(-1,1)
    ) + xlab("GDP") + ylab("GHG emissions") + labs(subtitle = "2030-2050") + theme(legend.position = "none")
  ) +
  # update_geom_defaults("text", list(size = 1)) +
  plot_annotation(title = "Decadal change and (de)coupling compared to IPCC C1 PAC_OECD",
                  caption = "Grey = IPCC C1 PAC_OECD, Blue = Australia historical decadal rates 1990-2019") +
  # plot_layout(widths = c(1,1,1), heights = c(1,1))
  plot_layout(widths = c(1,1), heights = c(1,1))
p.gr



# SAVE ====
save_ggplot(p = p.gr, 
            f = here("output", paste0("f10-",version)),
            h=200*0.9, w=260*1)
