#' Script for Kikstra et al. "Towards degrowth in integrated assessment models" presentation IAMC in Venice, 2023  
#' Figure 11: DLE deprivation analysis
#' 
#' 
#' Last updated: 04.11.2023

source("load_basics_and_version.R")

install.packages("ggrepel")
library(ggrepel)

# INIT ====


# LOAD ====

# LOAD: Li data ====
# Li data 
li <- vroom(here("data", "li.csv")) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type()
li.pop <- li %>% filter(
  variable %in% c(
    "Population"
  ) 
) %>% rename(pop=value) %>% select(-variable,-unit)
li.fe.to.ue <- li %>% filter(
  variable %in% c(
    "Final Energy",
    "Useful Energy"
  ) 
) %>% 
  left_join(li.pop) %>% mutate(value = value/pop/1e6*1e9, unit = "GJ/cap/yr") %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(
    `Final to Useful energy conversion` = `Useful Energy` / `Final Energy` 
  )
li.fe.to.ue.start <- li.fe.to.ue %>% filter(year==2020) %>% rename(`Final to Useful energy conversion start`=`Final to Useful energy conversion`) %>% select(model,scenario,region,`Final to Useful energy conversion start`)
li.fe.adjusted <-  li.fe.to.ue %>% 
  left_join(li.fe.to.ue.start) %>% 
  mutate(
    `Final Energy adjustment factor` = `Final to Useful energy conversion` / `Final to Useful energy conversion start`
  )

# LOAD: DLE ====
# DLE threshold (Australia)
dle <- vroom(here("data", "input", "dle_aus_ssp2_2050_total.csv")) %>% drop_na() %>% 
  summarise(DLE=sum(DLE))
dle.pcap <- vroom(here("data", "input", "pop_ssp.csv")) %>% filter(country=="AUS") %>% filter(year==2050, scenario=="SSP2") %>% 
  mutate(`DLE per capita raw threshold` = dle %>% pull(DLE) / pop_mil / 1e6 * 1e9 ) %>% 
  select(`DLE per capita raw threshold`)

# LOAD & PROCESS: income gini ====
# Gini data income (SSP2, Australia)
gini <- vroom(here("data", "input", "gini_ssp.csv")) %>% filter(country=="AUS") %>% filter(scenario=="SSP2") %>% 
  select(year,gini) %>% drop_na() %>% 
  mutate(gini = gini / 100)

c.gini <- gini %>% filter(year==2020) %>% pull(gini)
len.gini <- gini %>% pull(gini) %>% length()

gini.w.scens <- gini %>% 
  # constant gini
  mutate(constant.gini=c.gini) %>% 
  # declining gini - 1% per year
  mutate(gini.1p.a=constant.gini*((1-0.01)^(year-2020))) %>%
  # declining gini - 2% per year
  mutate(gini.2p.a=constant.gini*((1-0.02)^(year-2020))) %>%
  # declining gini - 3% per year
  mutate(gini.3p.a=constant.gini*((1-0.03)^(year-2020))) %>%
  # declining gini - 5pp per decade (with floor of 0.1)
  mutate(gini.5pp.dec=ifelse(constant.gini-0.005*(year-2020)>0.1,constant.gini-0.005*(year-2020),0.1))

gini.w.scens %>% 
  rename(gini.ssp2=gini) %>% 
  pivot_longer(gini.ssp2:gini.5pp.dec, names_to = "Inequality scenario", values_to = "Gini") %>%
  filter(year>=2020) %>% 
  ggplot() +
  geom_line(aes(x=year,y=Gini,colour=`Inequality scenario`), size=1.1) +
  theme_classic() +
  theme_hc() +
  scale_color_colorblind()

# LOAD & PROCESS: energy gini ====
# LENZEN:
# Gini data energy (SSP2, Australia)
get_gini <- function(df, var = "fe") { # input on per capita variable
  # get shares
  df.pop.total <- df %>%
    group_by(iso) %>%
    summarise(total.pop = sum(population))
  df <- df %>% mutate(
    var.mul.pop = (!!as.name(var)) * population,
    na.rm = T
  )
  df.var.total <- df %>%
    group_by(iso) %>%
    summarise(total.var = sum(var.mul.pop, na.rm = T))
  
  df.shares <- df %>%
    left_join(df.pop.total) %>%
    left_join(df.var.total) %>%
    arrange(iso, (!!as.name(var))) %>%
    group_by(iso) %>%
    mutate(
      pop.share = population / total.pop,
      var.share = ifelse(population == 0, 0, var.mul.pop / total.var)
    )
  
  # get cumulative shares
  df.cum.shares <- df.shares %>%
    arrange(iso, (!!as.name(var))) %>%
    group_by(iso) %>%
    mutate(
      cum.pop.share = cumsum(pop.share),
      cum.var.share = cumsum(var.share)
    ) %>%
    select(iso, cum.pop.share, cum.var.share, pop.share, var.share)
  
  # check cumulative shares adding up to 1
  # View(df.shares %>% group_by(iso) %>% summarise(pop=sum(pop.share), var=sum(var.share)))
  
  
  # get gini
  gini <- df.cum.shares %>%
    group_by(iso) %>%
    mutate(
      area.points.rec = lag(cum.var.share, default = 0) * pop.share,
      area.points.tri = (cum.var.share - lag(cum.var.share, default = 0)) * pop.share * 0.5
    ) %>%
    mutate(area.points = area.points.rec + area.points.tri) %>%
    summarise(
      area = sum(area.points)
    ) %>%
    mutate(
      gini = (0.5 - area) / 0.5
    )
  
  return(gini %>% select(iso, gini))
}
fe <- read_excel(here("data", "input", "energyrequirement_lenzen2006.xlsx")) %>% 
  filter(variable!="Income") %>% 
  mutate(iso="AUS") %>% 
  select(-unit) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  rename(fe=`Final Energy (direct + indirect)`, population=Population)
fe.gini <- get_gini(df = fe, var="fe")  


# PROCESS ====

# PROCESS: functions ====
# Lognormal cumulative distribution function, for depth-of-deficit calculation (methodology: see https://www.econstor.eu/bitstream/10419/173337/1/wp-gut-fme-a-41-Kot.pdf, page 9) ====
GetInverseCDF_lognormal <- function(g) {
  if (is.list(length(sqrt(2) * invcdf(normal(), (g + 1) / 2)[[1]]))) {
    stop("error")
  }
  
  return(
    sqrt(2) * invcdf(normal(), (g + 1) / 2)[[1]] %>% unlist()
  )
}
GetInverseCDF_lognormal <- Vectorize(GetInverseCDF_lognormal)

# get depth of deficit ====
GetDepthofDeficit_lognormal <- function(nu, sigma, thres, ret = "share") {
  if (is.na(nu) | is.na(sigma) | is.na(thres)) {
    return(NA)
  }
  
  # Typical lognormal distribution to be used for integration
  f <- function(x, nu, sigma) {
    dlnorm(x, meanlog = nu, sdlog = sigma, log = FALSE)
  }
  xf <- function(x, nu, sigma) {
    x * f(x, nu, sigma)
  }
  
  mean.subthres <- integrate(xf, 0, thres, nu, sigma)
  sh.subthres <- integrate(f, 0, thres, nu, sigma)
  
  DoD <- thres - mean.subthres$value / sh.subthres$value
  
  if (ret == "DoD") {
    return(DoD)
  }
  if (ret == "share") {
    return(sh.subthres$value)
  }
}
GetDepthofDeficit_lognormal <- Vectorize(GetDepthofDeficit_lognormal)

# get mean fe/cap from sigma and nu ====
# nu = log(fe.pc) - (sig^2)/2
GetMeanFEpc <- function(sigma, nu) {
  mean.fe.pc <- exp(
    nu + (sigma^2) / 2
  )
  return(mean.fe.pc)
}
GetMeanFEpc <- Vectorize(GetMeanFEpc)
f <- function(x, nu, sigma) {
  dlnorm(x, meanlog = nu, sdlog = sigma, log = FALSE)
}
xf <- function(x, nu, sigma) {
  x * f(x, nu, sigma)
}
ReturnMeanFE <- function(share, thres, sigma, ret = "mean", precision.nu = 0.01) {
  # since nu = log(mean.fe.pc) - (sigma^2)/2.
  # for mean.fe.pc going from 1 to 500,
  # and sigma (from gini 0.1 to 0.8) going from 0.178 to 1.81
  # we get nu from [-6.634898] 0 to 5.505619
  best.nu.error <- Inf
  best.nu <- NA
  for (x.nu in seq(0, 6, precision.nu)) {
    new.nu.error <- share - (integrate(f, lower = 0, upper = thres, nu = x.nu, sigma = sigma))$value
    
    if (abs(new.nu.error) < abs(best.nu.error)) {
      best.nu <- x.nu
      best.nu.error <- new.nu.error
    }
  }
  if (ret == "mean") {
    mean.fe.pc <- GetMeanFEpc(sigma = sigma, nu = best.nu)
    return(mean.fe.pc)
  }
  if (ret == "nu") {
    return(best.nu)
  }
}
ReturnMeanFE <- Vectorize(ReturnMeanFE)
# calculate DLE requirements
Calculate_DLE_Li <- function(gini.scenario = "constant.gini"){
  # gini scenarios:
  # gini.w.scens %>% colnames()
  
  
  income.gini.start <- gini.w.scens %>% filter(year==2020) %>% pull(gini.2p.a)
  li.dle <- li.fe.adjusted %>% 
    # calculate DLE
    bind_cols(dle.pcap) %>% 
    mutate(`DLE per capita (efficiency adjusted)` = `DLE per capita raw threshold` / `Final Energy adjustment factor`) %>% 
    # add inequality
    left_join(gini.w.scens) %>% rename(`Income gini` = (!!as.name(gini.scenario))) %>%
    mutate(`Income gini start` = income.gini.start) %>% 
    mutate(`Energy gini start` = fe.gini %>% pull(gini)) %>% 
    mutate(`Energy gini` = NA) %>% 
    mutate_cond(`Income gini`==`Income gini start`, `Energy gini` = `Energy gini start`) %>% 
    mutate_cond(`Income gini`<`Income gini start`, `Energy gini` = (`Energy gini start` - 0) * (`Income gini` / `Income gini start`)  ) %>% 
    mutate_cond(`Income gini`>`Income gini start`, 
                `Energy gini` = `Energy gini start` + 
                  ((`Income gini` - `Income gini start`)/(1 - `Income gini start`)) * (1 - `Energy gini start`) )
  
  # Calculate gaps ====
  li.dle.sigma <- li.dle %>%
    mutate(
      sig.tmp = GetInverseCDF_lognormal(`Energy gini`)
    )
  
  data.with.energy.req.avg <- li.dle.sigma %>%
    # mutate(
    #   mean.fe.pc.required.curtech = ReturnMeanFE(
    #     share = 0.05,
    #     thres = `DLE per capita raw threshold`,
    #     sigma = sig.tmp,
    #     ret = "mean",
    #     precision.nu = 0.1
    #   ),
    #   mean.fe.pc.required.adjusted = ReturnMeanFE(
    #     share = 0.05,
    #     thres = `DLE per capita (efficiency adjusted)`,
    #     sigma = sig.tmp,
    #     ret = "mean",
    #     precision.nu = 0.1
    #   )
    # ) %>% 
    mutate(nu = log(`Final Energy`) - (sig.tmp^2) / 2) %>%
    mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig.tmp, `DLE per capita raw threshold`, "share")) %>%
    mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig.tmp, `DLE per capita raw threshold`, "DoD")) %>%
    mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig.tmp, `DLE per capita (efficiency adjusted)`, "share")) %>%
    mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig.tmp, `DLE per capita (efficiency adjusted)`, "DoD")) %>% 
    
    mutate(share.below.projected.curtech.2x = GetDepthofDeficit_lognormal(nu, sig.tmp, 2*`DLE per capita raw threshold`, "share")) %>%
    mutate(depth.below.projected.curtech.2x = GetDepthofDeficit_lognormal(nu, sig.tmp, 2*`DLE per capita raw threshold`, "DoD")) %>%
    mutate(share.below.projected.adjusted.2x = GetDepthofDeficit_lognormal(nu, sig.tmp, 2*`DLE per capita (efficiency adjusted)`, "share")) %>%
    mutate(depth.below.projected.adjusted.2x = GetDepthofDeficit_lognormal(nu, sig.tmp, 2*`DLE per capita (efficiency adjusted)`, "DoD"))
  
  return(data.with.energy.req.avg)
}


# PLOT ====

# PLOT: functions ====
Visualise_DLE_Li <- function(gini.scenario = "constant.gini",
                             vis = "all-avg-dle-lines",
                             ghg.budget = "4Gt",
                             growth.scen = NULL,
                             until.year=2060,
                             clr=NA){
  
  if (vis=="all-avg-dle-lines"){
    p.dle.requirement.all.ghg.scens <- ggplot(
      Calculate_DLE_Li(gini.scenario = gini.scenario) %>% rename(`Average energy use per capita` = `Final Energy`) %>% 
        pivot_longer(cols = c("DLE per capita (efficiency adjusted)","Average energy use per capita"), 
                     names_to = "Average energy use or minimum requirement", 
                     values_to = "Final Energy (GJ/cap/yr)") %>%
        mutate(short.type = ifelse(`Average energy use or minimum requirement`=="DLE per capita (efficiency adjusted)","DLE", "Average")) %>% 
        filter(year<=until.year,
               `Climate policy`=="GHG budget",
               # `Annual consumption per capita (at utility peak)`%in%c("10k", "20k", "baseline")
        ),
      aes(
        x=year,
        y=`Final Energy (GJ/cap/yr)`,
        linetype=`Average energy use or minimum requirement`,
        colour=`Annual consumption per capita (at utility peak)`,
        label = short.type
      )
    ) + 
      facet_grid(`GHG budget`~`Annual consumption per capita (at utility peak)`) +
      geom_hline(yintercept = 0, linetype = "dashed", colour="darkgrey") +
      # geom_line(linewidth = 1.2) +
      geom_textline(show.legend = F, size = 3, vjust = -0.5, hjust=0.05, linewidth = 1.1) +
      theme_classic() +
      theme_hc() + 
      # labs(
      #   # title = "Energy requirements for decent living standards (DLE)",
      #   # subtitle = "Scenario: Expand renewables and NETs"
      # ) +
      xlab(NULL) +
      scale_y_continuous(limits = c(0,240), expand = c(0,0)) +
      scale_color_manual(
        values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8))
      ) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    return(p.dle.requirement.all.ghg.scens)
  }
  
  if (vis == "selected-avg-dle-lines"){
    p.dle.requirement <- ggplot(
      Calculate_DLE_Li(gini.scenario = gini.scenario) %>% rename(`Average energy use per capita` = `Final Energy`) %>% 
        mutate(`2x DLE per capita (efficiency adjusted)`=2*`DLE per capita (efficiency adjusted)`) %>% 
        pivot_longer(cols = c("DLE per capita (efficiency adjusted)",
                              "2x DLE per capita (efficiency adjusted)",
                              "Average energy use per capita"), 
                     names_to = "Average energy use or minimum requirement", 
                     values_to = "Final Energy (GJ/cap/yr)") %>%
        mutate(short.type = ifelse(`Average energy use or minimum requirement`=="DLE per capita (efficiency adjusted)","DLE",
                                   ifelse(`Average energy use or minimum requirement`=="2x DLE per capita (efficiency adjusted)","2x DLE",
                                          "Average"))) %>% 
        filter(year<=until.year,
               `Climate policy`=="GHG budget",
               `GHG budget`==ghg.budget,
               `Annual consumption per capita (at utility peak)`%in%growth.scen
        ),
      aes(
        x=year,
        y=`Final Energy (GJ/cap/yr)`,
        linetype=`Average energy use or minimum requirement`,
        colour=`Annual consumption per capita (at utility peak)`,
        label = short.type
      )
    ) + 
      facet_grid(`GHG budget`~`Annual consumption per capita (at utility peak)`) +
      
      # DLE, 2xDLE, and average
      geom_textline(show.legend = F, size = 3, vjust = -0.5, hjust=0.05, linewidth = 1.1) +
      
      theme_classic() +
      theme_hc() + 
      # labs(
      #   # title = "Energy requirements for decent living standards (DLE)",
      #   # subtitle = "Scenario: Expand renewables and NETs"
      # ) +
      xlab(NULL) +
      scale_y_continuous(limits = c(0,240), expand = c(0,0)) +
      scale_linetype_manual(breaks = c("DLE per capita (efficiency adjusted)",
                                       "2x DLE per capita (efficiency adjusted)",
                                       "Average energy use per capita"), 
                            values = c("dashed",
                                       "dotted",
                                       "solid")) +
      scale_color_manual(
        values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8))
      ) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    return(p.dle.requirement)
  }
  
  if (vis == "selected-share-deprivation"){
    p.dle.gaps <- 
      ggplot(
        Calculate_DLE_Li(gini.scenario = gini.scenario) %>% 
          filter(year<=until.year,
                 `Climate policy`=="GHG budget",
                 `GHG budget`==ghg.budget
                 # `Annual consumption per capita (at utility peak)`%in%c("10k", "20k", "baseline")
          ),
        aes(
          x=year,
          y=share.below.projected.adjusted,
          colour=`Annual consumption per capita (at utility peak)`
        )
      ) +
      facet_grid(~`Annual consumption per capita (at utility peak)`) +
      geom_hline(yintercept = 0, linetype = "dashed", colour="darkgrey") +
      geom_line(linewidth = 1.2, 
                linetype="dashed") +
      geom_line(aes(y=share.below.projected.adjusted.2x),
                linewidth = 1.2, 
                linetype="dotted") +
      geom_labelpath(data=. %>% filter(year==until.year-10),
                     aes(x=ifelse(share.below.projected.adjusted>0.01,year,0),
                         y=ifelse(
                           share.below.projected.adjusted>0.9,
                           -0.1+share.below.projected.adjusted+log(1+share.below.projected.adjusted/8, base = 10),
                           0.05+share.below.projected.adjusted+log(1+share.below.projected.adjusted/8, base = 10)),
                         label=paste0(as.character(round(share.below.projected.adjusted*100, digits=0)), "%")
                     )
      ) +
      geom_textpath(data=. %>% filter(year==until.year-10),
                    aes(x=ifelse(share.below.projected.adjusted.2x>0.01,year,0),
                        y=ifelse(
                          share.below.projected.adjusted.2x>0.9,
                          -0.1+share.below.projected.adjusted.2x+log(1+share.below.projected.adjusted.2x/8, base = 10),
                          0.05+share.below.projected.adjusted.2x+log(1+share.below.projected.adjusted.2x/8, base = 10)),
                        label=paste0(as.character(round(share.below.projected.adjusted.2x*100, digits=0)), "%")
                    )
      ) +
      theme_classic() +
      theme_hc() + 
      scale_x_continuous(limits = c(2020,until.year)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0), 
                         limits = c(0,1),
                         # trans = 'log10'
      ) +
      xlab(NULL) + ylab(NULL) +
      labs(subtitle = paste0("Population below DLE threshold.\nDashed: double (2x) DLE.\nGini scenario: ", gini.scenario),
           caption = paste0("Highlighted percentages are for the year ", as.character(until.year-10))) +
      scale_color_manual(
        values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8)),
      ) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    if (!is.na(clr)){
      p.dle.gaps <- p.dle.gaps +
        scale_color_manual(
          values = rep(clr,8),
        )
    }
    
    return(p.dle.gaps)
    
  }
  
  if (vis == "selected-share-deprivation-poster"){
    p.dle.gaps <- 
      ggplot(
        Calculate_DLE_Li(gini.scenario = gini.scenario[1]) %>% 
          filter(year<=until.year,
                 `Climate policy`=="GHG budget",
                 `GHG budget`==ghg.budget,
                 `Annual consumption per capita (at utility peak)`%in%growth.scen
          ) %>% mutate(`Gini scenario`=gini.scenario[1]) %>% 
          bind_rows(
            Calculate_DLE_Li(gini.scenario = gini.scenario[2]) %>% 
              filter(year<=until.year,
                     `Climate policy`=="GHG budget",
                     `GHG budget`==ghg.budget,
                     `Annual consumption per capita (at utility peak)`%in%growth.scen
              ) %>% mutate(`Gini scenario`=gini.scenario[2])
          ),
        aes(
          x=year,
          y=share.below.projected.adjusted,
          colour=`Gini scenario`
        )
      ) +
      facet_grid(~`Annual consumption per capita (at utility peak)`) +
      geom_hline(yintercept = 0, linetype = "dashed", colour="darkgrey") +
      geom_line(linewidth = 1.2, 
                linetype="dashed") +
      geom_line(aes(y=share.below.projected.adjusted.2x),
                linewidth = 1.2, 
                linetype="dotted") +
      geom_label_repel(data=. %>% filter(year==until.year-10),
                     aes(x=ifelse(share.below.projected.adjusted>0.01,year,0),
                         y=ifelse(
                           share.below.projected.adjusted>0.9,
                           -0.1+share.below.projected.adjusted+log(1+share.below.projected.adjusted/8, base = 10),
                           0.05+share.below.projected.adjusted+log(1+share.below.projected.adjusted/8, base = 10)),
                         label=paste0(as.character(round(share.below.projected.adjusted*100, digits=0)), "%")
                     )
      ) +
      geom_text_repel(data=. %>% filter(year==until.year-10),
                    aes(x=ifelse(share.below.projected.adjusted.2x>0.01,year,0),
                        y=ifelse(
                          share.below.projected.adjusted.2x>0.9,
                          -0.1+share.below.projected.adjusted.2x+log(1+share.below.projected.adjusted.2x/8, base = 10),
                          0.05+share.below.projected.adjusted.2x+log(1+share.below.projected.adjusted.2x/8, base = 10)),
                        label=paste0(as.character(round(share.below.projected.adjusted.2x*100, digits=0)), "%")
                    )
      ) +
      theme_classic() +
      theme_hc() + 
      scale_x_continuous(limits = c(2020,until.year)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0), 
                         limits = c(0,1),
                         # trans = 'log10'
      ) +
      xlab(NULL) + ylab(NULL) +
      labs(subtitle = paste0("Population below DLE threshold."),
           caption = paste0("Highlighted percentages are for the year ", as.character(until.year-10), ". Dashed: double (2x) DLE.")) +
      scale_color_colorblind() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    if (!is.na(clr)){
      p.dle.gaps <- p.dle.gaps +
        scale_color_manual(
          values = rep(clr,8),
        )
    }
    
    return(p.dle.gaps)
    
  }
  
}



# PLOT: figures and combine ====
p.gini.scens <- 
  gini.w.scens %>% 
  rename(gini.ssp2=gini) %>% 
  pivot_longer(gini.ssp2:gini.5pp.dec, names_to = "Inequality scenario", values_to = "Gini") %>%
  filter(year>=2020, year<=2060,
         `Inequality scenario`%in%c("constant.gini", "gini.2p.a")) %>% #, "gini.ssp2"
  ggplot() +
  geom_textpath(aes(x=year,y=Gini,colour=`Inequality scenario`,label=`Inequality scenario`)) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,0.4), expand = c(0,0)) +
  xlab(NULL) +
  labs(subtitle = "Income gini reduction") +
  scale_color_colorblind()
p.gini.scens

p.energy.gini.scens <- 
  gini.w.scens %>% 
  rename(gini.ssp2=gini) %>% 
  pivot_longer(gini.ssp2:gini.5pp.dec, names_to = "Inequality scenario", values_to = "Gini") %>%
  filter(year>=2020, year<=2060,
         `Inequality scenario`%in%c("constant.gini", "gini.2p.a")) %>% 
  ggplot() +
  geom_textpath(aes(x=year,y=Gini,colour=`Inequality scenario`,label=`Inequality scenario`)) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,0.4), expand = c(0,0)) +
  xlab(NULL) +
  labs(subtitle = "Income gini reduction") +
  scale_color_colorblind()
p.energy.gini.scens <- 
  Calculate_DLE_Li(gini.scenario = "constant.gini") %>% mutate(`Inequality scenario`="constant.gini") %>% 
  bind_rows(Calculate_DLE_Li(gini.scenario = "gini.2p.a") %>% mutate(`Inequality scenario`="gini.2p.a")) %>% 
  select(region,year,`Energy gini`,`Inequality scenario`) %>%
  filter(year>=2020, year<=2060,
         `Inequality scenario`%in%c("constant.gini", "gini.2p.a")) %>% distinct() %>% 
  ggplot() +
  geom_textpath(aes(x=year,y=`Energy gini`,colour=`Inequality scenario`,label=`Inequality scenario`)) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,0.4), expand = c(0,0)) +
  xlab(NULL) +
  labs(subtitle = "Energy gini reduction") +
  scale_color_colorblind()
p.energy.gini.scens


p.dle <- 
  # (((p.gini.scens / p.energy.gini.scens) + plot_layout(heights = c(1,1)))|
     (
       (Visualise_DLE_Li(gini.scenario = "constant.gini", 
                         vis = "selected-avg-dle-lines", 
                         ghg.budget = "4Gt",
                         growth.scen = c("20k", "40k","60k"),
                         until.year = 2060) + 
          labs(caption = "Using 4GtCO2eq GHG budget.")) /
         
         Visualise_DLE_Li(gini.scenario = c("constant.gini", "gini.2p.a"), 
                          vis = "selected-share-deprivation-poster", 
                          ghg.budget = "4Gt",
                          growth.scen = c("20k", "40k","60k"),
                          until.year = 2060)
     )
  # ) +
  # plot_layout( 
  #   widths = c(1,4)
  # )

p.dle




# SAVE ====
save_ggplot(p = p.dle,
            f = here("output", paste0("f11-",version, "-poster-selected")),
            h=200, w=200)

