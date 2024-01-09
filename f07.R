#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Figure 7: Primary Energy technology change compared to IPCC
#' 
#' 
#' Last updated: 27.06.2023

source("load_basics_and_version.R")



# INIT ====
normalization.year <- 2020

budget.choice.highlight <- "4Gt"
upsc.ts.years <- c(2020,2030,2040,2050,2060)


vars.tech <- c(
  # "Primary Energy|Non-Biomass Renewables",
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

ipcc.data <- vroom(here("data", "ipcc_r10.csv")) %>% 
  filter(variable%in%vars.tech)

ipcc.test <- ipcc.data %>% filter(!(variable%in%c("Primary Energy|Wind","Primary Energy|Solar"))) %>% 
  bind_rows(
    ipcc.data %>% filter(variable%in%c("Primary Energy|Wind","Primary Energy|Solar")) %>% 
      pivot_wider(names_from = variable, values_from = value) %>% 
      mutate(`Primary Energy|Wind and Solar` = `Primary Energy|Wind` + `Primary Energy|Solar`) %>% 
      select(-c("Primary Energy|Wind","Primary Energy|Solar")) %>% 
      mutate(variable="Primary Energy|Wind and Solar") %>% 
      rename(value=`Primary Energy|Wind and Solar`)
  )
ipcc <- ipcc.test %>% # 69 scenarios
  filter_by_cat_ar6(cat="C1") %>% 
  filter(region=="R10PAC_OECD") %>% 
  mutate(`Annual consumption per capita (at utility peak)` = NA)
ipcc.norm <- ipcc %>% normalise_iamc_long(starting.year = normalization.year)



# PROCESS ====
baselines <- 
  li.raw.notnorm %>% 
  filter(
    year %in% upsc.yrs,
    `Annual consumption per capita (at utility peak)`=="baseline"
  ) %>% ungroup() %>% 
  select(variable,year,value, `GHG budget`) %>% rename(baseline=value)


# PLOT ====
remove_prim_ene <- function(df){
  df %>% 
    mutate_cond(variable == "Primary Energy|Biomass", variable = "Biomass") %>% 
    mutate_cond(variable == "Primary Energy|Fossil", variable = "Fossil") %>% 
    mutate_cond(variable == "Primary Energy|Wind and Solar", variable = "Wind and Solar") %>% 
    return(.)
}

p.scaleup.relative.compared.ipcc.all <- ggplot(mapping=aes(x=year,y=value)) +
  # facet_grid() +
  facet_grid(variable~`GHG budget`, scales = "free_y") +
  
  geom_hline(yintercept = 1, colour = "darkgrey", linetype="dashed") +
  geom_hline(yintercept = 0, colour = "black") +
  
  geom_point(
    data = ipcc.norm %>% filter(year %in%upsc.ts.years ) %>% remove_prim_ene(),
    colour="grey",
    position = position_jitter(width = 0.3),
    alpha=0.2
  ) +
  geom_line(
    data = ipcc.norm %>% filter(year %in% upsc.ts.years) %>% mutate(MOD.SCEN=paste0(model,"_",scenario)) %>% remove_prim_ene(),
    colour="grey",
    aes(x=year, y=value, group=MOD.SCEN),
    alpha=0.2
  ) +
  
  geom_point(
    data = li.norm %>% filter(year %in% upsc.ts.years) %>% remove_prim_ene(),
    aes(color=`Annual consumption per capita (at utility peak)`, 
        alpha=`GHG budget`),
    size=2,
    position = position_dodge2(width = 0.5)
  ) +
  geom_line(
    data = li.norm %>% filter(year %in% upsc.ts.years) %>% mutate(MOD.SCEN=paste0(model,"_",scenario)) %>% remove_prim_ene(),
    aes(x=year, y=value, group=MOD.SCEN,
        color=`Annual consumption per capita (at utility peak)`, 
        alpha=`GHG budget`)
  ) +
  
  scale_color_manual(
    values = c(scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8)), "lightgrey")
    # colours from: https://loading.io/color/feature/PuOr-10/
  ) +
  scale_alpha_manual(breaks = c("3Gt","4Gt","5Gt","6Gt","7Gt"), values = c(1, 0.1, 1, 1, 1)) +
  
  theme_classic() +
  theme_hc() +
  ylab("Change relative to 2020") + xlab(NULL) +
  labs(subtitle = "Primary Energy: comparison to IPCC pathways across different GHG budgets and growth assumptions",
       caption = "Compared to IPCC R10 region PAC_OECD") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p.scaleup.relative.compared.ipcc.all

p.scaleup.relative.compared.ipcc.40k <- ggplot(mapping=aes(x=year,y=value)) +
  facet_wrap(`GHG budget`~variable, scales = "free_y") +
  
  geom_hline(yintercept = 1, colour = "darkgrey", linetype="dashed") +
  geom_hline(yintercept = 0, colour = "black") +
  
  geom_point(
    data = ipcc.norm %>% filter(year %in%upsc.ts.years ) %>% remove_prim_ene(),
    colour="grey",
    position = position_jitter(width = 0.3),
    alpha = 0.5
  ) +
  geom_line(
    data = ipcc.norm %>% filter(year %in% upsc.ts.years) %>% mutate(MOD.SCEN=paste0(model,"_",scenario)) %>% remove_prim_ene(),
    colour="grey",
    alpha = 0.5,
    aes(x=year, y=value, group=MOD.SCEN)
  ) +
  
  geom_point(
    data = li.norm %>% filter(year %in% upsc.ts.years) %>% remove_prim_ene() %>% filter(`Annual consumption per capita (at utility peak)`=="40k", `GHG budget`=="4Gt"),
    aes(color=`Annual consumption per capita (at utility peak)`),
    size=2,
    position = position_dodge2(width = 0.5),
    alpha=0.8
  ) +
  geom_line(
    data = li.norm %>% filter(year %in% upsc.ts.years) %>% mutate(MOD.SCEN=paste0(model,"_",scenario)) %>% remove_prim_ene() %>% filter(`Annual consumption per capita (at utility peak)`=="40k", `GHG budget`=="4Gt"),
    alpha = 0.8,
    aes(x=year, y=value, group=MOD.SCEN,
        color=`Annual consumption per capita (at utility peak)`)
  ) +
  
  
  scale_color_manual(
    values = c(scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8)), "lightgrey")
    # colours from: https://loading.io/color/feature/PuOr-10/
  ) +
  
  theme_classic() +
  theme_hc() +
  ylab("Change relative to 2020") + xlab(NULL) +
  labs(
    title = "Energy supply technology change compared to IPCC category C1 pathways",
    subtitle = "Primary Energy: 40k scenario with 4GtCO2eq GHG budget",
    caption = "Compared to IPCC R10 region PAC_OECD"
    ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p.scaleup.relative.compared.ipcc.40k

p.scaleup.relative.compared.ipcc <- ( (p.scaleup.relative.compared.ipcc.40k+theme(legend.position = "none")) / p.scaleup.relative.compared.ipcc.all) +
  plot_layout(heights = c(1,1.5)) +
  plot_annotation(tag_levels = "A")
p.scaleup.relative.compared.ipcc



# SAVE ====

save_ggplot(p = p.scaleup.relative.compared.ipcc, 
            f = here("output", paste0("f07-",version)),
            h=300, w=250)
