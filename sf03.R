#' Script for Kikstra et al. (2024) "Downscaling Down Under: Towards degrowth in integrated assessment models"
#' Supplementary Figure 3: Energy intensity change rates (GDP / final energy change per year)
#' 
#' 
#' Last updated: 11.05.2023

source("load_basics_and_version.R")





# INIT ====

# LOAD ====
li <- vroom(here("data", "li.csv")) %>% 
  add_degrowth_level() %>% 
  add_scenario_set_type() %>% 
  filter(
    variable %in% c(
      # totals
      "GDP (PPP)",
      "Final Energy"
    )
  )


# PROCESS ====
li.ei <- li %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(`Energy intensity`=`Final Energy`/`GDP (PPP)`,
         unit = "EJ/Thousand USD2005")

li.ei.growth.rate <- li.ei %>% 
  group_by(model,scenario,region) %>% 
  mutate(`Energy intensity growth rate` = ((((lead(`Energy intensity`)-`Energy intensity`)/`Energy intensity`)/10))) %>% 
  mutate(`Energy intensity growth rate period` = paste0(as.character(year), "-", as.character(lead(year))))
# View(li.ei.growth.rate)

# PLOT ====
p.ei <- ggplot(
  li.ei.growth.rate %>% filter(year<=2100) %>% drop_na(),
  aes(x=`Energy intensity growth rate period`,y=`Energy intensity growth rate`, colour=`Annual consumption per capita (at utility peak)`)
) +
  facet_wrap(`Climate policy`~., ncol = 1) +
  
  geom_hline(yintercept = 0) +
  
  geom_line(aes(group=scenario),
            linewidth=1.1,
            linetype="dotdash") +
  geom_line(aes(group=scenario),
            data = . %>% filter(`Annual consumption per capita (at utility peak)`=="baseline"),
            linewidth=1.4) +
  scale_color_manual(
    values = scales::seq_gradient_pal("#e08214", "#7f3b08", "Lab")(seq(0,1,length.out=8)),
  ) +
  # scale_color_colorblind() +
  xlab(NULL) +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.background = element_rect(linewidth=0.5, linetype="solid", 
                                         colour ="black")) +
  scale_y_continuous(labels = scales::percent, name = "Annual change in energy intensity (EJ/Thousand USD2005)") +
  labs(
    title = "Energy intensity change rate over time",
    subtitle = "In EJ/Thousand USD2005"
  ) +
  theme_classic() +
  theme_hc()

p.ei


# SAVE ====

save_ggplot(p = p.ei, 
            f = here("output", paste0("sf03-",version)),
            h=200, w=200)
