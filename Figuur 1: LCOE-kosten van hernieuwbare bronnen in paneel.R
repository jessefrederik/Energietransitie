library(tidyverse)

## INLADEN VAN DATA UIT IPCC EN IRENA RAPPORTEN 
## IPCC (2022): https://ipcc-browser.ipcc-data.org/browser/dataset?id=443 
## IRENA (2022): https://www.irena.org/-/media/Files/IRENA/Agency/Publication/2022/Jul/IRENA_Power_Generation_Costs_2021.pdf?rev=34c22a4b244d434da0accde7de7c73d8

ipcc <- read_sheet('https://docs.google.com/spreadsheets/d/1IWnKp07etvL0sNs0KuhIg7IrpiHxxgpJ_3dbNJVUF0A/edit#gid=0') %>%
  filter(soort != "Geconcentreerde zonne-energie")


## GRAFIEK MAKEN

source('corriethema.R')

ipcc %>%
  ggplot(aes(year, mid)) +
  geom_ribbon(aes(x= year, ymin= 54, ymax = 167, #DATA VOOR FOSSIELE BRANDBREEDTE UIT IRENA 2022
                  fill = "Bandbreedte fossiel (2021)"), 
              alpha = 0.2) +
  geom_ribbon(aes(x = year, 
                  ymin = low, 
                  ymax = high, 
                  fill = 'Bandbreedte hernieuwbaar'), 
              linetype = "dashed",
              alpha = 0.5,
              col = "black"
              ) +
  geom_line(col = "black") +
  corriethema +
  labs(title = "Dramatische kostendalingen van hernieuwbare energie",
       subtitle = "Gemiddelde wereldwijde kosten met 95%-bandbreedtes van een MWh over de levensduur van een elektriciteitsbron",
       x = "",
       y = "",
       caption = "Bron: IRENA (2022) en IPCC (2022)") +
  coord_cartesian(ylim = c(0, 600)) + 
  facet_grid(cols = vars(factor(soort, levels = c("Zonne-energie", 
                                                  "Onshore wind",
                                                  "Offshore wind",
                                                  "Geconcentreerde zonne-energie"))), 
             scales = "free_x",
             margins = "vs") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "/MWh")) +
  scale_fill_manual(guide ="legend",
                    values = c('black', '#9dceca'),
                    breaks = c( 'Bandbreedte fossiel (2021)', 'Bandbreedte hernieuwbaar'),
                    labels = c('Bandbreedte fossiel (2021)', 'Bandbreedte hernieuwbaar'))
