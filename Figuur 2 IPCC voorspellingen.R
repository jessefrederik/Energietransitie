library(tidyverse)
library(googlesheets4)

## Bronnen

# Methode voor pProbabilistische voorspellingen met Wright's Law komen uit 

# IPCC Scenario's uit 2022 komen uit: 

#  Edward Byers, Volker Krey, Elmar Kriegler, Keywan Riahi, Roberto Schaeffer, Jarmo Kikstra, Robin Lamboll, Zebedee Nicholls, Marit Sanstad, Chris Smith, Kaj-Ivar van der Wijst, Alaa Al Khourdajie, Franck Lecocq, Joana Portugal-Pereira, Yamina Saheb, Anders Strømann, Harald Winkler, Cornelia Auer, Elina Brutschin, Matthew Gidden, Philip Hackstock, Mathijs Harmsen, Daniel Huppmann, Peter Kolp, Claire Lepault, Jared Lewis, Giacomo Marangoni, Eduardo Müller-Casseres, Ragnhild Skeie, Michaela Werning, Katherine Calvin, Piers Forster, Celine Guivarch, Tomoko Hasegawa, Malte Meinshausen, Glen Peters, Joeri Rogelj, Bjorn Samset, Julia Steinberger, Massimo Tavoni, Detlef van Vuuren.
# AR6 Scenarios Database hosted by IIASA
# International Institute for Applied Systems Analysis, 2022.
# doi: 10.5281/zenodo.5886911 | url: data.ece.iiasa.ac.at/ar6/ 

# IPCC scenario's uit 2014 komen uit AMPERE-database.

# Riahi et al. (2014): Locked into Copenhagen Pledges - Implications of short-term emission targets for the cost and feasibility of long-term climate goals, Technological Forecasting and Social Change, online first, DOI: 10.1016/j.techfore.2013.09.016.
# Kriegler et al. (2014): Making or breaking climate targets: The AMPERE study on staged accession scenarios for climate policy, Technological Forecasting and Social Change, online first, DOI: 10.1016/j.techfore.2013.09.021.
# Capros et al. (2014): EU Decarbonisation pathways, Climate policy delays, Energy Roadmap, EU Energy Policy, Technological limitations, Energy Strategy Reviews 2(3-4), 231-245, DOI: 10.1016/j.esr.2013.12.007.
# Kriegler et al. (2014): Diagnostic indicators for integrated assessment models of climate policies, Technological Forecasting and Social Change, online first, DOI: 10.1016/j.techfore.2013.09.020.

# https://tntcat.iiasa.ac.at/AMPEREDB/dsd?Action=htmlpage&page=about 

# Actuele installation_cost van grootschalige zonne-energie.
# IRENA (2022), 'Renewable Power Generation Costs in 2021'
# https://www.irena.org/-/media/Files/IRENA/Agency/Publication/2022/Jul/IRENA_Power_Generation_Costs_2021.pdf?rev=34c22a4b244d434da0accde7de7c73d8

ipcc2022 <- read.csv("~/Documents/GitHub/Energietransitie/datasets.csv/ipcc2022.csv") %>%
  filter(installation_cost > 0 & installation_cost < 20000) %>% ## Verwijder onzindata
  group_by(year) %>%  
  summarise(median_installation_cost = median(installation_cost,na.rm=T),
            conf5_installation_cost = quantile(installation_cost, probs = 0.05, na.rm=T),
            conf95_installation_cost = quantile(installation_cost, probs = 0.95,na.rm=T),
            conf25_installation_cost = quantile(installation_cost, probs = .25,na.rm=T),
            conf75_installation_cost = quantile(installation_cost, probs = .75,na.rm=T)) %>%
  mutate(prog = "IPCC (2022)") %>%
  filter(year > 2015) %>%
  drop_na()

ipcc2014 <- read.csv("~/Documents/GitHub/Energietransitie/datasets.csv/ipcc2014.csv") %>%
  filter(installation_cost > 0 & installation_cost < 20000) %>%
  group_by(year) %>%  
  summarise(median_installation_cost = median(installation_cost,na.rm=T),
            conf5_installation_cost = quantile(installation_cost, probs = 0.05, na.rm=T),
            conf95_installation_cost = quantile(installation_cost, probs = 0.95,na.rm=T),
            conf25_installation_cost = quantile(installation_cost, probs = .25,na.rm=T),
            conf75_installation_cost = quantile(installation_cost, probs = .75,na.rm=T)) %>%
  mutate(prog = "IPCC (2014)") %>%
  filter(year > 2010) %>%
  drop_na()

IRENA2022 <- read.csv("~/Documents/GitHub/Energietransitie/datasets.csv/IRENA2022.csv") 
source('wrightsforecast.R') 

x <- merge(ipcc2022, ipcc2014, all =T) %>%
  merge(wrightsforecast, all =T)  %>%
  filter(year <= 2050)

## Grafiek maken

source("corriethema.R")

ggplot() + 
  geom_line(data = IRENA2022,
            aes(x=year,
                y= installation_cost), 
            size = 1) +
  geom_line(data = x, 
            aes(x = year,
                y = median_installation_cost),
            linetype = "dashed") +
  geom_ribbon(data = x,
              aes(year,
                  ymin = conf25_installation_cost,
                  ymax = conf75_installation_cost,
                  fill = prog), alpha = 0.6) +
  geom_ribbon(data = x, 
              aes(year, 
                  ymin = conf5_installation_cost,
                  ymax = conf95_installation_cost,
                  fill = prog),
              alpha = 0.2) +
  facet_wrap(~prog) + corriethema +
  theme(legend.position = 'none') + 
  labs(title = "IPCC-scenario's over de installatiekosten van zonne-energie: stelselmatig pessimistisch",
       subtitle = "Gerealiseerde installatiekosten per watt, veronderstellingen over toekomstige installatiekosten in IPCC-modellen (met 95%-interval)",
       caption = "AMPERE (2014), IPCC (2022), IRENA (2022)",
       x = "",
       y= "") +
  scale_fill_manual(guide = "legend", 
                    values = c('#9dceca', '#867abd', "#febfa0"),
                    breaks = c("IPCC (2022)", "Wet van Wright", "IPCC (2014)"),
                    labels = c("IPCC (2022)", "Wet van Wright", "IPCC (2014)")) +
  scale_y_log10(breaks = log_breaks(n = 10, base = 10),
                labels = scales::dollar_format(suffix = "/KW",
                                               accuracy = 1))