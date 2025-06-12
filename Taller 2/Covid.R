rm(list = ls())

devtools::install_github("RamiKrispin/coronavirus",force = TRUE)

library(coronavirus)
library(tidyverse)
library(scales)
coronavirus<-coronavirus
the_caption = "Fuente: OMS y varios via la Universidad John Hopkins y el paquete de R 'coronavirus' de Rami Krispin.\nAnálisis de Rashid C.J. Marcano Rivera, modificando código provisto por http://freerangestats.info (Peter Ellis)"
the_captionEng = "Source: WHO and many others via Johns Hopkins University and Rami Krispin's coronavirus R package.\nAnalysis by Rashid C.J. Marcano Rivera, modifying code provided by http://freerangestats.info"

top_countries <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(cases = sum(cases)) %>%
  top_n(10, wt = cases)


coronavirus %>%
  group_by(country, date) %>%
  summarize_at(vars(cases), list(~mean(.), ~median(.))) %>%
  arrange(date) %>%
  spread(type, cases) %>%
  ungroup() %>%
  mutate(casos_cumulativos = cumsum(confirmed))

d1 <- coronavirus %>%
  group_by(date, type) %>%
  summarise(cases = sum(cases)) %>%
  arrange(date) %>%
  spread(type, cases) %>%
  ungroup() %>%
  mutate(cfr_today = death / confirmed,
         cfr_cumulative = cumsum(death) / cumsum(confirmed))


#---------------------------global total-------------------

first_non_china_d <- coronavirus %>%
  filter(country != "China" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

first_italy_d <- coronavirus %>%
  filter(country == "Italy" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

first_us_d <- coronavirus %>%
  filter(country == "US" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

max_death_rate <- as.Date("2020-04-29")
today<-as.Date("2020-08-06")

d1 <- coronavirus %>%
  group_by(date, type) %>%
  summarise(cases = sum(cases)) %>%
  arrange(date) %>%
  spread(type, cases) %>%
  ungroup() %>%
  mutate(cfr_today = death / confirmed,
         cfr_cumulative = cumsum(death) / cumsum(confirmed))



d1b <- d1 %>%
  filter(date %in% c(first_italy_d, first_non_china_d,first_us_d))
ac <- "steelblue"
ac2<- "red1"
ac3<- "firebrick1"

d1c <- d1 %>%
  mutate(cc = cumsum(confirmed)) %>% 
  summarise(`10000` = min(date[cc > 10000]),
            `100000` = min(date[cc > 100000]),
            `1000000` = min(date[cc > 1000000]),
            `2000000` = min(date[cc > 2000000]),
            `5000000` = min(date[cc > 5000000]),
            `15000000` = min(date[cc > 15000000])) %>%
  gather(variable, date) %>%
  left_join(d1, by = "date") %>%
  mutate(label = paste0(format(as.numeric(variable), big.mark = ",", scientific = FALSE), " casos"))

d1d <- d1 %>%
  filter(date %in% c(max_death_rate))
d1e<-d1 %>%
  filter(date %in% c(today))
??geom_ma
library(tidyquant)
new<-ggplot(d1, aes(x=date,y=cfr_today)) +
  geom_line( aes(y=cfr_cumulative)) + 
  geom_ma(ma_fun = SMA, n = 7, color = "blue", size = 0.3,linetype=1)
#geom_line( aes(y=cfr_today))

# Custom the Y scales:
new<-new+  scale_y_continuous(label = percent_format(accuracy = 0.1),
                              # Features of the first axis
                              name = "Tasa de fatalidad de casos observados (cumulativa)",
                              # Add a second axis and specify its features
                              sec.axis = sec_axis( trans=~.*1, label = percent_format(accuracy = 0.1),name="Tasa de fatalidad diaria (promedio de 7 días)")
)+expand_limits(y = 0)+
  theme(axis.title.y = element_text(color = "black", size=13),
        axis.title.y.right = element_text(color = "blue", size=13))+
  labs(caption = the_caption,
       x = "",
       title = "Cambios en tasa de fatalidad de COVID-19",
       subtitle = "El aumento de la tasa cumulativa fue gradual hasta alcanzar su cénit el 29 de abril (7.18%). Luego ha descendido gradulmente a 3.74% (6 de agº). Se entiende que 
el aumento se debió a la rápida propagación a grupos vulnerables y relativa escasez de pruebas, mientras el descenso se debe posiblemente a la propagación a personas más jóvenes.
La tasa diaria, más volátil, alcanzó su máximo el 24 de febº (28.32%). Nótese que las tasas podrían ser más bajas en realidad debido a casos no diagnosticados.")

new<-new+geom_point(data = d1b, colour = ac, shape = 1, size = 2,inherit.aes = FALSE,aes(x=date,y=cfr_cumulative))+
  geom_point(data=  d1d, colour = ac2, shape = 1, size = 2,inherit.aes = FALSE,aes(x=date,y=cfr_cumulative))+
  geom_point(data= d1e, colour = ac3, shape = 1, size = 2,inherit.aes = FALSE,aes(x=date,y=cfr_cumulative)) +
  geom_point(data= d1e, color="blue1", shape = 1, size = 2)+
  geom_point(data=  d1c, colour = "grey70", shape = 1, size = 2,inherit.aes = FALSE,aes(x=date,y=cfr_cumulative))

new<-new+  annotate("text", x = first_italy_d, 
                    y = filter(d1, date == first_italy_d)$cfr_cumulative - 0.001, 
                    label = "Primera muerte en Italia",
                    hjust = 0, size = 3, colour = ac) +
  annotate("text", x = first_non_china_d, 
           y = filter(d1, date == first_non_china_d)$cfr_cumulative + 0.002, 
           label = "Primera muerte fuera de China",
           hjust = -0.002, size = 3, colour = ac) +
  annotate("text", x = first_us_d, 
           y = filter(d1, date == first_us_d)$cfr_cumulative + 0.002, 
           label = "Primera muerte en EEUU",
           hjust = 0, size = 3, colour = ac) +
  annotate("text", x=max_death_rate,
           y = filter(d1, date == max_death_rate)$cfr_cumulative + 0.001, 
           label = "7.18%",
           hjust = 0, size = 3, colour = "red1")+
  annotate("text", x=today,
           y = filter(d1, date == today)$cfr_cumulative + 0.001, 
           label = "3.74%",
           hjust = 0, size = 3, colour = ac3)
new
new+  geom_text(data = d1c, aes(x=date,y=cfr_cumulative,label = label), 
                size = 3, colour = "grey70", 
                hjust = 0.5, lineheight = 0.9, nudge_y = -0.003,
                inherit.aes = FALSE)+
  annotate("text", x=today,
           y = filter(d1, date == today)$cfr_today + 0.001, 
           label = "2.28%",
           hjust = 0, size = 3, colour = "blue1")

#d1 %>%
ggplot(aes(x = date, y = cfr_cumulative)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  expand_limits(y = 0) +
  geom_point(data = d1b, colour = ac, shape = 1, size = 2) +
  geom_point(data=  d1d, colour = ac2, shape = 1, size = 2)+
  geom_point(data= d1e, colour = ac3, shape = 1, size = 2) +
  geom_point(data=  d1c, colour = "grey70", shape = 1, size = 2) +
  annotate("text", x = first_italy_d, 
           y = filter(d1, date == first_italy_d)$cfr_cumulative - 0.001, 
           label = "Primera muerte en Italia",
           hjust = 0, size = 3, colour = ac) +
  annotate("text", x = first_non_china_d, 
           y = filter(d1, date == first_non_china_d)$cfr_cumulative + 0.002, 
           label = "Primera muerte fuera de China",
           hjust = -0.002, size = 3, colour = ac) +
  annotate("text", x = first_us_d, 
           y = filter(d1, date == first_us_d)$cfr_cumulative + 0.002, 
           label = "Primera muerte en EEUU",
           hjust = 0, size = 3, colour = ac) +
  annotate("text", x=max_death_rate,
           y = filter(d1, date == max_death_rate)$cfr_cumulative + 0.001, 
           label = "7.18%",
           hjust = 0, size = 3, colour = "red1")+
  annotate("text", x=today,
           y = filter(d1, date == today)$cfr_cumulative + 0.001, 
           label = "4.71%",
           hjust = 0, size = 3, colour = ac3)+
  geom_text(data = d1c, aes(label = label), 
            size = 3, colour = "grey70", 
            hjust = 0.5, lineheight = 0.9, nudge_y = -0.003) +
  labs(caption = the_caption,
       x = "",
       y = "Tasa de mortandad de casos observados",
       title = "Aumentos en tasa de mortalidad de COVID-19",
       subtitle = "El aumento fue gradual hasta alcanzar su cénit el 29 de abril. Luego ha descendido gradulmente a 4.7% (4 de julio).
Nótese que la tasa podría ser más baja en realidad debido a casos no diagnosticados.")

#-----------------Country-specific totals------------------------

d2 <- coronavirus %>%
  filter(date <= as.Date("2020-07-31")) %>%  # Filtro para datos dentro de 2020
  group_by(date, country, type) %>%
  summarise(cases = sum(cases)) %>%
  group_by(date, country) %>%
  spread(type, cases) %>%
  arrange(date) %>%
  group_by(country) %>%
  mutate(cfr_cumulative = cumsum(death) / cumsum(confirmed)) %>%
  filter(!is.na(cfr_cumulative)) %>%
  ungroup() %>%
  inner_join(top_countries, by = "country") 
library(ggrepel)
library(ggthemes)
x_limits <- c(today, NA)
d2<- d2 %>%
  mutate(cfr_cumulativeperc=round(cfr_cumulative*100,2))
d2 %>%
  ggplot(aes(x = date, y = cfr_cumulative, colour = country)) +
  geom_line() +
  geom_label_repel(data = filter(d2, date == max(date)), aes(label = paste("",country,"-",cfr_cumulativeperc,"%")), 
                   hjust = 1, size = 3, xlim = x_limits, segment.color="black"	) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, .25)) +
  scale_colour_brewer(type = 'qual', palette = 'Paired', direction = 2) +
  expand_limits(x = max(d2$date) + 13) +
  labs(caption = the_captionEng,
       x = "",
       x = "",
       y = "Observed case fatality rate",
       title = "Country-specific case fatality rate of COVID-19 in early and mid 2020",
       subtitle = "Ten countries with most diagnosed cases; Iran's early values truncated.
A high level of uncertainty reflecting rapidly changing denominators as well as many unresolved cases.") +
  theme_solarized()+  theme(legend.position = "none")

summary(as.factor(d2$country))
d2<- d2 %>%
  mutate(country=recode(country, US = "EEUU", Russia ="Rusia", Mexico = "México", Brazil = "Brasil", 
                        "United Kingdom" = "Reino Unido", France="Francia", Japan="Japón",
                        Italy = "Italia", Iran = "Irán", Germany = "Alemania", `Korea, South`="Surcorea",
                        "South Africa"= "Sudáfrica"))
d2<- d2 %>%
  mutate(cfr_cumulativeperc=round(cfr_cumulative*100,2))

d2 %>%
  ggplot(aes(x = date, y = cfr_cumulative, colour = country)) +
  geom_line() +
  geom_label_repel(data = filter(d2, date == max(date)), aes(label = paste("",country,"-",cfr_cumulativeperc,"%")), 
                   hjust = 1, size = 3, xlim = x_limits, segment.color="black"	) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, .25)) +
  scale_colour_brewer(type = 'qual', palette = 'Paired', direction = 1) +
  expand_limits(x = max(d2$date) + 13) +
  labs(caption = the_caption,
       x = "",
       y = "Tasa de mortandad de casos observados",
       title = "Fatalidad de casos de COVID-19 en los diez países con más casos diagnosticados",
       subtitle = "Los diez países con mayor cantidad de casos diagnosticados. El caso de Irán ha sido truncado en sus primeras observaciones. 
Un alto grado de incertidumbre refleja denominadores que cambian con fluidez, y el desconocimiento de casos no resueltos.") +
  theme_solarized()+  theme(legend.position = "none")



library(RColorBrewer)

display.brewer.all()


