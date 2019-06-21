# Paquetes y directorios ----
require(pacman)
p_load(
  tidyverse,
  treemap, treemapify,
  gganimate, ggcorrplot, gridExtra, ggthemes, magick, # Animación y elementos extras para ggplot
  blscrapeR # deflactar USD
  )

inp <- "R/fifa_wc/01_datos/"
out <- "R/fifa_wc/03_gráficas/"

# Datos ----
# base de datos sin deflactar
data <- read_csv(paste0(inp, "sin_deflactar.csv"))

# base de datos que contiene deflactor de USD; se usa paquete "blscrapeR"
def <- inflation_adjust(2019) %>% 
  subset(year>2001)

# se asigna el deflactor por año
data$def <- def$adj_value[match(data$year, def$year)]

# se deflacta
data <- data %>% 
  mutate(value = value_00*def,
         country = ifelse(year == 2019 & place > 0, "To be defined", country)) %>% 
  select(-def, -value_00)
rm(def)

# Análisis ----
premio_total <- subset(data, place==0) %>% 
  select(-country, -place) %>% 
  mutate(tournament = ifelse(tournament=="Women's World Cup",
                             "Copa Mundial Femenil", 
                             "Copa Mundial Varonil"),
         tamaño = ifelse(value>14,10,1))

premio_primeros <- subset(data, place==1) %>% 
  select(-place) %>% 
  mutate(tamaño = ifelse(value>3.9,2,0.3),
         country = case_when(str_detect(country, "To be") ~ "Por definirse",
                             str_detect(country, "USA") ~ "EEUU",
                             str_detect(country, "Germany") ~ "Alemania",
                             str_detect(country, "France") ~ "Francia",
                             str_detect(country, "Spain") ~ "España",
                             str_detect(country, "Italy") ~ "Italia",))

# Con base en un modelo de machine learning que puede consultarse aquí,
# https://eeecon.uibk.ac.at/~zeileis/news/fifawomen2019/
# se asignaron los primeros cuatro lugares a los cuatro equipos con mayor probabilidad de pasar a semifinales:
# EEUU, Francia, Inglaterra y Alemania
sim_wwc2019 <- data %>% 
  mutate(country = ifelse(year == 2019 & place == 1, "USA", country),
         country = ifelse(year == 2019 & place == 2, "France", country),
         country = ifelse(year == 2019 & place == 3, "England", country),
         country = ifelse(year == 2019 & place == 4, "Germany", country))
  
usa_comparado <- subset(sim_wwc2019, country=="USA") %>% 
  mutate(Equipo = ifelse(tournament=="Women's World Cup","Femenil\n[5.86]", 
                         "Varonil\n[20.85]"),
         logro = ifelse(place == 1, "Campeonas del mundo",
                        ifelse(place == 16, "Eliminados en\noctavos",
                               "Peores 16")),
         etiqueta = paste0(year, "\n", logro))

# Plots----
torneos <- c(
  "Francia\n2019",
  "Canadá\n2015",
  "Alemania\n2011",
  "Rusia\n2018",
  "Brasil\n2014",
  "Sudáfrica\n2010",
  "Alemania\n2006"
)
copas <- c(
  "Copa Mundial Femenil",
  "Copa Mundial"
)

fiuf <- "Dinero repartido en cada mundial"
fiuff <- "millones de dólares constantes (2019)"
fiuffi <- "Fuente: elaboración propia con datos de la FIFA."
colores <- c("#56B4E9", "#E69F00")
ggplot(premio_total,
       aes(x = reorder(host, year),
           y = 1,
           label = value)) +
  geom_point(aes(size = value,
                 col = tournament)) +
  geom_text(aes(size = tamaño)) +
  scale_size_area(max_size = 30) +
  scale_x_discrete("",
                   breaks = premio_total$host,
                   labels = torneos) +
  scale_colour_manual(values = colores) +
  guides(size = F) +
  labs(title = str_wrap(fiuf, width = 45),
       subtitle = str_wrap(fiuff, width = 100),
       caption = str_wrap(fiuffi, width = 65)) +
  theme_hc() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 16, angle = 15),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.spacing.x = unit(0.25, 'cm'),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12),
        panel.background = element_blank())
ggsave(paste0(out, "01.png"),
       height = 12, width = 17, units = "cm")

fiuf <- "Dinero entregado por la FIFA a la federación estadounidense de fútbol"
fiuff <- "millones de dólares constantes (2019)"
fiuffi <- "Fuente: elaboración propia con datos de la FIFA."
colores <- c("#56B4E9", "#E69F00")
ggplot(usa_comparado, aes(area = value,
                          fill = Equipo,
                          label = etiqueta, 
                          subgroup = Equipo,
                          subgroup2 = year)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre",
                    grow = TRUE, angle = 10) +
  labs(title = str_wrap(fiuf, width = 45),
       subtitle = str_wrap(fiuff, width = 100),
       caption = str_wrap(fiuffi, width = 65)) +
  scale_fill_manual(values = colores) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.key.height = unit(4, 'cm'),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12))
ggsave(paste0(out, "02.png"),
       height = 35, width = 17, units = "cm")


fiuf <- "Dinero recibido al coronarse campeonas/es del mundo"
fiuff <- "millones de dólares constantes (2019)"
fiuffi <- "Fuente: elaboración propia con datos de la FIFA."
colores <- c("#686F8F","#275399","#C40013","#D9DEF8","#118B54","pink")
ggplot(premio_primeros,
       aes(x = reorder(host, year),
           y = 1,
           label = round(value,1))) +
  geom_point(aes(size = value,
                 col = country)) +
  geom_text(aes(size = tamaño)) +
  scale_size_area(max_size = 30) +
  scale_x_discrete("",
                   breaks = premio_total$host,
                   labels = torneos) +
  scale_colour_manual(values = colores) +
  guides(size = F) +
  labs(title = str_wrap(fiuf, width = 45),
       subtitle = str_wrap(fiuff, width = 100),
       caption = str_wrap(fiuffi, width = 65)) +
  theme_hc() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 16,angle = 15),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12),
        panel.background = element_blank())
ggsave(paste0(out, "03.png"),
       height = 12, width = 17, units = "cm")

