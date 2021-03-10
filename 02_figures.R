## Load libraries
pacman::p_load("ggplot2", "dplyr", "cowplot", "scales",
               "lubridate")

############################################################

load("models/dengue/estimates.RData")
load("models/malaria/estimates.RData")

estimates <- 
  rbind(dengue_estimates,
    malaria_estimates) %>%
  mutate(source = recode(source, 
                           "chelsa" = "CHELSA",
                           "cru"   = "CRU",
                           "era5" = "ERA5",
                           "station" = "Station",
                           "terraclimate" = "TerraClimate",
                            "worldclim" = "WorldClim")) %>%
  mutate(source = factor(source, levels = c("Station",
                                                "CHELSA",
                                                "CRU",
                                                "ERA5",
                                                "TerraClimate",
                                                "WorldClim"))) %>%
ggplot(aes(variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey", size = 0.5) +
  geom_point(aes(x = variable, y = mean, colour = source), shape = 16, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci, ymax = uci, colour = source), position = position_dodge(width = 0.5)) +
  xlab("") + 
  coord_flip() + 
  facet_wrap(~model, scales = "free") +
  scale_y_continuous(name = "Estimate") +
  theme_light() +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(colour = "black", size = 12),
        strip.background = element_blank(),
        legend.title = element_blank()) +
  scale_colour_manual(values = rev(c("#FFC2C4", "#F593A8", "#EA638C", "#BA3365", "#89023E", #"#5D1B3F", 
                                     "#A1A9BA")))

# Climate timeseries
data <- read.csv("data/inla_input/data_malaria.csv")

station <- data %>% subset(source == "station" & Year > 1989 & Year < 2016) %>% 
  dplyr::select(Year, Month, tmean) %>%
  dplyr::rename(station = tmean) %>%
  mutate(date = make_date(Year, Month))

tmean <- data %>% subset(Year > 1989 & Year < 2016) %>%
  mutate(date = make_date(Year, Month)) %>% 
  subset(source != "station") %>%
  mutate(source = recode(source, 
                         "chelsa" = "CHELSA",
                         "cru"   = "CRU",
                         "era5" = "ERA5",
                         "terraclimate" = "TerraClimate",
                         "worldclim" = "WorldClim")) %>%
  mutate(source = factor(source, levels = c("CHELSA",
                                            "CRU",
                                            "ERA5",
                                            "TerraClimate",
                                            "WorldClim"))) %>% 
  ggplot() +
  geom_line(data = station, aes(date, station, colour = "station"), alpha = 0.7) +
  geom_line(aes(date, tmean, group = source, colour = "source"), alpha = 0.3) +
  theme_light() +
  theme(legend.position = "top",
        panel.grid = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black")) +
  ylab("Mean temperature (°C)") +
  scale_x_date(name = "Time",
               labels = date_format("%Y"),
               expand = c(0,0),
               breaks = date_breaks("5 years")) +
  facet_wrap(~source, scales = "fixed", ncol = 1) +
  scale_y_continuous(limits = c(20, 30),
                     breaks = seq(20, 30, 5)) +
  scale_colour_manual(values = rev(c("grey50", "red"))) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.3)))

station <- data %>% subset(source == "station" & Year > 1989 & Year < 2016) %>% 
  dplyr::select(Year, Month, prcp) %>%
  dplyr::rename(station = prcp) %>%
  mutate(date = make_date(Year, Month))

prcp <- data %>% subset(Year > 1989 & Year < 2016) %>%
  mutate(date = make_date(Year, Month)) %>% 
  subset(source != "station") %>%
  mutate(source = recode(source, 
                         "chelsa" = "CHELSA",
                         "cru"   = "CRU",
                         "era5" = "ERA5",
                         "terraclimate" = "TerraClimate",
                         "worldclim" = "WorldClim")) %>%
  mutate(source = factor(source, levels = c("CHELSA",
                                            "CRU",
                                            "ERA5",
                                            "TerraClimate",
                                            "WorldClim"))) %>% 
  ggplot() +
  geom_line(data = station, aes(date, station, colour = "station"), alpha = 0.7) +
  geom_line(aes(date, prcp, group = source, colour = "source"), alpha = 0.3) +
  theme_light() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black")) +
  ylab("Precipitation (mm/day)") +
  scale_x_date(name = "Time",
               labels = date_format("%Y"),
               expand = c(0,0),
               breaks = date_breaks("5 years")) +
  facet_wrap(~source, scales = "fixed",
             ncol = 1) +
  scale_colour_manual(values = rev(c("grey50", "blue"))) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.3)))

ggdraw() +
  draw_plot(tmean, x = 0, y = 0.4, width = 0.5, height = 0.6) +
  draw_plot(prcp, x = 0.5, y = 0.4, width = 0.5, height = 0.6) +
  draw_plot(estimates, x = 0, y = 0, width = 1, height = 0.4) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.4, 0.4))
ggsave("figures/fig1", device = "tiff", width = 8, height = 9, units = "in", dpi = 400)
