library(ggplot2)
library(ggthemes)
library(eurostat)
library(giscoR)
library(tidyr)
library(ggthemes)
library(sf)

col1 = "#013C58"
col2 = "#E69C38"
col3 = "#800000"
col4 = "#808000"
col5 = "#104C00"
col6 = "#7195C1"
col7 = "#996633"
col8 = "#E6E122"
szinek <- c(col1, col2, col3, col4, col5, col6, col7, col8)

### PENSION
# Pension data for Hungary 2000-2023
pension_data <- read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/pension_by_region.xlsx")

# Pensions by region (and by age for females)
data %>% 
  filter(age<=65 & age>=60, sex=="F") %>% 
  ggplot() +
  facet_wrap(~age) +
  geom_line(aes(x=cal_year, y=avg_pension, color=geo)) +
  labs(title="Average old-age female pensions by age")

data %>% 
  filter(age==65, sex=="M") %>% 
  ggplot() +
  geom_line(aes(x=cal_year, y=avg_pension, color=geo)) +
  labs(title="Average old-age male pensions in age 65")

data %>% 
  filter(age>=60 & sex=="F" | age>=65 & sex=="M") %>% 
  ggplot() +
  facet_grid(sex~geo, labeller = labeller(sex=c("F"="Nő", "M"="Férfi"))) +
  geom_hline(yintercept = c(seq(0, 4000, 1000)), col="lightgrey", linetype="dashed")+
  geom_line(aes(x=cal_year, y=avg_pension/1000, color=as.factor(birth_year))) +
  theme(text=element_text(size=14, family="serif"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size=13),
        axis.line = element_line(),
        legend.key = element_rect(fill = "white"),
        legend.position = "right")+
  scale_color_manual( values = pals::kovesi.linear_gow_60_85_c27(n=16)) +
  scale_x_continuous(breaks = c(2035, 2050, 2065, 2080))+
  labs(y="Nyugdíj (ezer Ft)", color="Születési év")

### EMPLOYMENT
# Plot of emp_rates by birth_years
data %>% 
  filter(age<65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  annotate("rect", xmin=1999, xmax=2022, ymin=0, ymax=Inf, alpha=0.1, fill="darkgreen") +
  geom_line(aes(x=cal_year, y=emp_rate, linetype=sex, color=geo)) + 
  geom_vline(xintercept = c(2001, 2011, 2016, 2022), linetype="dotted", color="black")+ 
  theme_fivethirtyeight()+
  theme(legend.title = element_blank(), text=element_text(size=12, family="serif"),
        legend.position = "right", plot.background = element_rect(fill="white"),
        legend.background = element_blank()) +
  scale_linetype_manual(values=c(1,2), labels = c("Nő","Férfi"))+ 
  guides(color = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1))+
  labs(title = "Foglalkoztatási ráta születési évenként nem, kor és régió szerint", 
       caption = "HU10: Közép-Magyarország, HU21: Közép-Dunántúl, HU22: Nyugat-Dunántúl, HU23: Dél-Dunántúl, HU31: Észak-Magyarország, HU32: Észak-Alfold, HU33: Dél-Alföld")

data %>% 
  filter(age<65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  geom_hline(yintercept = c(seq(0, 1, 0.25)), col="lightgrey", linetype="dashed")+
  annotate("rect", xmin=1999, xmax=2022, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgreen") +
  geom_line(aes(x=cal_year, y=emp_rate, linetype=sex, color=geo)) + 
  geom_vline(xintercept = c(2001, 2011, 2016, 2022), linetype="dotted", color="black")+ 
  theme(legend.title = element_blank(), text=element_text(size=14, family="serif"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_line(),
        legend.key = element_rect(fill = "white"),
        plot.caption = element_text(size=11))+
  scale_color_manual(values = szinek[1:7]) +
  scale_linetype_manual(values=c(1,2), labels = c("Nő","Férfi")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(#title="Foglalkoztatási ráta születési évenként nem, kor és régió szerint",
       y="Foglalkoztatási ráta (%)",
       #caption = "HU10: Közép-Magyarország, HU21: Közép-Dunántúl, HU22: Nyugat-Dunántúl, HU23: Dél-Dunántúl,
       #HU31: Észak-Magyarország, HU32: Észak-Alfold, HU33: Dél-Alföld"
       )

### INCOME
# Plot of incomes by birth_years
data %>% 
  filter(age<65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  geom_hline(yintercept = c(seq(0, 3000, 1000)), col="lightgrey", linetype="dashed")+
  annotate("rect", xmin=2003, xmax=2018, ymin=0, ymax=Inf, alpha=0.1, fill="darkgreen") +
  annotate("rect", xmin=2018, xmax=2023, ymin=0, ymax=Inf, alpha=0.2, fill="darkgreen") +
  geom_line(aes(x=cal_year, y=income/1000, linetype=sex, color=geo)) +
  theme(legend.title = element_blank(), text=element_text(size=14, family="serif"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_line(),
        legend.key = element_rect(fill = "white"),
        plot.caption = element_text(size=11))+
  scale_color_manual(values = szinek[1:7]) +
  scale_linetype_manual(values=c(1,2), labels = c("Nő","Férfi")) +
  labs(#title="Keresetek születési évenként nem, kor és régió szerint",
    y="Jövedelem (ezer Ft)",
    #caption = "HU10: Közép-Magyarország, HU21: Közép-Dunántúl, HU22: Nyugat-Dunántúl, HU23: Dél-Dunántúl,
    #HU31: Észak-Magyarország, HU32: Észak-Alfold, HU33: Dél-Alföld"
  )

### PROBABILITY ALIVE
#Plot of probabilities of being alive by birth years
probs %>% 
  filter(age>=65) %>% 
  ggplot() +
  facet_wrap(~birth_year) +
  geom_line(aes(x=age, y=prob_alive, linetype=sex, color=geo))

data %>% 
  filter(age>=65, birth_year %in% c(1970, 1975, 1980, 1985)) %>% 
  ggplot() +
  facet_grid(~geo) +
  geom_hline(yintercept = c(seq(0, 1, 0.25)), col="lightgrey", linetype="dashed")+
  geom_hline(yintercept = c(seq(0.125, 0.875, 0.25)), col="lightgrey", linetype="dashed", size=0.3)+
  geom_line(aes(x=age, y=prob_alive, linetype=sex, colour = as.factor(birth_year))) +
  theme(text=element_text(size=14, family="serif"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13),
        axis.line = element_line(),
        legend.key = element_rect(fill = "white"),
        legend.position = "top")+
  scale_color_manual(values = szinek) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_linetype_manual(values=c(1,2), labels = c("Nő","Férfi")) +
  labs(y="P(életben van)",
       x="Életkor",
       color="Születési év",
       linetype="")

#Development of P(alive) at age 100
data %>% 
  filter(age==100) %>% 
  ggplot() +
  facet_grid(~geo) +
  geom_hline(yintercept = c(seq(0, 0.025, 0.005)), col="lightgrey", linetype="dashed")+
  geom_line(aes(x=cal_year, y=prob_alive, linetype=sex)) +
  theme(text=element_text(size=14, family="serif"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13),
        axis.line = element_line(),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        legend.title = element_blank())+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_linetype_manual(values=c(1,2), labels = c("Nő","Férfi")) +
  labs(y="P(életben van)")


### RESULTS
# Results base

results %>% 
  ggplot(aes(x=birth_year, y=irr_r)) + 
  geom_point(aes(shape=sex, colour = geo))


### MAP
#cartography package: irrs by birth years or by gender on maps
# Get geometrics
Hungary_shp <- get_eurostat_geospatial(resolution = 10, 
                                       nuts_level = 2, 
                                       year = 2013) %>% 
  filter(CNTR_CODE=="HU")

if (is.na(st_crs(Hungary_shp))) {
  st_crs(Hungary_shp) <- 4326  # Set to WGS 84 as an example
}

# Optionally, transform to a different CRS
Hungary_shp <- st_transform(Hungary_shp, crs = 4326)

regions <- Hungary_shp %>%
  mutate(center = st_centroid(geometry),
         x = st_coordinates(center)[, 1],
         y = st_coordinates(center)[, 2])

# ÍGY NEM torzított!
ggplot(data = regions) +
  geom_sf() +
  geom_text(aes(x = x, y = y, label = NUTS_NAME), size = 3, family = "serif") + 
  coord_sf(crs = st_crs(regions)) + 
  theme_void()

mapdata <- results %>% 
  filter(birth_year==1970, sex=="M")

# Merge data data with geometrics
regions <- merge(regions, mapdata, all.x = TRUE, all.y = TRUE, by.x = "id", by.y = "geo")

# Create plot
vector_colors_gradient_2 <- RColorBrewer::brewer.pal(n=7, "Greens")

ggplot(aes(fill=irr_n), data = regions) +
  geom_sf(size=0.1, color="#F3F3F3") +
  geom_text(aes(x = x, y = y, label = NUTS_NAME), size = 4, family = "serif") + 
  geom_text(aes(x = x, y = y-0.1, label = scales::percent(irr_n, accuracy = 0.01,
                                                          decimal.mark = ",")),            size = 4, family = "serif") +
  coord_sf(crs = st_crs(regions)) + 
  scale_fill_gradientn(colours = vector_colors_gradient_2) +
  #labs(fill = "IRR") +
  theme_void() +
  theme(legend.position = "none")


# Incomes 2023
inc_2023 <- readxl::read_xlsx("C:/Nandi/BPM/Szakdoga/adatok/plots/jov_2023_plot.xlsx", sheet = "input", col_names = T)

# Merge data data with geometrics
regions <- merge(regions, inc_2023, all.x = TRUE, all.y = TRUE, by.x = "NUTS_NAME", by.y = "region")

ggplot(aes(fill=pers_inc_2023), data = regions) +
  geom_sf(size=0.1, color="#F3F3F3") +
  geom_text(aes(x = x, y = y, label = NUTS_NAME), size = 4, family = "serif", fontface = "bold") + 
  geom_text(aes(x = x, y = y-0.1,
                label = scales::dollar(pers_inc_2023, big.mark = " ", prefix="", 
                                       suffix = " Ft")),
            size = 4, family = "serif") +
  coord_sf(crs = st_crs(regions)) + 
  scale_fill_gradientn(colours = vector_colors_gradient_2, na.value = "lightblue") +
  #labs(fill = "IRR") +
  theme_void() +
  theme(legend.position = "none")
