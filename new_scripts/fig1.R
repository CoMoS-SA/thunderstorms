library(sf)
library(tidyverse)
library(magrittr)
library(spatstat)
library(lubridate)
library(ggpubr)
library(hurricaneexposure)
library(hurricaneexposuredata)


rm(list = ls())

data <- readRDS("data/clean_storm_step3.rds")
county_shape <- st_read("utilities/c_10nv20/") 

data %<>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020)

events <- data %>%
  select(BEGIN_LON, BEGIN_LAT, YEAR) %>% 
  filter(!is.na(BEGIN_LON)) %>% 
  filter(!is.na(BEGIN_LAT)) %>% 
  st_as_sf(.,                         
           coords = c("BEGIN_LON", "BEGIN_LAT"),
           crs = 4269)

sf::sf_use_s2(FALSE)
filt <- as.matrix(st_intersects(events$geometry, county_shape))
events <- events[apply(filt,1,any),]
events %>% nrow() #count to report in manuscript
nrow(events)/nrow(data) #count to report in manuscript
remove(filt)

county_wind(counties = unique(county_shape$FIPS), start_year = 1991, end_year = 2019, 
            wind_var = "v_max_gust", wind_limit = 800, wind_source = "modeled") %>% 
  group_by(fips) %>% 
  summarise(
    n21=sum(vmax_sust>21)
  ) %>% 
  filter(n21>0)-> hurr

county_shape %>% 
  left_join(
    .,
    hurr,
    by=c("FIPS"="fips")
  ) -> aux

aux %<>% 
  mutate(
    impacted = ifelse(is.na(n21),0,1)
  ) %>% 
  group_by(impacted) %>% 
  summarize(
    geometry = st_union(geometry)
  )

county_shape %>%
  summarize(
    geometry = st_union(geometry)
  ) -> usa

usa2 <- st_cast(usa, "MULTILINESTRING") 
aux2 <- st_cast(aux, "MULTILINESTRING")

inner_boundaries <- st_difference(aux2, usa2)

ggplot() +
  geom_sf(data=usa, fill="#132B43") + 
  geom_sf(data=events, size=0.001, color="#fec44f") +
  geom_sf(data=aux[2,] %>% st_cast("MULTILINESTRING"), color="firebrick", linewidth=0.4) +
  geom_sf(data=aux[2,], color="transparent", fill="indianred", alpha=0.3) +
  geom_density_2d() +
  xlim(-123,-69) +
  ylim(25.5,48.5) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  ) -> map

#### Duration ####

data %<>% 
  mutate(
    BEGIN_DATE_TIME = data$BEG_T,
    END_DATE_TIME = data$END_T,
    timediff = as.numeric((END_DATE_TIME - BEGIN_DATE_TIME))/3600 #in hours
  )

data %>% 
  filter(timediff>=0) %>% #5 obs with wrong dates
  filter(timediff<24) %>%
  filter(timediff>0) %>%
  ggplot() +
  geom_histogram(aes(x=timediff), bins = 60) +
  xlab("Duration") +
  ylab("Number of\nstorm episodes") +
  scale_x_continuous(breaks=c(6,12,18,24), labels = c("6h","12h","18h","24h")) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )-> time_a

data %>% 
  filter(timediff>=0) %>% 
  filter(timediff<(24)) %>% 
  filter(timediff>0) %>% 
  filter(timediff<1) %>% 
  ggplot() +
  geom_histogram(aes(x=timediff*60), bins=58) + 
  scale_x_continuous(breaks=c(5,15,30,45,60), labels = c("5m","15m","30m","45m","60m")) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) -> time_b

time_a + 
  annotation_custom(
    ggplotGrob(time_b), 
    xmin = 3.5, xmax = 23, ymin = 8000, ymax = 42000
  ) -> p_time

#### Distance #####

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

spacediff<-earth.dist(data$BEGIN_LON,data$BEGIN_LAT,data$END_LON,data$END_LAT)
data$distance <- spacediff

data %>% 
  filter(!is.na(distance)) %>% 
  pull(distance) %>% mean()

data %>% 
  filter(!is.na(distance)) %>% 
  pull(distance) %>% quantile(.,0.95)

data %>% 
  filter(!is.na(distance)) %>% 
  filter(distance!=0) %>% 
  pull(distance) %>% mean()

data %>% 
  filter(!is.na(distance)) %>% 
  filter(distance!=0) %>% 
  pull(distance) %>% quantile(.,0.95)

data %>% 
  filter(timediff>=0) %>% 
  filter(!is.na(distance)) %>% 
  filter(distance!=0) %>% 
  nrow()/nrow(data)

data %>%
  filter(distance==0) %>% 
  nrow()/nrow(data)

data %>% 
  filter(timediff==0) %>% 
  nrow()/nrow(data)

data %>% 
  filter(timediff<=24) 

data %>% 
  filter(!is.na(distance)) %>% #those who do not have end coordinates
  filter(distance!=0) %>% #those with equal start and end coordinates
  filter(distance<100) %>% #less than 100 km
  ggplot() + 
  geom_histogram(aes(x=distance), bins=60) +
  xlab("Span") +
  ylab("Number of\nstorm episodes") +
  scale_x_continuous(breaks=c(0,25,50,75,100), labels = c("0","25km","50km","75km","100km")) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )-> p_space

#### Summary stats ####

data %>% 
  mutate(
    yea = as.numeric(as.character(YEAR))
  ) %>% 
  mutate(
    sli = case_when(
      yea<1998 ~ "1991-1997",
      yea>=1998 & yea <2003 ~ "1998-2002",
      yea>=2003 & yea <2008 ~ "2003-2007",
      yea>=2008 & yea <2013 ~ "2008-2012",
      yea>=2013 & yea <2020 ~ "2013-2019"
    )
  ) %>% 
  group_by(sli) %>% 
  summarise(
    events=n(),
    mean = round(mean(MAGNITUDE)*1.60934,2)
  ) %>% 
  rename(Period=sli) %>% 
  rename('Number of\nstorm episodes'=events) %>% 
  rename('Mean wind\nspeed (km/h)'=mean) -> tab_data


ggtexttable(
  tab_data,
  rows = NULL,
  theme = ttheme(
    base_size = 14,
    colnames.style = colnames_style(parse = T, size=14, face="plain"),
    rownames.style = rownames_style(
      face = "plain",
      fill = c("white", "grey80", "grey80", "grey80", "grey80"),
      hjust=1.3,
      parse = T
    ),
  )
) -> tab

#### Put together ####

cowplot::plot_grid(
  p_time + 
    theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size=15),
    ), 
  p_space+ 
    theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size=15),
    ),
  tab + theme(plot.margin=grid::unit(c(0,0,10.5,0), "mm")),
  rel_widths = c(0.32,0.32,0.36), labels=c('B','C','D'), nrow=1,
  label_size = 22,
  label_y = 1.1
) -> bot_row

cowplot::plot_grid(
  map + theme(plot.margin=grid::unit(c(0,0,-10,0), "mm")), 
  NULL,
  bot_row,
  nrow=3, 
  rel_heights = c(0.75,-0.06,0.25), 
  labels=c('A'),
  label_y = 0.90,
  label_size = 22
) -> fig_0

ggsave("charts/fig_1.pdf", fig_0, height = 6.5, width = 7.5, scale = 1.7)
knitr::plot_crop("charts/fig_1.pdf")
