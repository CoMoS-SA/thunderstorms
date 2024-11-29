library(sf)
library(tidyverse)
library(magrittr)
library(spatstat)
library(lubridate)
library(ggpubr)

rm(list = ls())

data <- readRDS("data/clean_hail_step3.rds")

data %<>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020)

data %>% nrow() #report for manuscript

usa <- st_as_sf(maps::map("usa", fill=TRUE, plot =FALSE))

#### Duration ####

data %<>% 
  mutate(
    BEGIN_DATE_TIME = data$BEG_T,
    END_DATE_TIME = data$END_T,
    timediff = as.numeric((END_DATE_TIME - BEGIN_DATE_TIME))/3600 #in hours
  )

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

###### Map #####

panel <- readRDS(file=paste0("data/panel_hail_tr.rds")) %>% as_tibble()
county_shape <- st_read("utilities/c_10nv20/") #counties

panel$superex<-ifelse(panel$S3>mean(panel$S3)+sd(panel$S3),1,0)

df1_aggre = aggregate(superex ~ FIPS, data = panel, sum)
names(df1_aggre)[2] = "superex"
df1_aggre$superex<-(df1_aggre$superex/29)*100

datamap <- left_join(county_shape, df1_aggre, by = c("FIPS"))

datamap$superex[is.na(datamap$superex)]<-0

datamap %>%
  ggplot() +
  geom_sf(aes(fill=superex/100), colour="transparent") +
  scale_fill_gradient(labels = scales::percent, breaks=c(0,0.15, 0.3, 0.45)) +
  xlim(-123,-69) +
  ylim(25.5,48.5) +
  labs(fill = "Percentage of\noverexposed years") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  ) -> map

panel %>% 
  filter(S3>0) %>% 
  ggplot() + 
  geom_histogram(aes(x=S3), bins=60) +
  xlab(bquote('Exposure ('*H^3*')')) +
  ylab("Number of\nobservations") +
  theme_bw() + 
  theme(panel.grid = element_blank())-> s3_hist

panel %>% 
  filter(S3>1000) %>% 
  ggplot() + 
  geom_histogram(aes(x=S3), bins=60) +
  xlab(bquote('Exposure ('*H^3*')')) +
  ylab("Number of\nobservations") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    text=element_text(size=7)
    )-> s3_hist_2

s3_hist + 
  annotation_custom(
    ggplotGrob(s3_hist_2), 
    xmin = 900, xmax = 3300, ymin = 1500, ymax = 9800
  ) -> s3_hist_tot

#### Summary stats ####
data %>% 
  mutate(
    yea = as.numeric(as.character(YEAR))
  ) %>% 
  filter(yea>1990) %>% 
  filter(yea<2020) %>% 
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
    mean = round(mean(MAGNITUDE)*2.54,2)
  ) %>% 
  rename(Period=sli) %>% 
  rename('Number of\nrecorded storms'=events) %>% 
  rename('Mean size (cm)'=mean) -> tab_data1

data %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) %>% 
  filter(!is.na(distance)) %>%
  filter(distance!=0) %>% 
  filter(timediff>=0) %>% #5 obs with wrong dates
  filter(timediff<24) %>%
  filter(timediff>0) %>% 
  mutate(
    sli = case_when(
      YEAR<1998 ~ "1991-1997",
      YEAR>=1998 & YEAR <2003 ~ "1998-2002",
      YEAR>=2003 & YEAR <2008 ~ "2003-2007",
      YEAR>=2008 & YEAR <2013 ~ "2008-2012",
      YEAR>=2013 & YEAR <2020 ~ "2013-2019"
    )
  ) %>% 
  group_by(sli) %>% 
  summarise(
    mean_dist = round(mean(distance),2),
    mean_dur = round(mean(timediff),2)
  ) -> tab_data2
  
tbdt <- cbind(tab_data1, tab_data2[,2:3])

colnames(tbdt)<- c("Period", "Number of\nhail episodes", "Mean hail\nsize (cm)", "Mean storm\nspan (Km)", "Mean storm\nduration (h)")

ggtexttable(
  tbdt,
  rows = NULL,
  theme = ttheme(
    base_size = 7,
    colnames.style = colnames_style(parse = T, size=7, face="plain"),
    rownames.style = rownames_style(
      face = "bold",
      fill = c("white", "grey80", "grey80", "grey80", "grey80"),
      hjust=1.3,
      parse = T
    ),
  )
) -> tab2

#### Put together ####

cowplot::plot_grid(
  s3_hist_tot + 
    theme(
      axis.text = element_text(size=8),
      axis.title = element_text(size=9),
      panel.grid.minor = element_blank()
    ), 
  tab2 + 
    theme(plot.margin=grid::unit(c(0,3,9.5,0), "mm")), 
  rel_widths = c(0.5,0.5), labels=c('B','C'), label_y = 1.1
) -> bot_row
cowplot::plot_grid(
  map +
    guides(fill = guide_colorbar(title.position = "top")) +
    theme(
      panel.border = element_blank(),
      legend.text = element_text(size=8),
      legend.title = element_text(size=9, hjust=0.5),
      legend.position = c(0.2,0.15),
      legend.direction = "horizontal"
    ), 
  NULL,
  bot_row, 
  nrow=3, 
  rel_heights = c(0.7,-0.06,0.3), 
  labels=c('A'),
  label_y = 0.945
) -> fig_0

ggsave("charts/fig_hail.pdf", fig_0, height = 6.5, width = 7.5)
knitr::plot_crop("charts/fig_hail.pdf")
