library(tidyverse)
library(sf)
library(magrittr)
library(ggpubr)
library(kableExtra)

rm(list = ls())

data <- readRDS(file=paste0("data/f_panel_tr.rds"))
county_shape <- st_read("utilities/c_10nv20/")

data$superex<-ifelse(data$S4>mean(data$S4)+sd(data$S4),1,0)

df1_aggre = aggregate(superex ~ FIPS, data = data, sum)
names(df1_aggre)[2] = "superex"
df1_aggre$superex<-(df1_aggre$superex/29)*100

datamap <- left_join(county_shape, df1_aggre, by = c("FIPS"))

datamap$superex[is.na(datamap$superex)]<-0

datamap %>%
  ggplot() +
  geom_sf(aes(fill=superex/100), colour="transparent") +
  scale_fill_gradient(labels = scales::percent, low="#132B43", high="#fec44f") +
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

data %>% 
  filter(S4>0) %>% 
  ggplot() + 
  geom_histogram(aes(x=S4), bins=60) +
  #xlim(-1000,25000) +
  xlab(bquote('Exposure ('*S^4*')')) +
  ylab("Number of\nobservations") +
  theme_bw() + 
  theme(panel.grid = element_blank())-> s4_hist

corr_data <- cor(data %>% select(S1, S2, S3, S4) %>% as.matrix())
corr_data<- round(corr_data,2)

colnames(corr_data) <- c(
  expression(S^1),
  expression(S^2),
  expression(S^3),
  expression(S^4)
)

rownames(corr_data) <- c(
  expression(S^1),
  expression(S^2),
  expression(S^3),
  expression(S^4)
)

ggtexttable(
  corr_data,
  theme = ttheme(
    base_size = 9,
    colnames.style = colnames_style(parse = T, size=9),
    rownames.style = rownames_style(
      face = "bold",
      fill = c("white", "grey80", "grey80", "grey80", "grey80"),
      hjust=1.3,
      size=9,
      parse = T
    ),
  )
) -> corr_tab

cowplot::plot_grid(
  s4_hist + 
    theme(
      axis.text = element_text(size=8),
      axis.title = element_text(size=9),
      panel.grid.minor = element_blank()
    ), 
  corr_tab + 
    theme(plot.margin=grid::unit(c(0,0,10,0), "mm")), 
  rel_widths = c(0.5,0.5), labels=c('B','C'), label_x = c(0,0.12), label_y=1.05
) -> bot_row
cowplot::plot_grid(
  map +
    guides(fill = guide_colorbar(title.position = "top")) +
    theme(
      panel.border = element_blank(),
      legend.text = element_text(size=8),
      legend.title = element_text(size=9, hjust=0.5),
      legend.position = c(0.2,0.15),#c(0.825,0.9),
      legend.direction = "horizontal"
    ), 
  NULL,
  bot_row, 
  nrow=3, 
  rel_heights = c(0.7,-0.06,0.3), 
  labels=c('A'),
  label_y = 0.945
) -> fig_1

ggsave("charts/fig_2.pdf", fig_1, height = 6.5, width = 7.5)
knitr::plot_crop("charts/fig_2.pdf")

#Correlation matrix un-normalized measures

corr_data_m <- cor(data %>% select(M1, M2, M3, M4) %>% as.matrix())
corr_data_m<- round(corr_data_m,2)

colnames(corr_data_m) <- c(
  expression(M^1),
  expression(M^2),
  expression(M^3),
  expression(M^4)
)

rownames(corr_data_m) <- c(
  expression(M^1),
  expression(M^2),
  expression(M^3),
  expression(M^4)
)

corr_data_m %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  writeLines(.,"tables/corr_m_table_.tex")

