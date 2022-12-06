# Data Analysis of Granulometry for Catalina
## Date: 06/12/2022

## Load Libraries -----
library(tidyverse)
library(here)
library(kableExtra)
library(ggpubr)
library(rmarkdown)
library(usethis)


## Data Loading ----
### Loading the paths ----
files <- 
  here::here("Data","Analysis") %>%
  dir( recursive=TRUE, full.names=TRUE, pattern="\\.csv$")

### Function to identify the names of the data ----
names <- 
  here::here("Data","Analysis") %>%
  dir( recursive=TRUE, pattern="\\.csv$")


### Creating the Nested dataframe ----
Granulometry <- files %>%  map( ~ read.csv2(.))
Granulometry <- Granulometry %>%   set_names(names) %>% enframe("Size", "Datos")

### Organising the dataframe ----
Granulometry <- 
  Granulometry %>%  
  separate(Size,
           sep = "/",
           into = c("Granulometry", "File")
  )
  

### Unnesting the total dataframe for graphics ----
Granulometry <- Granulometry %>% unnest(Datos)

### Transforming type data to numeric -----
Granulometry$Area <- as.numeric(Granulometry$Area)

## Additinoal database for the Mean values for each type of Granulometry ----
data_mean <- 
  Granulometry %>% 
  group_by(Granulometry) %>% 
  summarise(mean = mean(Area),
            st = sd(Area))

### Filtering and reducing digits ----
data_mean <- data_mean %>% filter(Granulometry != "+5mm" )
data_mean$mean=round(data_mean$mean, 2)
data_mean$st=round(data_mean$st, 1)


# Doing the Graphics -----

### Making the Histographs ----

Granulometry %>% filter(Granulometry != "+5mm" ) %>% 
  ggplot(aes( Area )) + 
  geom_histogram(binwidth=0.2, color="black", fill="lightblue") +
  facet_wrap( ~ Granulometry , ncol=1) + 
  geom_text(data = data_mean   , 
            aes(x = c(4, 6, 10), y = c(7,7,7)), 
            label = paste( data_mean$mean, data_mean$st, sep = " \u00B1 "), 
            vjust = 1, 
            hjust=0) + 
  geom_vline(data = data_mean,
             aes(xintercept = mean, group = Granulometry),
             color="blue", linetype="dashed", size=1) + 
  scale_x_continuous(breaks = c(0, 5, 10, 15),
                      limits = c(0,15))  +
  #coord_cartesian(xlim = c(10,18), ylim=c(0,8)) +
  theme_minimal(base_size = 15, base_family = "Palatino") +
  labs(title="Granulometry analysis of the feedstock material",  
       subtitle = "Using 3 types of grid" ,
       y="Frequency", x="Pellet area  [mm2]" ) +
  theme(plot.background = element_rect(fill = "white"))

### Saving the Figure

ggsave( here("Figures/Granulometry.png") , width = 6, height = 8, dpi="print" )



#filtration of the data for size
Size1.5 <- 
  Granulometry %>%
  filter(Sample=="1.5mm.csv")

Size3 <- Granulometry %>%
  filter(Sample=="3mm.csv")

Size5 <- Granulometry %>%
  filter(Sample=="5mm.csv")

#Ploting size 1.5
S1.5 = Size1.5$Area
h = hist (S1.5,col =  "blue", xlab = "Area", ylab= "Number of particles",  main = "Granulometry 1.5mm")
xfit = seq(min(S1.5), max(S1.5), length= 20)
yfit = dnorm (xfit, mean = mean(S1.5),sd = sd(S1.5))
yfit = yfit*diff(h$mids [1:2])* length (S1.5)
lines (xfit , yfit, col ="red", lwd = 2)
box()

#Ploting size 3

S3 = Size3$Area
h = hist (x,col =  "blue", xlab = "Area", ylab= "Number of particles",  main = "Granulometry 3mm")
xfit = seq(min(x), max(x), length= 20)
yfit = dnorm (xfit, mean = mean(x),sd = sd(x))
yfit = yfit*diff(h$mids [1:2])* length (x)
lines (xfit , yfit, col ="red", lwd = 3)
box()

#Ploting size 5
S5 = Size5$Area
h = hist (x,col =  "blue", xlab = "Area", ylab= "Number of particles", main = "Granulometry 5mm")
xfit = seq(min(x), max(x), length= 20)
yfit = dnorm (xfit, mean = mean(x),sd = sd(x))
yfit = yfit*diff(h$mids [1:2])* length (x)
lines (xfit , yfit, col ="red", lwd = 3)
box()


