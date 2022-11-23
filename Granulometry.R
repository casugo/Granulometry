---
  title: "Granulometry Analysis"
author: "Catalina Suescun"
date: '2022-11-18'
output: word_document
---
  
 
knitr::opts_chunk$set(echo = TRUE)

# Load Libraries
library(tidyverse)
library(here)
library(kableExtra)
library(ggpubr)
library(rmarkdown)
library(usethis)

# Data Loading ----
## Loading the paths
files <- 
  here::here("Data","Analysis") %>%
  dir( recursive=TRUE, full.names=TRUE, pattern="\\.csv$")

## Creating the Nested dataframe ----
Granulometry <- files %>%  map( ~ read.csv(.))
Granulometry <- Granulometry %>%   set_names(files) %>% enframe("Size", "Datos")

## Organising the dataframe
Granulometry <- 
  Granulometry %>%  
  separate(Size,
           sep = "/",
           into = c(c(LETTERS[1:13]), c("Sample"))
  )            %>% 
  select(-c(LETTERS[1:13]), "Sample", "Datos")


## Adusting the Table
Granulometry <- 
  Granulometry %>%  
  separate(Sample, 
           sep = "_",
           into = c(c(LETTERS[1:2]))
  )
# Changing the names
colnames(Granulometry) <- c("Material", "Sample", "Datos")

# Unnesting the total dataframe for graphics
Granulometry <- Granulometry %>% unnest(Datos)

Granulometry <- 
  Granulometry %>%  
  separate(X.Area.Mean.StdDev.Min.Max.Perim..Median, 
           sep = ";",
           into = c(c(LETTERS[1:8]))
  ) %>% 
  select(-c("A","C","D","E","F","G","H"))

# Changing the names
colnames(Granulometry) <- c( "Material", "Sample", "Area")

#Transform Character columns in numbers
Granulometry <- transform(Granulometry, Area = as.numeric(Area))

#filtration of the data for size
Size1.5 <- Granulometry %>%
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

x = Size3$Area
h = hist (x,col =  "blue", xlab = "Area", ylab= "Number of particles",  main = "Granulometry 3mm")
xfit = seq(min(x), max(x), length= 20)
yfit = dnorm (xfit, mean = mean(x),sd = sd(x))
yfit = yfit*diff(h$mids [1:2])* length (x)
lines (xfit , yfit, col ="red", lwd = 3)
box()

#Ploting size 5
x = Size5$Area
h = hist (x,col =  "blue", xlab = "Area", ylab= "Number of particles", main = "Granulometry 5mm")
xfit = seq(min(x), max(x), length= 20)
yfit = dnorm (xfit, mean = mean(x),sd = sd(x))
yfit = yfit*diff(h$mids [1:2])* length (x)
lines (xfit , yfit, col ="red", lwd = 3)
box()
