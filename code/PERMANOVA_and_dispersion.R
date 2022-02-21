## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Seattle Aquarium long-term coral reef monitoring program  
## PERMANOVA and analysis of dispersion
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(vegan)
library(tidyverse)


## set your paths in a project folder 
input <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_input"
output <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_output"
code <- "D:/OneDrive/Active_Projects/Hawaii_fish/code" 
fig <- "D:/OneDrive/Active_Projects/Hawaii_fish/figures"


## load a path
setwd(output)


## load data (T1, T2, T3, T4)
dat <- read.csv("all_coords.csv")
dat <- dat[-1]
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data conversion / filtering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## define different cols for convert.factor function 
col.all <- c("Key", "PrePost", "Date", "Site", "Location", "Transect")


## function to convert columns to factors
convert.factor <- function(data, col_X){
  data[col_X] <- lapply(data[col_X], as.factor)
  return(data)
}


## make the conversion 
dat <- convert.factor(dat, col.all)


## filter individual locations
filter.location <- function(x){filter(dat, Location %in% c(x))}
Puako <- filter.location("Puako")
OldKona <- filter.location("Old Kona")
Mahukona <- filter.location("Mahukona")


## create dataframes with spp only 
dat_spp <- dat[, -c(1:8)]  
Puako_spp <- Puako[, -c(1:8)]
OldKona_spp <- OldKona[, -c(1:8)]
Mahukona_spp <- Mahukona[, -c(1:8)]
## END data filtering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## PERMANOVA analyses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
perm.all <- function(data, spp){
  t1 <- how(nperm=10000)
  setBlocks(t1) <- with(data, Location)
  permanova.1 <- adonis(spp ~ Location, 
                        by="terms", data=data, methods="bray", perm=10000)
  return(permanova.1)
}


## create function calling model and PERMANOVA function 
perm.location <- function(data, spp){
  t1 <- adonis(spp ~ Site + Site/Key + Site/PrePost, 
               by="terms", data=data, methods="bray", perm=10000)
  return(t1)
}



## run PERMANOVA analysis at the location level
p1 <- perm.all(dat, dat_spp)
p2 <- perm.location(Puako, Puako_spp)
p3 <- perm.location(Mahukona, Mahukona_spp)
p4 <- perm.location(OldKona, OldKona_spp)
## END PERMANOVA analyses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## Homogeneity of dispersion analyses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## function to calculate homogeneity of dispersion 
disper.f <- function(grouping, spp){
  group_location <- grouping
  dist <- vegdist(spp, method="bray")
  disp <- betadisper(dist, group=grouping, type="centroid")
  t1 <- anova(disp)
  return(t1)
}


## calculate homogeneity of dispersion
d1 <- disper.f(dat$Location, dat_spp)
d2 <- disper.f(Puako$PrePost, Puako_spp)
d3 <- disper.f(Mahukona$PrePost, Mahukona_spp)
d4 <- disper.f(OldKona$PrePost, OldKona_spp)
## END homogeneity of dispersion ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
