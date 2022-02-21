## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Seattle Aquarium long-term coral reef monitoring program  
## multivariate analyses community structure -- Hawai'i fish data 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(MASS)
library(fitdistrplus)
library(gridExtra)
library(grid)
library(gtable)
library(ggpubr)
library(scales)
library(vegan)
library(reshape2)
library(data.table)
library(tidyverse)


## set your paths in a project folder 
input <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_input"
output <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_output"
code <- "D:/OneDrive/Active_Projects/Hawaii_fish/code" 
fig <- "D:/OneDrive/Active_Projects/Hawaii_fish/figures"


## load a path
setwd(input)
dat <- read.csv("Hawaii_fish.csv", header = TRUE, stringsAsFactors = FALSE)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data processing: deal with rare or unobserved species ~~~~~~~~~~~~~~~~~~~~~~~
## remove rows with NA, i.e., transects that aren't surveyed 
dat <- na.omit(dat)


## calculate the percentage of transects that where species have 0 observations
## 1 = spp is unobserved across 100% of transects; 
## 0 = spp is unobserved across 0% of transects (i.e. is seen every time)
percent <- lapply(dat, function(x){length(which(x==0))/length(x)})


## bind percent to dat for further processing
## set location, site, transect, date columns to 0 so that filtering does not impact them
## set the percent cutoff for rare species; 0.98 retains species that were observed 
## along 0.02 (2%) or transects or more (cutOff)
dat <- rbind(dat, percent)
last.row <- nrow(dat)
cut.off <- 0.98


## filter data by rarity (cut.off), refer
rarity.f <- function(data){
  remove <- data[, c(data[last.row, ] > cut.off)] ## spp to remove 
  remain <- data[, c(data[last.row, ] <= cut.off)] ## spp to retain 
  return(list(remove, remain))
}


## complete calculations and extract data
out.list <- rarity.f(dat)
remove <- out.list[[1]]
remain <- out.list[[2]]


## create a data frame with frequency of occurence for all spp 
list.frequency <- function(data){
  t1 <- as.data.frame(unlist(data))
  t2 <- tibble::rownames_to_column(t1, "spp.names")
  t3 <- t2[-c(1:4), ]
  names(t3)[2]="percent_0"
  return(t3)
}


## run this function to retain a data frame with percent occurrence for all spp 
frequency.occurence <- list.frequency(percent)
#write.csv(frequency.occurence, 'frequency_occurence.csv')


## remove the last row of a dataframe
rm.row <- function(data){
  last.row <- nrow(data)
  data <- data[-c(last.row), ]
  return(data)
}


## apply function
remove <- rm.row(remove)
remain <- rm.row(remain)
dat <- rm.row(dat)


## clear out "old" dataframes and environmental definitions
remove(out.list, percent)
## END shortened labels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data cleaning: remove select sites & years ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## function to remove site 8 data, and site 5 2017 data
clean.data <- function(data){
  data$Location <- as.factor(data$Location)
  data$Transect <- as.factor(data$Transect)
  data <- filter(data, !Site %in% c(8))
  data <- data %>% group_by(Site, Date) %>% filter(!(Site==5 & Date==2017))
  return(data)
}


## pick a data stream to clean: remain, remove, or dat
dat.all <- clean.data(remain)


## if desired, filter by one or more transects
## produce data frame with a subset of T 
dat.T1 <- filter(dat.all, Transect %in% c("T1"))


## create a dataframe with transects averaged to the site level
dat.avg <- dat.all %>%
  group_by(Location, Site, Date) %>%
  summarise(across(
    .cols = -c(1),
    .fns = list(avg=mean),
    .names = "{col}"))


## function to place the last column [, ncol] in the first column position i.e. [, 1]
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## function to create binary factor designating each sample unit as pre or post 2013 
pre.post <- function(data){
  data$PrePost <- ifelse(data$Date==2009 | data$Date==2010 | data$Date==2011 | data$Date==2012, "Pre", "Post")
  data <- front.ofthe.line(data)
  return(data)
}


## apply function to the three active data frames
dat.all <- pre.post(dat.all)
dat.T1 <- pre.post(dat.T1)
dat.avg <- pre.post(dat.avg)


## create a unique site-transect identifying (for data with multiple transects)
create.key <- function(data){
  data$Key <- data$Site
  data$Key <- with(data, paste0(Key, Transect))
  data <- front.ofthe.line(data)
  return(data)
}


## create unique identifyer for data w/ multiple transects
dat.all <- create.key(dat.all)



## remove the "19" and "20" from year dates for easier visualization, e.g., "1991" --> "91" 
short.date <- function(data){
  data$short.date <- gsub("20","", as.character(data$Date))
  data <- front.ofthe.line(data)
  return(data)
}


## apply function to create column with short.date
dat.all <- short.date(dat.all)
dat.T1 <- short.date(dat.T1)
dat.avg <- short.date(dat.avg)


## clear out old dataframes
remove(dat, remain, remove, frequency.occurence, 
       last.row, cut.off)
## END data cleaning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## split up site information and community data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## simplify naming
all <- dat.all ## this contains ALL data i.e. T1, T2, T3, T4 
T1 <- dat.T1 ## this contains only T1
avg <- dat.avg ## this contains all transects averaged to the site level 


## clean up environment
remove(dat.all, dat.T1, dat.avg)


## split up site and community data; 
## X.info contains metadata; 
## X.spp contains community matrix
all.info <- all[, c(1:7)] 
all.spp <- all[, -c(1:7)]
T1.info <- T1[, c(1:6)]
T1.spp <- T1[, -c(1:6)]
avg.info <- avg[, c(1:5)]
avg.spp <- avg[, -c(1:5)]
  

## log base10 transformation (uncomment the below line, if desired)
log.transform <- function(data){
  num.col <- ncol(data)
  out <- log10(data[,1:num.col]+1)
  return(out)
}


## perform transformation on desired df: all.spp, T1.spp, avg.spp
#test <- log.transform(all.spp) 


## define different cols for convert.factor function 
col.all <- c("Key", "PrePost", "Date", "Site", "Location", "Transect")
col.T1 <- c("PrePost", "Date", "Site", "Location", "Transect")
col.avg <- c("PrePost", "Location", "Site", "Date")


## function to convert columns to factors
convert.factor <- function(data, col_X){
  data[col_X] <- lapply(data[col_X], as.factor)
  return(data)
}


## make the conversion 
all.info <- convert.factor(all.info, col.all)
T1.info <- convert.factor(T1.info, col.T1)
avg.info <- convert.factor(avg.info, col.avg)


## clear environment
remove(col.all, col.T1, col.avg)
## data processing: community matrix and transformations ~~~~~~~~~~~~~~~~~~~~~~~





## multivariate analyis: NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## comm = all.spp for T1, T2, T3, T4
## comm = T1.spp for T1 (or however else T1 is defined)
## comm = avg.spp for average(T1, T2, T3, T4)

## perform NMDS 
ord <- metaMDS(comm = avg.spp, distance="bray", k=2, min = 1000, trymax=2000, 
               autotransform = F, wascores = TRUE)


## save a new ordination 
setwd(output)
save(ord, file = "ord_avg.rda")


## work with ordination: stress, NMDS coords 
setwd(output)
load("ord_T1.rda")


## visualize stress, check ordination, xy coordinates 
## open graphics window
graphics.off()
windows(6,6,record=T)


## plot
plot(ord_All)
stressplot(ord_All)


## NMDS ordination coordinates saved as data frame
save.coords <- function(ord, info, spp){
  t1 <- as.data.frame(scores(ord))
  t2 <- cbind(t1, info, spp)
  return(t2)
}


## bind nmds coordinates to dataframe with site info and spp counts
all.coords <- save.coords(ord, avg.info, avg.spp)


## save final output as CSV files for further analysis / visualization  ~~~~~~~~
setwd(output)
write.csv(all.coords,'site.avg.csv')
## END save / load of final CSV output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
