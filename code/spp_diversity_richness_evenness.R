## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Seattle Aquarium long-term coral reef monitoring program  
## visualizations of multivariate analyses, time series, etc. 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(tidyverse)
library(vegan)


## set your paths in a project folder 
input <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_input"
output <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_output"
code <- "D:/OneDrive/Active_Projects/Hawaii_fish/code" 
fig <- "D:/OneDrive/Active_Projects/Hawaii_fish/figures"


## load a path
setwd(output)


## T1, T2, T3, T4
dat <- read.csv("all_coords.csv")
dat <- dat[-1]
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data management ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## function to convert columns to factors
col.all <- c("Key", "PrePost", "Date", "Site", "Location", "Transect")

convert.factor <- function(data, col_X){
  data[col_X] <- lapply(data[col_X], as.factor)
  return(data)
}

## make the conversion 
dat <- convert.factor(dat, col.all)


## subset by spp and metadata
comm <- dat[, -c(1:8)]
info <- dat[, c(1:8)]
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## spp diversity metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Shannon index
H <- diversity(comm)


## Pielou's evenness
J <- H/log10(specnumber(comm))


## function to calculate spp richness 
spp.richness <- function(x){length(x[x>0])}


## apply function across column 
richness <- as.data.frame(apply(comm, 1, spp.richness))

## calculation sum total observations per row (per transect)
total.obs <- as.data.frame(apply(comm, 1, sum))


## bind and remanme columns
diversity <- cbind(info, H, J, richness, total.obs)
names(diversity)[9:12]<-c("shannonDiversity","pielousEvenness","richness","totalObs")


## as.factor for plotting
diversity$Location <- factor(diversity$Location,
                             levels=c("Puako", "Old Kona", "Mahukona"),
                             labels=c("Puak\u014D", "Old Kona", "M\u0101hukona"))

diversity$Site <- factor(diversity$Site, levels=c('1', '2', '5',
                                                  '3', '4',
                                                  '6', '7'))
## END data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate species accumulation (over transects) curves ~~~~~~~~~~~~~~~~~~~~~~
## group by Site, select the spp columns, apply specaccum to all groups (sites)
curves <- dat %>% 
  group_by(Site) %>% 
  select(-c(1:8)) %>%
  group_map(~specaccum(., method="random", permutations = 1000))


## write a function to extract SU [3] richness [4] and sd [5]
extract <- function(x) {matrix(unlist(c(x[3], x[4], x[5])), ncol=3)}


## apply function to extract information from list of lists
list.of.lists <- lapply(curves, extract)


## function to calculate length of lists (use to reassign site names) 
row.length <- function(x) {
  out <- vector("double", length=7)
  for (i in 1:7){
    out[[i]] <- nrow(x[[i]])
  }
  out
}


## apply function to list data
out.list <- row.length(list.of.lists)


## unlist into dataframe
df <- as.data.frame(do.call(rbind, lapply(list.of.lists, as.data.frame)))


## site and location names to bind to new df
site.list <- c("Site1","Site2","Site3","Site4","Site5","Site6","Site7")
loc.list <- c("Puako", "Puako", "Old Kona", "Old Kona", "Puako", "Mahukona", "Mahukona")


## add site and location labels using out.list; rename first three columns 
df$site <- rep(site.list, out.list)
df$location <- rep(loc.list, out.list)
names(df)[1:3] <- c("SU", "richness", "sd")
all.sites <- df


## as.factor for plotting 
all.sites$location <- factor(all.sites$location,
                             levels=c("Puako", "Old Kona", "Mahukona"),
                             labels=c("Puak\u014D", "Old Kona", "M\u0101hukona"))

all.sites$site <- factor(all.sites$site, levels=c('Site1', 'Site2', 'Site5',
                                                  'Site3', 'Site4',
                                                  'Site6', 'Site7'))
## END species curve calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## species - area curves ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calculate rarefaction curves -- spp sampled per individuals observed
graphics.off()
windows(6,6,record=T)


rare <- rarecurve(comm)


## function to flatten list of lists                 
flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}


## call flattenlist within map
rare.curves <- map(rare, flattenlist) %>% bind_rows()


## bind with site info 
## remove NMDS information from info df as
info <- info[, -c(1:2)]
rare.curves <- cbind(info, rare.curves)


## pivot from wide to long form and prep for plotting
rare.curves <- pivot_longer(rare.curves, starts_with("N"), names_to="obs", values_drop_na=FALSE)
rare.curves$obs <- gsub("N", "", as.character(rare.curves$obs))
rare.curves$obs <- as.numeric(rare.curves$obs)


## as.factor for plotting 
rare.curves$Location <- factor(rare.curves$Location, 
                               levels=c('Puako', 'Old Kona', 'Mahukona'),
                               labels=c("Puak\u014D", "Old Kona", "M\u0101hukona"))


rare.curves$Site <- factor(rare.curves$Site, levels=c('1', '2', '5', '3', '4', '6', '7'))
## END species - area curves ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## custom graphing options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## specify hex codes for custom colors
site.cols <- c(
  "#6497b1",  #gray; site 1
  "#008080",  #teal; site 2
  "#03396c",  #dark blue; site 5
  "#7BBF6A",  #light green; site 3
  "#3D8B37",  #dark green; site 4
  "#CC6677",  #light maroon; site 6 
  "#882255"   #maroon; site 7
)


line.width <- 0.3
alph <- 0.5
pt.size <- 1
line.size <- 0.75
alph2 <- 0.7

my.fill <- scale_fill_manual(values=site.cols)
my.col <- scale_color_manual(values=site.cols)
no.leg <- theme(legend.position="none")
no.x <- theme(axis.text.x = element_blank())
x.lab <- theme(axis.text.x = element_text(angle = 45, hjust=1)) 


## set up custom ggplot theme 
my.theme = theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title.x=element_text(size=13),
                 axis.title.y=element_text(size=13),
                 axis.text=element_text(size=12),
                 plot.title = element_text(size=13))

custom.theme <- theme(axis.title.x = element_blank(),
                      legend.text = element_text(size=13),
                      legend.title = element_text(size=13),
                      strip.text.x = element_text(size=13)) 

no.ribbon <- theme(strip.background = element_blank(), 
                   strip.text.x = element_blank())
## END custom graphing options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## create species-richness and species-area curves ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fig2a <- ggplot(diversity, aes(Date, totalObs)) +
  geom_point(aes(group=Site,color=Site), size=pt.size, alpha=alph2) + 
  geom_smooth(aes(group=Site, color=Site), size=line.size) + facet_wrap(~Location) +
  my.theme + my.col + custom.theme + no.x + ylab("Total individuals") 

fig2b <- ggplot(diversity, aes(Date, richness)) +
  geom_point(aes(group=Site,color=Site), size=pt.size, alpha=alph2) + 
  geom_smooth(aes(group=Site, color=Site), size=line.size) + facet_wrap(~Location) +
  my.theme + my.col + custom.theme + x.lab + ylab("Total individuals") + no.ribbon + no.leg

fig2c <- ggplot(all.sites, aes(SU, richness, group=site)) +
  geom_ribbon(aes(y=richness, ymin=richness+sd, ymax=richness-sd, group=site, fill=site), alpha=alph) +
  geom_path(color="black") + facet_wrap(~location) +
  xlab("Transects surveyed") + ylab("Species richness") + my.theme + my.fill + no.ribbon

fig2d <- ggplot(rare.curves, aes(obs, value, group=Site, color=Site)) +
  geom_path(aes(group=Key), size=line.width, alpha=alph) + facet_wrap(~Location, nrow=1) + xlim(0, 1136) +
  xlab("Individuals observed") + ylab("Species richness") + my.theme + my.col + no.leg + no.ribbon
## END plot creation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize and save plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(egg)
graphics.off()
windows(16,16,record=T)


## create full plot 
fig2 <- ggarrange(fig2a, fig2b, fig2c, fig2d, nrow=4)


## save plot 
setwd(fig)
ggplot2::ggsave(filename = "fig2.pdf", plot = fig2, device=cairo_pdf, 
                width = 16, height = 16, units = "in")
## END plot visualization and save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
