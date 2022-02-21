## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Seattle Aquarium long-term coral reef monitoring program  
## visualizations of multivariate analyses, time series, etc. 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(tidyverse)
library(egg)


## set your paths in a project folder 
input <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_input"
output <- "D:/OneDrive/Active_Projects/Hawaii_fish/data_output"
code <- "D:/OneDrive/Active_Projects/Hawaii_fish/code" 
fig <- "D:/OneDrive/Active_Projects/Hawaii_fish/figures"


## load a path
setwd(output)


## T1, T2, T3, T4
all.coords <- read.csv("all_coords.csv")
all.coords <- all.coords[-1]


## T1 only data
T1.coords <- read.csv("T1_coords.csv", header = TRUE)
T1.coords <- T1.coords[,-1]


## site averaged data
site.avg <- read.csv("site_avg_coords.csv", header = TRUE)
site.avg <- site.avg[,-1]
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data processing / filtering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## function to move last column to data[, 1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## remove the "19" and "20" from year dates for easier visualization, e.g., "1991" --> "91" 
short.date <- function(data){
  data$short.date <- gsub("20","", as.character(data$Date))
  data <- front.ofthe.line(data)
  return(data)
}


## function to create binary factor designating each sample unit as pre or post 2013 
pre.post <- function(data){
  data$PrePost <- ifelse(data$Date==2009 | data$Date==2010 | data$Date==2011 | data$Date==2012, "Pre", "Post")
  data <- front.ofthe.line(data)
  return(data)
}


## set location and Site as factors, and reorder for plotting and rename 
factor.f <- function(data){
  data$Location <- factor(data$Location, 
                          levels=c("Puako", "Old Kona", "Mahukona"),
                          labels=c("Puak\u014D", "Old Kona", "M\u0101hukona")) #Puak\u014D, #M\u0101hukona
  data$Site <- factor(data$Site, 
                      levels=c("1", "2", "5", 
                               "3", "4", 
                               "6", "7"))
  return(data)
}


## apply short.date() function
all.coords <- short.date(all.coords)
T1.coords <- short.date(T1.coords)


## apply pre.post() function to the three active data frames
T1.coords <- pre.post(T1.coords)


## run factor.f()
all.coords <- factor.f(all.coords)
T1.coords <- factor.f(T1.coords)
site.avg <- factor.f(site.avg)


## simplify naming conventions
all <- all.coords
T1 <- T1.coords
avg <- site.avg 


## remove old dfs
remove(all.coords, T1.coords, site.avg)
## END data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## filtering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filter.location <- function(data, location){filter(data, Location %in% c(location))}
filter.site <- function(data, site){filter(data, Site %in% c(site))}

OldKona.all <- filter.location(all, "Old Kona")
Puako.all <- filter.location(all, "Puak\u014D")
Mahukona.all <- filter.location(all, "M\u0101hukona")

OldKona.T1 <- filter.location(T1, "Old Kona")
Puako.T1 <- filter.location(T1, "Puak\u014D")
Mahukona.T1 <- filter.location(T1, "M\u0101hukona")

S1.all <- filter.site(all, "1")
S2.all <- filter.site(all, "2")
S3.all <- filter.site(all, "3")
S4.all <- filter.site(all, "4")
S5.all <- filter.site(all, "5")
S6.all <- filter.site(all, "6")
S7.all <- filter.site(all, "7")

S1.T1 <- filter.site(T1, "1")
S2.T1 <- filter.site(T1, "2")
S3.T1 <- filter.site(T1, "3")
S4.T1 <- filter.site(T1, "4")
S5.T1 <- filter.site(T1, "5")
S6.T1 <- filter.site(T1, "6")
S7.T1 <- filter.site(T1, "7")
## END filtering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## custom graphical parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 plot.title = element_text(size=14))

legend.theme = theme(legend.title = element_text(size=13), 
                     legend.text = element_text(size=13)) 

no.axes = theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size=13))

left = theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.title.y = element_text(size=14),
             axis.text.y = element_text(size=13))

bottom = theme(axis.title.x = element_blank(),
               axis.text.x = element_text(size=11.5),
               axis.title.y = element_blank(),
               axis.text.y = element_text(size=13))


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


## custom graphical params
txt.angle <- 45
x.lab <- theme(axis.text.x = element_text(angle = txt.angle, hjust=1)) 
x.breaks <- scale_x_continuous(breaks=seq(2009,2019,by=1))
no.leg <- theme(legend.position = "none")
no.x.title <- theme(axis.title.x = element_blank())
strip.text <- theme(strip.text.x = element_text(size=14))
no.strip <- theme(strip.background = element_blank(), strip.text.x = element_blank())
## END custom graphical params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






## create spp time series averaged by site and grouped by location ~~~~~~~~~~~~~
## uncomment one of the following to select a data frame for visualization 
dat <- all ## select this line to visualize T1, T2, T3, T4
#dat <- T1 ## select this line to visualize T1 only 


time.series <- function(df, spp, spp.title){
  t1 <- ggplot(dat, aes(Date, spp, fill=Site)) +
    geom_bar(stat="identity") + facet_grid(~Location) + scale_fill_manual(values=site.cols) +
    ggtitle(spp.title) + ylab("Mean abundance per site") + 
    my.theme + x.lab + x.breaks + no.leg
  return(t1)
}


## call function to create individual sub-figures of fig 4 
p1 <- time.series(dat, dat$Agile.chromis, "Agile chromis") + no.axes + no.x.title + strip.text
p2 <- time.series(dat, dat$Blackfin.chromis, "Blackfin chromis") + no.axes + strip.text
p3 <- time.series(dat, dat$Hawaiian.sergeant.major, "Hawaiian sergeant major") + no.x.title + no.strip + left
p4 <- time.series(dat, dat$Kole.tang, "Goldring tang") + no.axes + no.strip 
p5 <- time.series(dat, dat$Lavender.brown.tang, "Lavender brown tang") + no.axes + no.x.title + no.strip 
p6 <- time.series(dat, dat$Niger.trigger, "Niger trigger") + no.axes + no.strip
p7 <- time.series(dat, dat$Yellowstripe.goatfish, "Yellowstripe goatfish") + bottom + no.strip
p8 <- time.series(dat, dat$Yellow.tang, "Yellow tang") + bottom + no.strip


## visualize and save combined figure of all plots
graphics.off()
windows(13,11, record=T)

fig4 <- ggarrange(p1, p2, 
                  p3, p4, 
                  p5, p6, 
                  p7, p8, 
                  nrow=4)


## save using "cairo_pdf" 
setwd(fig)
#ggplot2::ggsave(filename = "Fig4.pdf", plot = fig4, device = cairo_pdf, 
#                width = 13, height = 11, units = "in") #dpi = 6000)
## END plots of spp time series ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize summary NMDS ordination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Location colors and other additional graphical params 
location.cols <- c("#106a90", #Puako; blue
                   "#3D8B37", #Old Kona; green 
                   "#882255")  #Mahukona; red 

pt.type <- 16
pt.size <- 1
outline <- "black"

x.lim <- xlim(-1.1, 1.6)
y.lim <- ylim(-1.25, 1.05)

labs.fig <- c("A", "B")
labs.f <- function(x){annotate("text", x = -0.975, y=0.95, label=x, size = 7)}
lab_A <- labs.f(labs.fig[1])
lab_B <- labs.f(labs.fig[2])
## END additional graphical params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot all three locations -- no additional information ~~~~~~~~~~~~~~~~~~~~~~~
plot.ord <- function(data, L1_O, L2_P, L3_M){
  p1 <- ggplot(data, aes(x = NMDS1, y = NMDS2)) + my.theme + legend.theme + coord_fixed() +
    geom_point(pch = pt.type, aes(color=Location), size=pt.size) + 
    scale_color_manual(values=location.cols) + ylab("NMDS Axis-2") + xlab("NMDS Axis-1") +
    stat_ellipse(data=L1_O, aes(x=NMDS1, y=NMDS2), col=location.cols[2]) +
    stat_ellipse(data=L2_P, aes(x=NMDS1, y=NMDS2), col=location.cols[1]) +
    stat_ellipse(data=L3_M, aes(x=NMDS1, y=NMDS2), col=location.cols[3]) + 
    theme(legend.position="none") + x.lim + y.lim
  return(p1)
}


## create plots
fig3a <- plot.ord(all, OldKona.all, Puako.all, Mahukona.all) + lab_A
fig3b <- plot.ord(T1, OldKona.T1, Puako.T1, Mahukona.T1) + bottom + lab_B


## combine fig1a and fig1b to create Fig. 3a,b
graphics.off()
windows(10, 4.5, record=T)

fig3 <- ggarrange(fig3a, fig3b, nrow=1)
## END Fig1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize sites by pre and post warming event ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## "Pre" are dates less than and up to 2012
## "Post" are dates greater than and equal too 2013 
warm.col <- c(
  "#B81324", # >= 2013; red
  "#3299CC"  # < 2013; blue
  )  

transect.cols <- c(
  "#982000", #purple; #T1
  "#f1b232", #orange; #T2
  "#127253", #blue; #T3
  "#0D4F8B"  #yallow; #T4
) 

my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title=element_text(size=16),
                 axis.text=element_blank(),
                 plot.title = element_text(size=16))

left = theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.title.y = element_text(size=14),
             axis.text.y = element_blank())

bottom = theme(axis.title.x = element_text(size=14),
               axis.text.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank())

no.axes = theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank())

## graphical params 
dot.size <- .75
gray.size <- 0.5
gray.col <- "gray"
text.size <- 2.5
line.width <- 0.25
pt.type <- 20
alpha.site <- .75
alpha.gray <- .5
title <- theme(plot.title = element_text(size=14))
ticks <- theme(axis.ticks = element_blank())
x.lim <- xlim(-1.1, 1.5)
y.lim <- ylim(-1.25, 1.05)

titles <- c("Site 1: Puak\u014D", 
            "Site 2: Puak\u014D", 
            "Site 5: Puak\u014D", 
            "Site 3: Old Kona", 
            "Site 4: Old Kona",
            "Site 6: M\u0101hukona", 
            "Site 7: M\u0101hukona")


## create custom labels for each subfigure
labs <- c("A", "B", "C", "D", "E", "F", "G", 
          "H", "I", "J", "K", "L", "M", "N")

## control label placement with x and y coords 
labs.f <- function(x){annotate("text", x = -0.975, y=0.95, label=x, size = 7)}

## create labels
lab_A <- labs.f(labs[1]); lab_B <- labs.f(labs[2]); lab_C <- labs.f(labs[3]); lab_D <- labs.f(labs[4])
lab_E <- labs.f(labs[5]); lab_F <- labs.f(labs[6]); lab_G <- labs.f(labs[7]); lab_H <- labs.f(labs[8])
lab_I <- labs.f(labs[9]); lab_J <- labs.f(labs[10]); lab_K <- labs.f(labs[11]); lab_L <- labs.f(labs[12])
lab_M <- labs.f(labs[13]); lab_N <- labs.f(labs[14]); 
## END custom graphing params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.transects <- function(data, title){
  p1 <- ggplot(data=data, aes(x=NMDS1, y=NMDS2, label=Date)) + geom_point(color=gray.col, size=gray.size, pch=pt.type, alpha=alpha.gray) + my.theme +
    geom_point(aes(x=NMDS1, y=NMDS2), color=gray.col, size=gray.size, pch=pt.type, alpha=alpha.gray) +
    geom_point(aes(x=NMDS1, y=NMDS2, color=Transect), pch=pt.type, alpha=alpha.site, size=dot.size) +
    geom_path(aes(x=NMDS1, y=NMDS2, group=Key, color=Transect), size=line.width) + 
    coord_fixed() + ggtitle(title) + scale_color_manual(values=transect.cols, labels=c("Transect 1", "Transect 2", "Transect 3", "Transect 4")) +
    guides(color = "none") + ticks + x.lim + y.lim + xlab("NMS Axis-1") + ylab("NMS Axis-2")
  return(p1)
}


plot.PrePost.all <- function(data, title){
  p1 <- ggplot(data, aes(x=NMDS1, y=NMDS2)) + 
    geom_point(aes(color=PrePost), pch=pt.type, size=dot.size) +
    geom_path(aes(x=NMDS1, y=NMDS2, color=PrePost, group=Key), size=line.width) +
    coord_fixed() + ggtitle(title) + scale_color_manual(values=warm.col, labels = c('Post 2013', 'Pre 2013')) +
    xlab("NMS Axis-1") + ylab("NMS Axis-2") + guides(color = "none") + ticks + my.theme + x.lim + y.lim 
  return(p1)
}


plot.PrePost <- function(data){#, title){
  p1 <- ggplot(data, aes(x=NMDS1, y=NMDS2)) + 
    geom_point(aes(color=PrePost), pch=pt.type, size=dot.size) +
    geom_path(aes(x=NMDS1, y=NMDS2, color=PrePost, group=Key), size=line.width) +
    geom_text(aes(x=NMDS1, y=NMDS2, label=short.date), size=text.size) +
    scale_color_manual(values=warm.col, labels = c('Post 2013', 'Pre 2013')) +
    guides(color = "none") + ticks + my.theme + x.lim + y.lim + coord_fixed() + 
    xlab("NMS Axis-1") + ylab("NMS Axis-2")
  return(p1)
}
## END graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## print figs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## print PrePost figs with all transects
all.1 <- plot.PrePost.all(S1.all, titles[1]) + left + lab_A
all.2 <- plot.PrePost.all(S2.all, titles[2]) + no.axes + lab_B
all.5 <- plot.PrePost.all(S5.all, titles[3]) + no.axes + lab_C
all.3 <- plot.PrePost.all(S3.all, titles[4]) + no.axes + lab_D
all.4 <- plot.PrePost.all(S4.all, titles[5]) + no.axes + lab_E
all.6 <- plot.PrePost.all(S6.all, titles[6]) + no.axes + lab_F
all.7 <- plot.PrePost.all(S7.all, titles[7]) + no.axes + lab_G


## create PrePost fig with T1 only 
T1.1 <- plot.PrePost(S1.T1) + no.axes + lab_H
T1.2 <- plot.PrePost(S2.T1) + no.axes + lab_I
T1.5 <- plot.PrePost(S5.T1) + no.axes + lab_J
T1.3 <- plot.PrePost(S3.T1) + bottom + lab_K
T1.4 <- plot.PrePost(S4.T1) + no.axes + lab_L
T1.6 <- plot.PrePost(S6.T1) + no.axes + lab_M
T1.7 <- plot.PrePost(S7.T1) + no.axes + lab_N


## print figure 
graphics.off()
windows(19,5.5,record=T)

fig5 <- ggarrange(all.1, all.2, all.5, all.3, all.4, all.6, all.7,
                  T1.1, T1.2, T1.5, T1.3, T1.4, T1.6, T1.7, 
                  nrow=2)


## create fig with all transects  
t.1 <- plot.transects(S1.all, titles[1]) + lab_A + left 
t.2 <- plot.transects(S2.all, titles[2]) + lab_B + no.axes
t.3 <- plot.transects(S5.all, titles[3]) + lab_C + no.axes
t.4 <- plot.transects(S3.all, titles[4]) + lab_D + bottom 
t.5 <- plot.transects(S4.all, titles[5]) + lab_E + no.axes
t.6 <- plot.transects(S6.all, titles[6]) + lab_F + no.axes
t.7 <- plot.transects(S7.all, titles[7]) + lab_G + no.axes


graphics.off()
windows(19,3,record=T)

figS2 <- ggarrange(t.1, t.2, t.3, t.4, t.5, t.6, t.7, nrow=1)
## END fig creation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## save figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#setwd(figLocation)
#ggplot2::ggsave(filename = "Fig5.pdf", plot = fig_PrePost, device=cairo_pdf, 
#                width = 12, height = 10, units = "in")
## END saving ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






## FigS2: all 81 species ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graphics.off()
windows(18, 10, record=T)

allSpp <- ggplot(gather(all, key="Spp", value=avgDensity, 10:90), aes(x=Date, fill=Site)) +
  geom_bar(stat="identity", aes(y=avgDensity)) + scale_fill_manual(values=site.cols) +
  facet_wrap(~Spp, scales="free_y") + ylab("mean abundance per Site") +
  my.theme + x.breaks + x.lab + no.x.title + legend.theme

print(allSpp)
## END figure with all 81 species ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
