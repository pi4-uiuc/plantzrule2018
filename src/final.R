#obtaining canopy cover data

library(traits)

variables <- betydb_query(table = 'search', key = readLines('~/.betykey'), betyurl = 'https://terraref.ncsa.illinois.edu/bety')
variables <- betydb_query(table = 'variables',
                          key = readLines('~/.betykey'),
                          betyurl = 'https://terraref.ncsa.illinois.edu/bety')

options(betydb_key = readLines('~/.betykey', warn = FALSE),
        betydb_url = "https://terraref.ncsa.illinois.edu/bety/",
        betydb_api_version = 'beta')

canopy_cover <- betydb_query(table = 'search',
                             trait = "canopy_cover",
                             site  = "~Season 6",
                             limit = 'none')

write.csv(canopy_cover, file = 'data/canopy_cover.csv')
dir.create('data')
dir.create('doc')
dir.create('bin')
dir.create('results')

##isolating the variables of date and mean and plotting them against each other

canopy_cover$date
t <- as.Date(canopy_cover$date, "%Y %b %d")
c <- canopy_cover$mean
plot(t, c, 'p', col = 'blue')

##restricting to (what we think is) one growing period

plot(t, c, 'p', 
     xlim = c((as.Date('2017 Apr 01', format = "%Y %b %d")),
              (as.Date('2017 Sep 01', format = "%Y %b %d"))),
     col = 'blue')

##restricting now to just one sitename

D<-subset(canopy_cover, sitename == "MAC Field Scanner Season 4 Range 12 Column 6")$date
C<-subset(canopy_cover, sitename == "MAC Field Scanner Season 4 Range 12 Column 6")$mean
D1<-as.Date(D, "%Y %b %d")
plot(D1,C,'p', col = 'orange')




##Identifying a cultivar
cult <- canopy_cover$cultivar
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}
Mode(cult)
length(which(cult=='700D_BMR'))

##Cultivar 700D_BMR seems like a good choice. Now attempting to isolate those data points
D700 <- subset(canopy_cover, cult == '700D_BMR')$date
C700 <- subset(canopy_cover, cult == '700D_BMR')$mean
D701<-as.Date(D700, "%Y %b %d")
plot(D701,C700,'p', col = 'green')

##Now seems like less of a good choice. 
#Maybe we can count the number of distinct dates for a given cultivar?
##I tried some methods that were ultimately ineffective, David recommended doing this instead:
canopy_cover %>% filter(!is.na(cult))
str(canopy_cover)
canopy_cover %>% filter(!is.na(cultivar))
library(dplyr)
canopy_cover %>% filter(!is.na(cultivar))
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(date, cultivar) %>% summarise(n = n())
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar) %>% summarise(n = n())
#canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar) %>% summarise(n = n()) %>% sort()
#canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar) %>% summarise(n = n()) %>% sort(n)
#?sort
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar) %>% summarise(n = n()) %>% arrange(n)
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar) %>% summarise(n = n()) %>% arrange(desc(n))
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar, site_id) %>% summarise(n = n()) %>% arrange(desc(n))
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar, site_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(100)
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar, site_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% print()
View(canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar, site_id) %>% summarise(n = n()) %>% arrange(desc(n)))
canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar, site_id) %>% summarise(n = n()) %>% arrange(desc(n))

##Big_Kahuna is among those with 47 distinct dates (the max number), so let's plot that!
dbk <- subset(canopy_cover, cult == 'Big_Kahuna')$date
cbk <- subset(canopy_cover, cult == 'Big_Kahuna')$mean
dbk1 <-as.Date(dbk, "%Y %b %d")
plot(dbk1,cbk,'p', col = 'purple')

##definitely better, still not quite what I was expecting, 
#so let's try a couple more with 47 distinct dates
d51 <- subset(canopy_cover, cult == 'PI152751')$date
c51 <- subset(canopy_cover, cult == 'PI152751')$mean
d51.1 <-as.Date(d51, "%Y %b %d")
plot(d51.1,c51,'p', col = 'yellow')

d46 <- subset(canopy_cover, cult == 'PI154846')$date
c46 <- subset(canopy_cover, cult == 'PI154846')$mean
d46.1 <-as.Date(d46, "%Y %b %d")
plot(d46.1,c46,'p', col = 'pink')

d85 <- subset(canopy_cover, cult == 'PI155885')$date
c85 <- subset(canopy_cover, cult == 'PI155885')$mean
d85.1 <-as.Date(d85, "%Y %b %d")
plot(d85.1,c85,'p', col = "#6600CC")

##that last one looks good! Cultivar is PI155885. 
#But guess and check is not an ideal method. 
#How many have 47 distinct dates?
sum((canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar, site_id) %>% summarise(n = n()) %>% arrange(desc(n))
)$n == 47)

##result was 36. In that case, I think we might as well graph all of them. 
#But doing them by hand is likewise not ideal. 
#I'd much rather have a function that generates the plots for me.

##install.packages("drc")

Maxdates <- ((canopy_cover %>% filter(!is.na(cultivar)) %>% group_by(cultivar, site_id) %>% summarise(n = n()) %>% arrange(desc(n))
)[1:36,])

library(drc)

for (x in unique(Maxdates$cultivar)){
  Dd <- subset(canopy_cover, cult == x)$date
  Cc <- subset(canopy_cover, cult == x)$mean
  Dd1 <-as.Date(Dd, "%Y %b %d")
  #plot(Dd1, Cc, xlab = 'Date', ylab = 'Canopy Cover Percentage'
  mL <- drm(Cc ~ Dd1, data = canopy_cover, fct = L.3(), type = "continuous")
  summary(mL)
  mL_coef <- round(coef(mL), 3)
  
  # saveing the plots  
  jpeg(paste(x, 'rplot.jpg'))
  #plot(Dd1,Cc,'p', col = "#6600CC")
  #plot(mL, xlab = 'Date', ylab = append('Canopy cover mean for cultivar',x), type = "all")
  plot(mL, xlab = 'Date', 
       ylab = 'Canopy Cover Percentage',
       main = paste0('Canopy cover mean for cultivar ',x , '\n k = ', mL_coef[1], '; m = ', mL_coef[2]),
       type = "all")
  dev.off()
  
  #mtext(bquote(y == (mL_coef[2])/(1 + e^(-(mL_coef[3])(x + (mL_coef[1])), adj=1, padj=0)
}



?mtext
?bquote
?drm

##we especially like the plot for cultivar PI330185, 
##so we're going to try to create a logistic model for that particular cultivar

d85 <- subset(canopy_cover, cult == 'PI330185')$date
c85 <- subset(canopy_cover, cult == 'PI330185')$mean
d85.1 <-as.Date(d85, "%Y %b %d")
plot(d85.1,c85,'p', col = "#6600CC")

##let's try to get more specific with date and mean
?plot
plot(d85.1,c85,'p', col = "#6600CC",)

