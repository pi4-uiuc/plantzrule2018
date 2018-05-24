##obtaining canopy cover data

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
