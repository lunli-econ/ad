## Goal: Check if time trends are caused by coverage issues.
## Author: Lun Li
## Date Created: 11/09/2018

# Set up
packages <- c("reshape", "lubridate", "data.table", "plyr", "zoo", "bit64",
              "ggplot2", "xtable", "foreign", "dplyr", "foreach","doParallel",
              "readstata13", "stringr","plm", "blsAPI", "rjson")
lapply(packages, require, character.only = TRUE)

# If running at home
# path = "/Users/lunl/Dropbox/Research/2. Ad Intel/Github Code/ad/"
# If running on cluster 
path = "/home/lunl/Research/Advertisement/"
# If running in office
#path = "/home/lunl/Dropbox/Research/2. Ad Intel/Github Code/ad/"
setwd(path)

mediatypeID <- fread("Reference/Ref_data/MediaTypeID.csv")
names(mediatypeID)[2] <- "MediaType"
industryName <- fread("Reference/Ref_data/industryName.csv")

# - What's the total number of brands, ads, unique ads included each year?
# - What's the total "impression"" each year? 
# - How does "average cost per ad" and "average cost per person" change?
# - Within a fixed subset of firms that have positive ad expenditure in internet ads from 10-15, do we still observe the same trends? 
# - Conditional on a firm has positive expenditure on internet ads this year, what's the probability that it has lower expenditure on ads next year?


## Internet
# Read file
filelist <- paste0("Data/InternetSummary", 2010:2015, ".csv")
summary <- lapply(filelist, fread) %>% rbindlist()
internet <- summary[order(industry, MediaTypeID, ym)][ym>=2010]
internet[, ":="(ym = as.yearmon(ym), date = as.Date(as.yearmon(ym)), 
                year = year(as.yearmon(ym)))]

## 1 - What's the total number of brands, ads, unique ads included each year?
##   - What's the total "impression"" each year? 

dat.1 <- internet[,.(count = sum(count), exp = sum(exp), brands = sum(brands),
                     nunique.ads = sum(nunique.ads), total.imp = sum(total.imp)), by = .(year)]

## 2 - How does "average cost per ad" and "average cost per person" change?
###    a. Total average
dat.2 <- dat.1[,.(cost.occur = exp/count,
                  cost.brand = exp/brands,
                  cost.ad = exp/nunique.ads,
                  cost.imp = exp/total.imp), by = .(year)]

dat.2 <- melt(dat.2, id.vars = "year")
ggplot(dat.2, aes(year, value)) + geom_line() + facet_wrap(~variable, scales = "free_y")+
  xlab("") + ylab("")

###    b. By industry average
dat.3 <- internet[,.(count = sum(count), exp = sum(exp), brands = sum(brands),
                     nunique.ads = sum(nunique.ads), total.imp = sum(total.imp)), by = .(year,industry)]
dat.3 <- dat.3[,.(cost.occur = exp/count,
                  cost.brand = exp/brands,
                  cost.ad = exp/nunique.ads,
                  cost.imp = exp/total.imp), by = .(year,industry)]
dat.4 <- melt(dat.3, id.vars = c("year", "industry"))
ggplot(dat.4, aes(year, value, group = industry)) + geom_line(aes(color = industry)) + facet_wrap(~variable, scales = "free_y")+
  xlab("") + ylab("")

## 3 - Within a fixed subset of firms that have positive ad expenditure in internet ads from 10-15, do we still observe the same trends? 
##   - Conditional on a firm has positive expenditure on internet ads this year, what's the probability that it has lower expenditure on ads next year?

# This is slightly more complicated. Need to go back to 1_Summary.R
# Need to add a line that selects same set of advertisers and firms.

# Save all microdata to one file
InternetMicro <- function(year, output = NA){
  
  Occur <- paste0("/project/databases/nielsen-adintel/nielsen_extracts/AdIntel/", year,"/Occurrences/Internet.tsv")
  Brand <- paste0("/project/databases/nielsen-adintel/nielsen_extracts/AdIntel/", year, "/References/Brand.tsv")
  Distributor <- paste0("/project/databases/nielsen-adintel/nielsen_extracts/AdIntel/", year, "/References/Distributor.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  distributor <- fread(Distributor)
  
  occur <- occur[!is.na(Spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimeBrandCode", "Spend", "AdCode",
                     "Imp2Plus", "DistributorCode"), with = F]
  
  occur <- merge(occur, brand, by.x = "PrimeBrandCode", by.y = "BrandCode")
  occur <- merge(occur, distributor, by.x = c("DistributorCode", "MediaTypeID", "MarketCode"),
                  by.y = c("DistributorCode", "MediaTypeId", "MarketCode"))
  occur[, Imp2Plus:=as.integer64(Imp2Plus)]
  
  return(occur)
}

# This does not take very long
internetmicro <- lapply(2010:2015, InternetMicro)
datamicro <- rbindlist(internetmicro)

# Find:
# Fixed set of firms and distributors that are in the dataset every year.
datamicro[, year:= year(as.Date(AdDate))]

unique.firm = unique(datamicro$PrimeBrandCode)
unique.dist = unique(datamicro$DistributorCode)

for (y in 2010:2015){
  unique.firm.y = unique(datamicro[year==y]$PrimeBrandCode)
  unique.dist.y = unique(datamicro[year==y]$DistributorCode)
  
  unique.firm = intersect(unique.firm, unique.firm.y)
  unique.dist = intersect(unique.dist, unique.dist.y)
  
  message(paste0(y, " ", length(unique.firm)), " Firms, ", length(unique.dist), " Distributors")
}

datasmall <- datamicro[PrimeBrandCode %in% unique.firm & DistributorCode %in% unique.dist]
datasmall <- datasmall[year!=2009]
datasmall[, industry := substr(PCCSubCode, 1,2)]
fwrite(datasmall, "/scratch/midway2/lunl/datasmall.csv")

## Look at the same plot for these firms and distributors.
dat.5 <- datasmall[,.(exp = sum(Spend)), by = .(year)]
ggplot(data = dat.5, aes(year, exp)) + geom_line() + geom_point() + 
  xlab("Year")+ ylab("Expenditure") + theme_classic()


dat.6 <- datasmall[,.(exp = sum(Spend)), by = .(year, industry) ]
dat.6 <- merge(dat.6, industryName, by = "industry" )
ggplot(data = dat.6, aes(year, exp)) + geom_line() + geom_point() + 
  xlab("Year")+ ylab("Expenditure") + facet_wrap(~IndustryName,scale = "free_y")+ theme_classic()

## A. For each firm, what's the probability that they are spending more next year (from 2010-2014)
dat.7 <- datasmall[,.(exp = sum(Spend)), by = .(year, PrimeBrandCode)]
dat.7a <- dat.7[,.(year=year-1, PrimeBrandCode, exp)]
dat.7b <- merge(dat.7, dat.7a, by =c("year", "PrimeBrandCode"))
dat.7b[, exp.ratio:= exp.y/exp.x]
dat.7b <- dat.7b[exp.ratio < 3 & exp.ratio >0] 

ggplot(data = dat.7b, aes(exp.ratio)) + stat_density(fill = NA, color = "black") + geom_vline(xintercept = 1, color = "red") +facet_wrap(~year)

## B. For each distributor, what's the probability that they are getting more revenue next year (from 2010-2014)
dat.8 <- datasmall[,.(exp = sum(Spend)), by = .(year, DistributorCode)]
dat.8a <- dat.8[,.(year=year-1, DistributorCode, exp)]
dat.8b <- merge(dat.8, dat.8a, by =c("year", "DistributorCode"))
dat.8b[, exp.ratio:= exp.y/exp.x]
dat.8b <- dat.8b[exp.ratio < 3 & exp.ratio >0] 

ggplot(data = dat.8b, aes(exp.ratio)) + stat_density(fill = NA, color = "black") + geom_vline(xintercept = 1, color = "red") +facet_wrap(~year)

## Google's Market Share in Online Ads
google <- fread("/home/lunl/Research/Advertisement/Reference/Ref_data/google_share.csv")
