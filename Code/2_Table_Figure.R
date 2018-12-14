## Goal: Create graphs and tables from summary statistics data
## Author: Lun Li
## Date Created: 11/07/2018

# Set up
packages <- c("reshape", "lubridate", "data.table", "plyr", "zoo", "bit64",
              "ggplot2", "xtable", "foreign", "dplyr", "foreach","doParallel",
              "readstata13", "stringr","plm", "blsAPI", "rjson")
lapply(packages, require, character.only = TRUE)

# If running at home
#path = "/Users/lunl/Dropbox/Research/2. Ad Intel/Github Code/ad/"
# If running on cluster 
path = "/home/lunl/Research/Advertisement/"
setwd(path)

# A. Summary Statistics:

## A1. All industries
### A1.1 Total annual expenditure on all ads, 10-15
### A1.1a Total annual expenditure on ads, monthly, 10-15
### A1.1b Total annual expenditure on ads, by MediaType, 10-15
### A1.1c Total annual expenditure on ads, monthly, by MediaType, 10-15
### A1.2 Number of Ads, Combined, 10-15
### A1.2a Number of Ads, by Mediatype, 10-15
### A1.2b Number of Unique Ads, Combined, 10-15
### A1.2c Number of Unique Ads, by MediaType
### A1.3 Number of Brands, by MediaType
### A1.4 A pie chart showing composition of Ads by MediaType, 10-15 average
### A1.4a A pie chart showing composition of Ads by MediaType, 10-15 separately
### A1.5 For TV ads: number of total people reached, 10-15 (work in progress)
### A1.5 For TV ads: average cost of reaching 1 person, 10-15 (work in progress)
### A1.6 For Internet ads: number of total people reached 
### A1.6 For Internet ads: average cost of reaching 1 person, 10-15 

## A2. Faceted by industry
### A2.1 Total annual expenditure on all ads, 10-15
### A2.1a Total annual expenditure on all ads, monthly, 10-15
### A2.1b Total annual expenditure on ads, by MediaType, 10-15
### A2.2 Number of Ads, Combined, 10-15
### A2.2b Number of Unique Ads, Combined, 10-15
### A2.2c Number of Unique Ads, by MediaType
### A2.3 Number of Brands, by MediaType
### A2.4 A pie chart showing composition of Ads by MediaType, 10-15 average
### A2.4a A pie chart showing composition of Ads by MediaType, 10-15 separately
### A2.5 For TV ads: number of total people reached, 10-15 (work in progress)
### A2.5 For TV ads: average cost of reaching 1 person, 10-15 (work in progress)
### A2.6 For Internet ads: number of total people reached 
### A2.6 For Internet ads: average cost of reaching 1 person, 10-15 

# B. Time Series of expenditure on ads by industry and media type. 
## B1. A graph with stacked bars showing the change in compositions in MediaTypes.
### B1.1 Monthly
### B1.2 Quarterly
### B1.3 Annually

# Ref files
mediatypeID <- fread("Reference/Ref_data/MediaTypeID.csv")
names(mediatypeID)[2] <- "MediaType"
industryName <- fread("Reference/Ref_data/industryName.csv")

# There are 9 types of media. For simplicity let's ignore Cinema for now.
medialist <- c("NetworkTV", "SpotTV", "Radio", "Magazine", "Newspaper", "Internet", "Outdoor", "FSI")

get_exp <- function(media){
  filelist <- paste0("Data/", media, "Summary", 2010:2015, ".csv")
  summary <- lapply(filelist, fread) %>% rbindlist()
  summary <- summary[,.(ym, industry, MediaTypeID, exp, MajorMediaType = media)]
  return(summary)
}

# Get expenditure data
data <- lapply(as.list(medialist), get_exp) %>%rbindlist()
data[, ym := as.yearmon(ym)]
data[, y := year(ym)]
data <- data[y!=2009]
data <- merge(data, mediatypeID, by = "MediaTypeID")
data <- merge(data, industryName, by = "industry")
data <- data[,.(industry, IndustryName, MediaTypeID, MediaType, MajorMediaType, ym, exp, y)][order(industry, MediaTypeID, ym)]

#fwrite(data, "Data/expenditure.csv")

## A1. All industries
### A1.1 Total annual expenditure on all ads, 10-15
A1.1 <- data[,.(exp = sum(exp)), by = .(y)]

ggplot(A1.1, aes(y, exp)) + geom_line() + geom_point() + xlab("Year") + ylab("Expenditure") + 
  ggtitle("Total Expenditure on Advertisement, 10-15") + theme_classic()

### A1.1a Total annual expenditure on ads, monthly, 10-15
A1.1a <- data[,.(exp = sum(exp)), by = .(ym)][order(ym)]
A1.1a[, date:= as.Date(ym)]

ggplot(A1.1a, aes(date, exp)) + geom_line() + geom_point() + xlab("Date") + ylab("Expenditure") +
  ggtitle("Total Expenditure on Advertisement, Monthly, 10-15") + theme_classic() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y", limits = c(min(A1.1a$date), max(A1.1a$date)), expand = c(0.01,0.01))

### A1.1b Total annual expenditure on ads, by MediaType, 10-15
A1.1b <- data[,.(exp = sum(exp)), by = .(MediaType, y)][order(MediaType, y)]

ggplot(A1.1b, aes(y, exp)) + geom_line() + geom_point() + xlab("Year") + ylab("Expenditure") + 
  facet_wrap(~MediaType, scales = "free") + 
  ggtitle("Total Expenditure on Advertisement, 10-15, by Media Type") + theme_classic()

### A1.1c Total annual expenditure on ads, monthly, by MediaType, 10-15
A1.1c <- data[,.(exp = sum(exp)), by = .(MediaType, ym)][order(MediaType, ym)]
A1.1c[, date:= as.Date(ym)]

ggplot(A1.1c, aes(date, exp)) + geom_line() + geom_point() + xlab("Date") + ylab("Expenditure") +
  facet_wrap(~MediaType, scales = "free") + ggtitle("Total Expenditure on Advertisement, Monthly, 10-15, by Media Type") + 
  theme_classic() + scale_x_date(date_breaks = "years", date_labels = "%y", limits = c(min(A1.1a$date), max(A1.1a$date)), expand = c(0.01,0.01))

### A1.1d Total expenditure on Ads by MajorMediaType, all industries.
A1.1d <- data[,.(exp =sum(exp)) ,by = .(MajorMediaType, y)]
ggplot(data = A1.1d, aes(y)) + geom_bar(aes(weight = exp, fill = MajorMediaType)) + xlab("Year") +
  ylab("Expenditure")

### A1.1e Expenditure shares on Ads by MajorMediaType, all industries.
A1.1e <- A1.1d[,.(exp = sum(exp)), by = .(y)] %>% merge(A1.1d, by = "y")
A1.1e[,exp.share := exp.y/exp.x]
ggplot(data = A1.1e, aes(y)) + geom_bar(aes(weight = exp.share, fill = MajorMediaType)) + xlab("Year") +
  ylab("Expenditure Share")

### A1.1f Total Expenditure on Advertisement, All Industries, by Major Media Type, 10-15, Annual
A1.1f <- data[,.(exp = sum(exp)), by = .(MajorMediaType, y)][order(MajorMediaType, y)]
ggplot(A1.1f, aes(y, exp)) + geom_line() + geom_point() + xlab("Year") + ylab("Expenditure") + 
  facet_wrap(~MajorMediaType, scales = "free", ncol = 3) + 
  ggtitle("Total Expenditure on Advertisement, 10-15, by Major Media Type") + theme_classic()

## A2. Faceted by industry
### A2.1 Total annual expenditure on all ads, 10-15
A2.1 <- data[,.(exp = sum(exp)), by = .(IndustryName, y)][order(IndustryName, y)]
ggplot(A2.1, aes(y, exp)) + geom_line() + geom_point() + 
  xlab("Year") + ylab("Expenditure") + facet_wrap(~IndustryName, scale = "free") +
  ggtitle("Total Expenditure on Advertisement, 10-15, by Industry") + theme_classic()

### A2.1a Total annual expenditure on all ads, monthly, 10-15
A2.1a <- data[,.(exp = sum(exp)), by = .(IndustryName, ym)][order(IndustryName, ym)]
A2.1a[, date:= as.Date(ym)]

ggplot(A2.1a, aes(date, exp)) + geom_line() + geom_point() + xlab("Date") + ylab("Expenditure") +
  facet_wrap(~IndustryName, scale = "free")+ggtitle("Total Expenditure on Advertisement, Monthly, 10-15") + theme_classic()


