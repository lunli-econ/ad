## Goal: create summary statistics for Ad Intel Data
## Author: Lun Li

## This code can only be executed from the Univeristy of Chicago, Research
## Computing Center (RCC) cluster

# Completed Tasks (Line 25 to 449)
# A. For each industry x month x ad channel, show
# A1. How many ads
# A2. How much expenditure
# A3. How many seconds in total/average size
# A4. How many brands
# A5. How many unique ads
# A6. Average cost per second
# A7. How many days per month

# Ongoing Tasks (Line 451-End)
# B. Repeat A for National TV, Spot TV and Spot Radio only, add
# B1. How many people reached (impression)
# B2. Percentage of people reached (out of universe)
# B3. Cost per GBP
# B4. TBA


# A.
# Set up
packages <- c("reshape", "lubridate", "data.table", "plyr", "zoo", "bit64",
              "ggplot2", "xtable", "foreign", "dplyr", "foreach","doParallel",
              "readstata13", "stringr","plm", "blsAPI", "rjson")
lapply(packages, require, character.only = TRUE)
path = "/project/databases/nielsen-adintel/nielsen_extracts/AdIntel/"
setwd(path)

# List the paths of occurance data (available from 2010-2015)
NetworkTV <- paste0(year, "/Occurrences/NetworkTV.tsv")
SpotTV <- paste0(year,"/Occurrences/SpotTV.tsv")
Magazine <- paste0(year,"/Occurrences/Magazine.tsv")
FSICoupon <- paste0(year,"/Occurrences/FSICoupon.tsv")
Newspaper <- paste0(year,"/Occurrences/Newspaper.tsv")
Radio <- paste0(year,"/Occurrences/Radio.tsv")
Outdoor <- paste0(year,"/Occurrences/Outdoor.tsv")
Internet <- paste0(year,"/Occurrences/Internet.tsv")
Cinema <- paste0(year,"/Occurrences/Cinema.tsv")

# List the paths of reference data
Brand <- paste0(year, "/References/Brand.tsv")

# Write a function to compute summary table

# 1. NetworkTV
NetworkTVSummary <- function(year, output = NA){
  NetworkTV <- paste0(year, "/Occurrences/NetworkTV.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  networktv <- fread(NetworkTV)
  brand <- fread(Brand)
  networktv <- networktv[, c("AdDate", "AdTime", "MarketCode", "MediaTypeID",
                             "PrimBrandCode","Units", "Spend", "TVDaypartCode", "Duration", "AdCode",
                             "Impression Type", "Market Break Type", "NielsenProgramCode", "CreativeID",
                             "Monitor Plus Program Code", "UC_dim_Bridge_occ_ImpNationalTV_key"), with = F]
  
  networktv <- merge(networktv, brand, by.x = "PrimBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  lb <- quantile(networktv$Spend, 0.01)
  ub <- quantile(networktv$Spend, 0.99)
  networktvMID<- networktv[Spend> lb & Spend < ub]
  
  networktvMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- networktvMID[,.(count = sum(Units), exp= sum(Spend),
                                   seconds = sum(Duration), products = length(unique(ProductID)),
                                   brands = length(unique(PrimBrandCode)),
                                   nunique.ads = length(unique(AdCode)),
                                   nunique.days = length(unique(AdDate))), by = .(ym, industry, MediaTypeID)]
  
  
  if (is.na(output)){
    return(summary_table)
    message(paste0("NationalTV, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/NetworkTVSummary", year, ".csv"))
    message(paste0("NationalTV, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
networktv_summary <- mclapply(2010:2015, 
                              function(x) NetworkTVSummary(x, output = "/home/lunl/Research/Advertisement/Data"),
                              mc.cores = getOption("mc.cores", 3L))

# 2. SpotTV (Suggest running in cluster)
SpotTVSummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/SpotTV.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(Spend)]
  
  lb <- quantile(occur$Spend, 0.01)
  ub <- quantile(occur$Spend, 0.99)
  occur<- occur[Spend> lb & Spend < ub]
  
  occur <- occur[, c("AdDate", "AdTime", "MarketCode", "MediaTypeID",
                     "PrimBrandCode","Units", "Spend", "TVDaypartCode", "Duration", "AdCode",
                     "CreativeID","Monitor Plus Program Code", "UC_dim_Bridge_occ_ImpSpotTV_key"), with = F]
  
  occur <- merge(occur, brand, by.x = "PrimBrandCode", by.y = "BrandCode")
  
  
  
  
  occur[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occur[,.(count = sum(Units), exp= sum(Spend),
                            seconds = sum(Duration), products = length(unique(ProductID)),
                            brands = length(unique(PrimBrandCode)),
                            nunique.ads = length(unique(AdCode)),
                            nunique.days = length(unique(AdDate))), by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("SpotTV, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/SpotTVSummary", year, ".csv"))
    message(paste0("SpotTV, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
spottv_summary <- mclapply(2010:2015, 
                           function(x) SpotTVSummary(x, output = "/home/lunl/Research/Advertisement/Data"),
                           mc.cores = getOption("mc.cores", 3L))

# 3. Magazine
MagazineSummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/Magazine.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(Spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimBrandCode","Units", "Spend", "MagAdColor", "MagAdSize", "AdCode",
                     "CreativeFileName","AdNumber"), with = F]
  occur <- merge(occur, brand, by.x = "PrimBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  
  lb <- quantile(occur$Spend, 0.01)
  ub <- quantile(occur$Spend, 0.99)
  occurMID<- occur[Spend> lb & Spend < ub]
  
  occurMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occurMID[,.(count = sum(Units), exp= sum(Spend),
                               size = sum(MagAdSize), products = length(unique(ProductID)),
                               brands = length(unique(PrimBrandCode)),
                               nunique.ads = length(unique(AdCode)),
                               nunique.days = length(unique(AdDate))), by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("Magazine, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/MagazineSummary", year, ".csv"))
    message(paste0("Magazine, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
magazine_summary <- mclapply(2010:2015, 
                             function(x) MagazineSummary(x, output = "/home/lunl/Research/Advertisement/Data"),
                             mc.cores = getOption("mc.cores", 3L))

# 4. FSICoupon
FSISummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/FSICoupon.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(Spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimBrandCode","Units", "Spend", "AdCode",
                     "OffValue", "OffType", "CouponID"), with = F]
  occur <- merge(occur, brand, by.x = "PrimBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  
  lb <- quantile(occur$Spend, 0.01)
  ub <- quantile(occur$Spend, 0.99)
  occurMID<- occur[Spend> lb & Spend < ub]
  
  occurMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occurMID[,.(count = sum(Units), exp= sum(Spend),
                               avg.off = mean(OffValue, na.rm = T), products = length(unique(ProductID)),
                               brands = length(unique(PrimBrandCode)),
                               nunique.ads = length(unique(AdCode)),
                               nunique.coupons = length(unique(CouponID)),
                               nunique.days = length(unique(AdDate))), by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("FSI, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/FSISummary", year, ".csv"))
    message(paste0("FSI, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
fsi_summary <- mclapply(2010:2015, 
                        function(x) FSISummary(x, output = "/home/lunl/Research/Advertisement/Data"),
                        mc.cores = getOption("mc.cores", 3L))

# 5. Newspaper
NewspaperSummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/Newspaper.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(Spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimBrandCode","Units", "Spend", "AdCode",
                     "NewspAdSize"), with = F]
  occur <- merge(occur, brand, by.x = "PrimBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  
  lb <- quantile(occur$Spend, 0.01)
  ub <- quantile(occur$Spend, 0.99)
  occurMID<- occur[Spend> lb & Spend < ub]
  
  occurMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occurMID[,.(count = sum(Units), exp= sum(Spend),
                               size = sum(NewspAdSize), products = length(unique(ProductID)),
                               brands = length(unique(PrimBrandCode)),
                               nunique.ads = length(unique(AdCode)),
                               nunique.days = length(unique(AdDate))), by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("Newspaper, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/NewspaperSummary", year, ".csv"))
    message(paste0("Newspaper, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
newspaper_summary <- mclapply(2010:2015, 
                              function(x) NewspaperSummary(x, output = "/home/lunl/Research/Advertisement/Data"),
                              mc.cores = getOption("mc.cores", 3L))

# 6. Radio
RadioSummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/Radio.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(Spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimeBrandCode","Units", "Spend", "AdCode",
                     "UC_dim_Bridge_occ_ImpSpotRadio_key"), with = F]
  
  occur <- merge(occur, brand, by.x = "PrimeBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  
  lb <- quantile(occur$Spend, 0.01)
  ub <- quantile(occur$Spend, 0.99)
  occurMID<- occur[Spend> lb & Spend < ub]
  
  occurMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occurMID[,.(count = sum(Units), exp= sum(Spend),
                               products = length(unique(ProductID)),
                               brands = length(unique(PrimeBrandCode)),
                               nunique.ads = length(unique(AdCode)),
                               nunique.days = length(unique(AdDate))), by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("Radio, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/RadioSummary", year, ".csv"))
    message(paste0("Radio, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
radio_summary <- mclapply(2010:2015, 
                          function(x) RadioSummary(x, output = "/home/lunl/Research/Advertisement/Data"),
                          mc.cores = getOption("mc.cores", 3L))

# 7. Outdoor
OutdoorSummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/Outdoor.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(Spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimBrandCode", "Spend", "AdCode"), with = F]
  
  occur <- merge(occur, brand, by.x = "PrimBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  
  lb <- quantile(occur$Spend, 0.01)
  ub <- quantile(occur$Spend, 0.99)
  occurMID<- occur[Spend> lb & Spend < ub]
  
  occurMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occurMID[,.(count = .N, exp= sum(Spend),
                               products = length(unique(ProductID)),
                               brands = length(unique(PrimBrandCode)),
                               nunique.ads = length(unique(AdCode)),
                               nunique.days = length(unique(AdDate))), by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("Outdoor, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/OutdoorSummary", year, ".csv"))
    message(paste0("Outdoor, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
outdoor_summary <- mclapply(2010:2015, 
                            function(x) OutdoorSummary(x, output = "/home/lunl/Research/Advertisement/Data"),
                            mc.cores = getOption("mc.cores", 3L))

# 8. Internet
InternetSummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/Internet.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(Spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimeBrandCode", "Spend", "AdCode",
                     "Imp2Plus"), with = F]
  
  occur <- merge(occur, brand, by.x = "PrimeBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  
  lb <- quantile(occur$Spend, 0.01)
  ub <- quantile(occur$Spend, 0.99)
  occurMID<- occur[Spend> lb & Spend < ub]
  
  occurMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occurMID[,.(count = .N, exp= sum(Spend),
                               products = length(unique(ProductID)),
                               brands = length(unique(PrimeBrandCode)),
                               nunique.ads = length(unique(AdCode)),
                               nunique.days = length(unique(AdDate)),
                               total.imp = sum(as.numeric(Imp2Plus), na.rm = T)),
                            by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("Internet, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/InternetSummary", year, ".csv"))
    message(paste0("Internet, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
internet_summary <- lapply(2010:2015, 
                           function(x) InternetSummary(x, output = "/home/lunl/Research/Advertisement/Data"))

# 9. Cinema (only available since 2013)
CinemaSummary <- function(year, output = NA){
  
  Occur <- paste0(year,"/Occurrences/Cinema.tsv")
  Brand <- paste0(year, "/References/Brand.tsv")
  
  occur <- fread(Occur)
  brand <- fread(Brand)
  
  occur <- occur[!is.na(spend)]
  
  occur <- occur[, c("AdDate", "MarketCode", "MediaTypeID",
                     "PrimeBrandCode","Duration", "spend", "AdCode",
                     "Imp2Plus", "MpaaRatingId"), with = F]
  
  occur <- merge(occur, brand, by.x = "PrimeBrandCode", by.y = "BrandCode")
  
  # Discard occurances with really small costs (bottom 1%) and really large ones (top 1%)
  # (We can add them back later for rigidity)
  
  lb <- quantile(occur$spend, 0.01)
  ub <- quantile(occur$spend, 0.99)
  occurMID<- occur[spend> lb & spend < ub]
  
  occurMID[, ":="(ym = substr(AdDate, 1,7), industry = substr(PCCSubCode, 1,2))]
  
  summary_table <- occurMID[,.(count = .N, exp= sum(spend),
                               products = length(unique(ProductID)),
                               brands = length(unique(PrimeBrandCode)),
                               nunique.ads = length(unique(AdCode)),
                               nunique.days = length(unique(AdDate)),
                               total.imp = sum(as.numeric(Imp2Plus), na.rm = T)), by = .(ym, industry, MediaTypeID)]
  if (is.na(output)){
    return(summary_table)
    message(paste0("Cinema, year ",year, "Completed"))
  } else {
    fwrite(summary_table, paste0(output,"/CinemaSummary", year, ".csv"))
    message(paste0("Cinema, year ",year, " Completed"))
    rm(list = ls())
    gc()
  }
}
cinema_summary <- lapply(2013:2015, 
                         function(x) CinemaSummary(x, output = "/home/lunl/Research/Advertisement/Data"))


# Under construction; do not delete anhything below this line.
# 
# # B. 
# 
# # Create the paths of impression data
# ImpNationalTV = paste0(year, "/Impressions/ImpNationalTV.tsv")
# ImpSpotTV = paste0(year, "/Impressions/ImpSpotTV.tsv")
# ImpSpotRadio = paste0(year, "/Impressions/ImpSpotRadio.tsv")
# 
# # Read impression data
# impnationaltv <- fread(ImpNationalTV)
# 
# # Merge networktv with impressions.
# # Set DataStreamID=4 for commercial average estimates. This is an estimate
# # of how many people watched the program either live or in the next few days
# # Note that not all occurances have impression data
# imp <- merge(networktv,
#              impnationaltv[is.na(UC_dim_Bridge_occ_ImpNationalTV_key)==FALSE &
#                              DataStreamID==4 & HispanicFlag == "F"],
#              by = "UC_dim_Bridge_occ_ImpNationalTV_key", all.x = T)
