packages <- c("reshape", "lubridate", "data.table", "plyr", "zoo", "bit64",
              "ggplot2", "xtable", "foreign", "dplyr", "foreach","doParallel",
              "readstata13", "stringr","plm", "blsAPI", "rjson")
lapply(packages, require, character.only = TRUE)
path = "/project/databases/nielsen-adintel/nielsen_extracts/AdIntel/"
setwd(path)

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
                               nunique = length(unique(AdCode))), by = .(ym, industry, MediaTypeID)]
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
