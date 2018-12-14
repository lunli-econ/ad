# Exploration of Ad Intel Dataset

packages <- c("reshape", "lubridate", "data.table", "plyr", "zoo", "bit64",
              "ggplot2", "xtable", "foreign", "dplyr", "foreach","doParallel",
              "readstata13", "stringr","plm", "blsAPI", "rjson")

lapply(packages, require, character.only = TRUE)

# File Structure

path = "/project/databases/nielsen-adintel/nielsen_extracts/AdIntel/"
setwd(path)

# Folders: 2010-2015, Master_Files, Reference_Documentation

# 1. Occurrence Data

NetworkTV <- "2010/Occurrences/NetworkTV.tsv"
SpotTV <- "2010/Occurrences/SpotTV.tsv"
Magazine <- "2010/Occurrences/Magazine.tsv"
FSICoupon <- "2010/Occurrences/FSICoupon.tsv"
Newspaper <- "2010/Occurrences/Newspaper.tsv"
Radio <- "2010/Occurrences/Radio.tsv"
Outdoor <- "2010/Occurrences/Outdoor.tsv"
Internet <- "2010/Occurrences/Internet.tsv"
Cinema <- "2010/Occurrences/Cinema.tsv"

# 2. Reference Data

# Dynamic Reference Data Files (DRDF)
Advertiser <- "2010/References/Advertiser.tsv"
Brand <- "2010/References/Brand.tsv"
CinemaAdType <- "2010/References/CinemaAdType.tsv"
CreativeDescription <- "2010/References/CreativeDescription.tsv"
Distributor <- "2010/References/Distributor.tsv"
MagazineGenre <- "2010/References/MagazineGenre.tsv"
ProductCategories <- "2010/References/ProductCategories.tsv"
Publisher <- "2010/References/Publisher.tsv"
TV_Program <- "2010/References/TV Program.tsv"

# Static Reference Data Files (SRDF)
CinemaMPAARating <- "Master_Files/Latest/CinemaMPAARating.tsv"
Market <- "Master_Files/Latest/Market.tsv"
Market_Breaks <- "Master_Files/Latest/Market Breaks.tsv"
MediaType <- "Master_Files/Latest/MediaType.tsv"
Newspaper_Event <- "Master_Files/Latest/Newspaper Event.tsv"
Newspaper_Section <- "Master_Files/Latest/Newspaper Section.tsv"
TV_Daypart <- "Master_Files/Latest/TV Daypart.tsv"
TV_ProgramType <- "Master_Files/Latest/TV ProgramType.tsv"
TV_ProgramEvent <- "Master_Files/Latest/Tv ProgramEvent.tsv"
TV_ProgramSubType <- "Master_Files/Latest/TV ProgramSubType.tsv"

# 3. Impression File Formats
ImpNationalTV = "2010/Impressions/ImpNationalTV.tsv"
ImpSpotTV = "2010/Impressions/ImpSpotTV.tsv"
ImpSpotRadio = "2010/Impressions/ImpSpotRadio.tsv"

# 4. Universe Estimate Files
UENationalTV = "2010/Universe_Estimates/UENationalTV.tsv"
UESpotTV = "2010/Universe_Estimates/UESpotTV.tsv"
UESpotRadio = "2010/Universe_Estimates/UESpotRadio.tsv"

# 5. Market Break Files (only for TVs)
IMPMarketBreaks = "2010/Market_Breaks/IMPMarketBreaks.tsv"
UEMarketBreaks = "2010/Universe_Estimates/UEMarketBreaks.tsv"

