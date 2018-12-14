---
title: "Research Log"
output: html_document
---
## Updates:
- 11/06: Generate plots/tables from summary statistics files.

## To-do:
- Look at if the trends are caused by Nielsen's coverage
- Representation of Ads Effectiveness of Data
- What about Imp? It seems that there is a way to compare imp between TV and internet (provided that the data is accurate enough).

## FAQ
- Is data coverage consistent from 2010-2015?

      **A: TBA**

- Why there are spend with 0 dollars? 

      **A: Most likely missing data**

- Are brand codes in diffrent markets (tv, radio, etc) the same?
  
      **A: Need to check, but since there is only 1 brand file per year, I guess they are the same.**

- What is an FSI coupon?
  
      **A: FSI coupon, or "Free Standing Insert" coupon, is an advertising brochure, card,
      or leaflet inserted in a newspaper or magazine, usually to serve as a reply coupon
      or discount voucher.**

- What is a ProductID?
  
      **A: A unique identifier of product category at the base level. Note that 
      similar TV programs may have the same product ID. 
      (Check industry P2 in national TV, for example.)**

- What are the units of a radio ad (why it's not 1?)
  
      **A: The total number of incidences of the occurrence. Only in Spot Radio media type.**



## Bugs: 
1. The variale "PrimBrandCode" is named "PrimeBrandCode" with an additional "e" in some occurence files.

2. The variable "Spend" is named "spend" in cinema occurances.
