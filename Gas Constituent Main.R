#### Gas Constituent Analysis ####
## These functions aggregate the percentages of constituents
## from well level to the screw level, to the recip level
## and then to the sales delivery point level. No magic
## numbers needed here. Call the network mapping information,
## then call your flow information, and run the functins.
## Written by Danielle Duran for Xpansiv, Nov 1, 2018

source("/Volumes/GoogleDrive/Team Drives/Technical Team/Production Partners/Carbon Creek Energy/Analysis/Network/Gas Constituent Functions.R")
library(dplyr)

## Call the file which contains network mapping information ##
network_constituents <- read.csv("/Volumes/GoogleDrive/Team Drives/Technical Team/Production Partners/Carbon Creek Energy/Analysis/Network/apicodes_network_constituents.csv")
network_constituents <- prep.networkdata.f(network_constituents)

## Merge this file with your flow data - flow must be called 'gas'
# For building this code, I used the last day in the lastest dump file
## Take out lines 17 - 21 later and replace 'flow' with actual flow data
latest_dump <- read.csv("/Volumes/GoogleDrive/Team Drives/Technical Team/Production Partners/Carbon Creek Energy/Analysis/latest_dump.csv")
latest_dump$apicode <- as.character(latest_dump$apicode)
latest_dump$date <- as.Date(latest_dump$date)

flow <- latest_dump %>% group_by(apicode) %>%
  filter(date=="2018-01-01")
  #filter(row_number()==n())
flow <- prep.flowdata.f(flow)

network_with_flow <- merge(flow, network_constituents, by="apicode")

  ## If we know that the network has a many:one relationship
  ## at all nodes throughout the network
## Outputs a dataframe aggregaetd to the screw level
screw_aggregate <- aggregate_screw.f(network_with_flow)

## Outputs a dataframe aggregated to the recip level
recip_aggregate <- aggregate_recip.f(screw_aggregate)

## Outputs a dataframe aggregated to the sales delivery point level
sales_aggregate <- aggregate_sales.f(recip_aggregate)



  ## If we know that the network has a many:many relationship
  ## at all nodes throughout the network
## Outputs a dataframe aggregaetd to the screw level
screw_aggregate_many <- aggregate_screw_many.f(network_with_flow)

## Outputs a dataframe aggregated to the recip level
recip_aggregate_many <- aggregate_recip_many.f(screw_aggregate_many)

## Outputs a dataframe aggregated to the sales delivery point level
sales_aggregate_many <- aggregate_sales.f(recip_aggregate_many)



at_well <- network_with_flow %>% group_by(sales_delivery_point) %>% summarise(count = length(apicode))
at_screw <- screw_aggregate_many %>% group_by(sales_delivery_point) %>% summarise(count = sum(screw_api_count + screw_null_api_count))
at_recip <- recip_aggregate_many %>% group_by(sales_delivery_point) %>% summarise(count = sum(recip_api_count + recip_null_api_count))
at_sales <- sales_aggregate_many %>% group_by(sales_delivery_point) %>% summarise(count = sum(sales_api_count+ sales_null_api_count))
at_well
at_screw
at_recip
at_sales










