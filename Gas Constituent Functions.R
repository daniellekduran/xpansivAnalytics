prep.networkdata.f <- function(dataset) {
  ## The names in the network need to be characters
  id = (1:4)
  dataset[id] = as.character(unlist(dataset[id]))
  dataset$sales_delivery_point[dataset$sales_delivery_point==''] <- "Unknown"
  return(dataset)
  
}

prep.flowdata.f <- function(dataset) {
  ## Apicode and date need to be correctly coded
  ## Subset to include only necessary varaibles
  
  dataset$apicode <- as.character(dataset$apicode)
  dataset$date <- as.Date(dataset$date)
  dataset <- subset(dataset, select=c("apicode","date","gas"))
  dataset$non0 <- as.numeric(dataset$gas>0)
  return(dataset)
  
}

aggregate_screw.f <- function(dataset) {
  ## Computes the percent of flow for each constituent
  ## to each screw. A warning message is displayed if 
  ## no flow is detected for screws; showing the number
  ## of screws with 0 flow. The actual computations are
  ## simple. Percentages are taken from the flows which 
  ## have been normed to 100, then we calculate how much
  ## of the flow is attributable to each constituent. 
  ## The, we calculate total flow for each screw, total
  ## constituent flow for each screw, and compute that
  ## percent. 
  
  dataset$methane_pct <- dataset$MethaneNorm/100
  dataset$nitrogen_pct <- dataset$NitrogenNorm/100
  dataset$co2_pct <- dataset$CO2Norm/100
  dataset$ethane_pct <- dataset$EthaneNorm/100
  dataset$propane_pct <- dataset$PropaneNorm/100
  dataset$butane_pct <- dataset$ButaneNorm/100
  
  dataset$methane <- dataset$gas*dataset$methane_pct
  dataset$nitrogen <- dataset$gas*dataset$nitrogen_pct
  dataset$co2 <- dataset$gas*dataset$co2_pct
  dataset$ethane <- dataset$gas*dataset$ethane_pct
  dataset$propane <- dataset$gas*dataset$propane_pct
  dataset$butane <- dataset$gas*dataset$butane_pct
  
  dataset_agg <- dataset %>% group_by(primary_screw) %>% 
    mutate(screw_api_count = sum(non0)) %>%
    mutate(screw_null_api_count = (length(primary_screw) - screw_api_count)) %>%
    mutate(total_flow = sum(gas)) %>%
    mutate(total_methane = sum(methane)) %>%
    mutate(total_nitrogen = sum(nitrogen)) %>%
    mutate(total_co2 = sum(co2)) %>%
    mutate(total_ethane = sum(ethane)) %>%
    mutate(total_propane = sum(propane)) %>%
    mutate(total_butane = sum(butane)) %>%
    mutate(methane_pct = total_methane/total_flow)  %>%
    mutate(nitrogen_pct = total_nitrogen/total_flow) %>%
    mutate(co2_pct = total_co2/total_flow) %>%
    mutate(ethane_pct = total_ethane/total_flow) %>%
    mutate(propane_pct = total_propane/total_flow) %>%
    mutate(butane_pct = total_butane/total_flow) %>%
    slice(1) %>%
    select(primary_screw,  screw_api_count, screw_null_api_count, 
    total_flow, total_methane, total_nitrogen, total_co2, total_ethane, 
    total_propane, total_butane, methane_pct, nitrogen_pct, co2_pct, 
    ethane_pct, propane_pct, butane_pct, recip, sales_delivery_point)
  
  ## Get rid of any dplyr funny business - we just want a nice dataframe.
  dataset_agg <- as.data.frame(dataset_agg)
  
  ## Are there screws for which flows are not computed?
  nrow0 <- nrow(dataset_agg[dataset_agg$total_flow==0,]) 
  warning <- paste("Contituent Percentages not computed for",nrow0,"Screws with no active wells")
  if(nrow0>0) warning(warning)
  
  return(dataset_agg)
}

aggregate_recip.f <- function(dataset) {
  
  dataset <- dataset[c(1:10,17,18)]
  colnames(dataset) <- c("primary_screw","screw_api_count","screw_null_api_count","flow","methane","nitrogen",
                         "co2","ethane","propane","butane","recip","sales_delivery_point")
  
  dataset_agg <- dataset %>% group_by(recip) %>% 
    mutate(recip_api_count = sum(screw_api_count)) %>%
    mutate(recip_null_api_count = sum(screw_null_api_count)) %>%
    mutate(total_flow = sum(flow)) %>%
    mutate(total_methane = sum(methane)) %>%
    mutate(total_nitrogen = sum(nitrogen)) %>%
    mutate(total_co2 = sum(co2)) %>%
    mutate(total_ethane = sum(ethane)) %>%
    mutate(total_propane = sum(propane)) %>%
    mutate(total_butane = sum(butane)) %>%
    mutate(methane_pct = total_methane/total_flow)  %>%
    mutate(nitrogen_pct = total_nitrogen/total_flow) %>%
    mutate(co2_pct = total_co2/total_flow) %>%
    mutate(ethane_pct = total_ethane/total_flow) %>%
    mutate(propane_pct = total_propane/total_flow) %>%
    mutate(butane_pct = total_butane/total_flow) %>%
    slice(1) %>%
    select(recip, recip_api_count, recip_null_api_count, total_flow, total_methane, total_nitrogen, 
           total_co2, total_ethane, total_propane, total_butane, methane_pct, nitrogen_pct, 
           co2_pct, ethane_pct, propane_pct, butane_pct, sales_delivery_point)
  ## Get rid of any dplyr funny business - we just want a nice dataframe.
  dataset_agg <- as.data.frame(dataset_agg)
  
  ## Are there recips for which flows are not computed?
  nrow0 <- nrow(dataset_agg[dataset_agg$total_flow==0,]) 
  warning <- paste("Contituent Percentages not computed for",nrow0,"Recips with no active wells")
  if(nrow0>0) warning(warning)
  
  return(dataset_agg)
  
}

aggregate_sales.f <- function(dataset) {
  
  dataset <- dataset[c(1:10,17)]
  colnames(dataset) <- c("recip","recip_api_count","recip_null_api_count","flow","methane","nitrogen",
                         "co2","ethane","propane","butane","sales_delivery_point")
  
  dataset_agg <- dataset %>% group_by(sales_delivery_point) %>% 
    mutate(sales_api_count = sum(recip_api_count)) %>%
    mutate(sales_null_api_count = sum(recip_null_api_count)) %>%
    mutate(total_flow = sum(flow)) %>%
    mutate(total_methane = sum(methane)) %>%
    mutate(total_nitrogen = sum(nitrogen)) %>%
    mutate(total_co2 = sum(co2)) %>%
    mutate(total_ethane = sum(ethane)) %>%
    mutate(total_propane = sum(propane)) %>%
    mutate(total_butane = sum(butane)) %>%
    mutate(methane_pct = total_methane/total_flow)  %>%
    mutate(nitrogen_pct = total_nitrogen/total_flow) %>%
    mutate(co2_pct = total_co2/total_flow) %>%
    mutate(ethane_pct = total_ethane/total_flow) %>%
    mutate(propane_pct = total_propane/total_flow) %>%
    mutate(butane_pct = total_butane/total_flow) %>%
    slice(1) %>%
    select(sales_delivery_point, sales_api_count, sales_null_api_count, total_flow, total_methane, total_nitrogen, 
           total_co2, total_ethane, total_propane, total_butane, methane_pct, nitrogen_pct, 
           co2_pct, ethane_pct, propane_pct, butane_pct)
  ## Get rid of any dplyr funny business - we just want a nice dataframe.
  dataset_agg <- as.data.frame(dataset_agg)
  
  ## Are there sales delivery points for which flows are not computed?
  nrow0 <- nrow(dataset_agg[dataset_agg$total_flow==0,]) 
  warning <- paste("Contituent Percentages not computed for",nrow0,"Sales delivery points with no active wells")
  if(nrow0>0) warning(warning)
  
  return(dataset_agg)
  
}


aggregate_screw_many.f <- function(dataset) {
  ## Computes the percent of flow for each constituent
  ## to each screw. A warning message is displayed if 
  ## no flow is detected for screws; showing the number
  ## of screws with 0 flow. The actual computations are
  ## simple. Percentages are taken from the flows which 
  ## have been normed to 100, then we calculate how much
  ## of the flow is attributable to each constituent. 
  ## The, we calculate total flow for each screw, total
  ## constituent flow for each screw, and compute that
  ## percent. 
  
  dataset$methane_pct <- dataset$MethaneNorm/100
  dataset$nitrogen_pct <- dataset$NitrogenNorm/100
  dataset$co2_pct <- dataset$CO2Norm/100
  dataset$ethane_pct <- dataset$EthaneNorm/100
  dataset$propane_pct <- dataset$PropaneNorm/100
  dataset$butane_pct <- dataset$ButaneNorm/100
  
  dataset$methane <- dataset$gas*dataset$methane_pct
  dataset$nitrogen <- dataset$gas*dataset$nitrogen_pct
  dataset$co2 <- dataset$gas*dataset$co2_pct
  dataset$ethane <- dataset$gas*dataset$ethane_pct
  dataset$propane <- dataset$gas*dataset$propane_pct
  dataset$butane <- dataset$gas*dataset$butane_pct
  
  dataset_agg <- dataset %>% group_by(primary_screw, recip, sales_delivery_point) %>% 
    mutate(screw_api_count = sum(non0)) %>%
    mutate(screw_null_api_count = (length(primary_screw) - screw_api_count)) %>%
    mutate(total_flow = sum(gas)) %>%
    mutate(total_methane = sum(methane)) %>%
    mutate(total_nitrogen = sum(nitrogen)) %>%
    mutate(total_co2 = sum(co2)) %>%
    mutate(total_ethane = sum(ethane)) %>%
    mutate(total_propane = sum(propane)) %>%
    mutate(total_butane = sum(butane)) %>%
    mutate(methane_pct = total_methane/total_flow)  %>%
    mutate(nitrogen_pct = total_nitrogen/total_flow) %>%
    mutate(co2_pct = total_co2/total_flow) %>%
    mutate(ethane_pct = total_ethane/total_flow) %>%
    mutate(propane_pct = total_propane/total_flow) %>%
    mutate(butane_pct = total_butane/total_flow) %>%
    slice(1) %>%
    select(primary_screw,  screw_api_count, screw_null_api_count, 
           total_flow, total_methane, total_nitrogen, total_co2, total_ethane, 
           total_propane, total_butane, methane_pct, nitrogen_pct, co2_pct, 
           ethane_pct, propane_pct, butane_pct, recip, sales_delivery_point)
  
  ## Get rid of any dplyr funny business - we just want a nice dataframe.
  dataset_agg <- as.data.frame(dataset_agg)
  
  ## Are there screws for which flows are not computed?
  nrow0 <- nrow(dataset_agg[dataset_agg$total_flow==0,]) 
  warning <- paste("Contituent Percentages not computed for",nrow0,"Screws with no active wells")
  if(nrow0>0) warning(warning)
  
  return(dataset_agg)
}

aggregate_recip_many.f <- function(dataset) {
  
  dataset <- dataset[c(1:10,17,18)]
  colnames(dataset) <- c("primary_screw","screw_api_count","screw_null_api_count","flow","methane","nitrogen",
                         "co2","ethane","propane","butane","recip","sales_delivery_point")
  
  dataset_agg <- dataset %>% group_by(recip, sales_delivery_point) %>% 
    mutate(recip_api_count = sum(screw_api_count)) %>%
    mutate(recip_null_api_count = sum(screw_null_api_count)) %>%
    mutate(total_flow = sum(flow)) %>%
    mutate(total_methane = sum(methane)) %>%
    mutate(total_nitrogen = sum(nitrogen)) %>%
    mutate(total_co2 = sum(co2)) %>%
    mutate(total_ethane = sum(ethane)) %>%
    mutate(total_propane = sum(propane)) %>%
    mutate(total_butane = sum(butane)) %>%
    mutate(methane_pct = total_methane/total_flow)  %>%
    mutate(nitrogen_pct = total_nitrogen/total_flow) %>%
    mutate(co2_pct = total_co2/total_flow) %>%
    mutate(ethane_pct = total_ethane/total_flow) %>%
    mutate(propane_pct = total_propane/total_flow) %>%
    mutate(butane_pct = total_butane/total_flow) %>%
    slice(1) %>%
    select(recip, recip_api_count, recip_null_api_count, total_flow, total_methane, total_nitrogen, 
           total_co2, total_ethane, total_propane, total_butane, methane_pct, nitrogen_pct, 
           co2_pct, ethane_pct, propane_pct, butane_pct, sales_delivery_point)
  ## Get rid of any dplyr funny business - we just want a nice dataframe.
  dataset_agg <- as.data.frame(dataset_agg)
  
  ## Are there recips for which flows are not computed?
  nrow0 <- nrow(dataset_agg[dataset_agg$total_flow==0,]) 
  warning <- paste("Contituent Percentages not computed for",nrow0,"Recips with no active wells")
  if(nrow0>0) warning(warning)
  
  return(dataset_agg)
  
}


