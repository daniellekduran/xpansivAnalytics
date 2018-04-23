#########################################################################################
# MAIN FILE FOR BP RINS FORECASTS
# Danielle Duran and Mike Fuller (Xpansiv Data Systems)
# April 2018
#
# Version 4.0
#
# Depends on R script file: "BP Projection Functions v2.R" (contains function definitions)
# Depends on BP data file:  "River_Birch-gas_production_data 2015.xlsx"
#
# GLOBALS:
#          predictionDays = number of days to forecast; set to 15
#          BPdataDir      = path to default data directory (string)
#
#########################################################################################

# Function definitions file
source("/Volumes/GoogleDrive/Team Drives/Technical Team/Production Partners/BP/River Birch Production Data/BP Projection Functions v5.R")

####===========================================================
# Function Arguments:

# directory paths 
# (UPDATE PATH AS NEEDED)
DataDir <- "/Volumes/GoogleDrive/Team Drives/Technical Team/Production Partners/BP/River Birch Production Data"
OutputDir <- "MODEL OUTPUT (TABLES)"

# file names
# CHOOSE A FILE BY UNCOMMENTING IT
#---------------------------------
# DataFile <- "RB.all.csv"
DataFile <- "wyoming_dairy_production_actual.csv"
# DataFile <- "mardigras_production_actual.csv"

# length of forecast in days
predictionDays <- 15 
# prediction intervals
predictionLimits <- c(50, 80, 99)

###===========================================================

# MAIN

# Run model on data and generate predictions
PredictionDF <- ComputeModelProjections_daily.f(predictionDays, predictionLimits, DataDir, DataFile, 
	OutputDir)


