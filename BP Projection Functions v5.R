#########################################################################################
# FUNCTION DEFINITIONS FOR BP RINS FORECASTS
# Danielle Duran and Mike Fuller (Xpansiv Data Systems)
# April 2018
#
# Version 3
#
# This file is a dependency of "main" file: "BP RIN main v2.R"
# Contains defintions of functions called by main file
#
# R Package Dependencies:
#   readxl
#   forecast
#   lubridate
#
#########################################################################################

get_BPdata.f <- function(sourceDir){
	# Download MCF data needed for forecast
 	require(readxl)

	 #Import the rin data from Google Drive
	rb2015 <- read_excel(file.path(sourceDir, "River_Birch-gas_production_data 2015.xlsx"), sheet = "2015") 
	rb2016 <- read_excel(file.path(sourceDir, "River_Birch-gas_production_data 2015.xlsx"), sheet = "2016")
	rb2017 <- read_excel(file.path(sourceDir, "River_Birch-gas_production_data 2015.xlsx"), sheet = "2017")                   
	rb2018 <- read_excel(file.path(sourceDir, "River_Birch-gas_production_data 2015.xlsx"), sheet = "2018")

	bp.dat <- rbind(rb2015,rb2016,rb2017,rb2018)
	## Add formatted date
	bp.dat$flow_date <- as.Date(bp.dat$flow_date)

	bp.dat
}

load_DataFile.f <- function(fileNameORpath){
	# Reads data from a file
	# Returns a data frame with file contents
	# Runs a check to see if input file has a header in first row
	# Assumes second column of first row of file is numeric if there is no header

	# get first row of data file
	test <- read.csv(fileNameORpath, header=F, stringsAsFactors=F,nrows=1)

	# check whether second column is not numeric
	if(!is.numeric(test[,2])){
		# file has a header
		dataobj <- read.csv(fileNameORpath, header=T, stringsAsFactors=F)
	} else {
		# file does not have a header
		dataobj <- read.csv(fileNameORpath, header=F, stringsAsFactors=F)
	}
	dataobj 
}

get_DateRange.f <- function(df, datecol, daysout){
	### generates sequence of dates based on a Date column of an existing data frame
	### returns a vector of dates (Date class)
	### df      = source data frame
	### datecol = name of Date column
	### daysout = desired number of days to add to end of Date column

	lastday <- tail(df[[datecol]], n=1)

	date.seq <- (as.Date( lastday ) +1):(as.Date( lastday ) +daysout)
	#date.seq <- (tail(df[[datecol]], n=1) +1):(tail(df[[datecol]], n=1) +daysout)
	date.seq
}

setArma_p_q.f <- function(vec) {
	# generates arma parameters ("p" and "q") needed by Arima function
	# vec = vector of floats

	require(forecast)

   	twoElemVector <- c(auto.arima(vec)$arma[1],auto.arima(vec)$arma[2])
   	twoElemVector
}

isDate_mdy <- function(mydate, date.format = "%m/%d/%Y") {
	# Check whether format of date object is "%m/%d/%Y"
  	tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}


rinDF.f <- function(df) {
	# Constructs two data frames: 
	#   1) modified copy of input data frame; adds columns for season, day, month, and year
	#   2) new data frame with dates categorized by season of year (columns: all_dates, season, month_day)
	# Returns list containing the two data frames 
	# Assumes df is a data frame with a date column named "flow_date"

	require(lubridate)

	# Fct to create the season variable
	d = function(month_day) which(lut$month_day == month_day)

	#make sure this is formatted as date for the next bit to run correctly
	# if(!is.Date(df$flow_date)) { df$flow_date <- as.Date(df$flow_date)} 

	# Fix dates given in m/d/Y format (convert to Y-m-d format)
	if(isDate_mdy(df$flow_date[1])){ df$flow_date <- as.Date(df$flow_date, "%m/%d/%Y") }

	lut = data.frame(all_dates = as.POSIXct("2012-1-1") + ((0:365) * 3600 * 24), season = NA)
	lut = within(lut, { month_day = strftime(all_dates, "%b-%d") }) 

	lut[c(d("Sep-10"):d("Nov-19")), "season"] = "autumn"
	lut[c(d("Jan-20"):d("Mar-29")), "season"] = "late winter"
	lut[c(d("Jan-01"):d("Jan-19"), d("Nov-20"):d("Dec-31")), "season"] = " early winter"
	lut[c(d("Mar-30"):d("Jun-17")), "season"] = "spring"
	lut[c(d("Jun-18"):d("Sep-09")), "season"] = "summer"
	rownames(lut) = lut$month_day

	dat = data.frame(dates = Sys.Date() + (0:11)*30)
	df = within(df, { season =  lut[strftime(flow_date, "%b-%d"), "season"] })

	  #Format the time variables
	df$season <- as.factor(df$season)
	df$day <- yday(df$flow_date)
	df$month <- month(df$flow_date)
	df$year <- substring(df$flow_date,1,4) 
	df$month <- as.factor(df$month)
	df$year <- ordered(df$year)
	list(df,lut)
}

lmeMod.f <- function(df){
	# Construct statistical model from data
	# Returns lme object (fitted model)

	require(lme4)
	require(nlme)

	#get number of years
	numLevels <- nlevels(df$year)

	if(numLevels==1) {
		# for single year data
		pq <- setArma_p_q.f(df$diff.mcf)
		mod <- lme(diff.mcf ~ month + season, random = ~ 1|month, correlation = corARMA(form = ~ day|month, p=pq[1],q=pq[2]), data= df, na.action = na.omit)
	} else {
		# for multi-year data; incorporates mixed effects into the model
		pq <- setArma_p_q.f(df$diff.mcf)
		mod <- lme(diff.mcf ~ month + season, random = ~ 1|year, correlation = corARMA(form = ~ day|year, p=pq[1],q=pq[2]), data= df, na.action = na.omit)
	}
	mod
}


construct_predictorsDF.f <- function(datesvec, seasonDF){
	# generates prediction intervals using data and output of mixed model
	# vec = vector of strings (dates)
	# df  = data frame with MCF data
	# seasonDF = data frame with season dates

	require(lubridate)

	# Add predictor variables to data frame
	Predictors <- data.frame(flow_date = as.Date(datesvec, origin = "1970-01-01"))
	Predictors = within(Predictors, {  season = seasonDF[strftime(flow_date, "%b-%d"), "season"] })
	Predictors$season <- as.factor(Predictors$season)
	Predictors$day <- yday(Predictors$flow_date)
	Predictors$month <- month(Predictors$flow_date)
	Predictors$year <- substring(Predictors$flow_date,1,4) 
	Predictors$month <- as.factor(Predictors$month)
	Predictors$year <- as.factor(Predictors$year)
	Predictors
}


predIntv.f <- function(dat, mod, predictorDF, SE_multipliers){
	# generates prediction intervals using data and output of mixed model
	# df  = data frame with MCF data
	# mod = output object from non-linear model
	# predictorDF    = data frame with predictor variables; copied and modified by this function
	# SE_multipliers = vector of integers which define confidence intervals (e.g. c(50, 80, 99));
	#                  these are used to compute intervals based on std error of model predictions

	# Drop flow data col 
	#predictorDF <- predictorDF[,-1]

	# Convert integer prediction intervals to decimal
	SE_multipliers <- SE_multipliers /100
	# Save text version with trailing zeros, for col names
	SE_multipliers_text <- sprintf("%.2f", round(SE_multipliers,2))

	# Compute predictions
	predictorDF$pred <- predict(mod, newdata=predictorDF)

	# Get number of intervals to include in output
	# Need high and low PI, so multiply length of intervals vector by 2
	intv_n <- length(SE_multipliers)*2

	# initialize df to hold confidence intervals...
	pred_rows <- nrow(predictorDF)
	pred_cols <- ncol(predictorDF)
	PI <- as.data.frame(matrix(nrow=pred_rows, ncol=intv_n))
	# set column names 
	tempnames <- paste0("PI.",rep(SE_multipliers_text, each=2))
	names(PI) <- paste0(tempnames,c(".lo",".hi"))

	# combine predictors and CI data frames
	Prediction <- cbind(predictorDF, PI)

	# compute standard errors; used to construct final confidence intervals
	Prediction$SE2 <- sqrt(predictorDF$pred + mod$sigma^2) 

	#find the starting point for the backtransformation
	startValue <- (tail(dat$mcf_volume_actual,n=1) + (head(predictorDF$pred,n=1)))

	# fill in PI intervals of data 
	count <- 1
	for(i in 1:intv_n) 
	{
		if(i %% 2 == 1) 
		{           Prediction[, pred_cols +i] <- Prediction$pred - (SE_multipliers[count] * Prediction$SE2) # low point of interval
		} else {
				    Prediction[, pred_cols +i] <- Prediction$pred + (SE_multipliers[count] * Prediction$SE2) # high point of interval
				    count <- count +1
		}	
	}

	#backtransform the predicted mcf
	pred.mcf <- diffinv(as.numeric(predictorDF$pred),xi=startValue)
	Prediction$pred <- head(pred.mcf,-1)

	for(i in 1:intv_n)
	{
			temp_var <- diffinv(as.numeric(Prediction[, pred_cols +i]),xi=startValue)
			Prediction[, pred_cols +i] <- tail(temp_var,-1)
	}

	Prediction
}

predTable.f <- function(pred.df){
	# Extract table of final prediction intervals
	# from data frame with sequance of predictions
	# Depends on input having a specific structure and content
	# Assumes pred.df has minimum of seven cols: flow_date, season, day, month, year, pred, PI.lo, PI.hi
	# Assumes last col = SE2

	size <- dim(pred.df)
	nr <- size[1]
	nc <- size[2]
	numIntervals <- (nc - 7)/2 # first 6 cols plus last col; divide by 2 to account for upper/lower values

	# Get prediction intervals as integers from column names
	Pcol_names <- names(pred.df)[8:nc-1]
	P_intervals.names <- unique(as.numeric(substr(Pcol_names,5,7)) *100)

	# Last predicted value in sequence
	pred <- pred.df$pred[nr]
	# Last set of prediction intervals
	temp <- pred.df[nr,8:nc-1]
	temp <- matrix(temp, nrow=2,ncol=numIntervals)
	P_intervals <- as.data.frame(t(temp))
	names(P_intervals) <- c("Lower","Upper")

	temp <- data.frame("Pred.Interval"=P_intervals.names, Prediction=pred)
	out.df <- cbind(temp, P_intervals)
	out.df
}

write_PredTable.f <- function(df, filenameORpath){
	# Reformats data frame and saves as table in text file
	# Assumes data frame has columns Upper and Lower

	# flatten list cols
	df$Upper <- vapply(df$Upper, paste, collapse = ", ", character(1L))
	df$Lower <- vapply(df$Lower, paste, collapse = ", ", character(1L))

	# reclass and round character cols
	df$Upper <- as.numeric(df$Upper)
	df$Lower <- as.numeric(df$Lower)
	df$Upper <- round(df$Upper, 0)
	df$Lower <- round(df$Lower, 0)

	# save to file
	write.table(df, quote=F, row.names=F, file=filenameORpath)

	# return reformatted data frame
	df
}

plotPred.f <- function(df, intervals=c(0.50,0.80,0.99)){

	# Generate plot (i.e. a figure) of model prediction and prediction intervals
	#
	# THIS FUNCTION IS NOT WORKING WITH ALL DATA SETS
	#
	
	# Colors:
	colr1 <- "lightsteelblue2"
	colr2 <- "grey70"
	colr3 <- "skyblue"

	intervals_text <- sprintf("%.2f", round(intervals,2))

	  require(ggplot2)
	  par(mfrow=c(1,1))
	  par(mar=c(5,5,5,3))
	  plot(cumsum(df$pred)~df$flow_date,type="l",lwd=2,cex.lab=1.7,cex.axis=1.9,cex.main=2.0,
	  	ylab="Cumulative Sum of Predicted MCF",xlab="2018",ylim=c(0,max(cumsum(df$PI.0.99.hi))),
	  	col="white",main="Forecast with Prediction Intervals")
	  
	  polygon(c(rev(df$flow_date), df$flow_date), 
	          c(rev(cumsum(df$PI.0.99.hi)), (cumsum(df$PI.0.99.lo))), col = colr1, border = NA)
	  
	  polygon(c(rev(df$flow_date), df$flow_date), 
	          c(rev(cumsum(df$PI.0.8.hi)), (cumsum(df$PI.0.8.lo))), col = colr2, border = NA)
	  
	  polygon(c(rev(df$flow_date), df$flow_date), 
	          c(rev(cumsum(df$PI.0.5.hi)), (cumsum(df$PI.0.5.lo))), col = colr3, border = NA)
	  
	  lines((cumsum(df$pred))~df$flow_date,lwd=3,lty=3,col="grey20")
	  legend("topleft",cex=1.5,pt.cex=2,fill=c("skyblue","grey70","lightsteelblue2"),
	         legend=c("50% Prediction Interval","80% Prediction Interval","99% Prediction Interval"),bty="n")
	  legend("bottomleft",cex=1.5,pt.cex=2,col="grey20",lty=3,lwd=3,legend="Forecast",bty="n")
}


ComputeModelProjections_daily.f <- function(daysout, intervals, DataDirpath, DataFileName, Outputpath, verbose=T, modelVer=4){

	# Computes prediction intevals for daily cumulative MCF data, based on linear mixed effects model
	# Returns data frame with predictions
	# Prompts user to:
	#     1) save table of projections and prediction intervals
	#     2) plot projections and prediction intervals

	# daysout = int; number of days from end of data to make projections for
	# intervals = vector of integers representing desired prediction intervals; e.g. c(50, 95)
	# DataDirpath = file path to data files
	# DataFileName = name of data file
	# Outputpath = file path for output
	# verbose = boolean for whether or not to show progress messages

	if(verbose) { cat(paste0("\n||||||||>  Xpansiv RINs Forecast Model, version: ", modelVer, "\n")) }

	# Get data from file; check for file header row
	if(verbose) { cat(paste("\n||||||||> Loading data file: ", DataFileName)) }
	rawdata <- load_DataFile.f(file.path(DataDir,"Data",DataFileName))
	names(rawdata)[1:2] <- c("flow_date","mcf_volume_actual")
	if(ncol(rawdata > 2)){ rawdata <- rawdata[,1:2] }

	# SET UP
	if(verbose) { cat("\n||||||||> Prepping data") }
	# Construct data frame
	out.l <- rinDF.f(rawdata)
	bp.rins <- out.l[[1]]
	seasons.df <- out.l[[2]]

	diff.mcf <- diff(bp.rins$mcf_volume_actual)
	bp.rins <- bp.rins[order(bp.rins$flow_date),]
	bp.rins$diff.mcf <- c(NA,diff.mcf)

	# Create the model needed to make predictions...
	cat("\n||||||||> Fitting model; this may take some time...\n")
	lme1 <- lmeMod.f(bp.rins)

	if(verbose) { cat("\n||||||||> Getting forecast") }
	# Set date range for forecast
	date.seq <- get_DateRange.f(bp.rins, "flow_date", predictionDays)

	# Construct predictors data frame
	predictors.df <- construct_predictorsDF.f(date.seq, seasons.df)
	# Compute forecast

	# Generate prediction intervals
	predict <- predIntv.f(bp.rins, lme1, predictors.df, predictionLimits)

	# Format results in table
	if(verbose) { cat("\n||||||||> Here are the model predictions:\n\n") }
	PI_table <- predTable.f(predict)
	print(PI_table)

	ans <- readline(prompt="\n\n||||||||> SAVE TABLE TO FILE? (y/n): ")
	if(ans == "y" || ans == "Y"){ 
		outfilename <- paste0("prediction table__",DataFile, "__.txt")
		try(write_PredTable.f(PI_table, file.path(DataDir,OutputDir,outfilename)))
	}

	# Plot the prediction
	ans <- readline(prompt="\n||||||||> SHOW DATA PLOT? (y/n): ")
	if(ans == "y" || ans == "Y"){ plotPred.f(predict) }

	if(verbose) { cat("\n||||||||> END PROGRAM\n\n") }
}




















