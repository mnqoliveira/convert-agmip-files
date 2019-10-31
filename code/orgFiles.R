# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("zoo")


# Source functions --------------------------------------------------------

filterLength <- function(x, length_x){
  
  if(length(x) == length_x){
    x_mod <- x[length(x) == length_x]
  } else { 
    x_mod <- rep(NA, length_x)
  }
  
  return(x_mod)
  
}

roundDigits <- function(data, x){
  
   data[, x$retrieve] <- round(data[, x$retrieve], x$digits)
  
}

# Load files --------------------------------------------------------------
dssatFiles <- list.files("../data/", full.names = TRUE, pattern = ".OUT")
namesFiles <- list.files("../data/", full.names = FALSE, pattern = ".OUT")
namesFiles <- substring(namesFiles, 1, nchar(namesFiles) - 4)
columns_daily <- read.csv("../data/equivalence_daily.csv")
columns_summary <- read.csv("../data/equivalence_summary.csv")


# Read files --------------------------------------------------------------
# Extract inputs
outputs <- list()
treatments <- list()

for (it in 1:length(dssatFiles)){
  
  temp <- readLines(dssatFiles[it])
  
  colsOUT_pos <- grep('@', temp)
  colsOUT <- temp[colsOUT_pos[1]]
  colsOUT <- unlist(strsplit(colsOUT, " "))
  colsOUT <- colsOUT[colsOUT != ""]

  values <- trimws(gsub("\\s+", " ", temp))
  values <- strsplit(values, " ")
  values2 <- lapply(values, filterLength, length(colsOUT))
  
  # For some reason I wasn't able to directly convert the list to a dataframe.
  df <- matrix(unlist(values2), ncol = length(colsOUT), byrow = TRUE)
  df <- as.data.frame(df)
  colsOUT <- gsub("@", "", colsOUT)
  colnames(df) <- colsOUT

  outputs[[namesFiles[it]]] <- df
  
  # For this to work, one file can only have one treatment. This doesn't 
  # deal with multiple treatments per file.
  sub_treatments <- temp[grep("TREATMENT", temp)]
  sub_treatments <- strsplit(sub_treatments[1], ":")
  sub_treatments <- sub_treatments[[1]][1]
  grep("[[:digit:]]", sub_treatments, value = T)
  treatments[[namesFiles[it]]] <- gsub("[^0-9.]", "", sub_treatments)
  
}


# Process daily files -----------------------------------------------------
# Daily Files are retrieved from the list and only the columns that are present
# in the AgMIP format are kept. Runs are manually included based on the number
# of times "@" appears in the file. These columns are all merged in a dataframe.

# Merge
fullFiles <- namesFiles[namesFiles != "Evaluate"]
for (it in 1:length(fullFiles)){
  
  temp <- outputs[[fullFiles[it]]]
  temp$RUN <- NA
  
  # Include run number
  pos_init <- grep('@', temp$YEAR)
  temp$RUN[pos_init] <- seq(1, length(pos_init))
  
  # Remove unwanted columns and lines
  cols_filt <- colnames(temp)[colnames(temp) %in% columns_daily$retrieve]
  temp <- temp[, c("RUN", cols_filt)]
  # This next line will raise warnings, but the behavior is expected, given
  # it's trying to coerce text into numeric.
  temp <- as.data.frame(apply(temp, 2, as.numeric))
  
  # Include run number in all rows and remove those that are incomplete
  temp$RUN[1] <- 0
  temp$RUN <- zoo::na.locf(temp$RUN)
  
  # Even though Treatment is not a variable, it can be treated as one, since
  # it is in the final template 
  temp$TT <- treatments[[fullFiles[it]]]
  
  temp <- temp[complete.cases(temp), ]

  print(colnames(temp))
  
  if(it == 1){

    dailyData <- temp

  } else {

    dailyData <- merge(dailyData, temp, by = c("RUN", "YEAR", "DOY", "DAS", 
                                               "TT"),
                       sort = FALSE)
    dailyData <- dailyData[complete.cases(dailyData), ]

  }

}

# Rename columns
dailyData <- dailyData[, -match(c("RUN", "DAS"), colnames(dailyData))]
dailyData$DOY <- as.Date(paste(dailyData$YEAR, dailyData$DOY, sep = "-"),
                         format = "%Y-%j")

namesOutFull <- sort(match(colnames(dailyData), columns_daily$retrieve))
dailyData <- dailyData[, columns_daily$retrieve[namesOutFull]]
colnames(dailyData) <- columns_daily$final[namesOutFull]

# Adjust the number of digits
# Only works properly if there are only two cases: 0 and 1 digits
col_round <- columns_daily$final[columns_daily$digits != 0]
dailyData[, col_round] <- apply(dailyData[, col_round], 2, round, 1)

cond <- colnames(dailyData)[!colnames(dailyData) %in% col_round]
cond <- cond[sapply(dailyData[, cond], is.numeric)]
dailyData[, cond] <- apply(dailyData[, cond], 2, round, 0)

# Write
# Format output so that everything is on the right

dailyData$Model <- "DN"
dailyData$cumPARi <- "na"

orderColumns <- columns_daily$final[nchar(columns_daily$final) > 1]
orderColumns <- orderColumns[orderColumns %in% colnames(dailyData)]
dailyData <- dailyData[, orderColumns]


headerDaily <- c("AgMIP_Wheat_4_ROTS																				
Model:																				
Modeler_name:																				
Simulation:																				
Site:																				
Model	Year	Date	Treatment	Yield	Biom	LAI	WDrain	CumET	SoilAvW	Runoff	Transp	CroN	Nleac	GrainN	Nmin	Nvol	Nimmo	SoilN	Nden	cumPARi
(2letters)	(YYYY)	(YYYY-MM-DD)	(-)	(t/ha)	(t/ha)	(-)	(mm)	(mm)	(mm)	(mm)	(mm)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(MJ/m²/d)
")

writeLines(headerDaily, con = "../data/test_daily.txt")
write.table(format(dailyData, justify = "right"), 
            file = "../data/test_daily.txt", sep = "\t", 
            append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)


# 
# # Process summary files ---------------------------------------------------
# temp <- outputs[["Evaluate"]]
# 
# # Remove unwanted columns and lines
# cols_filt <- colnames(temp)[colnames(temp) %in% columns_daily$retrieve]
# temp <- temp[, c("RUN", cols_filt)]
# temp <- as.data.frame(apply(temp, 2, as.numeric))
# 
# # Include run number in other rows and remove those incomplete
# temp$RUN[1] <- 0
# temp$RUN <- zoo::na.locf(temp$RUN)
# df <- temp[complete.cases(temp), ]
# 
# print(dim(temp))
# namesFiles[namesFiles != ]
# 
# # Write outputs
# 
# 
# headerSummary <- c("Model	Planting.date	Treatment	Yield	Emergence	Ant	Mat	FLN	GNumber	Biom-an	Biom-ma	MaxLAI	WDrain	CumET	SoilAvW	Runoff	Transp	CroN-an	CroN-ma	Nleac	GrainN	Nmin	Nvol	Nimmo	SoilN	Nden	cumPARi
# (2letters)	(YYYY-MM-DD)	(-)	(t/ha)	(YYYY-MM-DD)	(YYYY-MM-DD)	(YYYY-MM-DD)	(leaf/mainstem)	(grain/m2)	(t/ha)	(t/ha)	(-)	(mm)	(mm)	(mm)	(mm)	(mm)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(kgN/ha)	(MJ/m?)")
