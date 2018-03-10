## Trase for development finance
## combine data for all years into one file, drop some variables, aggregate by country


library(readxl)
library(data.table)
library(dplyr)

# set location of files and get all file paths
din <- 'C:/Users/laura.delduca/Desktop/Data analysis/data'

ff <- list.files(din, full = TRUE)

# create an empty list to store the data of each file
J <- list()

i = 1
# for each file...
for (f in ff){
	# read the file
	j <- fread(f)
	# get index of all rows that have NAs across all columns
	k <- which( apply(j, 1, function(x) all(is.na(x))) )
	# remove those rows with all NAs
	if(length(k)>0) j<- j[-k,]
	# add the data to the list
	J[[i]] <- j
	i <- i + 1
}

# append all data, earlier stored in a list of dataframes in J
D <- do.call(rbind, J)


# select only variables of interest

D <- select(D, Year, DonorCode, DonorName, AgencyCode, AgencyName, 
RecipientCode, RecipientName, RegionCode, RegionName, IncomegroupCode, 
IncomegroupName, FlowCode, FlowName, bi_multi, usd_commitment, usd_disbursement, 
ShortDescription, ProjectTitle, PurposeCode, PurposeName, SectorCode, 
SectorName, ChannelCode, ChannelName)


# aggregate by country
# not yet useful?


# write file
write.csv2(D, 'CRS_1973_2016_data.csv', quote = FALSE, row.names = FALSE)


## to clear the R environment
rm(list = ls())
