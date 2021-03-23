
#
## 1. Set working directory:
#

setwd("./input-data")

#
## 2. Load R packages:
#

require(openxlsx)


#
## 3. Load data from COVer-AGE-DB:
#

filename <- 'COVer-AGE-DB/Output_5.zip'
Dat <- read_csv(filename,skip=3)

#
## 4. Countries of interest:
# 

all_countries <- unique(Dat$Country)

setwd("./input-data")
source(countries_by_world_region.R)

#
## 5. Load reference and scaled IFRs by age for all countries of interest:
#

IFRs <- read.csv("IFRs.csv",row.names=1,check.names=FALSE,header=TRUE)
IFRs_EA_by_sex <- read.csv("IFRs_EA_by_sex.csv")

## 5.1 IFRs_Verity_scaled:

IFRs_Verity_scaled_central <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Verity_scaled_central) <- all_countries
rownames(IFRs_Verity_scaled_central) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Verity_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Verity_scaled_central[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("Verity_",current_coi,sep="")]
	}
}

IFRs_Verity_scaled_up <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Verity_scaled_up) <- all_countries
rownames(IFRs_Verity_scaled_up) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Verity_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Verity_scaled_up[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("VerityUp_",current_coi,sep="")]
	}
}

IFRs_Verity_scaled_low <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Verity_scaled_low) <- all_countries
rownames(IFRs_Verity_scaled_low) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Verity_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Verity_scaled_low[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("VerityLow_",current_coi,sep="")]
	}
}

## 5.2 IFRs_Verity_original:

IFRs_Verity_original <- matrix(NA,nr=length(seq(0,95,5)),nc=3)
colnames(IFRs_Verity_original) <- c("central","low","up")
rownames(IFRs_Verity_original) <- seq(0,95,5)

IFRs_Verity_original[,"central"] <-  IFRs[as.character(seq(2.5,97.5,5)),"Verity"]
IFRs_Verity_original[,"low"] <-  IFRs[as.character(seq(2.5,97.5,5)),"VerityLow"]
IFRs_Verity_original[,"up"] <-  IFRs[as.character(seq(2.5,97.5,5)),"VerityUp"] 

## 5.3 IFRs_Levin_scaled:

IFRs_Levin_scaled_central <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Levin_scaled_central) <- all_countries
rownames(IFRs_Levin_scaled_central) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Levin_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Levin_scaled_central[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("Levin_",current_coi,sep="")]
	}
}

IFRs_Levin_scaled_up <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Levin_scaled_up) <- all_countries
rownames(IFRs_Levin_scaled_up) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Levin_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Levin_scaled_up[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("LevinUp_",current_coi,sep="")]
	}
}

IFRs_Levin_scaled_low <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Levin_scaled_low) <- all_countries
rownames(IFRs_Levin_scaled_low) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Levin_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Levin_scaled_low[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("LevinLow_",current_coi,sep="")]
	}
}

## 5.4 IFRs_Levin_original:

IFRs_Levin_original <- matrix(NA,nr=length(seq(0,95,5)),nc=3)
colnames(IFRs_Levin_original) <- c("central","low","up")
rownames(IFRs_Levin_original) <- seq(0,95,5)

IFRs_Levin_original[,"central"] <-  IFRs[as.character(seq(2.5,97.5,5)),"Levin"]
IFRs_Levin_original[,"low"] <-  IFRs[as.character(seq(2.5,97.5,5)),"LevinLow"]
IFRs_Levin_original[,"up"] <-  IFRs[as.character(seq(2.5,97.5,5)),"LevinUp"]

## 5.5 IFRs_Salje_scaled:

IFRs_Salje_scaled_central <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Salje_scaled_central) <- all_countries
rownames(IFRs_Salje_scaled_central) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Salje_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Salje_scaled_central[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("Salje_",current_coi,sep="")]
	}
}

IFRs_Salje_scaled_up <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Salje_scaled_up) <- all_countries
rownames(IFRs_Salje_scaled_up) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Salje_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Salje_scaled_up[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("SaljeUp_",current_coi,sep="")]
	}
}

IFRs_Salje_scaled_low <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_Salje_scaled_low) <- all_countries
rownames(IFRs_Salje_scaled_low) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Salje_",current_coi,sep="") %in% colnames(IFRs)){
		IFRs_Salje_scaled_low[,current_coi] <- IFRs[as.character(seq(2.5,97.5,5)),paste("SaljeLow_",current_coi,sep="")]
	}
}

## 5.6 IFRs_Salje_original:

IFRs_Salje_original <- matrix(NA,nr=length(seq(0,95,5)),nc=3)
colnames(IFRs_Salje_original) <- c("central","low","up")
rownames(IFRs_Salje_original) <- seq(0,95,5)

IFRs_Salje_original[,"central"] <-  IFRs[as.character(seq(2.5,97.5,5)),"Salje"]
IFRs_Salje_original[,"low"] <-  IFRs[as.character(seq(2.5,97.5,5)),"SaljeLow"]
IFRs_Salje_original[,"up"] <-  IFRs[as.character(seq(2.5,97.5,5)),"SaljeUp"]

## 5.7 IFRs_m_Acosta_scaled:
#### for males

IFRs_m_Acosta_scaled_central <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_m_Acosta_scaled_central) <- all_countries
rownames(IFRs_m_Acosta_scaled_central) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Acosta_",current_coi,"_m",sep="") %in% colnames(IFRs_EA_by_sex)){
		IFRs_m_Acosta_scaled_central[,current_coi] <- IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),paste("Acosta_",current_coi,"_m",sep="")]
	}
}

IFRs_m_Acosta_scaled_up <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_m_Acosta_scaled_up) <- all_countries
rownames(IFRs_m_Acosta_scaled_up) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Acosta_",current_coi,"_m",sep="") %in% colnames(IFRs_EA_by_sex)){
		IFRs_m_Acosta_scaled_up[,current_coi] <- IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),paste("AcostaUp_",current_coi,"_m",sep="")]
	}
}

IFRs_m_Acosta_scaled_low <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_m_Acosta_scaled_low) <- all_countries
rownames(IFRs_m_Acosta_scaled_low) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Acosta_",current_coi,"_m",sep="") %in% colnames(IFRs_EA_by_sex)){
		IFRs_m_Acosta_scaled_low[,current_coi] <- IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),paste("AcostaLow_",current_coi,"_m",sep="")]
	}
}

## 5.8 IFRs_m_Acosta_original:
#### for males

IFRs_m_Acosta_original <- matrix(NA,nr=length(seq(0,95,5)),nc=3)
colnames(IFRs_m_Acosta_original) <- c("central","low","up")
rownames(IFRs_m_Acosta_original) <- seq(0,95,5)

IFRs_m_Acosta_original[,"central"] <-  IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),"Acosta_m"]
IFRs_m_Acosta_original[,"low"] <-  IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),"AcostaLow_m"]
IFRs_m_Acosta_original[,"up"] <-  IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),"AcostaUp_m"]

## 5.9 IFRs_f_Acosta_scaled:
#### for females

IFRs_f_Acosta_scaled_central <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_f_Acosta_scaled_central) <- all_countries
rownames(IFRs_f_Acosta_scaled_central) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Acosta_",current_coi,"_f",sep="") %in% colnames(IFRs_EA_by_sex)){
		IFRs_f_Acosta_scaled_central[,current_coi] <- IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),paste("Acosta_",current_coi,"_f",sep="")]
	}
}

IFRs_f_Acosta_scaled_up <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_f_Acosta_scaled_up) <- all_countries
rownames(IFRs_f_Acosta_scaled_up) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Acosta_",current_coi,"_f",sep="") %in% colnames(IFRs_EA_by_sex)){
		IFRs_f_Acosta_scaled_up[,current_coi] <- IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),paste("AcostaUp_",current_coi,"_f",sep="")]
	}
}

IFRs_f_Acosta_scaled_low <- matrix(NA,nr=length(seq(0,95,5)),nc=length(all_countries))
colnames(IFRs_f_Acosta_scaled_low) <- all_countries
rownames(IFRs_f_Acosta_scaled_low) <- seq(0,95,5)

for(country in 1:length(all_countries)){
	current_coi <- all_countries[country]  
	if(paste("Acosta_",current_coi,"_f",sep="") %in% colnames(IFRs_EA_by_sex)){
		IFRs_f_Acosta_scaled_low[,current_coi] <- IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),paste("AcostaLow_",current_coi,"_f",sep="")]
	}
}

## 5.10 IFRs_f_Acosta_original:
#### for females

IFRs_f_Acosta_original <- matrix(NA,nr=length(seq(0,95,5)),nc=3)
colnames(IFRs_f_Acosta_original) <- c("central","low","up")
rownames(IFRs_f_Acosta_original) <- seq(0,95,5)

IFRs_f_Acosta_original[,"central"] <-  IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),"Acosta_m"]
IFRs_f_Acosta_original[,"low"] <-  IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),"AcostaLow_m"]
IFRs_f_Acosta_original[,"up"] <-  IFRs_EA_by_sex[as.character(seq(2.5,97.5,5)),"AcostaUp_m"]

#
## 6. Population median age from UNWPP 2019:
#

median_age <- read.xlsx("WPP2019_POP_F05_MEDIAN_AGE.xlsx",sheet = 1,startRow = 17)

#
## 7. Population count by age from UNWPP 2019:
#

wom <- read.xlsx("WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx",sheet = 1,startRow = 17)
wom_select <- wom[which(wom[,"Reference.date.(as.of.1.July)"]=="2019"),c(3,8:109)] 
## wom_select[1:2,]

##

men <- read.xlsx("WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx",sheet = 1,startRow = 17)
men_select <- men[which(men[,"Reference.date.(as.of.1.July)"]=="2019"),c(3,8:109)] ## men[,c(3,8:109)]
## men_select[1:2,]

##

pop_count <- matrix(NA,nr=length(seq(0,100,1)),nc=length(countries_by_world_region[,1]))
rownames(pop_count) <- seq(0,100,1)
colnames(pop_count) <- countries_by_world_region[,1]

for(country in 1:length(countries_by_world_region[,1])){

	current_coi <- countries_by_world_region[country,1]
	current_coi_long <- current_coi
	if(current_coi == "USA"){
		current_coi_long <- "United States of America"
	}
	if(current_coi == "Venezuela"){
		current_coi_long <- "Venezuela (Bolivarian Republic of)"
	}
	if(current_coi == "Bolivia"){
		current_coi_long <- "Bolivia (Plurinational State of)"
	}
	if(current_coi == "South Korea"){
		current_coi_long <- "Republic of Korea"
	}
	if(current_coi == "Taiwan"){
		current_coi_long <- "China, Taiwan Province of China"
	}
	if(current_coi == "Palestine"){
		current_coi_long <- "State of Palestine"
	}

	current_pop_count <- as.numeric(wom_select[which(wom_select["Region,.subregion,.country.or.area.*"]==current_coi_long),3:103]) + as.numeric(men_select[which(men_select["Region,.subregion,.country.or.area.*"]==current_coi_long),3:103])

	pop_count[,country] <- current_pop_count
}

#
## 8. Remaining life expectancy UNWPP 2019:
#

lt_1950_2020 <- read.xlsx("WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",sheet = 1,startRow = 17)

#
## 9. Excess deaths from STMF:
#

setwd("./input-data")
excess_deaths <- readRDS("excess_array.rds")


