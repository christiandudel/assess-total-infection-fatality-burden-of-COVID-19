

#
## Set working directory:
#

setwd(./input-data)

#
## 0. Some help functions
#

get_ungrouped_lx_100 <- function(current_lx_data){
	smooth_current_lx_data <- smooth.spline(x=c(0,1,seq(5,100,5)),y=current_lx_data)
	new_x <- c(seq(0,1,1),seq(2,5,1),seq(6,100,1))
	predict_smooth_current_lx_data <- predict(smooth_current_lx_data,new_x,len=new_x)
	return(predict_smooth_current_lx_data)
}

##
##

lexp_age_specific <- function(lx,ax,mx){  

		dx <- c(-diff(lx), lx[length(lx)])  
  
            Lx1 <- lx[-1]+ax[1:(length(ax)-1)]*dx[1:(length(dx)-1)]  
  
            Lx2 <- if(mx[length(mx)] == 0){ 

                0}else{  
  
                    dx[length(dx)]/mx[length(mx)]  
  
                }  
  
            Lx <- c(Lx1, Lx2)  
  
            Tx <- rev(cumsum(rev(Lx)))  
  
            ex <- Tx/lx  
  
            ex[is.nan(ex)] <- 0  
  
            return(ex)    
  
}  

#
## 1. Download and save data from COVer-AGE-DB
#

library(tidyverse)
library(httr)

download_data <- TRUE

if(download_data==TRUE){	

  ## Online source	
  url <- 'https://osf.io/7tnfh/download'
  
  ## Create a folder *Data* 
  if (!"COVer-AGE-DB" %in% dir()){
    dir.create("COVer-AGE-DB")
  }
  
  ## Filename and where to save it on your computer
  filename <- 'COVer-AGE-DB/Output_5.zip'
  
  ## Download the data
  GET(url, write_disk(filename, overwrite = TRUE))
  
  ## Unzip the downloaded and saved zip file 
  unzip(filename)

} ## if download_data


#
## 2. Download and save data from UNWPP 2019
#

#### Please go to UNWPP2019 website and download data files as described in "information-on-downloading-input-data.txt".

#
## 3. Generate population-weighted ex for scaling reference IFR of Levin et al. 
#

## 3.1 Load UNWPP2019 population counts in 2019:

wom <- read.xlsx(file.path(the.data.path,paste("WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx",sep="")),sheet = 1,startRow = 17)
wom_select <- wom[which(wom[,"Reference.date.(as.of.1.July)"]=="2019"),c(3,8:109)] 

men <- read.xlsx(file.path(the.data.path,paste("WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx",sep="")),sheet = 1,startRow = 17)
men_select <- men[which(men[,"Reference.date.(as.of.1.July)"]=="2019"),c(3,8:109)] ## men[,c(3,8:109)]

## 3.2 Load UNWPP 2019 remaining life expectancy:

lt_1950_2020 <- read.xlsx("WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",sheet = 1,startRow = 17)

## 3.3 Vector with UN names for major countries used in metaregression of Levin et al. (2020): 

major_countries_Levin <- c("United Kingdom",
					"Ireland",
					"Italy",
					"Netherlands",
					"Portugal",
					"Spain",
					"Switzerland",
					"United States of America",
					"Belgium",
					"France",
					"Sweden",
					"Australia",
					"Iceland",
					"Republic of Korea",
					"Lithuania",
					"New Zealand")

## 3.4 Collect UN abridged life tables (number of survivors, a_x, m_x) for those countries:

lt_survivors <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
rownames(lt_survivors) <- c(0,1,seq(5,100,5))
colnames(lt_survivors) <- major_countries_Levin

for(country in 1:length(major_countries_Levin)){
	coi <- major_countries_Levin[country]
	current_lt <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Number.of.survivors.l(x)"])
	lt_survivors[,country] <- current_lt
}

lt_survivors 

##
##

lt_ax <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
rownames(lt_ax) <- c(0,1,seq(5,100,5))
colnames(lt_ax) <- major_countries_Levin

for(country in 1:length(major_countries_Levin)){
	coi <- major_countries_Levin[country]
	current_lt_ax <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Average.number.of.years.lived.a(x,n)"])
	lt_ax[,country] <- current_lt_ax
}

lt_ax

##
##

lt_mx <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
rownames(lt_mx) <- c(0,1,seq(5,100,5))
colnames(lt_mx) <- major_countries_Levin

for(country in 1:length(major_countries_Levin)){
	coi <- major_countries_Levin[country]
	current_lt_mx <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Central.death.rate.m(x,n)"])
	lt_mx[,country] <- current_lt_mx
}

lt_mx

##
##


lt_ex <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
rownames(lt_ex) <- c(0,1,seq(5,100,5))
colnames(lt_ex) <- major_countries_Levin

for(country in 1:length(major_countries_Levin)){
	coi <- major_countries_Levin[country]
	current_lt_ex <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Expectation.of.life.e(x)"])
	lt_ex[,country] <- current_lt_ex
}

lt_ex

## 3.5 Calculate life table assigning equal weight to major_countries_Levin:

lt_survivors_single_age <- get_ungrouped_lx_100(current_lx_data=rowMeans(lt_survivors)) 
ax <- c(rowMeans(lt_ax)[1],rep(0.5,length(seq(2,101,1))))
lt_mx_single_age <- get_ungrouped_lx_100(current_lx_data=rowMeans(lt_mx)) 
ex_major_countries_Levin <- lexp_age_specific(lx=as.vector(lt_survivors_single_age$y),ax=ax,mx=as.vector(lt_mx_single_age$y)) 
names(ex_major_countries_Levin) <- 0:100
ex_major_countries_Levin 

## 3.6 Collect UN population counts for those countries and calculate population weights:

pop_count_l <- matrix(NA,nr=length(seq(0,100,1)),nc=length(major_countries_Levin))
rownames(pop_count_l) <- seq(0,100,1)
colnames(pop_count_l) <- major_countries_Levin

for(country in 1:length(major_countries_Levin)){
	coi <- major_countries_Levin[country]
	current_pop_count <- as.numeric(wom_select[which(wom_select["Region,.subregion,.country.or.area.*"]==coi),3:103]) + as.numeric(men_select[which(men_select["Region,.subregion,.country.or.area.*"]==coi),3:103])
	pop_count_l[,country] <- current_pop_count
}

pop_weights <- colSums(pop_count_l) / sum(colSums(pop_count_l))

## 3.7 Calculate life table assigning population weight to major_countries_Levin:

#### 3.7.1 Ungroup population weighted average of lt_survivors into single years of age:

lt_survivors_single_age_pop_weight <- get_ungrouped_lx_100(current_lx_data=rowSums(t(apply(X=lt_survivors,1,FUN=function(x){pop_weights*x})))) 

#### 3.7.2 Determine ax for single years of age using population weighted average of major countries for age 0:

ax_pop_weight <- c(rowSums(t(apply(X=lt_ax,1,FUN=function(x){pop_weights*x})))[1],rep(0.5,length(seq(2,101,1))))

#### 3.7.3 Ungroup population weighted average lt_mx into single years of age:

lt_mx_single_age_pop_weight <- get_ungrouped_lx_100(current_lx_data=rowSums(t(apply(X=lt_mx,1,FUN=function(x){pop_weights*x})))) 

### 3.7.4 Compute life table ex based on lx, ax, and mx by single years of age:

ex_major_countries_Levin_pop_weight <- lexp_age_specific(lx=as.vector(lt_survivors_single_age_pop_weight$y),ax=ax_pop_weight,mx=as.vector(lt_mx_single_age_pop_weight$y)) 
names(ex_major_countries_Levin_pop_weight) <- 0:100
ex_major_countries_Levin_pop_weight

## 3.8 Save output: 

dump("ex_major_countries_Levin_pop_weight",file="ex_major_countries_Levin_pop_weight.R")
dump("ex_major_countries_Levin",file="ex_major_countries_Levin.R")

#
## 4. Scaling reference IFRs  
#


#
## 5. Estimating excess mortality deaths  
#


