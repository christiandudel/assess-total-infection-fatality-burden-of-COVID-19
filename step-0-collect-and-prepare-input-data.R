library(covidAgeData)
library(here)
library(DemoTools) # remotes::install_github("timriffe/DemoTools")
library(readxl)
library(tidyverse)
#
## Set working directory:
#

#setwd("./input-data")

#
## 0. Some help functions
#

get_ungrouped_lx_100 <- function(current_lx_data){
  # TR: how about spline(x=c(0,1,seq(5,100,5)),y=current_lx_data, method = "hyman")
  #   for a monotonicity constraint?
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

download_data <- TRUE
if (download_data){	

  ## Online source	
  url <- 'https://osf.io/7tnfh/download'
  
  cdb_dir <- here::here("input-data","COVerAGE-DB")
  if (!dir.exists(cdb_dir)){
    dir.create(cdb_dir)
  }
  # TR: always downloads the most recent version. Fine until we submit.
  # The file will be zipped. No need to unzip it in order to read it
  # in however.
  download_covid("Output_5",
                 progress = TRUE, 
                 dest = cdb_dir, 
                 download_only = TRUE)
} 


#
## 2. Download and save data from UNWPP 2019
#

base_url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/"

file_names <- c("WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx",
                "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx",
                "WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",
                "WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx",
                "WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx",
                "WPP2019_POP_F05_MEDIAN_AGE.xlsx")
wpp_folders <- c("5_Interpolated/",
                 "5_Interpolated/",
                 "3_Mortality/",
                 "3_Mortality/",
                 "3_Mortality/",
                 "1_Population/")

#
for (i in 1:6){
  this_local_file <-  here::here("input-data",  file_names[i])
  if (!file.exists(this_local_file)){
    this.url  <- paste0(base_url,
                        wpp_folders[i],
                        file_names[i])
    download.file(url = this.url,
                  destfile = this_local_file)
  }
}

#
## 3. Generate population-weighted ex for scaling reference IFR of Levin et al. 
#

## 3.1 Load UNWPP2019 population counts in 2019:

wom <- read_excel(here::here("input-data","WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"),
                  sheet = 1,
                  skip = 16,
                  na = "...") %>% 
  dplyr::filter(`Reference date (as of 1 July)` == 2019) %>% 
  dplyr::select(Location = `Region, subregion, country or area *`,
                Year = `Reference date (as of 1 July)`,
                `0`:`100`) %>% 
  pivot_longer( `0`:`100`, names_to = "Age", values_to = "Population") %>% 
  dplyr::filter(!is.na(Population)) 

#

men <- read_excel(here::here("input-data","WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"),
                  sheet = 1,
                  skip = 16,
                  na = "...") %>% 
  dplyr::filter(`Reference date (as of 1 July)` == 2019) %>% 
  dplyr::select(Location = `Region, subregion, country or area *`,
                Year = `Reference date (as of 1 July)`,
                `0`:`100`) %>% 
  pivot_longer( `0`:`100`, names_to = "Age", values_to = "Population") %>% 
  filter(!is.na(Population)) 


## 3.2 Vector with UN names for major countries used in metaregression of Levin et al. (2020): 
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

## 3.3 Load & filter UNWPP 2019 remaining life expectancy:
lt_survivors <- read_excel(here::here("input-data","WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx"),
           sheet = 1,
           skip = 16,
           na = "...") %>%
 dplyr::select(Location = `Region, subregion, country or area *`,
               Period,
               Age = `Age (x)`,
               nMx = `Central death rate m(x,n)`,
               nAx = `Average number of years lived a(x,n)`,
               lx = `Number of survivors l(x)`,
               ex = `Expectation of life e(x)`) %>% 
  dplyr::filter(Location %in% major_countries_Levin,
         Period == "2015-2020")

## 3.4 Collect UN abridged life tables (number of survivors, a_x, m_x) for those countries:

# lt_survivors <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
# rownames(lt_survivors) <- c(0,1,seq(5,100,5))
# colnames(lt_survivors) <- major_countries_Levin
# 
# for(country in 1:length(major_countries_Levin)){
# 	coi <- major_countries_Levin[country]
# 	current_lt <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Number.of.survivors.l(x)"])
# 	lt_survivors[,country] <- current_lt
# }
# 
# lt_survivors 

##
##
# 
# lt_ax <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
# rownames(lt_ax) <- c(0,1,seq(5,100,5))
# colnames(lt_ax) <- major_countries_Levin
# 
# for(country in 1:length(major_countries_Levin)){
# 	coi <- major_countries_Levin[country]
# 	current_lt_ax <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Average.number.of.years.lived.a(x,n)"])
# 	lt_ax[,country] <- current_lt_ax
# }
# 
# lt_ax
# 
# ##
# ##
# 
# lt_mx <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
# rownames(lt_mx) <- c(0,1,seq(5,100,5))
# colnames(lt_mx) <- major_countries_Levin
# 
# for(country in 1:length(major_countries_Levin)){
# 	coi <- major_countries_Levin[country]
# 	current_lt_mx <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Central.death.rate.m(x,n)"])
# 	lt_mx[,country] <- current_lt_mx
# }
# 
# lt_mx
# 
# ##
# ##
# 
# 
# lt_ex <- matrix(NA,nr=length(c(0,1,seq(5,100,5))),nc=length(major_countries_Levin))
# rownames(lt_ex) <- c(0,1,seq(5,100,5))
# colnames(lt_ex) <- major_countries_Levin
# 
# for(country in 1:length(major_countries_Levin)){
# 	coi <- major_countries_Levin[country]
# 	current_lt_ex <- as.numeric(lt_1950_2020[which(lt_1950_2020["Region,.subregion,.country.or.area.*"]==coi & lt_1950_2020["Period"]=="2015-2020"),"Expectation.of.life.e(x)"])
# 	lt_ex[,country] <- current_lt_ex
# }
# 
# lt_ex

## 3.5 Calculate life table assigning equal weight to major_countries_Levin:

agg_lt <-
  lt_survivors %>% 
  group_by(Age) %>% 
  summarize(nMx = mean(nMx))

# TR: alternatively, one could send in ndx as Deaths and nLx as exposures,
# presumably arithmetic averaged. Result could be slightly different. This also differs
# from results after taking means of nAx, lx, nMx 

lt_unweighted <- lt_abridged2single(nMx = agg_lt$nMx, Age = agg_lt$Age)


# lt_survivors_single_age  <- get_ungrouped_lx_100(current_lx_data=rowMeans(lt_survivors)) 
# ax                       <- c(rowMeans(lt_ax)[1],rep(0.5,length(seq(2,101,1))))
# lt_mx_single_age         <- get_ungrouped_lx_100(current_lx_data=rowMeans(lt_mx)) 
# ex_major_countries_Levin <- lexp_age_specific(lx=as.vector(lt_survivors_single_age$y),ax=ax,mx=as.vector(lt_mx_single_age$y)) 
# names(ex_major_countries_Levin) <- 0:100
# ex_major_countries_Levin 

## 3.6 Collect UN population counts for those countries and calculate population weights:

pop_weights <-
  wom %>% 
  bind_rows(men) %>% 
  dplyr::filter(Location %in% major_countries_Levin) %>% 
  group_by(Location) %>% 
  summarize(Population = sum(Population), .groups = "drop") %>% 
  mutate(Prop = Population / sum(Population))
  
# pop_count_l <- matrix(NA,nr=length(seq(0,100,1)),nc=length(major_countries_Levin))
# rownames(pop_count_l) <- seq(0,100,1)
# colnames(pop_count_l) <- major_countries_Levin
# 
# for(country in 1:length(major_countries_Levin)){
#   # TR: coi = country of interest?
# 	coi <- major_countries_Levin[country]
# 	current_pop_count <- as.numeric(wom_select[which(wom_select["Region,.subregion,.country.or.area.*"]==coi),3:103]) + 
# 	  as.numeric(men_select[which(men_select["Region,.subregion,.country.or.area.*"]==coi),3:103])
# 	pop_count_l[,country] <- current_pop_count
# }

# pop_weights <- colSums(pop_count_l) / sum(colSums(pop_count_l))

## 3.7 Calculate life table assigning population weight to major_countries_Levin:

#### 3.7.1 Ungroup population weighted average of lt_survivors into single years of age:

# lt_survivors_single_age_pop_weight <- 
#   get_ungrouped_lx_100(current_lx_data = rowSums(t(apply(X = lt_survivors, 1, FUN = function(x){pop_weights * x})))) 

#### 3.7.2 Determine ax for single years of age using population weighted average of major countries for age 0:

# ax_pop_weight <- c(rowSums(t(apply(X=lt_ax,1,FUN=function(x){pop_weights*x})))[1],rep(0.5,length(seq(2,101,1))))
# 
# #### 3.7.3 Ungroup population weighted average lt_mx into single years of age:
# 
# lt_mx_single_age_pop_weight <- get_ungrouped_lx_100(current_lx_data=rowSums(t(apply(X=lt_mx,1,FUN=function(x){pop_weights*x})))) 
# --------------------------------
# TR redux- just weight nMx for now
wt_lt <-
  lt_survivors %>% 
  left_join(pop_weights) %>% 
  group_by(Age) %>% 
  summarize(nMx = sum(nMx * Prop))

lt_weighted <-  lt_abridged2single(nMx = wt_lt$nMx, Age = wt_lt$Age)

# TR: get names ex vectors for saving
ex_major_countries_Levin        <- lt_unweighted$ex
names(ex_major_countries_Levin) <- lt_unweighted$Age

ex_major_countries_Levin_pop_weight        <- lt_weighted$ex
names(ex_major_countries_Levin_pop_weight) <- lt_weighted$Age
### 3.7.4 Compute life table ex based on lx, ax, and mx by single years of age:

# ex_major_countries_Levin_pop_weight <- lexp_age_specific(lx=as.vector(lt_survivors_single_age_pop_weight$y),ax=ax_pop_weight,mx=as.vector(lt_mx_single_age_pop_weight$y)) 
# names(ex_major_countries_Levin_pop_weight) <- 0:100
# ex_major_countries_Levin_pop_weight

## 3.8 Save output: 
dump("ex_major_countries_Levin_pop_weight",
     file=here::here("input-data", "ex_major_countries_Levin_pop_weight_TR.R"))
dump("ex_major_countries_Levin",
     file=here::here("input-data", "ex_major_countries_Levin_TR.R"))

# dump("ex_major_countries_Levin_pop_weight",
#      file="ex_major_countries_Levin_pop_weight.R")
# dump("ex_major_countries_Levin",
#      file="ex_major_countries_Levin.R")

#
## 4. Scaling reference IFRs  
#


#
## 5. Estimating excess mortality deaths  
#


