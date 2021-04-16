
#
## 1. Set working directory:
#

setwd(".")

## 2. Sensitivity of IFR
#### towards I_x based on either reported deaths (COVer-AGE-DB) or excess mortality deaths (estimated based on data of STMF)

## 2.1 Align country names between excess mortality deaths (estimated based on data of STMF) and reported deaths (COVer-AGE-DB):

dimnames(excess_deaths)
dimnames(excess_deaths)[[1]][which(dimnames(excess_deaths)[[1]]=="Republic of Korea")] <- "South Korea" 
dimnames(excess_deaths)[[1]][which(dimnames(excess_deaths)[[1]]=="England_Wales")] <- "England and Wales" 

## 2.2 Compare set of countries and dates between reported and excess mortality deaths:  

dimnames(excess_deaths)
dimnames(deaths_array)

## 2.3 To split 90+ excess deaths into 90-94 and 95+ excess mortality deaths: 

deaths_array_share_95plus_of_90plus <- rowSums(deaths_array[,,as.character(seq(95,100,5))],dims=2,na.rm=TRUE) / rowSums(deaths_array[,,as.character(seq(90,100,5))],dims=2,na.rm=TRUE)

## 2.4 Estimate numbers of infections and total IFR based on excess mortality deaths:

  row_names <- all_countries
  column_names <- as.character(sort(unique(dat_deaths$Date)))
  matrix_names <- seq(0,95,5)
  matrix_names_2 <- c("low95","central","up95")

  infections_array_excessDeaths_Verity_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names,matrix_names_2))
  infections_array_excessDeaths_Salje_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names,matrix_names_2))
  infections_array_excessDeaths_Levin_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names,matrix_names_2))

########
########

  row_names <- all_countries
  column_names <- as.character(sort(unique(dat_deaths$Date)))
  matrix_names <- c("low95","central","up95")

  total_IFR_array_excessDeaths_Verity_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names))
  total_IFR_array_excessDeaths_Salje_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
 								dimnames=list(row_names,column_names,matrix_names))
  total_IFR_array_excessDeaths_Levin_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names))

########
########

  for(date in 1:length(sort(unique(dat_deaths$Date)))){
	print(date)
	current_date <- sort(unique(dat_deaths$Date))[date]
	if(length(which(dimnames(excess_deaths)[[2]]==current_date))>0){
		for(country in 1:length(all_countries)){
			current_coi <- all_countries[country]  
			if(length(which(dimnames(excess_deaths)[[1]]==current_coi))>0){
				if((sum(excess_deaths[current_coi,as.character(current_date),"b",],na.rm=TRUE)>0) & length(excess_deaths[current_coi,as.character(current_date),"b",])==19){
					current_coi_current_date_deaths <- excess_deaths[current_coi,as.character(current_date),"b",]
					## if(is.na(current_coi_current_date_deaths["95"]) & !is.na(current_coi_current_date_deaths["90"])){
						current_to_add_95plus <- current_coi_current_date_deaths["90"] * deaths_array_share_95plus_of_90plus[current_coi,as.character(current_date)]
						current_to_replace_90 <- current_coi_current_date_deaths["90"] - current_to_add_95plus
						current_coi_current_date_deaths["90"] <- current_to_replace_90 
						current_coi_current_date_deaths["95"] <- current_to_add_95plus 
					## }

			#### Verity, scaled:

			## central IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Verity_scaled_central[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Verity_scaled_central[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Verity_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
			total_IFR_array_excessDeaths_Verity_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Verity_scaled_low[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Verity_scaled_low[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Verity_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
			total_IFR_array_excessDeaths_Verity_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Verity_scaled_up[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Verity_scaled_up[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Verity_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
			total_IFR_array_excessDeaths_Verity_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			#### Salje, scaled:

			## central IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Salje_scaled_central[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Salje_scaled_central[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Salje_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
			total_IFR_array_excessDeaths_Salje_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Salje_scaled_low[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Salje_scaled_low[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Salje_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
			total_IFR_array_excessDeaths_Salje_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Salje_scaled_up[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Salje_scaled_up[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Salje_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
			total_IFR_array_excessDeaths_Salje_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			#### Levin, scaled:

			## central IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Levin_scaled_central[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Levin_scaled_central[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Levin_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
			total_IFR_array_excessDeaths_Levin_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Levin_scaled_low[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Levin_scaled_low[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Levin_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
			total_IFR_array_excessDeaths_Levin_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths / IFRs_Levin_scaled_up[1:20,current_coi]
			current_total_ifr <- sum ( IFRs_Levin_scaled_up[1:20,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_excessDeaths_Levin_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
			total_IFR_array_excessDeaths_Levin_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

				}## if length excess deaths
			} ## if excess deaths are available for current country
		} ## for all_countries 
	} ## if for current_date excess deaths are available 
  } ## for date 
  
## 2.5 Calculate difference in total IFR that are based on infections using reported and excess mortality deaths:   

total_IFR_difference_overlap_countries <- total_IFR_array_Verity_scaled[,,"central"]-total_IFR_array_excessDeaths_Verity_scaled[,,"central"]
overlap_countries <- which(!is.na(total_IFR_difference_overlap_countries))

no_of_diff_val <- sort(apply(X=total_IFR_difference_overlap_countries,1,FUN=function(x){length(x[which(!is.na(x))])}),decreasing=TRUE)
no_of_diff_val_largerThan_0 <- no_of_diff_val[which(no_of_diff_val>0)]
sum(no_of_diff_val_largerThan_0)
 
## mean(total_IFR_difference_overlap_countries,na.rm=TRUE) 
## quantile(total_IFR_difference_overlap_countries,probs=c(0.1,0.5,0.9),na.rm=TRUE) 

## total_IFR_difference_overlap_countries_sort_by_median <- rev(sort(apply(X=total_IFR_difference_overlap_countries,1,function(x){median(x,na.rm=TRUE)})))
## total_IFR_difference_overlap_countries_sort_by_median

##
##

no_of_diff_val_largerThan_0_and_deaths_largerThan_199 <- NA
median_diff_val_largerThan_0_and_deaths_largerThan_199 <- NA

for(coi in length(no_of_diff_val_largerThan_0):1){
	current_coi <- names(no_of_diff_val_largerThan_0)[coi]
	current_diff <- total_IFR_difference_overlap_countries[current_coi,]
	current_diff_nonNA <- current_diff[which(!is.na(current_diff))]
	current_dates <- names(current_diff_nonNA)
	if(length(current_dates)>1){
		current_dates_deaths <- apply(deaths_array[current_coi,current_dates,],1,sum)
	}
	if(length(current_dates)==1){
		current_dates_deaths <- sum(deaths_array[current_coi,current_dates,])
	}
	current_dates_deaths_lt_200 <- names(current_dates_deaths[which(current_dates_deaths>=200)]) 
	current_diff_nonNA <- current_diff_nonNA[which(names(current_diff_nonNA)%in%current_dates_deaths_lt_200)]  

	if(length(current_diff_nonNA)>0){
		current_to_add <- length(current_diff_nonNA)
		names(current_to_add) <- current_coi
		no_of_diff_val_largerThan_0_and_deaths_largerThan_199 <- c(no_of_diff_val_largerThan_0_and_deaths_largerThan_199,current_to_add)

		current_median <- median(current_diff_nonNA)
		names(current_median) <- current_coi
		median_diff_val_largerThan_0_and_deaths_largerThan_199 <- c(median_diff_val_largerThan_0_and_deaths_largerThan_199,current_median)
	}

} ## coi

no_of_diff_val_largerThan_0_and_deaths_largerThan_199 <- no_of_diff_val_largerThan_0_and_deaths_largerThan_199[-1]
median_diff_val_largerThan_0_and_deaths_largerThan_199 <- median_diff_val_largerThan_0_and_deaths_largerThan_199[-1]
median_diff_val_largerThan_0_and_deaths_largerThan_199 

##
##

## 2.6 Visualize this difference in total IFR:

setwd("./plots")

dev.off()

pdf(file=paste("total-IFR-difference-reported-vs-excessDeaths.pdf",sep=""), width=15, height=15, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.8,0.0,1.0,0.0))

plot(x=-100,y=-100,ylim=c(1,30.5-4),xlim=c(-6.0,4.5),xlab="",ylab="",main="",axes=FALSE)
title(bquote(atop("Difference in total IFR, in percentage points, estimating " ~ I[x],  "based on reported deaths and excess deaths")),font.main=2)

axis(side=1,at=seq(-4,4,1),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(-4,4,1),labels=TRUE,lwd=3,pos=0)

yy <- 0

segments(x0=-4,x1=4,y0=seq(1,23+yy,1),y1=seq(1,23+yy,1),lty=2,col=grey(0.8),lwd=1)
segments(x0=seq(-4,4,1),x1=seq(-4,4,1),y0=0,y1=23+yy,lty=2,col=grey(0.8),lwd=1)
segments(x0=0,x1=0,y0=0,y1=25.5+yy,col="black",lwd=3)

text(x=0, y=24.8+yy, expression('total IFR['*'IFR'[x]*',I'[x]*'(reported deaths)] <'), col="black",pos=2,cex=0.9,font=2)	
text(x=0, y=23.8+yy, expression('total IFR['*'IFR'[x]*',I'[x]*'(excess deaths)]'), col="black",pos=2,cex=0.9,font=2)	

text(x=0, y=24.8+yy, expression('total IFR['*'IFR'[x]*',I'[x]*'(reported deaths)] >'), col="black",pos=4,cex=0.9,font=2)	
text(x=0, y=23.8+yy, expression('total IFR['*'IFR'[x]*',I'[x]*'(excess deaths)]'), col="black",pos=4,cex=0.9,font=2)	

points(x=seq(-6.0,0.0,length=3),y=rep(27+yy,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(-6.0,0.0,length=3),y=rep(27+yy,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.75)
points(x=seq(-6.0,0.0,length=3),y=rep(26+yy,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(-6.0,0.0,length=3),y=rep(26+yy,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.75)

points(x=2.0,y=26.5+yy,pch=15,cex=2.1,col="black")
segments(x0=2.0,x1=2.0,y0=25.75+yy,y1=27.25+yy)

text(x=2.0,y=25.75+yy,c("Quantile 0.1"),font=2,pos=4,cex=0.8)
text(x=2.0,y=27.25+yy,c("Quantile 0.9"),font=2,pos=4,cex=0.8)
text(x=2.0,y=26.5+yy,c("Median"),font=2,pos=4,cex=0.8)

points(x=3.5,y=26.5+yy,pch=4,cex=1.4,,lwd=2,col="black")
text(x=3.5,y=26.5+yy,"2021-03-14",font=2,pos=4,cex=0.8)

text(x=4.25,y=23.25+yy,"No. of values:",cex=0.8,pos=3)

current_yy <- 1

for(coi in length(median_diff_val_largerThan_0_and_deaths_largerThan_199):1){
	current_coi <- names(rev(sort(median_diff_val_largerThan_0_and_deaths_largerThan_199)))[coi]
	current_diff <- total_IFR_difference_overlap_countries[current_coi,]
	current_diff_nonNA <- current_diff[which(!is.na(current_diff))]
	if(length(current_diff_nonNA)>1){
		deaths_per_date <- apply(deaths_array[current_coi,names(current_diff_nonNA),],1,sum)
		deaths_per_date_min_200 <- deaths_per_date[which(deaths_per_date >= 200)]
	}
	if(length(current_diff_nonNA)==1){
		deaths_per_date <- sum(deaths_array[current_coi,names(current_diff_nonNA),])
		deaths_per_date_min_200 <- deaths_per_date[which(deaths_per_date >= 200)]
	}
	current_diff_nonNA <- current_diff_nonNA[which(deaths_per_date >= 200)] 
	
	if(length(current_diff_nonNA)>0){

		current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
		text(x=-4.5, y=current_yy, current_coi, col=adjustcolor(pal[current_pal],alpha.f=0.6), 
			pos=2,cex=1.0,font=2)	
		rect(xleft=median(current_diff_nonNA,na.rm=TRUE)-0.1,
			xright=median(current_diff_nonNA,na.rm=TRUE)+0.1,
			ybottom=current_yy-0.3,ytop=current_yy+0.3,col=adjustcolor(pal[current_pal],alpha.f=0.6),border=NA,lwd=3)
		segments(x0=quantile(current_diff_nonNA,probs=(0.1),na.rm=TRUE),
			x1=quantile(current_diff_nonNA,probs=(0.9),na.rm=TRUE),
			y0=current_yy,y1=current_yy,col=adjustcolor(pal[current_pal],alpha.f=0.6),lwd=3)

		text(x=4,y=current_yy,length(current_diff_nonNA),cex=0.9,pos=4)

		if(!is.na(current_diff_nonNA["2021-03-14"])){
			points(x=current_diff_nonNA["2021-03-14"],y=current_yy,pch=4,col=adjustcolor(pal[current_pal],alpha.f=1.0),cex=1.4,lwd=3)
		}

		current_yy <- current_yy + 1
		
	} ## if

} ## for coi

##
##
## 3 Sensitivity of IFR
#### towards approximating no. of infected individuals with confirmed cases 


## 3.1 Get accumulated COVerAGE-DB confirmed cases by age (seq(0,95,5)) and date for both sexes combined:  

  dat_cases <- dat %>% select(Country,Date,Sex,Age,Cases)

#### 3.1.1 Table unique dates and corresponding number of countries with non-NA & non-0 case entries:  

  cases_available <- matrix(0,nr=length(all_countries),nc=length(unique(dat_cases$Date)))
  rownames(cases_available) <- all_countries
  colnames(cases_available) <- as.character(sort(unique(dat_cases$Date)))

  for(date in 1:length(sort(unique(dat_cases$Date)))){
	print(date)
	current_date <- sort(unique(dat_cases$Date))[date]
	for(country in 1:length(all_countries)){
		current_coi <- all_countries[country]  
		if(current_coi %in% unique(dat_cases$Country)){
			current_coi_cases <- (dat_cases[which(dat_cases$Country==current_coi),])
			current_coi_current_date_cases <- pull(current_coi_cases[which(current_coi_cases$Date==current_date),"Cases"])
			if(length(which(is.na(current_coi_current_date_cases)))==0 & sum(current_coi_current_date_cases,na.rm=TRUE)>0){
				cases_available[current_coi,as.character(current_date)] <- 1
			} ## if 
		} ## if current_coi
	} ## for country 
  } ## for date 

  cases_available

#### 3.1.2 Use cases_available as indicator for calculating total IFR for particular countries and dates 
#### when approximating I_x with C_x

  row_names <- all_countries
  column_names <- as.character(sort(unique(dat_deaths$Date)))
  matrix_names <- seq(0,95,5)

  cases_array <- array(NA,dim=c(length(all_countries),length(unique(dat_cases$Date)),length(seq(0,95,5))),
								dimnames=list(row_names,column_names,matrix_names))

########
########

  row_names <- all_countries
  column_names <- as.character(sort(unique(dat_deaths$Date)))
  matrix_names <- c("low95","central","up95")

  total_IFR_array_Cx_Verity_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names))
  total_IFR_array_Cx_Salje_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
 								dimnames=list(row_names,column_names,matrix_names))
  total_IFR_array_Cx_Levin_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names))

########
########

current_coi <- "Norway"

  for(date in 1:length(sort(unique(dat_cases$Date)))){
	print(date)
	current_date <- sort(unique(dat_cases$Date))[date]
	for(country in 1:length(all_countries)){
		current_coi <- all_countries[country]  
		if(cases_available[current_coi,as.character(current_date)]==1){
			current_coi_cases <- (dat_cases[which(dat_cases$Country==current_coi),])
			current_coi_current_date_cases <- pull(current_coi_cases[which(current_coi_cases$Date==current_date),"Cases"])

			if(length(current_coi_current_date_cases)==21){

			current_cases <- current_coi_current_date_cases[-length(current_coi_current_date_cases)] 
			cases_array[current_coi,as.character(current_date),] <- current_cases

			#### Verity, scaled:

			## central IFR_x^COI:

			current_total_ifr <- sum ( IFRs_Verity_scaled_central[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Verity_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Verity_scaled_low[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Verity_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Verity_scaled_up[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Verity_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			#### Salje, scaled:

			## central IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Salje_scaled_central[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Salje_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Salje_scaled_low[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Salje_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Salje_scaled_up[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Salje_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			#### Levin, scaled:

			## central IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Levin_scaled_central[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Levin_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Levin_scaled_low[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Levin_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_total_ifr <- sum ( IFRs_Levin_scaled_up[,current_coi] * current_cases / sum(current_cases) ) * 100
			total_IFR_array_Cx_Levin_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			}## if length pulled cases
		} ## if cases_available
	} ## for country 
  } ## for date 

##
##

no_of_val <- sort(apply(X=(total_IFR_array_Verity_scaled[,,"central"]-total_IFR_array_Cx_Verity_scaled[,,"central"]),1,FUN=function(x){length(x[which(!is.na(x))])}),decreasing=TRUE)
no_of_val_largerThan_0 <- no_of_val[which(no_of_val>0)]
sum(no_of_val_largerThan_0)

##
## 3.2 Calculate difference in IFRs based on I_x and Cx and select data points with non-zero values for countries that have at least 200 deaths 

no_of_val_largerThan_0_and_deaths_largerThan_199 <- NA
median_val_largerThan_0_and_deaths_largerThan_199 <- NA

for(coi in length(no_of_val_largerThan_0):1){
	current_coi <- names(no_of_val_largerThan_0)[coi]
	current_val <- (total_IFR_array_Verity_scaled[current_coi,,"central"]-total_IFR_array_Cx_Verity_scaled[current_coi,,"central"])
	current_val_nonNA <- current_val[which(!is.na(current_val))]
	current_dates <- names(current_val_nonNA)
	if(length(current_dates)>1){
		current_dates_deaths <- apply(deaths_array[current_coi,current_dates,],1,sum)
	}
	if(length(current_dates)==1){
		current_dates_deaths <- sum(deaths_array[current_coi,current_dates,])
	}
	current_dates_deaths_lt_200 <- names(current_dates_deaths[which(current_dates_deaths>=200)]) 
	current_val_nonNA <- current_val_nonNA[which(names(current_val_nonNA)%in%current_dates_deaths_lt_200)]  

	if(length(current_val_nonNA)>0){
		current_to_add <- length(current_val_nonNA)
		names(current_to_add) <- current_coi
		no_of_val_largerThan_0_and_deaths_largerThan_199 <- c(no_of_val_largerThan_0_and_deaths_largerThan_199,current_to_add)

		current_median <- median(current_val_nonNA)
		names(current_median) <- current_coi
		median_val_largerThan_0_and_deaths_largerThan_199 <- c(median_val_largerThan_0_and_deaths_largerThan_199,current_median)
	}

} ## coi

no_of_val_largerThan_0_and_deaths_largerThan_199 <- no_of_val_largerThan_0_and_deaths_largerThan_199[-1]
median_val_largerThan_0_and_deaths_largerThan_199 <- median_val_largerThan_0_and_deaths_largerThan_199[-1]
median_val_largerThan_0_and_deaths_largerThan_199

##
## 3.3 Visualize difference in IFR based on Ix and Cx:

par(fig = c(0,1,0,1), las=1, mai=c(0.8,0.0,0.0,0.0))

plot(x=-100,y=-100,ylim=c(1,57.0),xlim=c(-3.1,2.5),xlab="",ylab="",main="",axes=FALSE)
## title(bquote(atop("Difference in total IFR, in percentage points, approximating" ~ I[x],  "with" ~ C[x])),font.main=2)

axis(side=1,at=seq(-2,2,0.5),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(-2,2,0.5),labels=TRUE,lwd=3,pos=0)

yy <- 32

segments(x0=-2,x1=2,y0=seq(1,23+yy,1),y1=seq(1,23+yy,1),lty=2,col=grey(0.8),lwd=1)
segments(x0=seq(-2,2,0.5),x1=seq(-2,2,0.5),y0=0,y1=23+yy,lty=2,col=grey(0.8),lwd=1)
segments(x0=0,x1=0,y0=0,y1=25.5+yy,col="black",lwd=3)

text(x=0, y=24.8+yy, expression('IFR['*'IFR'[x]*',I'[x]*'] < IFR['*'IFR'[x]*',C'[x]*']'), col="black",pos=2,cex=0.9,font=2)	
text(x=0, y=24.8+yy, expression('IFR['*'IFR'[x]*',I'[x]*'] > IFR['*'IFR'[x]*',C'[x]*']'), col="black",pos=4,cex=0.9,font=2)	

text(x=2.0,y=24.8+yy,"No. of values:",cex=0.8,pos=4)

current_yy <- 1

for(coi in length(median_val_largerThan_0_and_deaths_largerThan_199):1){
	current_coi <- names(rev(sort(median_val_largerThan_0_and_deaths_largerThan_199)))[coi]
	current_diff <- (total_IFR_array_Verity_scaled[current_coi,,"central"]-total_IFR_array_Cx_Verity_scaled[current_coi,,"central"])
	current_diff_nonNA <- current_diff[which(!is.na(current_diff))]
	if(length(current_diff_nonNA)>1){
		deaths_per_date <- apply(deaths_array[current_coi,names(current_diff_nonNA),],1,sum)
		deaths_per_date_min_200 <- deaths_per_date[which(deaths_per_date >= 200)]
	}
	if(length(current_diff_nonNA)==1){
		deaths_per_date <- sum(deaths_array[current_coi,names(current_diff_nonNA),])
		deaths_per_date_min_200 <- deaths_per_date[which(deaths_per_date >= 200)]
	}
	current_diff_nonNA <- current_diff_nonNA[which(deaths_per_date >= 200)] 
	
	if(length(current_diff_nonNA)>0){

		current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
		text(x=-2.0, y=current_yy, current_coi, col=adjustcolor(pal[current_pal],alpha.f=0.6), 
			pos=2,cex=1.0,font=2)	
		rect(xleft=median(current_diff_nonNA,na.rm=TRUE)-0.025,
			xright=median(current_diff_nonNA,na.rm=TRUE)+0.025,
			ybottom=current_yy-0.3,ytop=current_yy+0.3,col=adjustcolor(pal[current_pal],alpha.f=0.6),border=NA,lwd=3)
		segments(x0=quantile(current_diff_nonNA,probs=(0.1),na.rm=TRUE),
			x1=quantile(current_diff_nonNA,probs=(0.9),na.rm=TRUE),
			y0=current_yy,y1=current_yy,col=adjustcolor(pal[current_pal],alpha.f=0.6),lwd=3)

		text(x=2.3,y=current_yy,length(current_diff_nonNA),cex=0.9,pos=4)

		if(!is.na(current_diff_nonNA["2021-03-31"])){
			points(x=current_diff_nonNA["2021-03-31"],y=current_yy,pch=4,col=adjustcolor(pal[current_pal],alpha.f=1.0),cex=1.4,lwd=3)
		}

		current_yy <- current_yy + 1
		
	} ## if

} ## for coi

dev.off()


