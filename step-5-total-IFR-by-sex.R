
## 1. Load scaled IFRs of Acosta by sex:

#### step-1-load-input-data.R

## 2. Select data for all countries of interest from COVer-AGE-DB:

  region <- c("All")
  dat_o <- Dat %>% filter(Country %in% all_countries & Region %in% region)
  dat_f <- dat_o %>% filter(Sex=="f")
  dat_m <- dat_o %>% filter(Sex=="m")
  dat_f$Date <- as.Date(dat_f$Date,"%d.%m.%Y")
  dat_m$Date <- as.Date(dat_m$Date,"%d.%m.%Y")

## 3. Load reported deaths from COVerAGE-DB and pull them for all available dates:

  dat_deaths <- dat %>% select(Country,Date,Sex,Age,Deaths)
  dat_f_deaths <- dat_f %>% select(Country,Date,Sex,Age,Deaths)
  dat_m_deaths <- dat_m %>% select(Country,Date,Sex,Age,Deaths)

## 4. Generate indicator matrix that lists for what countries and dates deaths are available:

  deaths_f_available <- matrix(0,nr=length(all_countries),nc=length(unique(dat_f_deaths$Date)))
  rownames(deaths_f_available) <- all_countries
  colnames(deaths_f_available) <- as.character(sort(unique(dat_f_deaths$Date)))

  deaths_m_available <- matrix(0,nr=length(all_countries),nc=length(unique(dat_m_deaths$Date)))
  rownames(deaths_m_available) <- all_countries
  colnames(deaths_m_available) <- as.character(sort(unique(dat_m_deaths$Date)))

  for(date in 1:length(sort(unique(dat_f_deaths$Date)))){
	print(date)
	current_date <- sort(unique(dat_f_deaths$Date))[date]
	for(country in 1:length(all_countries)){
		current_coi <- all_countries[country]  
		if(current_coi %in% unique(dat_f_deaths$Country)){
			current_coi_deaths <- (dat_f_deaths[which(dat_f_deaths$Country==current_coi),])
			current_coi_current_date_deaths <- pull(current_coi_deaths[which(current_coi_deaths$Date==current_date),"Deaths"])
			if(length(which(is.na(current_coi_recent_deaths)))==0 & sum(current_coi_current_date_deaths,na.rm=TRUE)>0){
				deaths_f_available[current_coi,as.character(current_date)] <- 1
			} ## if 
		} ## if current_coi
	} ## for country 
  } ## for date 

##
##

  for(date in 1:length(sort(unique(dat_m_deaths$Date)))){
	print(date)
	current_date <- sort(unique(dat_m_deaths$Date))[date]
	for(country in 1:length(all_countries)){
		current_coi <- all_countries[country]  
		if(current_coi %in% unique(dat_m_deaths$Country)){
			current_coi_deaths <- (dat_m_deaths[which(dat_m_deaths$Country==current_coi),])
			current_coi_current_date_deaths <- pull(current_coi_deaths[which(current_coi_deaths$Date==current_date),"Deaths"])
			if(length(which(is.na(current_coi_recent_deaths)))==0 & sum(current_coi_current_date_deaths,na.rm=TRUE)>0){
				deaths_m_available[current_coi,as.character(current_date)] <- 1
			} ## if 
		} ## if current_coi
	} ## for country 
  } ## for date 

#### 5. Estimate total IFR by sex for those countries and dates for which deaths are available:

  row_names <- all_countries
  column_names_f <- as.character(sort(unique(dat_f_deaths$Date)))
  column_names_m <- as.character(sort(unique(dat_m_deaths$Date)))
  matrix_names <- seq(0,95,5)
  matrix_names_2 <- c("low95","central","up95")

  infections_f_array_Acosta_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_f_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names_f,matrix_names,matrix_names_2))
  infections_m_array_Acosta_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_m_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names_m,matrix_names,matrix_names_2))

########
########

  row_names <- all_countries
  column_names_f <- as.character(sort(unique(dat_f_deaths$Date)))
  column_names_m <- as.character(sort(unique(dat_m_deaths$Date)))
  matrix_names <- seq(0,100,5)

  deaths_f_array <- array(NA,dim=c(length(all_countries),length(unique(dat_f_deaths$Date)),length(seq(0,100,5))),
								dimnames=list(row_names,column_names_f,matrix_names))
  deaths_m_array <- array(NA,dim=c(length(all_countries),length(unique(dat_m_deaths$Date)),length(seq(0,100,5))),
								dimnames=list(row_names,column_names_m,matrix_names))

########
########

  row_names <- all_countries
  column_names_f <- as.character(sort(unique(dat_f_deaths$Date)))
  column_names_m <- as.character(sort(unique(dat_m_deaths$Date)))
  matrix_names <- c("low95","central","up95")

  total_IFR_f_array_Acosta_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_f_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names_f,matrix_names))
  total_IFR_m_array_Acosta_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_m_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names_m,matrix_names))

########
########

for(sex in 1:2){
  current_sex <- c("f","m")[sex]		
  if(current_sex=="f"){	
	  for(date in 1:length(sort(unique(dat_f_deaths$Date)))){
		print(date)
		current_date <- sort(unique(dat_f_deaths$Date))[date]
		for(country in 1:length(all_countries)){
			current_coi <- all_countries[country]  
			if(deaths_f_available[current_coi,as.character(current_date)]==1){
				current_coi_deaths <- (dat_f_deaths[which(dat_f_deaths$Country==current_coi),])
				current_coi_current_date_deaths <- pull(current_coi_deaths[which(current_coi_deaths$Date==current_date),"Deaths"])
				if(length(current_coi_current_date_deaths)==21){
					deaths_f_array[current_coi,as.character(current_date),] <- current_coi_current_date_deaths

					## central IFR_x^COI:
					current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_f_Acosta_scaled_central[,current_coi]
					current_total_ifr <- sum ( IFRs_f_Acosta_scaled_central[,current_coi] * current_infections / sum(current_infections) ) * 100
					infections_f_array_Acosta_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
					total_IFR_f_array_Acosta_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

					## low95 IFR_x^COI:
					current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_f_Acosta_scaled_low[,current_coi]
					current_total_ifr <- sum ( IFRs_f_Acosta_scaled_low[,current_coi] * current_infections / sum(current_infections) ) * 100
					infections_f_array_Acosta_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
					total_IFR_f_array_Acosta_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

					## up95 IFR_x^COI:
					current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_f_Acosta_scaled_up[,current_coi]
					current_total_ifr <- sum ( IFRs_Acosta_f_scaled_up[,current_coi] * current_infections / sum(current_infections) ) * 100
					infections_f_array_Acosta_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
					total_IFR_f_array_Acosta_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

				}## if length pulled deaths
			} ## if deaths_available
		} ## for country 
	  } ## for date 
  } ## if female
 
  ##
  ##

  if(current_sex=="m"){	
	  for(date in 1:length(sort(unique(dat_m_deaths$Date)))){
		print(date)
		current_date <- sort(unique(dat_m_deaths$Date))[date]
		for(country in 1:length(all_countries)){
			current_coi <- all_countries[country]  
			if(deaths_m_available[current_coi,as.character(current_date)]==1){
				current_coi_deaths <- (dat_m_deaths[which(dat_m_deaths$Country==current_coi),])
				current_coi_current_date_deaths <- pull(current_coi_deaths[which(current_coi_deaths$Date==current_date),"Deaths"])
				if(length(current_coi_current_date_deaths)==21){
					deaths_m_array[current_coi,as.character(current_date),] <- current_coi_current_date_deaths

					## central IFR_x^COI:
					current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_m_Acosta_scaled_central[,current_coi]
					current_total_ifr <- sum ( IFRs_m_Acosta_scaled_central[,current_coi] * current_infections / sum(current_infections) ) * 100
					infections_m_array_Acosta_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
					total_IFR_m_array_Acosta_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

					## low95 IFR_x^COI:
					current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_m_Acosta_scaled_low[,current_coi]
					current_total_ifr <- sum ( IFRs_m_Acosta_scaled_low[,current_coi] * current_infections / sum(current_infections) ) * 100
					infections_m_array_Acosta_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
					total_IFR_m_array_Acosta_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

					## up95 IFR_x^COI:
					current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_m_Acosta_scaled_up[,current_coi]
					current_total_ifr <- sum ( IFRs_Acosta_m_scaled_up[,current_coi] * current_infections / sum(current_infections) ) * 100
					infections_m_array_Acosta_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
					total_IFR_m_array_Acosta_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

				}## if length pulled deaths
			} ## if deaths_available
		} ## for country 
	  } ## for date 
  } ## if male
} ## for sex

## 6. Calculate and visualize difference in total IFRs btween women and men:

total_IFR_difference_by_sex <- total_IFR_f_array_Acosta_scaled - total_IFR_m_array_Acosta_scaled
dimnames(total_IFR_difference_by_sex)

##
##

total_IFR_difference_by_sex_overlap_countries <- total_IFR_f_array_Acosta_scaled[,,"central"]-total_IFR_m_array_Acosta_scaled[,,"central"]
difference_by_sex_overlap_countries <- which(!is.na(total_IFR_difference_by_sex_overlap_countries))

no_of_diff_by_sex_val <- sort(apply(X=total_IFR_difference_by_sex_overlap_countries,1,FUN=function(x){length(x[which(!is.na(x))])}),decreasing=TRUE)
no_of_diff_by_sex_val_largerThan_0 <- no_of_diff_by_sex_val[which(no_of_diff_by_sex_val>0)]
sum(no_of_diff_by_sex_val_largerThan_0)
 
##
##

no_of_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199 <- NA
median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199 <- NA

for(coi in length(no_of_diff_by_sex_val_largerThan_0):1){
	current_coi <- names(no_of_diff_by_sex_val_largerThan_0)[coi]
	current_diff <- total_IFR_difference_by_sex_overlap_countries[current_coi,]
	current_diff_nonNA <- current_diff[which(!is.na(current_diff))]
	current_dates <- names(current_diff_nonNA)
	if(length(current_dates)>1){
		current_dates_deaths_f <- apply(deaths_f_array[current_coi,current_dates,],1,sum)
	}
	if(length(current_dates)==1){
		current_dates_deaths_f <- sum(deaths_f_array[current_coi,current_dates,])
	}
	current_dates_deaths_f_lt_200 <- names(current_dates_deaths_f[which(current_dates_deaths_f>=200)]) 
	current_diff_nonNA <- current_diff_nonNA[which(names(current_diff_nonNA)%in%current_dates_deaths_f_lt_200)]  

	if(length(current_diff_nonNA)>0){
		current_to_add <- length(current_diff_nonNA)
		names(current_to_add) <- current_coi
		no_of_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199 <- c(no_of_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199,current_to_add)

		current_median <- median(current_diff_nonNA)
		names(current_median) <- current_coi
		median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199 <- c(median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199,current_median)
	}

} ## coi

no_of_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199 <- no_of_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199[-1]
median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199 <- median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199[-1]

## sort(median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199)

##
##

collect_median_diff_by_sex <- NA
collect_median_diff_by_sex_coi <- NA
collect_median_diff_by_sex_noVal <- NA

setwd("./plots")

dev.off()

pdf(file=paste("total-IFR-difference-by-sex.pdf",sep=""), width=15, height=15, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.8,0.0,1.0,0.0))

plot(x=-100,y=-100,ylim=c(1,42),xlim=c(-3.0,3.5),xlab="",ylab="",main="",axes=FALSE)
title(bquote(atop("Difference in total IFR by sex, in percentage points")),font.main=2)
axis(side=1,at=seq(-2,3,1),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(-2,3,1),labels=TRUE,lwd=3,pos=0)
segments(x0=-2,x1=3,y0=seq(1,37,1),y1=seq(1,37,1),lty=2,col=grey(0.8),lwd=1)
segments(x0=seq(-2,3,0.5),x1=seq(-2,3,0.5),y0=0,y1=37,lty=2,col=grey(0.8),lwd=1)
segments(x0=0,x1=0,y0=0,y1=39,col="black",lwd=3)
text(x=0, y=38.5, expression('female total IFR < male total IFR'), col="black",pos=2,cex=0.9,font=2)	
text(x=0, y=38.5, expression('female total IFR > male total IFR'), col="black",pos=4,cex=0.9,font=2)	
points(x=seq(-3.0,0.8,length=3),y=rep(43,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(-3.0,0.8,length=3),y=rep(43,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.75)
points(x=seq(-3.0,0.8,length=3),y=rep(41,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(-3.0,0.8,length=3),y=rep(41,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.75)
points(x=2.0,y=42,pch=15,cex=2.1,col="black")
segments(x0=2.0,x1=2.0,y0=40.75,y1=43.25)
text(x=2.0,y=40.75,c("Quantile 0.1"),font=2,pos=4,cex=0.8)
text(x=2.0,y=43.25,c("Quantile 0.9"),font=2,pos=4,cex=0.8)
text(x=2.0,y=42,c("Median"),font=2,pos=4,cex=0.8)
points(x=2.9,y=42,pch=4,cex=1.4,,lwd=2,col="black")
text(x=2.9,y=42,"2021-01-13",font=2,pos=4,cex=0.8)
text(x=3.25,y=38,"No. of values:",cex=0.8,pos=3)
current_yy <- 1
for(coi in length(median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199):1){
	current_coi <- names(rev(sort(median_diff_by_sex_val_largerThan_0_and_deaths_f_largerThan_199)))[coi]
	if(max(apply(X=deaths_f_array[current_coi,,],1,sum),na.rm=TRUE)>=200 & max(apply(X=deaths_m_array[current_coi,,],1,sum),na.rm=TRUE)>=200){
		current_diff <- total_IFR_difference_by_sex_overlap_countries[current_coi,]
		current_diff_nonNA <- current_diff[which(!is.na(current_diff))]
	
		current_dates <- names(current_diff_nonNA)
		if(length(current_dates)>1){
			current_dates_deaths_f <- apply(deaths_f_array[current_coi,current_dates,],1,sum)
		}
		if(length(current_dates)==1){
			current_dates_deaths_f <- sum(deaths_f_array[current_coi,current_dates,])
		}
		current_dates_deaths_f_lt_200 <- names(current_dates_deaths_f[which(current_dates_deaths_f>=200)]) 
		current_diff_nonNA <- current_diff_nonNA[which(names(current_diff_nonNA)%in%current_dates_deaths_f_lt_200)]  
	
		if(length(current_diff_nonNA)>0){

			current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
			text(x=-2.0, y=current_yy, current_coi, col=adjustcolor(pal[current_pal],alpha.f=0.6), 
				pos=2,cex=1.0,font=2)	
			rect(xleft=median(current_diff_nonNA,na.rm=TRUE)-0.05,
				xright=median(current_diff_nonNA,na.rm=TRUE)+0.05,
				ybottom=current_yy-0.3,ytop=current_yy+0.3,col=adjustcolor(pal[current_pal],alpha.f=0.6),border=NA,lwd=3)
			segments(x0=quantile(current_diff_nonNA,probs=(0.1),na.rm=TRUE),
				x1=quantile(current_diff_nonNA,probs=(0.9),na.rm=TRUE),
				y0=current_yy,y1=current_yy,col=adjustcolor(pal[current_pal],alpha.f=0.6),lwd=3)

			text(x=3.25,y=current_yy,length(current_diff_nonNA),cex=0.9,pos=4)

			if(!is.na(current_diff_nonNA["2021-01-10"])){
				points(x=current_diff_nonNA["2021-01-13"],y=current_yy,pch=4,col=adjustcolor(pal[current_pal],alpha.f=0.6),lwd=3)
			}

			current_yy <- current_yy + 1

			collect_median_diff_by_sex[coi] <- median(current_diff_nonNA,na.rm=TRUE)
			collect_median_diff_by_sex_coi[coi] <- current_coi
			collect_median_diff_by_sex_noVal[coi] <- length(current_diff_nonNA)
		} ## if
	} ## if
} ## for coi

dev.off()




