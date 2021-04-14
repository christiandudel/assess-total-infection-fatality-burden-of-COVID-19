
#
## 1. Set working directory:
#

setwd(.)

#
## 2. : Estimate total IFR for all countries and dates
## 2.1: select data for all countries of interest from COVer-AGE-DB
## 2.2: load reported deaths from COVerAGE-DB and pull them for all available dates
#### 2.2.1: generate indicator matrix *deaths_available* that indicates for what countries and dates deaths are available
#### 2.2.2: estimate total IFR for those countries and dates for which deaths are available
## 2.3: estimate numbers of infections with demographic scaling model sum_x(D_x / IFR_x)
## 2.4: and estimate total IFR as sum( IFR_x times I_x/I)
#

## 2.1 Select data for both sexes combined for all countries of interest from COVer-AGE-DB:

  region <- c("All")
  dat <- Dat %>% filter(Country %in% all_countries & Region %in% region)
  dat <- dat %>% filter(Sex=="b")
  dat$Date <- as.Date(dat$Date,"%d.%m.%Y")

## 2.2 Load reported deaths from COVerAGE-DB and pull them for all available dates:
  
  dat_deaths <- dat %>% select(Country,Date,Sex,Age,Deaths)

#### 2.2.1 Generate indicator matrix that lists for what countries and dates deaths are available:

  deaths_available <- matrix(0,nr=length(all_countries),nc=length(unique(dat_deaths$Date)))
  rownames(deaths_available) <- all_countries
  colnames(deaths_available) <- as.character(sort(unique(dat_deaths$Date)))

  for(date in 1:length(sort(unique(dat_deaths$Date)))){
	print(date)
	current_date <- sort(unique(dat_deaths$Date))[date]
	for(country in 1:length(all_countries)){
		current_coi <- all_countries[country]  
		if(current_coi %in% unique(dat_deaths$Country)){
			current_coi_deaths <- (dat_deaths[which(dat_deaths$Country==current_coi),])
			current_coi_current_date_deaths <- pull(current_coi_deaths[which(current_coi_deaths$Date==current_date),"Deaths"])
			if(length(which(is.na(current_coi_current_date_deaths)))==0 & sum(current_coi_current_date_deaths,na.rm=TRUE)>0){
				deaths_available[current_coi,as.character(current_date)] <- 1
			} ## if 
		} ## if current_coi
	} ## for country 
  } ## for date 

  deaths_available

#### 2.2.2: estimate total IFR for those countries and dates for which deaths are available
######## for_loop: go to, pull, and save non-NA & non-0 deaths for respective countries and dates 
######## and estimate total IFR with COI-specific IFR_x scaled from different sources (-> Veriy et al., Levin et al., Salje et al.)
######## This way we will have global dist at all possible time points:   

  row_names <- all_countries
  column_names <- as.character(sort(unique(dat_deaths$Date)))
  matrix_names <- seq(0,95,5)
  matrix_names_2 <- c("low95","central","up95")

  infections_array_Verity_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names,matrix_names_2))
  infections_array_Salje_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names,matrix_names_2))
  infections_array_Levin_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(seq(0,95,5)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names,matrix_names_2))

########
########

  row_names <- all_countries
  column_names <- as.character(sort(unique(dat_deaths$Date)))
  matrix_names <- seq(0,100,5)

  deaths_array <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(seq(0,100,5))),
								dimnames=list(row_names,column_names,matrix_names))

########
########

  row_names <- all_countries
  column_names <- as.character(sort(unique(dat_deaths$Date)))
  matrix_names <- c("low95","central","up95")

  total_IFR_array_Verity_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names))
  total_IFR_array_Salje_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
 								dimnames=list(row_names,column_names,matrix_names))
  total_IFR_array_Levin_scaled <- array(NA,dim=c(length(all_countries),length(unique(dat_deaths$Date)),length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names))

########
########

  for(date in 1:length(sort(unique(dat_deaths$Date)))){
	print(date)
	current_date <- sort(unique(dat_deaths$Date))[date]
	for(country in 1:length(all_countries)){
		current_coi <- all_countries[country]  
		if(deaths_available[current_coi,as.character(current_date)]==1){
			current_coi_deaths <- (dat_deaths[which(dat_deaths$Country==current_coi),])
			current_coi_current_date_deaths <- pull(current_coi_deaths[which(current_coi_deaths$Date==current_date),"Deaths"])

			if(length(current_coi_current_date_deaths)==21){

			deaths_array[current_coi,as.character(current_date),] <- current_coi_current_date_deaths

			#### Verity, scaled:

			## central IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Verity_scaled_central[,current_coi]
			current_total_ifr <- sum ( IFRs_Verity_scaled_central[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Verity_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
			total_IFR_array_Verity_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Verity_scaled_low[,current_coi]
			current_total_ifr <- sum ( IFRs_Verity_scaled_low[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Verity_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
			total_IFR_array_Verity_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Verity_scaled_up[,current_coi]
			current_total_ifr <- sum ( IFRs_Verity_scaled_up[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Verity_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
			total_IFR_array_Verity_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			#### Salje, scaled:

			## central IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Salje_scaled_central[,current_coi]
			current_total_ifr <- sum ( IFRs_Salje_scaled_central[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Salje_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
			total_IFR_array_Salje_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Salje_scaled_low[,current_coi]
			current_total_ifr <- sum ( IFRs_Salje_scaled_low[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Salje_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
			total_IFR_array_Salje_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Salje_scaled_up[,current_coi]
			current_total_ifr <- sum ( IFRs_Salje_scaled_up[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Salje_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
			total_IFR_array_Salje_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			#### Levin, scaled:

			## central IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Levin_scaled_central[,current_coi]
			current_total_ifr <- sum ( IFRs_Levin_scaled_central[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Levin_scaled[current_coi,as.character(current_date),,"central"] <- current_infections
			total_IFR_array_Levin_scaled[current_coi,as.character(current_date),"central"] <- current_total_ifr

			## low95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Levin_scaled_low[,current_coi]
			current_total_ifr <- sum ( IFRs_Levin_scaled_low[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Levin_scaled[current_coi,as.character(current_date),,"low95"] <- current_infections
			total_IFR_array_Levin_scaled[current_coi,as.character(current_date),"low95"] <- current_total_ifr

			## up95 IFR_x^COI:
			current_infections <- current_coi_current_date_deaths[-length(current_coi_current_date_deaths)] / IFRs_Levin_scaled_up[,current_coi]
			current_total_ifr <- sum ( IFRs_Levin_scaled_up[,current_coi] * current_infections / sum(current_infections) ) * 100
			infections_array_Levin_scaled[current_coi,as.character(current_date),,"up95"] <- current_infections
			total_IFR_array_Levin_scaled[current_coi,as.character(current_date),"up95"] <- current_total_ifr

			}## if length pulled deaths
		} ## if deaths_available
	} ## for country 
  } ## for date 

#
## 3. Visualize spatio-temporal distribution of total infection fatality rate, in %, over time
#

## 3.1 Get all dates for which total IFR has been estimated for at least 25 countries:
######## data object: dates_and_number_of_values_lt_24

total_IFR_array_Verity_scaled_available <- apply(X=total_IFR_array_Verity_scaled[,,"central"],2,function(X){length(X[-which(is.na(X))])})
dates_and_number_of_values_lt_24 <- total_IFR_array_Verity_scaled_available[which(total_IFR_array_Verity_scaled_available > 24)] 
calendar_dates <- seq(as.Date("28/03/2020", format = "%d/%m/%Y"), by = "days", length = 369 )  

### 3.2 Finally plot spatio-temporal distribution of estimated total IFR, in %, over time 
######## (-> for dates for which we have total IFR for at lest 25 countries)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
names(pal)[1:length(unique(countries_by_world_region[,2]))] <- unique(countries_by_world_region[,2])

setwd(./plots)

dev.off()

pdf(file="total-IFR-spatio-temporal-dist.pdf", width=20, height=10, family="Times", pointsize=20, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.4,0.0,0.8,0.0))

plot(x=-1000,y=-1000,xlim=c(-10,length(calendar_dates)),ylim=c(-2,4),xlab="",ylab="",axes=FALSE)
title(bquote(atop("Global distribution of the total infection fatality rate, in %", "Based on country-specific " ~ IFR[x] ~ " and " ~I[x])),font.main=2)

xx <- 45

points(x=seq(75-xx,175-xx,length=4),y=rep(3.75,4),pch=20,cex=1.8,col=pal[1:4])
text(x=seq(75-xx,175-xx,length=4),y=rep(3.75,4),c(names(pal)[1:4]),font=2,pos=4,cex=0.8)
points(x=seq(80-xx,170-xx,length=2),y=rep(3.5,2),pch=20,cex=1.8,col=pal[5:6])
text(x=seq(80-xx,170-xx,length=2),y=rep(3.5,2),c(names(pal)[5:6]),font=2,pos=4,cex=0.8)

points(x=255,y=(3.5+3.75)/2,pch=20,cex=2.1,col=gray(0.6))
points(x=210,y=(3.5+3.75)/2,pch=15,cex=2.1,col="black")
segments(x0=210,x1=210,y0=3.4,y1=3.85)

text(x=215,y=3.4,c("Quantile 0.1"),font=2,pos=4,cex=0.8)
text(x=215,y=3.85,c("Quantile 0.9"),font=2,pos=4,cex=0.8)
text(x=215,y=(3.5+3.75)/2,c("Median"),font=2,pos=4,cex=0.8)
text(x=255,y=(3.5+3.75)/2,c("Mean"),font=2,pos=4,cex=0.8)

axis(side=1,at=seq(1,length(calendar_dates),7),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(1,length(calendar_dates),28),labels=FALSE,lwd=3,pos=0)
axis(side=1,at=seq(1,length(calendar_dates),28*2),labels=calendar_dates[seq(1,length(calendar_dates),28*2)],lwd=3,pos=0)
axis(side=2,at=seq(0,4,0.5),labels=FALSE,lwd=1,pos=-10)
axis(side=2,at=seq(0,4,1),labels=TRUE,lwd=3,pos=-10)

for(date in 1:length(dates_and_number_of_values_lt_24)){

	current_x <- which(calendar_dates==names(dates_and_number_of_values_lt_24[date]))
	current_y <- total_IFR_array_Verity_scaled[,names(dates_and_number_of_values_lt_24)[date],"central"]
	current_coi_no_deaths <- apply(X=deaths_array[,names(dates_and_number_of_values_lt_24)[date],],1,function(x){sum(x)})
	current_coi_with_min_200_deaths <- which(current_coi_no_deaths>=200) 
	current_y <- current_y[current_coi_with_min_200_deaths]  

	## single countries:
	current_coi <- names(current_y[which(!is.na(current_y))])
	current_row <- c(NA)
	current_pal <- c(NA)
	for(pop in 1:length(current_coi)){
		current_row[pop] <- which(countries_by_world_region[,1]==current_coi[pop])
		current_pal[pop] <- which(names(pal)==countries_by_world_region[current_row[pop],2])
	}
	points(x=rep(current_x,dates_and_number_of_values_lt_24[date]),y=current_y[which(!is.na(current_y))],
		pch=20,col=adjustcolor(pal[current_pal],alpha.f=0.2))

	## mean:
	points(x=current_x,y=mean(current_y[which(!is.na(current_y))]),pch=20,col=gray(0.6),cex=1.25,lwd=2)
}

text(x=164,y=total_IFR_array_Verity_scaled["Australia","2020-09-18","central"],"Australia",col=pal[5],font=2,cex=1.0,pos=3)  
text(x=65,y=total_IFR_array_Verity_scaled["Canada","2020-05-29","central"],"Canada",col=pal[6],font=2,cex=1.0,pos=3)  
text(x=285,y=total_IFR_array_Verity_scaled["South Korea","2021-01-29","central"],"South Korea",col=pal[1],font=2,cex=1.0,pos=3)  
text(x=140,y=total_IFR_array_Verity_scaled["Japan","2020-08-14","central"],"Japan",col=pal[1],font=2,cex=1.0,pos=4)  
text(x=335,y=total_IFR_array_Verity_scaled["Slovenia","2021-02-27","central"],"Slovenia",col=pal[2],font=2,cex=1.0,pos=3)  

#### distribution every four weeks:

  dates_to_plot <- c(calendar_dates[seq(1,length(calendar_dates),by=14)],names(dates_and_number_of_values_lt_24[length(dates_and_number_of_values_lt_24)]))

  actual_dates_to_plot <- c(NA)
  for(date in 1:length(dates_to_plot)){
      current_date <- which(dates_to_plot[date]==names(dates_and_number_of_values_lt_24))
	if(length(current_date)==1){
		actual_dates_to_plot <- c(actual_dates_to_plot,as.character(dates_to_plot[date])) 
	}
  }
  actual_dates_to_plot <- actual_dates_to_plot[-1]

  for(date in 1:length(actual_dates_to_plot)){

	current_xx <- which(calendar_dates==actual_dates_to_plot[date])
	calendar_dates[current_xx]
	current_yy <- total_IFR_array_Verity_scaled[,as.character(actual_dates_to_plot[date]),"central"]

	current_coi_no_deaths <- apply(X=deaths_array[,as.character(actual_dates_to_plot[date]),],1,function(x){sum(x)})
	current_coi_with_min_200_deaths <- which(current_coi_no_deaths>=200) 
	current_yy <- current_yy[current_coi_with_min_200_deaths]  

	## median:
	points(x=current_xx,y=median(current_yy[which(!is.na(current_yy))]),pch=15,col="black",cex=1.25,lwd=2)

	## 0.1 and 0.9 quantile:
	segments(x0=current_xx,x1=current_xx,y0=quantile(current_yy[which(!is.na(current_yy))],prob=c(0.1)),y1=quantile(current_yy[which(!is.na(current_yy))],prob=c(0.9)),col="black",lwd=2)
  }

  axis(side=1,at=seq(1,length(calendar_dates),7),labels=FALSE,lwd=1,pos=-2)
  axis(side=1,at=seq(1,length(calendar_dates),28),labels=FALSE,lwd=3,pos=-2)
  axis(side=1,at=seq(1,length(calendar_dates),28*2),labels=calendar_dates[seq(1,length(calendar_dates),28*2)],lwd=3,pos=-2)
  axis(side=2,at=seq(-2,-1.0,1/4),labels=seq(0,40,10),lwd=2,pos=-10)

  text(x=140,y=-1,"Number of countries:",pos=3)

  for(date in 1:length(actual_dates_to_plot)){

	current_xx <- which(calendar_dates==actual_dates_to_plot[date])
	calendar_dates[current_xx]
	current_yy <- total_IFR_array_Verity_scaled[,as.character(actual_dates_to_plot[date]),"central"]
	current_coi_no_deaths <- apply(X=deaths_array[,as.character(actual_dates_to_plot[date]),],1,function(x){sum(x)})
	current_coi_with_min_200_deaths <- which(current_coi_no_deaths>=200) 
	current_yy <- current_yy[current_coi_with_min_200_deaths]  
	current_coi <- names(current_yy[which(!is.na(current_yy))])
	current_row <- c(NA)
	current_pal <- c(NA)
	for(pop in 1:length(current_coi)){
		current_row[pop] <- which(countries_by_world_region[,1]==current_coi[pop])
		current_pal[pop] <- which(names(pal)==countries_by_world_region[current_row[pop],2])
	}
	table_current_pal <- table(current_pal)
	print(sum(table_current_pal))
	for(wr in 1:length(table_current_pal)){
		current_ytop <- sum(table_current_pal)-sum(rev(c(table_current_pal,0))[1:wr])
		rect(xleft=current_xx-1,xright=current_xx+1,
			ybottom=-2,ytop=-2+current_ytop/40,col=pal[rev(as.numeric(names(table_current_pal)))[wr]],border=NA)
	}
  }
  
dev.off()



