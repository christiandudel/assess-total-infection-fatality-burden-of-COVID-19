
#
## 1. Set working directory:
#

setwd(.)

#
## 2. Association between total IFR and mean age of infected individuals
#

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
names(pal)[1:length(unique(countries_by_world_region[,2]))] <- unique(countries_by_world_region[,2])

setwd(./plots)

dev.off()

pdf(file="association-total-IFR-mean-age-infected-individuals.pdf", width=10, height=10, family="Times", pointsize=16, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(1.2,0.4,0.8,0.0))

plot(-100,-100,xlim=c(0,80),ylim=c(0,4.5),xlab="Mean age of infected individuals", ylab="", main="", axes=FALSE)
title(bquote(atop("Association between mean age of infected individuals and total IFR, in %")),font.main=2)

points(x=seq(0,70,length=3),y=rep(4.5,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(0,70,length=3),y=rep(4.5,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.7)
points(x=seq(0,70,length=3),y=rep(4.3,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(0,70,length=3),y=rep(4.3,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.7)

segments(x0=seq(10,80,10),x1=seq(10,80,10),y0=0,y1=4,lty=2,col=gray(0.8))
segments(x0=0,x1=80,y0=seq(0,4,1),y1=seq(0,4,1),lty=2,col=gray(0.8))

axis(side=1,at=seq(0,80,10),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(0,80,20),labels=TRUE,lwd=3,pos=0)
axis(side=2,at=seq(0,4,0.5),labels=FALSE,lwd=1,pos=0)
axis(side=2,at=seq(0,4,1),labels=TRUE,lwd=3,pos=0)

collect_total_ifr <- c(NA)
collect_mean_age_I <- c(NA)
collect_current_coi <- c(NA)
collect_current_date <- c(NA)

for(date in 1:length(dates_and_number_of_values_lt_24)){

	current_x <- which(calendar_dates==names(dates_and_number_of_values_lt_24[date]))
	current_y <- total_IFR_array_Verity_scaled[,names(dates_and_number_of_values_lt_24)[date],"central"]

	## single countries:
	current_cois <- names(current_y[which(!is.na(current_y))])

	for(pop in 1:length(current_cois)){
		
		current_coi <- current_cois[pop]
		current_total_ifr <- total_IFR_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),"central"]
		current_mean_age_I <- sum ( seq(2.5,97.5,5) * (infections_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),,"central"])/sum(infections_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),,"central"]) )

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

		current_median_age_pop <- as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]==current_coi_long),"2020"])

		current_row <- which(countries_by_world_region[,1]==current_coi)
		current_pal <- which(names(pal)==countries_by_world_region[current_row,2])

		points( current_mean_age_I, current_total_ifr , pch=20, col=adjustcolor(pal[current_pal],alpha.f=0.2))

		collect_total_ifr <- c(collect_total_ifr,current_total_ifr)
		collect_mean_age_I <- c(collect_mean_age_I,current_mean_age_I)
		collect_current_coi <- c(collect_current_coi,current_coi)
		collect_current_date <- c(collect_current_date,names(dates_and_number_of_values_lt_24[date]))

	} ## for pop
} ## for date

		correlation <- round(cor(x=collect_total_ifr[-1], y=collect_mean_age_I[-1], method = "pearson"),2)

		text(x=5,y=3.5,paste("Pearson's correlation coefficient: ",correlation,sep=""),pos=4)

dev.off()

#
## 3. Association between total IFR and median age of infected individuals
#

dev.off()

pdf(file="association-total-IFR-median-age-infected-individuals.pdf", width=10, height=10, family="Times", pointsize=16, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(1.2,0.4,0.8,0.0))

plot(-100,-100,xlim=c(0,80),ylim=c(0,4.5),xlab="Median age of infected individuals", ylab="", main="", axes=FALSE)
title(bquote(atop("Association between median age of infected individuals and total IFR, in %")),font.main=2)

points(x=seq(0,70,length=3),y=rep(4.5,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(0,70,length=3),y=rep(4.5,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.7)
points(x=seq(0,70,length=3),y=rep(4.3,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(0,70,length=3),y=rep(4.3,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.7)

segments(x0=seq(10,80,10),x1=seq(10,80,10),y0=0,y1=4,lty=2,col=gray(0.8))
segments(x0=0,x1=80,y0=seq(0,4,1),y1=seq(0,4,1),lty=2,col=gray(0.8))

axis(side=1,at=seq(0,80,10),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(0,80,20),labels=TRUE,lwd=3,pos=0)
axis(side=2,at=seq(0,4,0.5),labels=FALSE,lwd=1,pos=0)
axis(side=2,at=seq(0,4,1),labels=TRUE,lwd=3,pos=0)

collect_total_ifr <- c(NA)
collect_median_age_I <- c(NA)

for(date in 1:length(dates_and_number_of_values_lt_24)){

	current_x <- which(calendar_dates==names(dates_and_number_of_values_lt_24[date]))
	current_y <- total_IFR_array_Verity_scaled[,names(dates_and_number_of_values_lt_24)[date],"central"]

	## single countries:
	current_cois <- names(current_y[which(!is.na(current_y))])

	for(pop in 1:length(current_cois)){
		
		current_coi <- current_cois[pop]
		current_total_ifr <- total_IFR_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),"central"]
		current_mean_age_I <- sum ( seq(2.5,97.5,5) * (infections_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),,"central"])/sum(infections_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),,"central"]) )
		current_median_age_I <- median ( rep( seq(2.5,97.5,5) , (infections_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),,"central"]) ) )

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

		current_median_age_pop <- as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]==current_coi_long),"2020"])

		current_row <- which(countries_by_world_region[,1]==current_coi)
		current_pal <- which(names(pal)==countries_by_world_region[current_row,2])

		points( current_median_age_I, current_total_ifr , pch=20, col=adjustcolor(pal[current_pal],alpha.f=0.2))

		collect_total_ifr <- c(collect_total_ifr,current_total_ifr)
		collect_median_age_I <- c(collect_median_age_I,current_median_age_I)

	} ## for pop
} ## for date

		correlation <- round(cor(x=collect_total_ifr[-1], y=collect_median_age_I[-1], method = "pearson"),2)

		text(x=5,y=3.5,paste("Pearson's correlation coefficient: ",correlation,sep=""),pos=4)

dev.off()

#
## 4. Association between total IFR and median age of population
#

dev.off()

pdf(file="association-total-IFR-median-age-population.pdf", width=10, height=10, family="Times", pointsize=16, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(1.2,0.4,0.8,0.0))

plot(-100,-100,xlim=c(10,50),ylim=c(0,4.5),xlab="Median age of population", ylab="", main="", axes=FALSE)
title(bquote(atop("Association between population median age and total IFR, in %")),font.main=2)

points(x=seq(10,40,length=3),y=rep(4.5,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(10,40,length=3),y=rep(4.5,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.7)
points(x=seq(10,40,length=3),y=rep(4.3,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(10,40,length=3),y=rep(4.3,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.7)

segments(x0=seq(20,50,10),x1=seq(20,50,10),y0=0,y1=4,lty=2,col=gray(0.8))
segments(x0=10,x1=50,y0=seq(0,4,1),y1=seq(0,4,1),lty=2,col=gray(0.8))

axis(side=1,at=seq(10,50,5),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(10,50,10),labels=TRUE,lwd=3,pos=0)
axis(side=2,at=seq(0,4,0.5),labels=FALSE,lwd=1,pos=10)
axis(side=2,at=seq(0,4,1),labels=TRUE,lwd=3,pos=10)

collect_total_ifr <- c(NA)
collect_median_age_pop <- c(NA)

for(date in 1:length(dates_and_number_of_values_lt_24)){

	current_x <- which(calendar_dates==names(dates_and_number_of_values_lt_24[date]))
	current_y <- total_IFR_array_Verity_scaled[,names(dates_and_number_of_values_lt_24)[date],"central"]

	## single countries:
	current_cois <- names(current_y[which(!is.na(current_y))])

	for(pop in 1:length(current_cois)){
		
		current_coi <- current_cois[pop]
		current_total_ifr <- total_IFR_array_Verity_scaled[current_coi,as.character(names(dates_and_number_of_values_lt_24[date])),"central"]

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

		current_median_age_pop <- as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]==current_coi_long),"2020"])

		current_row <- which(countries_by_world_region[,1]==current_coi)
		current_pal <- which(names(pal)==countries_by_world_region[current_row,2])

		points( current_median_age_pop, current_total_ifr , pch=20, col=adjustcolor(pal[current_pal],alpha.f=0.2))

		collect_total_ifr <- c(collect_total_ifr,current_total_ifr)
		collect_median_age_pop <- c(collect_median_age_pop,current_median_age_pop)

	} ## for pop
} ## for date

		correlation <- round(cor(x=collect_total_ifr[-1], y=collect_median_age_pop[-1], method = "pearson"),2)

		text(x=15,y=3.5,paste("Pearson's correlation coefficient: ",correlation,sep=""),pos=4)

dev.off()

#
## 5. To consider:
#

table(collect_median_age_pop[which(collect_current_coi=="Japan")])
table(collect_median_age_I[which(collect_current_coi=="Japan")])
mean(collect_median_age_I[which(collect_current_coi=="Japan")])
median(collect_median_age_I[which(collect_current_coi=="Japan")])
quantile(collect_total_ifr[which(collect_current_coi=="Japan")])

collect_median_age_pop[which(collect_current_coi=="Australia")]
table(collect_median_age_I[which(collect_current_coi=="Australia")])
mean(collect_median_age_I[which(collect_current_coi=="Australia")])
median(collect_median_age_I[which(collect_current_coi=="Australia")])
quantile(collect_total_ifr[which(collect_current_coi=="Australia")])
quantile(collect_total_ifr,prob=c(0.8,0.9,0.925),na.rm=TRUE)

#
## 6. Calculate effectiveness in protecting the elderly (here called performance):
#

## 6.1: Calculate initial population risk (  IFR(P_x , IFR_x ) )

  row_names <- all_countries
  column_names <- "2019"
  matrix_names <- c("low95","central","up95")

  initial_total_IFR_array_Verity_scaled <- array(NA,dim=c(length(all_countries),1,length(c("low95","central","up95"))),
								dimnames=list(row_names,column_names,matrix_names))

  for(coi in 1:length(all_countries)){
	current_coi <- all_countries[coi]
	current_pop <- pop_count[,current_coi]
	start <- seq(1,96,5)
	end <- seq(5,100,5)
	current_pop_5age <- 0
	for(agegroup in 1:length(start)){
		current_pop_5age[agegroup] <- sum(current_pop[start[agegroup]:end[agegroup]])
	}
	sum(current_pop_5age)
	sum(current_pop)
	current_total_IFR <- sum ( IFRs_Verity_scaled_central[,current_coi] * current_pop_5age / sum(current_pop_5age) ) * 100
	initial_total_IFR_array_Verity_scaled[current_coi,"2019","central"] <- current_total_IFR
  }

## 6.2: Calculate effectiveness in protecting the elderly
#### as relative performance between realized infection risk and initial population risk

performance <- (total_IFR_array_Verity_scaled[,,"central"] - initial_total_IFR_array_Verity_scaled[,"2019","central"]) / initial_total_IFR_array_Verity_scaled[,"2019","central"] 

## quantile(performance,na.rm=TRUE)

## 6.3: What countries have performed well and what countries could have performed better?
#### Visualize effectiveness in protecting the elderly with population median age

collect_median_performance <- NA
collect_median_ifr <- NA
collect_performance_coi <- NA
collect_median_age_coi <- NA


require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
names(pal)[1:length(unique(countries_by_world_region[,2]))] <- unique(countries_by_world_region[,2])

setwd("./plots")

dev.off()

pdf(file="total-IFR-median-performance-median-pop-age.pdf", width=10, height=10, family="Times", pointsize=16, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.4,0.4,0.8,0.0))

plot(-100,-100,xlim=c(-1.5,2.5),ylim=c(15,54),xlab="", ylab="", main="", axes=FALSE)
title(bquote(atop("Effectiveness in protecting the elderly in relation to initial population risk")),font.main=2)

points(x=seq(-1,1.8,length=3),y=rep(55,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(-1,1.8,length=3),y=rep(55,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.7)
points(x=seq(-1,1.8,length=3),y=rep(53,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(-1,1.8,length=3),y=rep(53,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.7)

segments(x0=seq(-1,2.5,0.5),x1=seq(-1,2.5,0.5),y0=15,y1=50,lty=2,col=gray(0.8))
segments(x0=-1,x1=2.5,y0=seq(20,50,5),y1=seq(20,50,5),lty=2,col=gray(0.8))

lines(x=seq(-1,2.5,length=2),y=rep(0,2),col="black",lwd=3)
lines(x=rep(0,2),y=seq(15,52,length=2),col="black",lwd=3)

axis(side=1,at=seq(-1,2.5,0.25),labels=FALSE,lwd=1,pos=15)
axis(side=1,at=seq(-1,2.5,0.5),labels=TRUE,lwd=3,pos=15)
axis(side=2,at=seq(15,50,1),labels=FALSE,lwd=1,pos=-1.5)
axis(side=2,at=seq(15,50,5),labels=TRUE,lwd=3,pos=-1.5)

text(x=0,y=51,"More effective",font=2,pos=2)
text(x=0,y=51,"Less effective",font=2,pos=4)

text(x=-1.25,y=24,"Lower initial risk",font=2,pos=3,srt=90)
text(x=-1.25,y=41,"Higher initial risk",font=2,pos=3,srt=90)

text(x=-1.65,y=51.75,"Median",font=2,cex=0.7,pos=4)
text(x=-1.65,y=51,"population age",font=2,cex=0.7,pos=4)

for(coi in 1:length(all_countries)){
	current_coi <- all_countries[coi]

	current_ifr <- median(total_IFR_array_Verity_scaled[current_coi,,"central"],na.rm=TRUE) 
	current_performance <- median(performance[current_coi,],na.rm=TRUE)

	current_row <- which(countries_by_world_region[,1]==current_coi)
	current_pal <- which(names(pal)==countries_by_world_region[current_row,2])

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

	current_median_age_pop <- as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]==current_coi_long),"2020"])

	if(length(current_median_age_pop)>0 & !is.na(current_performance)){
		text(x=current_performance, y=current_median_age_pop, current_coi, col=pal[current_pal], font=2)

		collect_median_performance[coi] <- current_performance
		collect_median_ifr[coi] <- current_ifr
		collect_performance_coi[coi] <- current_coi
		collect_median_age_coi[coi] <- current_median_age_pop
	}

}

dev.off()

#
## 7. Calculate effectiveness in protecting the elderly (here called performance):
## and perhaps account for infection prevalence
#

