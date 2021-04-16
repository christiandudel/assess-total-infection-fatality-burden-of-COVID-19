
#
## 1. Set working directory:
#

setwd(.)

#
## 2. Association between IFR and population median age
#

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
names(pal)[1:length(unique(countries_by_world_region[,2]))] <- unique(countries_by_world_region[,2])

setwd(./plots)

dev.off()

par(fig = c(0,1,0,1), las=1, mai=c(1.2,0.4,0.8,0.0))

plot(-100,-100,xlim=c(10,50),ylim=c(0,4.0),xlab="Population median age", ylab="", main="", axes=FALSE)
title(bquote(atop("Correlation between IFR, in %, and population median age")),font.main=2)

points(x=seq(10,40,length=3),y=rep(4.0,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(10,40,length=3),y=rep(4.0,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.7)
points(x=seq(10,40,length=3),y=rep(3.8,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(10,40,length=3),y=rep(3.8,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.7)

segments(x0=seq(20,50,10),x1=seq(20,50,10),y0=0,y1=3.5,lty=2,col=gray(0.8))
segments(x0=10,x1=50,y0=seq(0,3.5,0.5),y1=seq(0,3.5,0.5),lty=2,col=gray(0.8))

axis(side=1,at=seq(10,50,5),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(10,50,10),labels=TRUE,lwd=3,pos=0)
axis(side=2,at=seq(0,3.5,0.25),labels=FALSE,lwd=1,pos=10)
axis(side=2,at=seq(0,3.5,0.5),labels=TRUE,lwd=3,pos=10)

collect_total_ifr <- c(NA)
collect_median_age_pop <- c(NA)

for(date in 1:length(dates_and_number_of_values_lt_24)){

	current_y <- total_IFR_array_Verity_scaled[,names(dates_and_number_of_values_lt_24)[date],"central"]
	current_yy <- rowSums(deaths_array[,names(dates_and_number_of_values_lt_24)[date],])

	## single countries (non_NA, dates with at least 25 countries, countries with at least 200 deaths):
	current_cois <- names(current_y[which(!is.na(current_y))])[which(names(current_y[which(!is.na(current_y))])%in%names(current_yy[which(current_yy>199)]))]

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
		if(current_coi == "Moldova"){
			current_coi_long <- "Republic of Moldova"
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
		text(x=15,y=3.25,paste("Pearson's correlation coefficient: ",correlation,sep=""),pos=4,cex=1.4,font=2)

		mean_IFR_AUS <- mean(collect_total_ifr[which(collect_current_coi=="Australia")],na.rm=TRUE)
		median_age_pop_AUS <- as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Australia"),"2020"])
		text(x=38,y=mean_IFR_AUS,"Australia",pos=2,col=pal[5],font=2,cex=1.4)

		mean_IFR_JPN <- mean(collect_total_ifr[which(collect_current_coi=="Japan")],na.rm=TRUE)
		median_age_pop_JPN <- as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Japan"),"2020"])
		text(x=48,y=mean_IFR_JPN,"Japan",pos=4,col=pal[1],font=2,cex=1.4)

dev.off()


#
## 3. Association between IFR and mean age of infected individuals
#

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
names(pal)[1:length(unique(countries_by_world_region[,2]))] <- unique(countries_by_world_region[,2])

setwd(./plots)

dev.off()

pdf(file="association-total-IFR-mean-age-infected-individuals.pdf", width=10, height=10, family="Times", pointsize=16, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(1.2,0.4,0.8,0.0))

plot(-100,-100,xlim=c(0,80),ylim=c(0,4.0),xlab="Mean age of infected population", ylab="", main="", axes=FALSE)
title(bquote(atop("Correlation between IFR, in %, and mean age of infected population")),font.main=2)

points(x=seq(0,70,length=3),y=rep(4.0,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(0,70,length=3),y=rep(4.0,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.7)
points(x=seq(0,70,length=3),y=rep(3.8,3),pch=20,cex=1.8,col=pal[4:6])
text(x=seq(0,70,length=3),y=rep(3.8,3),c(names(pal)[4:6]),font=2,pos=4,cex=0.7)

segments(x0=seq(10,80,10),x1=seq(10,80,10),y0=0,y1=3.5,lty=2,col=gray(0.8))
segments(x0=0,x1=80,y0=seq(0,3.5,0.5),y1=seq(0,3.5,0.5),lty=2,col=gray(0.8))

axis(side=1,at=seq(0,80,10),labels=FALSE,lwd=1,pos=0)
axis(side=1,at=seq(0,80,20),labels=TRUE,lwd=3,pos=0)
axis(side=2,at=seq(0,3.5,0.25),labels=FALSE,lwd=1,pos=0)
axis(side=2,at=seq(0,3.5,0.5),labels=TRUE,lwd=3,pos=0)

collect_total_ifr <- c(NA)
collect_mean_age_I <- c(NA)
collect_current_coi <- c(NA)
collect_current_date <- c(NA)

for(date in 1:length(dates_and_number_of_values_lt_24)){

	current_y <- total_IFR_array_Verity_scaled[,names(dates_and_number_of_values_lt_24)[date],"central"]
	current_yy <- rowSums(deaths_array[,names(dates_and_number_of_values_lt_24)[date],])

	## single countries (non_NA, dates with at least 25 countries, countries with at least 200 deaths):
	current_cois <- names(current_y[which(!is.na(current_y))])[which(names(current_y[which(!is.na(current_y))])%in%names(current_yy[which(current_yy>199)]))]

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
		text(x=5,y=3.25,paste("Pearson's correlation coefficient: ",correlation,sep=""),pos=4,cex=1.4,font=2)

		mean_IFR_AUS <- mean(collect_total_ifr[which(collect_current_coi=="Australia")],na.rm=TRUE)
		mean_age_I_AUS <- mean(collect_mean_age_I[which(collect_current_coi=="Australia")],na.rm=TRUE)
		text(x=45,y=mean_IFR_AUS,"Australia",pos=2,col=pal[5],font=2,cex=1.4)
		segments(x0=mean_age_I_AUS,x1=45,y0=mean_IFR_AUS,y1=mean_IFR_AUS,col=pal[5],lwd=2)

		mean_IFR_JPN <- mean(collect_total_ifr[which(collect_current_coi=="Japan")],na.rm=TRUE)
		mean_age_I_JPN <- mean(collect_mean_age_I[which(collect_current_coi=="Japan")],na.rm=TRUE)
		text(x=60,y=mean_IFR_JPN,"Japan",pos=4,col=pal[1],font=2,cex=1.4)
		segments(x0=mean_age_I_JPN,x1=60,y0=mean_IFR_JPN,y1=mean_IFR_JPN,col=pal[1],lwd=2)

dev.off()


#
## 4. To consider:
#

table(collect_median_age_pop[which(collect_current_coi=="Japan")])
table(collect_mean_age_I[which(collect_current_coi=="Japan")])
mean(collect_mean_age_I[which(collect_current_coi=="Japan")])
median(collect_mean_age_I[which(collect_current_coi=="Japan")])
quantile(collect_total_ifr[which(collect_current_coi=="Japan")])

collect_median_age_pop[which(collect_current_coi=="Australia")]
table(collect_mean_age_I[which(collect_current_coi=="Australia")])
mean(collect_mean_age_I[which(collect_current_coi=="Australia")])
median(collect_mean_age_I[which(collect_current_coi=="Australia")])
quantile(collect_total_ifr[which(collect_current_coi=="Australia")])
quantile(collect_total_ifr,prob=c(0.8,0.9,0.925),na.rm=TRUE)

