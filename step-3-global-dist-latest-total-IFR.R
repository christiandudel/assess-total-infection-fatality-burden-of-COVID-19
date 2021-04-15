
#
## 1. Set working directory:
#

setwd(".")

#
## 2. Zoom into the spatio-temporal distribution,
#### and compare the total IFR at the latest COMMON date 
#### (taking COVer-AGE-DB from 14 April 2021, this is 31 March 2021)
#### based on COI-specific IFR_x and I_x/I 
#### including sensitivity towards scaling IFR_x from various sources
#

## 2.1: Collect countries of interest with total IFR estimate as of 31 March 2021 and their population median age:

latest_common_date <- names(dates_and_number_of_values_lt_24)[length(dates_and_number_of_values_lt_24)]
## latest_common_date <- "2021-03-31"
countries_of_interest <- total_IFR_array_Verity_scaled[,as.character(latest_common_date),"central"]
current_deaths <- apply(X=deaths_array[,latest_common_date,],1,function(x){sum(x)})
countries_of_interest <- countries_of_interest[which(current_deaths>=200)] 
countries_of_interest <- names(countries_of_interest[which(!is.na(countries_of_interest ))]) 
countries_of_interest 

coi_ifr_median_age <- matrix(NA,nr=length(countries_of_interest),nc=2)
rownames(coi_ifr_median_age) <- countries_of_interest
coi_ifr_median_age[,1] <- total_IFR_array_Verity_scaled[countries_of_interest,as.character(latest_common_date),"central"]
  
for(country in 1:length(countries_of_interest)){
	current_coi <- countries_of_interest[country] 
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
	current_median_age <- as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]==current_coi_long),"2020"])
	coi_ifr_median_age[current_coi,2] <- current_median_age
} ## for country

coi_ifr_median_age <- coi_ifr_median_age[order(coi_ifr_median_age[,2]),]

## 2.2: Finally plot global distribution of latest total IFR, in %

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
names(pal)[1:length(unique(countries_by_world_region[,2]))] <- unique(countries_by_world_region[,2])

setwd(./plots)

dev.off()

pdf(file=paste("total-IFR-latest-common-date-log-scale.pdf",sep=""), width=15, height=15, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.4,0.4,1.0,0.4))

plot(x=-100,y=-100,xlim=c(log(0.02),log(10+10)),ylim=c(22,52),xlab="",ylab="",main="",axes=FALSE)
title(bquote(atop("Total infection fatality rate, in %", "Based on country-specific " ~ IFR[x] ~ " and " ~I[x] ~ " as of March 31, 2021")),font.main=2)

axis(side=1,at=log(c(0.05,0.1,0.25,0.5,1,2,3,5,10)),labels=c(0.05,0.1,0.25,0.5,1,2,3,5,10),lwd=3,pos=22)
axis(side=4,at=seq(22,48,1),labels=FALSE,lwd=1,pos=log(10+5))
axis(side=4,at=seq(22,48,2),labels=TRUE,lwd=3,pos=log(10+5))

segments(x0=log(c(0.05,0.1,0.25,0.5,1,2,3,5,10)),x1=log(c(0.05,0.1,0.25,0.5,1,2,3,5,10)),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

text(x=log(10+5),y=49.25,"Population",pos=3,font=2)
text(x=log(10+5),y=48.5,"median age",pos=3,font=2)

points(x=seq(log(0.25),log(3),length=length(c("Verity","Levin","Salje"))),y=rep(51,length(c("Verity","Levin","Salje"))),pch=c(19,17,15,18),col="black",cex=2.5)

text(x=seq(log(0.25),log(3),length=length(c("Verity","Levin","Salje"))),y=rep(50.7,length(c("Verity","Levin","Salje"))),c("Verity","Levin","Salje"),pos=1,cex=0.6,font=2)
text(x=log(0.15),y=50.7,expression('Scaled '*'IFR'[x]*' of:'),pos=1,cex=0.6,font=2)
text(x=seq(log(0.25),log(3),length=length(c("Verity","Levin","Salje"))),y=rep(51.3,length(c("Verity","Levin","Salje"))),expression('I'[x]),pos=3,cex=0.6,font=2)

points(x=rep(log(5),2),y=c(51.5,50.5),pch=19,col=gray(0.7),cex=c(2.5,0.7))
text(x=log(5.25),y=51.5,"Central estimate",pos=4,cex=0.6,font=2)
text(x=log(5),y=50.5,"95% PI",pos=4,cex=0.6,font=2)

points(x=seq(log(0.016),log(0.06),length=3),y=rep(51.3,3),pch=20,cex=1.8,col=pal[1:3])
text(x=seq(log(0.016),log(0.06),length=3),y=rep(51.3,3),c(names(pal)[1:3]),font=2,pos=4,cex=0.6)
points(x=log(0.016),y=50.3,pch=20,cex=1.8,col=pal[4])
text(x=log(0.016),y=50.3,c(names(pal)[4]),font=2,pos=4,cex=0.6)
points(x=log(0.016),y=49.3,pch=20,cex=1.8,col=pal[5])
text(x=log(0.016),y=49.3,c(names(pal)[5]),font=2,pos=4,cex=0.6)
points(x=log(0.016),y=48.3,pch=20,cex=1.8,col=pal[6])
text(x=log(0.016),y=48.3,c(names(pal)[6]),font=2,pos=4,cex=0.6)

selected_coi_for_vis_by_mean_age <- c("Italy","Germany","Slovenia",
						"South Korea","France","Sweden","Australia",
						"Colombia","Jamaica",
						"Philippines","Jordan")

## selected_coi_for_vis_by_mean_age <- c("Portugal","South Korea",
##					"France","Sweden","Australia",
##						"Chile","Colombia","Mexico",
##						"Philippines","Jordan","Slovenia")

for(country in 1:dim(coi_ifr_median_age)[1]){

	current_coi <- countries_of_interest[country] 

	if(current_coi %in% selected_coi_for_vis_by_mean_age){

		current_median_age <- coi_ifr_median_age[current_coi,2]
		current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
	
		segments(x0=log(0.05),x1=log(10),y0=current_median_age,y1=current_median_age,col=gray(0.6),lwd=1,lty=2)
		text(x=log(0.05),y=as.numeric(current_median_age),paste(current_coi),pos=2,col=pal[current_pal],font=2)

		for(sourceIFR in c("Verity","Levin","Salje")){

			inputIFRx <- sourceIFR
			current_pch <- c(19,17,15,18)[which(c("Verity","Levin","Salje")==sourceIFR)]

			current_total_IFR_central <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),2]",sep=""))))
			current_total_IFR_low <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),1]",sep=""))))
			current_total_IFR_up <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),3]",sep=""))))

			points(x=log(current_total_IFR_central),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=0.6),cex=2.5)
			points(x=log(current_total_IFR_low),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=0.6),cex=0.7)
			points(x=log(current_total_IFR_up),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=0.6),cex=0.7)

		} ## for sourceIFR
	} ## if current_coi
} ## for country
 
dev.off()

## 3. Sensitivity of latest total IFR
#### towards proportionally changing scaled IFR_x
#### in order to simulate possible improved medical treatment and effective vaccination
#### and / or increased spread of deadlier SARS-CoV-2 variants

setwd(./plots)

dev.off()

pdf(file=paste("latest-total-IFR-sensitivity-towards-change-in-scaled-IFRx.pdf",sep=""), width=25, height=15, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,3.2,1.0,0.4))

	plot(x=-100,y=-100,xlim=c(-4.5,4),ylim=c(22,53),xlab="",ylab="",main="",axes=FALSE)
	title(bquote(atop("Impact on latest IFR, in percentage points, ", "as a result of proportionally changing scaled " ~ IFR[x] ~ ", using data as of March 31, 2021")),font.main=2)

	selected_coi_for_vis_by_mean_age <- c("Italy","Germany","Slovenia",
						"South Korea","France","Sweden","Australia",
						"Colombia","Jamaica",
						"Philippines","Jordan")

	points(x=seq(-4.8,3.2,length=6),y=rep(53.2,6),pch=20,cex=1.8,col=pal[1:6])
	text(x=seq(-4.8,3.2,length=6),y=rep(53.2,6),c(names(pal)[1:6]),font=2,pos=4,cex=0.65)

	pal_inflation <- colorRampPalette(c(gray(0.85),gray(0.15),gray(0.85)))
	pal_inflation <- pal_inflation(length(seq(0.4,1.6,0.2)))

	points(x=seq(-2.2,1.0,length=length(seq(0.4,1.6,0.2))),y=rep(51,length(seq(0.4,1.6,0.2))),pch=19,col=adjustcolor(pal_inflation,alpha.f=0.6),cex=2.0)

	text(x=seq(-2.2,1.0,length=length(seq(0.4,1.6,0.2))),y=rep(50.8,length(seq(0.4,1.6,0.2))),seq(0.4,1.6,0.2),pos=1,cex=0.6)
	text(x=-2.8,y=50.8,expression('Scaled '*'IFR'[x]*' times:'),pos=1,cex=0.6)
	text(x=seq(-2.2,1.0,length=length(seq(0.4,1.6,0.2))),y=rep(51.2,length(seq(0.4,1.6,0.2))),expression('I'[x]),pos=3,cex=0.6)

	points(x=rep(1.5,2),y=c(51.5,50.5),pch=19,col=gray(0.7),cex=c(2.0,0.7))
	text(x=1.5,y=51.5,"Central estimate",pos=4,cex=0.6)
	text(x=1.5,y=50.5,"95% PI",pos=4,cex=0.6)

par(fig = c(0,0.39,0,1), las=1, mai=c(0.6,0.0,1.4,0.0))

	axis(side=1,at=seq(-3.5,3.5,0.5),labels=TRUE,lwd=3,pos=22)
	axis(side=4,at=seq(22,48,1),labels=FALSE,lwd=1,pos=3.8,yaxt="n")
	axis(side=4,at=seq(22,48,2),labels=TRUE,lwd=2,pos=3.8,yaxt="n")

	segments(x0=seq(-3.5,3.5,0.5),x1=seq(-3.5,3.5,0.5),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

	segments(x0=0,x1=0,y0=18,y1=48,lwd=3,col="black")

	text(x=0,y=48,"Verity et al.",pos=3,font=2)

	for(country in 1:dim(coi_ifr_median_age)[1]){

		current_coi <- countries_of_interest[country] 

		if(current_coi %in% selected_coi_for_vis_by_mean_age){
	
			current_median_age <- coi_ifr_median_age[current_coi,2]
			current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
	
			segments(x0=-3.5,x1=3.5,y0=current_median_age,y1=current_median_age,col=gray(0.6),lwd=1,lty=2)
			text(x=-3.5,y=as.numeric(current_median_age),paste(current_coi),pos=2,col=pal[current_pal],font=2)

			for(sourceIFR in c("Verity")){

				inputIFRx <- sourceIFR
				current_pch <- c(19,17,15,18)[which(c("Verity","Levin","Salje","Acosta")==sourceIFR)]

				current_total_IFR_Ix_central <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),2]",sep=""))))
				current_total_IFR_Ix_low <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),1]",sep=""))))
				current_total_IFR_Ix_up <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),3]",sep=""))))
	
				for(factor in seq(0.4,0.8,0.2)){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=0.7)
				} ## for factor	
				for(factor in seq(1.2,1.6,0.2)){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=0.7)
				} ## for factor					

				for(factor in 1:1){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=0.7)
				} ## for factor					

			} ## for sourceIFR

		} ## if current_coi

	} ## for country

par(fig = c(0.3,0.69,0,1), las=1, mai=c(0.6,0.0,1.4,0.0))

	axis(side=1,at=seq(-3.5,3.5,0.5),labels=TRUE,lwd=3,pos=22)
	axis(side=4,at=seq(22,48,1),labels=FALSE,lwd=1,pos=3.8,yaxt="n")
	axis(side=4,at=seq(22,48,2),labels=TRUE,lwd=2,pos=3.8,yaxt="n")

	segments(x0=seq(-3.5,3.5,0.5),x1=seq(-3.5,3.5,0.5),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

	segments(x0=0,x1=0,y0=18,y1=48,lwd=3,col="black")

	text(x=0,y=48,"Levin et al.",pos=3,font=2)

	for(country in 1:dim(coi_ifr_median_age)[1]){

		current_coi <- countries_of_interest[country] 

		if(current_coi %in% selected_coi_for_vis_by_mean_age){
	
			current_median_age <- coi_ifr_median_age[current_coi,2]
			current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
	
			segments(x0=-3.5,x1=3.5,y0=current_median_age,y1=current_median_age,col=gray(0.6),lwd=1,lty=2)

			for(sourceIFR in c("Levin")){

				inputIFRx <- sourceIFR
				current_pch <- c(19,17,15,18)[which(c("Verity","Levin","Salje","Acosta")==sourceIFR)]

				current_total_IFR_Ix_central <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),2]",sep=""))))
				current_total_IFR_Ix_low <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),1]",sep=""))))
				current_total_IFR_Ix_up <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),3]",sep=""))))
	
				for(factor in seq(0.4,0.8,0.2)){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=0.7)
				} ## for factor	
				for(factor in seq(1.2,1.6,0.2)){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=0.7)
				} ## for factor					

				for(factor in 1:1){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=0.7)
				} ## for factor					

			} ## for sourceIFR

		} ## if current_coi

	} ## for country

par(fig = c(0.6,0.99,0,1), las=1, mai=c(0.6,0.0,1.4,0.4))

	axis(side=1,at=seq(-3.5,3.5,0.5),labels=TRUE,lwd=3,pos=22)
	axis(side=4,at=seq(22,48,1),labels=FALSE,lwd=1,pos=3.8)
	axis(side=4,at=seq(22,48,2),labels=TRUE,lwd=2,pos=3.8)

	segments(x0=seq(-3.5,3.5,0.5),x1=seq(-3.5,3.5,0.5),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

	text(x=3.0,y=49.25,"Population",pos=3,font=2)
	text(x=3.0,y=48.5,"median age",pos=3,font=2)

	segments(x0=0,x1=0,y0=18,y1=48,lwd=3,col="black")

	text(x=0,y=48,"Salje et al.",pos=3,font=2)

	for(country in 1:dim(coi_ifr_median_age)[1]){

		current_coi <- countries_of_interest[country] 

		if(current_coi %in% selected_coi_for_vis_by_mean_age){
	
			current_median_age <- coi_ifr_median_age[current_coi,2]
			current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
	
			segments(x0=-3.5,x1=3.5,y0=current_median_age,y1=current_median_age,col=gray(0.6),lwd=1,lty=2)

			for(sourceIFR in c("Salje")){

				inputIFRx <- sourceIFR
				current_pch <- c(19,17,15,18)[which(c("Verity","Levin","Salje","Acosta")==sourceIFR)]

				current_total_IFR_Ix_central <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),2]",sep=""))))
				current_total_IFR_Ix_low <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),1]",sep=""))))
				current_total_IFR_Ix_up <- eval(parse(text=(paste("total_IFR_array_",inputIFRx,"_scaled[current_coi,as.character(latest_common_date),3]",sep=""))))
	
				for(factor in seq(0.4,0.8,0.2)){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(1-factor)),cex=0.7)
				} ## for factor	
				for(factor in seq(1.2,1.6,0.2)){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=1-abs(factor-1)),cex=0.7)
				} ## for factor					

				for(factor in 1:1){
					current_total_IFR_Ix_central_factor <- current_total_IFR_Ix_central * factor
					current_total_IFR_Ix_low_factor <- current_total_IFR_Ix_low * factor
					current_total_IFR_Ix_up_factor <- current_total_IFR_Ix_up * factor

					points(x=((current_total_IFR_Ix_central_factor-current_total_IFR_Ix_central)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=2.0)
					points(x=((current_total_IFR_Ix_low_factor-current_total_IFR_Ix_low)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=0.7)
					points(x=((current_total_IFR_Ix_up_factor-current_total_IFR_Ix_up)),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=abs(factor)),cex=0.7)
				} ## for factor					

			} ## for sourceIFR

		} ## if current_coi

	} ## for country

dev.off()


## 4. Sensitivity of latest total IFR
#### towards I_x based on either reported deaths (COVer-AGE-DB) or excess mortality deaths (STMF)

## 4.1 Align country names between excess mortality deaths (estimated based on data of STMF) and reported deaths (COVer-AGE-DB):

dimnames(excess_deaths)
dimnames(excess_deaths)[[1]][which(dimnames(excess_deaths)[[1]]=="Republic of Korea")] <- "South Korea" 
dimnames(excess_deaths)[[1]][which(dimnames(excess_deaths)[[1]]=="England_Wales")] <- "England and Wales" 

## 4.2 Compare set of countries and dates between reported and excess mortality deaths:  

dimnames(excess_deaths)
dimnames(deaths_array)

## 4.3 To split 90+ excess deaths into 90-94 and 95+ excess mortality deaths: 

deaths_array_share_95plus_of_90plus <- rowSums(deaths_array[,,as.character(seq(95,100,5))],dims=2,na.rm=TRUE) / rowSums(deaths_array[,,as.character(seq(90,100,5))],dims=2,na.rm=TRUE)

## 4.4 Estimate numbers of infections and total IFR based on excess mortality deaths:

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
  
## 4.5 Calculate difference in total IFR that are based on infections using reported and excess mortality deaths:   

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

## 4.6 Visualize this difference in total IFR:

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



  
 


