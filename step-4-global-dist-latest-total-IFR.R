
#
## 1. Set working directory:
#

setwd(.)


#
## 2. Zoom into the spatio-temporal distribution,
#### and compare the total IFR at the latest COMMON date 
#### (taking COVer-AGE-DB from 8 February 2021, this is 13 January 2021)
#### based on COI-specific IFR_x and I_x/I 
#### including sensitivity towards scaling IFR_x from various sources
#

## 2.1: Collect countries of interest with total IFR estimate as of 13 Jan 2021 and their population median age:

latest_common_date <- names(dates_and_number_of_values_lt_24)[length(dates_and_number_of_values_lt_24)]
countries_of_interest <- total_IFR_array_Verity_scaled[,as.character(latest_common_date),"central"]
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

coi_ifr_median_age <- coi_ifr_median_age[order(coi_ifr_median_age[,1]),]

## 2.2: Finally plot global distribution of latest total IFR, in %

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
names(pal)[1:length(unique(countries_by_world_region[,2]))] <- unique(countries_by_world_region[,2])

setwd(./plots)

dev.off()

pdf(file=paste("total-IFR-latest-common-date-log-scale.pdf",sep=""), width=15, height=15, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.4,0.4,1.0,0.4))

plot(x=-100,y=-100,xlim=c(log(0.02),log(10+10)),ylim=c(18,52),xlab="",ylab="",main="",axes=FALSE)
title(bquote(atop("Total infection fatality rate, in %", "Based on country-specific " ~ IFR[x] ~ " and " ~I[x] ~ " as of January 13, 2021")),font.main=2)

axis(side=1,at=log(c(0.05,0.1,0.25,0.5,1,2,3,5,10)),labels=c(0.05,0.1,0.25,0.5,1,2,3,5,10),lwd=3,pos=18)
axis(side=4,at=seq(18,48,1),labels=FALSE,lwd=1,pos=log(10+5))
axis(side=4,at=seq(18,48,3),labels=TRUE,lwd=2,pos=log(10+5))
axis(side=4,at=seq(18,48,6),labels=TRUE,lwd=3,pos=log(10+5))

segments(x0=log(c(0.05,0.1,0.25,0.5,1,2,3,5,10)),x1=log(c(0.05,0.1,0.25,0.5,1,2,3,5,10)),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

text(x=log(10+5),y=48.5,"Median age",pos=3,font=2)

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

selected_coi_for_vis_by_mean_age <- c("Portugal","South Korea",
					"France","Sweden","Australia",
						"Chile","Colombia","Mexico",
						"Philippines","Togo","Slovenia")

selected_coi_for_vis_by_ifr <- c("Slovenia","Portugal","Spain","South Korea",
					"France","Sweden","Australia",
						"Chile","Colombia","Mexico",
						"Philippines","Togo")


for(country in 1:dim(coi_median_age)[1]){

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

			if(infections){
				points(x=log(current_total_IFR_central),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=0.6),cex=2.5)
			}
			if(infections_PI){	
				points(x=log(current_total_IFR_low),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=0.6),cex=0.7)
				points(x=log(current_total_IFR_up),y=current_median_age,pch=current_pch,col=adjustcolor(pal[current_pal],alpha.f=0.6),cex=0.7)
			}

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

	plot(x=-100,y=-100,xlim=c(-4.5,4),ylim=c(18,53),xlab="",ylab="",main="",axes=FALSE)
	title(bquote(atop("Sensitivity of total infection fatality rate, in percentage points, ", "towards proportional changes in scaled " ~ IFR[x] ~ ", using data as of January 13, 2021")),font.main=2)

	selected_coi_for_vis_by_mean_age <- c("Portugal","Slovenia","South Korea",
					"France","Sweden","Australia","Chile",
					"Colombia","Mexico","Philippines","Togo")

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

par(fig = c(0,0.35,0,1), las=1, mai=c(0.6,0.0,1.4,0.4))

	axis(side=1,at=seq(-3.5,3.5,0.5),labels=TRUE,lwd=3,pos=18)
	axis(side=4,at=seq(18,48,1),labels=FALSE,lwd=1,pos=3.8,yaxt="n")
	axis(side=4,at=seq(18,48,3),labels=TRUE,lwd=2,pos=3.8,yaxt="n")
	axis(side=4,at=seq(18,48,6),labels=TRUE,lwd=3,pos=3.8,yaxt="n")

	segments(x0=seq(-3.5,3.5,0.5),x1=seq(-3.5,3.5,0.5),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

	segments(x0=0,x1=0,y0=18,y1=48,lwd=3,col="black")

	text(x=0,y=48,"Verity et al.",pos=3,font=2)

	for(country in 1:dim(coi_median_age)[1]){

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

par(fig = c(0.32,0.67,0,1), las=1, mai=c(0.6,0.0,1.4,0.4))

	axis(side=1,at=seq(-3.5,3.5,0.5),labels=TRUE,lwd=3,pos=18)
	axis(side=4,at=seq(18,48,1),labels=FALSE,lwd=1,pos=3.8,yaxt="n")
	axis(side=4,at=seq(18,48,3),labels=TRUE,lwd=2,pos=3.8,yaxt="n")
	axis(side=4,at=seq(18,48,6),labels=TRUE,lwd=3,pos=3.8,yaxt="n")

	segments(x0=seq(-3.5,3.5,0.5),x1=seq(-3.5,3.5,0.5),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

	segments(x0=0,x1=0,y0=18,y1=48,lwd=3,col="black")

	text(x=0,y=48,"Levin et al.",pos=3,font=2)

	for(country in 1:dim(coi_median_age)[1]){

		current_coi <- countries_of_interest[country] 

		if(current_coi %in% selected_coi_for_vis_by_mean_age){
	
			current_median_age <- coi_ifr_median_age[current_coi,2]
			current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
	
			segments(x0=-3.5,x1=3.5,y0=current_median_age,y1=current_median_age,col=gray(0.6),lwd=1,lty=2)
			## text(x=-3.5,y=as.numeric(current_median_age),paste(current_coi),pos=2,col=pal[current_pal],font=2)

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

par(fig = c(0.64,0.99,0,1), las=1, mai=c(0.6,0.0,1.4,0.4))

	axis(side=1,at=seq(-3.5,3.5,0.5),labels=TRUE,lwd=3,pos=18)
	axis(side=4,at=seq(18,48,1),labels=FALSE,lwd=1,pos=3.8)
	axis(side=4,at=seq(18,48,3),labels=TRUE,lwd=2,pos=3.8)
	axis(side=4,at=seq(18,48,6),labels=TRUE,lwd=3,pos=3.8)

	segments(x0=seq(-3.5,3.5,0.5),x1=seq(-3.5,3.5,0.5),
		y=18,y1=as.numeric(median_age[which(median_age[,"Region,.subregion,.country.or.area.*"]=="Portugal"),"2020"]),
		col=gray(0.6),lwd=1,lty=2)

	text(x=3.0,y=48.5,"Median age",pos=3,font=2)

	segments(x0=0,x1=0,y0=18,y1=48,lwd=3,col="black")

	text(x=0,y=48,"Salje et al.",pos=3,font=2)

	for(country in 1:dim(coi_median_age)[1]){

		current_coi <- countries_of_interest[country] 

		if(current_coi %in% selected_coi_for_vis_by_mean_age){
	
			current_median_age <- coi_ifr_median_age[current_coi,2]
			current_pal <- which(names(pal)==countries_by_world_region[which(countries_by_world_region[,1]==current_coi),2])
	
			segments(x0=-3.5,x1=3.5,y0=current_median_age,y1=current_median_age,col=gray(0.6),lwd=1,lty=2)
			## text(x=-3.5,y=as.numeric(current_median_age),paste(current_coi),pos=2,col=pal[current_pal],font=2)

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
#### towards I_x based on excess mortality death counts




  
 


