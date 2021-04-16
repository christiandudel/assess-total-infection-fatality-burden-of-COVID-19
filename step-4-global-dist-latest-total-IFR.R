
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




  
 


