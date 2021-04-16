
#
## 1. Set working directory:
#

setwd(".")

## 2. Scenarios of latest total IFR
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

