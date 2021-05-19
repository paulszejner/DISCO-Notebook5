
thin_sections <- read.csv("Data_Notebook5/iso_data_Ed.csv")

samples <- names(thin_sections[seq(2,24, 3)])

all_samples_list <- list()

colu <- seq(1,24, 3)

for(s in 1:8){
  sample_1 <- thin_sections[,(colu[s]):(colu[s]+2)]
  sample_1 <- sample_1[1:length(na.omit(sample_1[,1])),]
  
  p25 <- round(0.25*nrow(sample_1),0)
  p50 <- round(0.5*nrow(sample_1),0)
  p75 <- round(0.75*nrow(sample_1),0)
  p100 <- round(1*nrow(sample_1),0)
  
  sample_1$p25 <- c(rep(mean(sample_1[,2][1:p25], na.rm = T),length(1:p25)),
                  rep(mean(sample_1[,2][(1+p25):p50], na.rm = T),length((1+p25):p50)),
                  rep(mean(sample_1[,2][(1+p50):p75], na.rm = T),length((1+p50):p75)),
                  rep(mean(sample_1[,2][(1+p75):p100], na.rm = T),length((1+p75):p100)))
  sample_1$p50<- c(rep(mean(sample_1[,2][1:p50], na.rm = T),length(1:p50)),
                 rep(mean(sample_1[,2][(1+p50):p100], na.rm = T),length((1+p50):p100)))
  sample_1$p100 <- rep(mean(sample_1[,2][1:p100], na.rm = T),length(1:p100))
  
  sample_1$relpos <- seq(0,1,length.out =  nrow(sample_1))
  
  all_samples_list[[s]] <- sample_1
}


### plot

layout(mat = matrix(1:8, nrow = 2, ncol = 4, byrow = T), widths = 1, heights = c(2,2), respect = T)
par(mar=c(1,1,1,1))
for(s in 1:8){

plot(all_samples_list[[s]][,1], all_samples_list[[s]][,2], type="l", ylim=c(22,40))
lines(all_samples_list[[s]][,1], all_samples_list[[s]][,4],type="s", col="blue")
lines(all_samples_list[[s]][,1], all_samples_list[[s]][,5],type="s", col="darkgreen")
lines(all_samples_list[[s]][,1], all_samples_list[[s]][,6],type="s", col="darkred")
lines(all_samples_list[[s]][,1], all_samples_list[[s]][,3],type="p", col="red")
}


layout(mat = matrix(1:8, nrow = 2, ncol = 4, byrow = T), widths = 1, heights = c(2,2), respect = T)
par(mar=c(1,1,1,1))
for(s in 1:8){
  
  plot.ts( all_samples_list[[s]][,2], type="l", ylim=c(22,40))
  lines( all_samples_list[[s]][,4],type="s", col="blue")
  lines(all_samples_list[[s]][,5],type="s", col="darkgreen")
  lines(all_samples_list[[s]][,6],type="s", col="darkred")
  lines( all_samples_list[[s]][,3],type="p", col="red")
}





samples

site_FR <- c("CPP false ring", "UAC false ring", "WCP false ring", "HEL false ring") 
site_tot <- c("CPP ring mean", "UAC ring mean", "WCP ring mean", "HEL ring mean") 
site_25 <- c("CPP forth of a ring", "UAC forth of a ring", "WCP forth of a ring", "HEL forth of a ring") 
site_th <- c("CPP thin sections", "UAC thin sections", "WCP thin sections", "HEL thin sections") 

library(wesanderson)
colb1 <- c("#D8B70A","#02401B","#A2A475","#D67236") #<- wes_palette(4, name = "Zissou1", type = "continuous")




layout(mat = matrix(c(1:4), nrow = 2, ncol = 2, byrow = T), widths = c(2,2), heights = c(2,2), respect = T)

par(mar=c(2,2,1,0),lwd=0.5)

plot(all_samples_list[[1]][,7], all_samples_list[[1]][,2], type="n", ylim=c(23,40), xlim=c(0,1.1), axes=F)
for(s in 1:4){
  abline(h = all_samples_list[[s]][,6][1],col=colb1[s],lwd=1,lty=2)
  lines( all_samples_list[[s]][,7],all_samples_list[[s]][,2], type="p", ylim=c(22,40),col=colb1[s],lwd=0.5 )
  lines(all_samples_list[[s]][,7], all_samples_list[[s]][,4],type="s",col=colb1[s],lwd=2  )
  #lines(all_samples_list[[s]][,7],all_samples_list[[s]][,5],type="s",col=colb1[s],lwd=2,lty=2  )
  lines(1.1,all_samples_list[[s]][,6][1],type="p",col=colb1[s], cex=1, pch=2, lwd=2)
}

for(s in 1:4){  lines(all_samples_list[[s]][,7], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s])}

box()
axis(1, lwd=0.5)
axis(2,lwd=0.5,las=2)
grid(lwd=1)
legend("topleft", legend = site_th, pch = 1, col=colb1 , bty = "n")
legend("topright", legend = site_tot, pch = 2, col=colb1 , bty = "n",lwd=1,lty=2)
legend("bottom", legend = "1998", bty = "n")


par(mar=c(2,0,1,2),lwd=0.5)
plot( all_samples_list[[s]][,7],all_samples_list[[s]][,2], type="n", ylim=c(23,40), xlim=c(0,1.1), axes=F)
for(s in 5:8){
  abline(h = all_samples_list[[s]][,6][1],col=colb1[s-4],lwd=1,lty=2  )
  lines( all_samples_list[[s]][,7],all_samples_list[[s]][,2], type="p", ylim=c(22,40),col=colb1[s-4],lwd=0.5 )
  lines(all_samples_list[[s]][,7], all_samples_list[[s]][,4],type="s",col=colb1[s-4],lwd=2  )
  #lines(all_samples_list[[s]][,7],all_samples_list[[s]][,5],type="s",col=colb1[s-4],lwd=2,lty=2  )
  lines(1.1,all_samples_list[[s]][,6][1],type="p",col=colb1[s-4], cex=1, pch=2, lwd=2)
  
}

for(s in 5:8){lines(all_samples_list[[s]][,7], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s-4])}

axis(2, labels = F,lwd=0.5)
axis(1,lwd=0.5)
axis(4, las=2,lwd=0.5)
box()
grid(lwd=1)
legend("topleft", legend = site_25, col=colb1 , bty = "n",lwd=2)
legend("topright", legend = site_FR, pch = 21, bty = "n" ,col="blue", pt.bg = colb1)
legend("bottom", legend = "1999", bty = "n")

par(mar=c(2,2,1,0),lwd=0.5)
plot(all_samples_list[[1]][,1], all_samples_list[[1]][,2], type="n", ylim=c(23,40), xlim=c(1,365), axes=F)
for(s in 1:4){
  abline(h=all_samples_list[[s]][,6][1],col=colb1[s], lwd=1, lty=2)
  lines( all_samples_list[[s]][,1],all_samples_list[[s]][,2], type="p", ylim=c(22,40),col=colb1[s],lwd=0.5 )
  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,4],type="s",col=colb1[s],lwd=2  )
  #lines(all_samples_list[[s]][,1],all_samples_list[[s]][,5],type="s",col=colb1[s],lwd=2,lty=2  )
  lines(360,all_samples_list[[s]][,6][1],type="p",col=colb1[s], cex=1, pch=2, lwd=2)
  
}
for(s in 1:4){  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s])}
box()
axis(1,lwd=0.5)
axis(2, las=2,lwd=0.5)
grid(lwd=1)
legend("bottom", legend = "1998", bty = "n")

par(mar=c(2,0,1,2),lwd=0.5)
plot(all_samples_list[[1]][,1], all_samples_list[[1]][,2], type="n", ylim=c(22,40), xlim=c(1,365), axes=F)
for(s in 5:8){
  abline(h = all_samples_list[[s]][,6][1],col=colb1[s-4],lwd=1,lty=2  )
  lines( all_samples_list[[s]][,1],all_samples_list[[s]][,2], type="p", ylim=c(22,40),col=colb1[s-4],lwd=0.5 )
  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,4],type="s",col=colb1[s-4],lwd=2  )
  #lines(all_samples_list[[s]][,1],all_samples_list[[s]][,5],type="s",col=colb1[s-4],lwd=2,lty=2  )
  lines(360,all_samples_list[[s]][,6][1],type="p",col=colb1[s-4], cex=1, pch=2, lwd=2)
  
}
for(s in 5:8){lines(all_samples_list[[s]][,1], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s-4])}
axis(2, labels = F,lwd=0.5)
axis(1,lwd=0.5)
axis(4, las=2,lwd=0.5)
box()
grid(lwd=1)
legend("bottom", legend = "1999", bty = "n")




EW_1998 <- mean(all_samples_list[[4]][,2][1:11], na.rm = T)
LW_1998 <- mean(all_samples_list[[4]][,2][12:16], na.rm = T)
EW_1999 <-  mean(all_samples_list[[8]][,2][1:12], na.rm = T)
LW_1999 <-  mean(all_samples_list[[8]][,2][13:16], na.rm = T)

library(dplR)
HEL_PRISM_1998_1999 <- read.csv("~/Google Drive File Stream/My Drive/E-W-transect/daily_HEL1998-1999.csv") #only days from 135-304
plot.ts(HEL_PRISM_1998_1999[,9],type="l")

HEL_PRISM_1998_1999$Smoth_daylyVPD_max <- ffcsaps(HEL_PRISM_1998_1999[,10],nyrs = 30)
HEL_PRISM_1998_1999$Smoth_daylyVPD_min <- ffcsaps(HEL_PRISM_1998_1999[,9],nyrs = 30)

1:170#1998
171:nrow(HEL_PRISM_1998_1999)#1999
VPD_max_1998 <- HEL_PRISM_1998_1999$Smoth_daylyVPD_max[1:170]
VPD_min_1998 <- HEL_PRISM_1998_1999$Smoth_daylyVPD_min[1:170]

VPD_max_1999 <- HEL_PRISM_1998_1999$Smoth_daylyVPD_max[171:nrow(HEL_PRISM_1998_1999)]
VPD_min_1999 <- HEL_PRISM_1998_1999$Smoth_daylyVPD_min[171:nrow(HEL_PRISM_1998_1999)]

days_doy <- c((135:304),(304:135))

pdf("Plot_chapter_4_testing_HEL.pdf", width = 6.5, height = 6.5, pointsize = 10, useDingbats = F )

layout(mat = matrix(c(1:4), nrow = 2, ncol = 2, byrow = T), widths = c(2,2), heights = c(2,2), respect = T)


#VPD_1998
par(mar=c(2,2,1,0),lwd=0.5)
plot(x = c(1:10),y=c(1:10),xlim = c(135,304), ylim = c(0,150) , type = "n", axes = F, xlab = "", ylab = "")
polygon(x = days_doy, y = c(VPD_max_1998, rev(VPD_min_1998)), border = F, col = rgb(0.2,0.3,0.4,0.6))
axis(1, at = seq(135,304,15),lwd=0.5,las=2)
axis(4,at = seq(0,40, 10), las=2,lwd=0.5)

par(new=T,mar=c(2,2,1,0),lwd=0.5)
plot(all_samples_list[[1]][,1], all_samples_list[[1]][,2], type="n", ylim=c(23,34), xlim=c(135,304), axes=F)
s=4
  abline(h = all_samples_list[[s]][,6][1],col=colb1[s],lwd=1,lty=2)
  lines( all_samples_list[[s]][,1],all_samples_list[[s]][,2], type="o",col=colb1[s],lwd=0.5, pch=1)
  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,4],type="s",col=colb1[s],lwd=2  )
  
  lines(all_samples_list[[s]][,1][1:11], rep(EW_1998, 11),type="s",col="darkblue",lwd=2  )#EW
  lines(all_samples_list[[s]][,1][12:16], rep(LW_1998, 5),type="s",col="darkred",lwd=2  )#LW
  #lines(all_samples_list[[s]][,7],all_samples_list[[s]][,5],type="s",col=colb1[s],lwd=2,lty=2  )
#  lines(1.1,all_samples_list[[s]][,6][1],type="p",col=colb1[s], cex=1, pch=2, lwd=2)

  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s])

  
  
box()
axis(2,lwd=0.5,las=2)
grid(lwd=1)
legend("topleft", legend = site_th[4], pch = 1, col=colb1[4] , bty = "n")
legend("topright", legend = site_tot[4], col=colb1[4] , bty = "n",lwd=1,lty=2)
legend("bottom", legend = "1998", bty = "n")



#VPD_1998
par(mar=c(2,2,1,0),lwd=0.5)
plot(x = c(1:10),y=c(1:10),xlim = c(135,304), ylim = c(0,150) , type = "n", axes = F, xlab = "", ylab = "")
polygon(x = days_doy, y = c(VPD_max_1999, rev(VPD_min_1999)), border = F, col = rgb(0.2,0.3,0.4,0.6))
axis(1, at = seq(135,304,15))
axis(2,at = seq(0,40, 10), las=2,lwd=0.5, labels = F)


par(new=T,mar=c(2,2,1,0),lwd=0.5) 
plot( all_samples_list[[s]][,1],all_samples_list[[s]][,2], type="n", ylim=c(23,34), xlim=c(135,304), axes=F)
s=8
  abline(h = all_samples_list[[s]][,6][1],col=colb1[s-4],lwd=1,lty=2  )
  lines( all_samples_list[[s]][,1],all_samples_list[[s]][,2], type="o",col=colb1[s-4],lwd=0.5 ,pch=1)
  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,4],type="s",col=colb1[s-4],lwd=2  )
  #lines(all_samples_list[[s]][,7],all_samples_list[[s]][,5],type="s",col=colb1[s-4],lwd=2,lty=2  )
#  lines(1.1,all_samples_list[[s]][,6][1],type="p",col=colb1[s-4], cex=1, pch=2, lwd=2)
  
  lines(all_samples_list[[s]][,1][1:12], rep(EW_1999, 12),type="s",col="darkblue",lwd=2  )#EW
  lines(all_samples_list[[s]][,1][13:16], rep(LW_1999, 4),type="s",col="darkred",lwd=2  )#LW
  
lines(all_samples_list[[s]][,1], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s-4])


axis(4, las=2,lwd=0.5)
axis(2, labels = F,lwd=0.5)
box()
grid(lwd=1)
legend("topleft", legend = site_25[4], col=colb1[4] , bty = "n",lwd=2)
legend("topright", legend = site_FR[4], pch = 21, bty = "n" ,col="blue", pt.bg = colb1[4])
legend("bottom", legend = "1999", bty = "n")

dev.off()

###### by day of the year only
par(mar=c(2,2,1,0),lwd=0.5)
plot(x = c(1:10),y=c(1:10),xlim = c(135,304), ylim = c(0,150) , type = "n", axes = F, xlab = "", ylab = "")
polygon(x = days_doy, y = c(VPD_max_1998, rev(VPD_min_1998)), border = F, col = rgb(0.2,0.3,0.4,0.6))
axis(3, at = seq(135,304,15))
axis(4,at = seq(0,150, 10), las=2)

par(new=T,mar=c(2,2,1,0),lwd=0.5)
plot(all_samples_list[[1]][,1], all_samples_list[[1]][,2], type="n", ylim=c(23,34), xlim=c(135,304), axes=F)
s=4
  abline(h=all_samples_list[[s]][,6][1],col=colb1[s], lwd=1, lty=2)
  lines( all_samples_list[[s]][,1],all_samples_list[[s]][,2], type="o", ylim=c(22,40),col=colb1[s],lwd=0.5 )
  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,4],type="s",col=colb1[s],lwd=2  )
  #lines(all_samples_list[[s]][,1],all_samples_list[[s]][,5],type="s",col=colb1[s],lwd=2,lty=2  )
  lines(360,all_samples_list[[s]][,6][1],col=colb1[s], cex=1, lwd=2)
  
lines(all_samples_list[[s]][,1], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s])

box()
axis(1,lwd=0.5)
axis(2, las=2,lwd=0.5)
grid(lwd=1)
legend("bottom", legend = "1998", bty = "n")

par(mar=c(2,0,1,2),lwd=0.5)
plot(all_samples_list[[1]][,1], all_samples_list[[1]][,2], type="n", ylim=c(23,34), xlim=c(1,365), axes=F)
s=8
  abline(h = all_samples_list[[s]][,6][1],col=colb1[s-4],lwd=1,lty=2  )
  lines( all_samples_list[[s]][,1],all_samples_list[[s]][,2], type="p", ylim=c(22,40),col=colb1[s-4],lwd=0.5 )
  lines(all_samples_list[[s]][,1], all_samples_list[[s]][,4],type="s",col=colb1[s-4],lwd=2  )
  #lines(all_samples_list[[s]][,1],all_samples_list[[s]][,5],type="s",col=colb1[s-4],lwd=2,lty=2  )
  lines(360,all_samples_list[[s]][,6][1],col=colb1[s-4], cex=1, lwd=2)
  
lines(all_samples_list[[s]][,1], all_samples_list[[s]][,3],type="p", pch=21 , col="blue", bg=colb1[s-4])

axis(2, labels = F,lwd=0.5)
axis(1,lwd=0.5)
axis(4, las=2,lwd=0.5)
box()
grid(lwd=1)
legend("bottom", legend = "1999", bty = "n")

dev.off()

## estimatinf EW and LW
# EW1998 1:11, LW1998 12:16
# EW1999 1:12, LW1998 13:16
EW_1998 <- mean(all_samples_list[[4]][,2][1:11], na.rm = T)
LW_1998 <- mean(all_samples_list[[4]][,2][12:16], na.rm = T)
EW_199p <-  mean(all_samples_list[[4]][,2][1:12], na.rm = T)
LW_1999 <-  mean(all_samples_list[[4]][,2][13:16], na.rm = T)
