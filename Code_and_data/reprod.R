## Read the data
setwd("~/Repos/mangroveCstabilization/Code_and_Data/")
Ergebnisse<-read.csv("Ergebnisse_vereinfacht.csv", stringsAsFactors = FALSE)
C_N_13C<-read.table("C_N_13C.csv", sep=";", dec=".", header = TRUE)
TOC<-read.csv("TOC.csv")
Al_Fe<-read.csv2("Al_Fe.csv", dec=".")
Sand_3<-read.csv("Sand_3.csv")
Sinu_3<-read.csv("Sinu_3.csv")
R5_80_90<-read.csv("R5_80-90.csv")
C3_80_85<-read.csv("C3_80-85.csv")


#######################################################
# Statistical comparisons
mangroveOnly=rbind(Ergebnisse[Ergebnisse$Type=="Fringe",], Ergebnisse[Ergebnisse$Type=="Basin",]) # Exclude data from the end members
mangAlFe=Al_Fe[Al_Fe[,"Type"]=="Fringe" | Al_Fe[,"Type"]=="Basin",c("Type","Al_Sum", "Fe_Sum")] # Exclude data from the end members

# Comparison for Organic carbon
shapiro.test(mangroveOnly[mangroveOnly[,"Type"]=="Fringe","Corg"]) #Normality test
shapiro.test(mangroveOnly[mangroveOnly[,"Type"]=="Basin","Corg"])

wilcox.test(Corg~Type, data=mangroveOnly, exact=FALSE) #Non-parametric test for comparing whether samples are from the same distribution. For two samples, the test is know as 'Mann-Whitney test'.

# Comparison for total nitrogen
shapiro.test(mangroveOnly[mangroveOnly[,"Type"]=="Fringe","Ntotal"])
shapiro.test(mangroveOnly[mangroveOnly[,"Type"]=="Basin","Ntotal"])

wilcox.test(Ntotal~Type, data=mangroveOnly, exact=FALSE)

 # Comparison 13C
shapiro.test(mangroveOnly[mangroveOnly[,"Type"]=="Fringe","d13C"])
shapiro.test(mangroveOnly[mangroveOnly[,"Type"]=="Basin","d13C"])

wilcox.test(d13C~Type, data=mangroveOnly, exact=FALSE)

# Comparisons for Al and Fe oxides
shapiro.test(Al_Fe[Al_Fe[,"Type"]=="Fringe","Al_Sum"])
shapiro.test(Al_Fe[Al_Fe[,"Type"]=="Fringe","Al_Sum"])

wilcox.test(Al_Sum~Type, data=mangAlFe, exact=FALSE)

wilcox.test(Fe_Sum~Type, data=mangAlFe, exact=FALSE)


#######################################################
# Fig 2
#pdf("../Manuscript/Figures/Fig2.pdf")
par(mfrow=c(3,1))
boxplot(Cinorg~Type, data=Ergebnisse,  par(mar=c(4,4,1,1)), ylab="Inorganic carbon (%)")

boxplot(Corg~Depth_adjusted, data=Ergebnisse[1:51,],  par(mar=c(4,4,1,1)), xaxt = "n", ylab="Organic carbon (%)",
        col=c("peachpuff","peachpuff","peachpuff","peachpuff","peachpuff","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue"))
axis(1, at=1:10, labels=c("0-20","20-40","40-60","60-80","80-100","0-20","20-40","40-60","60-80","80-100"), las=1)

boxplot(Ntotal~Depth_adjusted, data=Ergebnisse[1:51,], par(mar=c(5,4,1,1)), xaxt = "n", xlab="Depth interval (cm)",ylab="Total nitrogen (%)",
        col=c("peachpuff","peachpuff","peachpuff","peachpuff","peachpuff","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","rosybrown3","rosybrown3"))
axis(1, at=1:10, labels=c("0-20","20-40","40-60","60-80","80-100","0-20","20-40","40-60","60-80","80-100"), las=1)
legend("topright", c("basin", "fringe"), col = c("peachpuff","lightsteelblue"), bg = "gray90", pch=15, bty="n", y.intersp = 0.7)
par(mfrow=c(1,1))
#dev.off()

# Fig 3
#pdf("../Manuscript/Figures/Fig3.pdf")
par(mfrow=c(3,1), mar=c(4,5,1,1))
boxplot(DeltaVsPDB_Corr_13_12~Type,data=C_N_13C, ylab=expression(paste(delta^13, "C (per mille)")), ylim=c(-30,0))
legend("topleft", "a", bty="n")
boxplot(DeltaVsPDB_Corr_13_12~Depth_adjusted, data=C_N_13C[C_N_13C[,"Type"]=="Basin",], ylab=expression(paste(delta^13, "C (per mille)")), ylim=c(-30,-22),
        col="peachpuff")
legend("topleft", "b", bty="n")
boxplot(DeltaVsPDB_Corr_13_12~Depth_adjusted, data=C_N_13C[C_N_13C[,"Type"]=="Fringe",], ylab=expression(paste(delta^13, "C (per mille)")), 
        xlab="Depth interval (cm)", ylim=c(-30,-22), col="lightsteelblue")
legend("topleft", "c", bty="n")
par(mfrow=c(1,1))
#dev.off()

# Fig 4
par(mar=c(5,4,1,1))
plot(Sand_3, type="l", main="Sand Nispeyal, Sample 3", xlab=expression(Bragg~angle~~~2~theta~(degree)), ylab="Intensity (cps)")
grid(col="darkgray")
text(27.5, 5300, "Ar", cex=0.8)
text(27.5, 3300, "Ar", cex=0.8)
text(31.5, 700, "Ar", cex=0.8)
text(33, 3300, "Ar", cex=0.8)
text(37, 2200, "Ar", cex=0.8)
text(39, 1800, "Ar", cex=0.8)
text(42, 1100, "Ar", cex=0.8)
text(46, 3900, "Ar", cex=0.8)
text(49.5, 1700, "Ar", cex=0.8)
text(53.5, 1300, "Ar", cex=0.8)
text(21, 700, "Qz", cex=0.8)
text(26.5, 1000, "Qz", cex=0.8)
text(29.5, 1000, "Ca", cex=0.8)
text(39.5, 500, "Ca", cex=0.8)
text(47.5, 500, "Ca", cex=0.8)

text(50.5, 5000, "Qz", cex=1, pos = 2)
text(50.5, 5000, "Quartz", cex=1, pos = 4)
text(50.5, 4700, "Ar", cex=1, pos = 2)
text(50.5, 4700, "Aragonite", cex=1, pos = 4)
text(50.5, 4400, "Ca", cex=1, pos = 2)
text(50.5, 4400, "Calcite", cex=1, pos = 4)

par(fig=c(0,0.4,0.6,1),new=TRUE)
pie(c(0.852, 0.084, 0.064),labels = c("Ar(85.2)", "Qz(8.4)","Ca(6.4)"), col="white")
par(fig=c(0,1,0,1),new=FALSE)

plot(Sinu_3, type="l", main="Sinu River, Sample 3", xlab=expression(Bragg~angle~~~2~theta~(degree)), ylab="Intensity (cps)")
grid(col="darkgray")
text(7.5, 2300, "Cl", cex=1, pos = 2)
text(10, 2500, "Il", cex=1, pos = 2)
text(26, 2000, "Il", cex=1, pos = 2)
text(13.5, 2300, "Cl", cex=1, pos = 2)
text(15.5, 2600, "Al", cex=1, pos = 2)
text(22, 8000, "Qz", cex=1, pos = 2)
text(23, 2300, "Al", cex=1, pos = 2)
text(27, 37000, "Qz", cex=1, pos = 2)
text(29, 9000, "Al", cex=1, pos = 2)
text(38, 2900, "Qz", cex=1, pos = 2)
text(41.5, 2900, "Qz", cex=1, pos = 2)
text(44, 2900, "Qz", cex=1, pos = 2)
text(47, 2700, "Qz", cex=1, pos = 2)
text(52, 6500, "Qz", cex=1, pos = 2)
text(52.5, 2000, "Al", cex=1, pos = 2)
text(57, 2000, "Qz", cex=1, pos = 2)

text(50.5, 35000, "Cl", cex=1, pos = 2)
text(50.5, 35000, "Clinochlore", cex=1, pos = 4)
text(50.5, 33000, "Il", cex=1, pos = 2)
text(50.5, 33000, "Illite", cex=1, pos = 4)
text(50.5, 31000, "Qz", cex=1, pos = 2)
text(50.5, 31000, "Quartz", cex=1, pos = 4)
text(50.5, 29000, "Al", cex=1, pos = 2)
text(50.5, 29000, "Albite", cex=1, pos = 4)

par(fig=c(0,0.4,0.6,1),new=TRUE)
pie(c(0.688, 0.196, 0.067, 0.049),labels = c("Qz(68.8)", "Al(19.6)","Cl(6.7)","Il(4.9)"), col="white")
par(fig=c(0,1,0,1),new=FALSE)


# Fig 5
plot(R5_80_90, type="l", main="R5 Fringe / 80-90 cm", xlab=expression(Bragg~angle~~~2~theta~(degree)), ylab="Intensity (cps)")
grid(col="darkgray")

text(8, 1800, "Cl", cex=1, pos = 2)
text(11, 1600, "Il", cex=1, pos = 2)
text(15, 1900, "Cl", cex=1, pos = 2)
text(22, 1600, "Il", cex=1, pos = 2)
text(24, 2600, "Qz", cex=1, pos = 2)
text(32, 8300, "Ha", cex=1, pos = 2)
text(27, 7800, "Qz", cex=1, pos = 2)
text(27, 1300, "Al", cex=1, pos = 2)
text(32, 1900, "Al", cex=1, pos = 2)
text(48, 4500, "Ha", cex=1, pos = 2)
text(59, 2000, "Ha", cex=1, pos = 2)
text(53, 1400, "Qz", cex=1, pos = 2)
text(37, 1300, "Il", cex=1, pos = 2)
text(44, 1300, "Qz", cex=1, pos = 2)

text(9, 7000, "Cl", cex=1, pos = 2)
text(9, 7000, "Clinochlore", cex=1, pos = 4)
text(9, 6500, "Il", cex=1, pos = 2)
text(9, 6500, "Illite", cex=1, pos = 4)
text(9, 6000, "Qz", cex=1, pos = 2)
text(9, 6000, "Quartz", cex=1, pos = 4)
text(9, 5500, "Al", cex=1, pos = 2)
text(9, 5500, "Albite", cex=1, pos = 4)
text(9, 5000, "Ha", cex=1, pos = 2)
text(9, 5000, "Halite", cex=1, pos = 4)

par(fig=c(0.6,1,0.6,1),new=TRUE)
pie(c(0.109, 0.063, 0.338, 0.155,0.334),labels = c("Cl(10.9)", "Al(6.3)","Ha(33.8)","Il(15.5)","Qz(33.4)"), col="white")
par(fig=c(0,1,0,1),new=FALSE)

plot(C3_80_85, type="l", main="C3 Basin / 80-85 cm", xlab=expression(Bragg~angle~~~2~theta~(degree)), ylab="Intensity (cps)")
grid(col="darkgray")

text(9, 2300, "Cl", cex=1, pos = 2)
text(12, 1800, "Il", cex=1, pos = 2)
text(16, 1800, "Cl", cex=1, pos = 2)
text(22, 1800, "Il", cex=1, pos = 2)
text(23, 5000, "Qz", cex=1, pos = 2)
text(27, 17000, "Qz", cex=1, pos = 2)
text(27, 2000, "Al", cex=1, pos = 2)
text(32, 2200, "Al", cex=1, pos = 2)
text(36, 2700, "Ha", cex=1, pos = 2)
text(37, 1500, "Il", cex=1, pos = 2)
text(49, 2200, "Ha", cex=1, pos = 2)
text(42, 2100, "Qz", cex=1, pos = 2)
text(57, 1600, "Qz", cex=1, pos = 2)

par(fig=c(0.6,1,0.6,1),new=TRUE)
pie(c(0.588, 0.111, 0.109, 0.057,0.135),labels = c("Qz(58.8)", "Cl(11.1)","Al(10.9)","Ha(5.7)","Il(13.5)"), col="white")
par(fig=c(0,1,0,1),new=FALSE)


# Fig 6
pdf("../Manuscript/Figures/Fig6.pdf")
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(Ox_Al~Depth, data=Al_Fe,las=2,xaxt = "n",xlab="",ylab=expression('Al'[o]~(ppm)),col=c("peachpuff","peachpuff","peachpuff","peachpuff","peachpuff","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","rosybrown3","rosybrown3"))
axis(1, at=1:12, labels=c("0-20","20-40","40-60","60-80","80-100","0-20","20-40","40-60","60-80","80-100","Sand","River"), las=2)
plot(Dit_Al~Depth, data=Al_Fe,las=2,xaxt = "n",xlab="",ylab=expression('Al'[d]~(ppm)),col=c("peachpuff","peachpuff","peachpuff","peachpuff","peachpuff","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","rosybrown3","rosybrown3"))
axis(1, at=1:12, labels=c("0-20","20-40","40-60","60-80","80-100","0-20","20-40","40-60","60-80","80-100","Sand","Sinu"), las=2)
plot(Ox_Fe~Depth, data=Al_Fe,las=2,xaxt = "n", xlab="Depth interval (cm)",ylab=expression('Fe'[o]~(ppm)),col=c("peachpuff","peachpuff","peachpuff","peachpuff","peachpuff","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","rosybrown3","rosybrown3"))
axis(3, at=1:12, labels = FALSE)
plot(Dit_Fe~Depth, data=Al_Fe,las=2,xaxt = "n",xlab="Depth interval (cm)",ylab=expression('Fe'[d]~(ppm)),col=c("peachpuff","peachpuff","peachpuff","peachpuff","peachpuff","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","lightsteelblue","rosybrown3","rosybrown3"))
axis(3, at=1:12, labels = FALSE)
legend(4.5, 6.5, c("basin", "fringe", "end-members"), col = c("peachpuff","lightsteelblue","rosybrown3"), text.col = "1", bg = "gray90", pch=15, bty="n", y.intersp = 1)
par(mfrow=c(1,1))
dev.off()


# Mean values with propagated uncertainty
# For TOC
meanTOCdepth=tapply(TOC[,"TOC"], list(TOC[,"Type"], TOC[,"Depth_adjusted"]),FUN=mean)
sdTOCdepth=tapply(TOC[,"TOC"], list(TOC[,"Type"], TOC[,"Depth_adjusted"]),FUN=sd)

sumTOC=rowSums(meanTOCdepth, na.rm=TRUE)
round(sumTOC, 2)
sdTOC=apply(sdTOCdepth, 1, FUN=function(x){sqrt(sum(x^2,na.rm=TRUE))})
round(sdTOC,2)

# For Bulk density
round(mean(TOC[TOC[,"Type"]=="Basin","BulkDens"]), 2)
round(sd(TOC[TOC[,"Type"]=="Basin","BulkDens"]), 2)

round(mean(TOC[TOC[,"Type"]=="Fringe","BulkDens"]), 2)
round(sd(TOC[TOC[,"Type"]=="Fringe","BulkDens"]), 2)

