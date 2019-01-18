####Contains spatial packages and need to deal with rasters
require(raster)
####Contains autoKrige method
require(automap)

######Water quality prediction at site (lat,lon) - rebalance
###problems include misisng chemistry and only including major ones...
####use minteq


#####Data from csv
Alll<-read.csv("C:/Users/TMoore24/Documents/DFS/TDS/minn.csv")
####columns of intrest for loop###Pick your variables
Alll<-subset(Alll[,c(8,9,63,72,77,88,89,90,92,106,109,119,130)])
####QC remove bad charge balances....but data is okay where the bad cb is due to missing cl or na which is common.
###chose not to reblance.... new data cl not ca_1
Alll<-subset(Alll,Alll$chargebalance<=5|is.na(Alll$Na) &Alll$chargebalance>=-5|is.na(Alll$Ca_1 ))
######Create empty dataframe, chose 15 but can make dynamic **-2 removes column of lat/long
d = data.frame( Var=rep(0, ncol(Alll)-2), Pred=rep(0,ncol(Alll)-2), SE=rep(0,ncol(Alll)-2))
####Well corrdinates and make spatial data
coords = cbind(-105.459,44.379)
sp = SpatialPoints(coords)
######Begin loop
for (i in 3:ncol(Alll)){
####reset data for NA removal
All<-Alll
All<-subset(Alll,!is.na(All[,i]))
####spatialpoints
coordinates(All)<-~LONGITUDE+LATITUDE

#####clip 15 miles from wellsite
All<-crop(All,extent(-105.76, -105.16, 44.16,44.59))
####Varaible name
var<-as.symbol(colnames(All@data)[i-2])
#####Auto Krige
kriging_result<- autoKrige(as.formula(paste0(var,"~1")),All)
###Dont need but if you want any figures
#pk<-plot(kriging_result)
###Dont need but dataframe of results
#out<-as.data.frame(kriging_result$krige_output)
#####Pull estimated value and standard error at site
est<-raster(kriging_result$krige_output[1])
ste<-raster(kriging_result$krige_output[3])
###########Create value for iteration
x<-colnames(All@data)[i-2]
y<-extract(est,sp)
z<-extract(ste,sp)
########## Input into the black datframe **-2 removes column of lat/long
d[i-2, ] = c(x, y, z)
}
####balance?
####Linear model for Lakota/Dakota?