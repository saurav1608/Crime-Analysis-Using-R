                              #CRIME RATE ANALYSIS -> Data Set : San Fransisco Police Department for the year 2016
							  #NOTE - Certain Analysis done for whole data. Results may vary if done for certain subset of the data
							  #WARNING - Same incident's case is filed under different categoried(IncidntNum would remain same):Should have no effect on analysis
#Clearing the workspace
	rm(list = ls())

#Importing necessary libraries
	library(ggplot2)
	library(stringi)       
        library(lubridate)     #to get the week 
	library(dplyr)
	library(chron)
	library(arules)        #for apriori algorithm
        library(rpart)
	
	library(png)
	library(ggmap)
	library(mapproj)
	library(cluster)
	
	
	
	
	
	CrimeData<-read.csv("CrimeData.csv")
	# Data Preperation
	
	for(col in CrimeData)
	{
	  print("Frequency for each value of Columns:")
	  print(as.data.frame(table(CrimeData$Category)))
	}
	
	PreparedData<-as.data.frame(table(CrimeData$PdDistrict))

	
	DiffCrimesFreq<-as.data.frame(table(CrimeData$Category))
	DiffCrimes<-DiffCrimesFreq$Var1
	for(i in DiffCrimes)
	{
	  print(i)
	  if(i=="Sex Offenses, Forcible")
	  {
	    PreparedData['Force Sex Offence']<-paste0("",0)
	  }
	  else if(i=="Sex Offenses, Non Forcible")
	  {
	    PreparedData['NonForce Sex Offence']<-paste0("",0) 
	  }
	  else
	  {
	    PreparedData[i]<-paste0("",0)
	  }
	}
	
	CrimeCityWise<-as.data.frame(table(CrimeData$Category,CrimeData$PdDistrict))
	Allfres<-matrix(CrimeCityWise$Freq,1,429)
	
	PreparedData[1,3:(ncol(PreparedData))]<-Allfres[1:39]
	PreparedData[2,3:(ncol(PreparedData))]<-Allfres[40:78]
	PreparedData[3,3:(ncol(PreparedData))]<-Allfres[79:117]
	PreparedData[4,3:(ncol(PreparedData))]<-Allfres[118:156]
	PreparedData[5,3:(ncol(PreparedData))]<-Allfres[157:195]
	PreparedData[6,3:(ncol(PreparedData))]<-Allfres[196:234]
	PreparedData[7,3:(ncol(PreparedData))]<-Allfres[235:273]
	PreparedData[8,3:(ncol(PreparedData))]<-Allfres[274:312]
	PreparedData[9,3:(ncol(PreparedData))]<-Allfres[313:351]
	PreparedData[10,3:(ncol(PreparedData))]<-Allfres[352:390]
	PreparedData[11,3:(ncol(PreparedData))]<-Allfres[391:529]
	
	is.na(PreparedData$Var1)
	#PreparedData[which(is.na(PreparedData)),] <-c("Others")
#	PreparedData[1,1]<- "unknown"
	
	write.csv(PreparedData,file="Result.csv",row.names=FALSE,quote=FALSE)
	
	#Clusterining
	
	readPrepareData<-as.data.frame(read.csv("Result.csv"))
	#plot(wt, mpg, main="Scatterplot Example", 
	#    xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
	#plot(readPrepareData[,-c(1,6)])
	plot(readPrepareData[,3:8],pch=".",cex=3)
	#par(mar = rep(2, 4))
	#par(mfrow=c(4,2))
	
	subset(readPrepareData,readPrepareData$Extortion>5)
	
	plot(readPrepareData[,3:7],pch=c(".","+")[(rownames(readPrepareData)=="2")+1],cex=1.5)
	
	sapply(readPrepareData[3:ncol(readPrepareData)],var)                                           
	
	rng<-sapply(readPrepareData[,3:ncol(readPrepareData)],function(x)diff(range(x,na.rm = TRUE)))
	
	Crime_s<-sweep(readPrepareData[3:ncol(readPrepareData)],2,rng,FUN = "/")
	sapply(Crime_s,var)
	n<-nrow(Crime_s)
	wss<-rep(0,6)
	wss[1]<- (n-1) * sum(sapply(Crime_s,var))
	for(i in 2:6)
	{
	  wss[i]<-sum(kmeans(Crime_s,centers = i)$withinss)
	}
	
	plot(1:6,wss,type="b",xlab="No of Groups",ylab="Within groups sum of Squares")
	
	#fit<-kmeans(Crime_s,centers = 2)$centers*rng
	
	Crime_pca<-prcomp(Crime_s)
	
	plot(Crime_pca$x[,1:2],pch=kmeans(Crime_s,centers =2)$cluster)
	
	
	#aggregate(Crime_s,by=list(fit$cluster),FUN=mean)
	#Crime_s<-data.frame(Crime_s,fit$c)
	
	
#Reading the dataset and getting summary of the dataset
	MainSet<-as.data.frame(read.csv("CrimeData.csv"))
	summary(MainSet)
	str(MainSet)
	MainSet$PdDistrict=tolower(MainSet$PdDistrict)
	
#Checking if there are any null values in the dataset : No
	table(is.na(MainSet))

#Check if any value has been left blank while filling the entries: One Entry Left Blank->Deleted as it will have negligible effect on the analysis
	which(MainSet$IncidntNum=="")
	which(MainSet$Category=="")
	which(MainSet$Descript=="")
	which(MainSet$PdDistrict=="")
	which(MainSet$DayOfWeek=="")
	which(MainSet$Date=="")
	which(MainSet$Time=="")
	which(MainSet$Address=="")
	which(MainSet$Resolution=="")
	
#Initial Analysis 1 : District Wise Crime
	g <- ggplot(MainSet, aes(x=PdDistrict)) + geom_bar(colour="black", fill="red")
	g <- g + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0))
	g <- g + xlab("District") + ggtitle("Crime By District")
	g <- g + ylab("Crime Count")
	print(g)
#Initial Analysis 2 : Crime Category in the most vulnerable district->"Southern"
	MostVulnerable<-MainSet[MainSet$PdDistrict=="southern",]
	gm<-ggplot(MostVulnerable,aes(Category))+ geom_bar(colour="black", fill="red")
	gm<-gm + theme(axis.text.x = element_text(angle=90,hjust=0,vjust=0))
	gm<-gm+xlab("Crime Category") + ggtitle("Crime in the Most Vulnerable Region")+ylab("Count")
	print(gm)

#Initial Analysis 3 : Getting a complete idea of crime happening in different regions
	gm<-ggplot(MainSet,aes(x=Category,fill=PdDistrict))+geom_bar()
	gm <- gm + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0))
	gm<-gm+xlab("Category")+ylab("Count")+ggtitle("Vulnerable District according to crime")
	print(gm)
	
#Initial Analysis 4 : To find out if on a particular day the crime rate is higher than the other
	gm<-ggplot(MainSet,aes(DayOfWeek))+ geom_bar(colour="black", fill="red")
	gm<-gm + theme(axis.text.x = element_text(angle=90,hjust=0,vjust=0))
	gm<-gm+xlab("Day Of Week") + ggtitle("Day Wise Analysis of Crime")+ylab("Count")
	print(gm)
	
#Adding the Week Attribute,Weekend Attribute and reordering the attributes
	MainSet$Week<-lubridate::isoweek(mdy(MainSet$Date))
	MainSet$Weekends<-is.weekend(MainSet$Date)
	MainSet$Weekends<-as.factor(MainSet$Weekends)
	MainSet <- select( MainSet, c(IncidntNum, Category,Descript,Resolution,Date,Week,DayOfWeek,Time,Weekends,PdDistrict,Address, X,Y, Location,PdId ))
#Analysis 5 : Getting to know whether there are more crimes committed on weekdays or weekends
    labels<-c("Weekends","Weekday")
	count<-sum(MainSet$Weekends==TRUE)
	count1<-sum(MainSet$Weekends==FALSE)
	combi<-c(count,count1)
	pie(combi,labels)	
	
#Analysis 5 contd : Calculating the density of crimes over weekdays and weekends
	x<-52+53
	y<-366-x
	counter<-count/x
	counter1<-count1/y
	print("Density of Crimes Recorder on Weekdays")
	print(counter1)
	print("Density of Crimes Recorder on Weekends")
	print(counter)
#Dividing data into Test and Train Sets : Odd Rows->TestSet;EvenRows->TrainSet
	TestSet <- MainSet[seq(1, nrow(MainSet), 2),]
	TrainSet <- MainSet[seq(2, nrow(MainSet), 2),]
#Adding Time Levels to data
	Time<-as.numeric(gsub(":","",MainSet$Time))
    MainSet$Time<-Time 
	MainSet$Time_Level <- as.factor(
     ifelse(MainSet$Time < 1200, "Morning",
            ifelse(MainSet$Time < 1600, "Afternoon",
                   ifelse(MainSet$Time < 2000, "Evening", "Night"))))
#Building Test Set
	SecondMainSet<-as.data.frame(read.csv("CrimeData1.csv"))
	SecondMainSet$Week<-lubridate::isoweek(mdy(SecondMainSet$Date))
    SecondMainSet$Weekends<-is.weekend(SecondMainSet$Date)
    SecondMainSet$Weekends<-as.factor(SecondMainSet$Weekends)
    SecondMainSet <- select( SecondMainSet, c(IncidntNum, Category,Descript,Resolution,Date,Week,DayOfWeek,Time,Weekends,PdDistrict,Address, X,Y, Location,PdId ))
    Time<-as.numeric(gsub(":","",SecondMainSet$Time))
    SecondMainSet$Time<-Time 
    SecondMainSet$Time_Level <- as.factor(
      ifelse(SecondMainSet$Time < 1200, "Morning",
            ifelse(SecondMainSet$Time < 1600, "Afternoon",
                    ifelse(SecondMainSet$Time < 2000, "Evening", "Night"))))
	SecondMainSet$Date=as.character(SecondMainSet$Date)
	SecondMainSet$Date<-as.numeric(gsub("/","",SecondMainSet$Date,fixed=TRUE))
	SecondMainSet$Date2 <- as.numeric(unclass(as.POSIXct(SecondMainSet$Date,origin="1970-01-01")))	
	SecondMainSet$PdDistrict=tolower(SecondMainSet$PdDistrict)

	
#Analysing Crime Category using Decision Trees based on location and data TrainSet$Date=as.character(TrainSet$Date)
	TestSet$Date=as.character(TestSet$Date)
	TrainSet$Date=as.character(TrainSet$Date)
	TrainSet$Date<-as.numeric(gsub("/","",TrainSet$Date,fixed=TRUE))
	TestSet$Date<-as.numeric(gsub("/","",TestSet$Date,fixed=TRUE))
	TrainSet$Date2 <- as.numeric(unclass(as.POSIXct(TrainSet$Date,origin="1970-01-01")))
	TestSet$Date2 <- as.numeric(unclass(as.POSIXct(TestSet$Date,origin="1970-01-01")))
	#tree <- rpart(Category ~ Week + DayOfWeek + Time_Level + X + Y + PdDistrict ,data = MainSet,method = "class",control = rpart.control(minsplit = 200,cp=0))
   #predicted <- predict(object = tree,newdata = MainSet)
    MainSet$Date=as.character(MainSet$Date)
	MainSet$Date<-as.numeric(gsub("/","",MainSet$Date,fixed=TRUE))
	MainSet$Date2 <- as.numeric(unclass(as.POSIXct(MainSet$Date,origin="1970-01-01")))
	tree <- rpart(Category ~ Week + DayOfWeek + Time_Level + X + Y + PdDistrict ,data = MainSet,method = "class",control = rpart.control(minsplit = 200,cp=0))
	predicted <- predict(object = tree,newdata = SecondMainSet)
  predicted
    
    
   #userdata1<-as.data.frame(read.csv("userData.csv"))
    #n <- readline(prompt="Enter Week: ")
    #n <- as.integer(n)
    #m <- readline(prompt="Enter DayofWeek: ")
    #m <- as.factor(m)
    #o <- readline(prompt="Enter Time Level: ")
    #o <- as.factor(o)
    #p <- readline(prompt="Enter Location Latitute: ")
    #p <- as.double(n)
    #q <- readline(prompt="Enter Location Logitude: ")
    #q <- as.double(m)
    #r <- readline(prompt="Enter Location: ")
    #r <- as.character(o)
    
    #userdata1$Week<-n
    #userdata1$DayOfWeek<-m
    #userdata1$Time_Level
    #userdata1$X<-n
    #userdata1$Y<-m
    #userdata1$PdDistrict<-o
    
    #write.csv(userdata1, file = "UserData.csv",quote = FALSE,row.names = FALSE)
    #userData2<-read.csv("userData.csv")
    
    #predicted <- predict(object = tree,newdata = userData2[1,c("Week","DayOfWeek","Time_Level",X","Y","Date2"),],type="class")
    #predicted<-as.data.frame(predicted)
    
    
    
	final <- data.frame(Id = seq(1,nrow(predicted),1) , predicted)
	colnames(final)  <- c("Id",levels(MainSet$Category))
	write.csv(final,file = "Prediction.csv",row.names = FALSE,quote = F)

	 
#Spatial Analysis : Mapping the "Assault" data onto the map
	 forMining<-as.data.frame(MainSet[MainSet[,"Category"]=="Assault",])
	 lon<-forMining$X
     lat<-forMining$Y
	 map<-get_map("San Fracisco",zoom=14)
	 mapPoints<-ggmap(map)+geom_point(data=forMining,x=lon,y=lat,alpha=0.5,colour="red")
     mapPoints



