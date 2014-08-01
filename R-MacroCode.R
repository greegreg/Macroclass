#This is the code for extracting and plotting macro data for an intro to macro course

setwd("p://test-repo/Macroclass/")

#FRED has a very convenient interface with Excel. The data were first downloaded into excel where they were
#manipulated into a usable form. Thus, I will be importing from this excel file.

#Construct a list of URLs
urlGDP<-"http://research.stlouisfed.org/fred2/data/GDP.txt" #This is the url for nominal GDP there are 19 blank spaces
urlC<-"http://research.stlouisfed.org/fred2/data/PCEDG.txt" #This is the consumption text file, there are 13 blanks
urlInv<-"http://research.stlouisfed.org/fred2/data/GPDI.txt" #This is the url for the investment data, there are 13 blank Spaces
urlNX<-"http://research.stlouisfed.org/fred2/data/NETEXP.txt"#This is the url for Net Exports, ther are 13 blank lines
download.file(urlGDP,destfile="./NGDP.txt") #Construct a txt file named NGDP, nominal GDP.
download.file(urlC,destfile="./C.txt") #This will download the consumption data into a txt file
download.file(urlInv,destfile="./I.txt")
download.file(urlNX,destfile="./NX.txt")

#Next, load the data into R and merge the data into a single data frame.
#From the new data frame construct the Government consumption data.
#before constructing this data frame I opened every txt file and eliminated the useless information at the top
#of each file and renamed the variable what I wanted it named.  In the future I need to figure out how to get
#R to read my xlsx file (no luck so far) or skip the dead lines on the top of these files. The dead lines varies from 
#File to file.
#I've settled on using excel to create my data set and then saving the sheet of interest as a tab delimited
#Text file.

#This function is not need right now
files<-c("./NGDP.txt","./C.txt","./I.txt","./NX")
CreateDTA<-function(X){
  n<-length(X) #This measures how many files the function will loop through
  for(i in 1:n){
    print(i)
    Y<-read.table(X[i],header=TRUE,skip=18)#Will read files one at a time
    print(X[1])
    if(i == 1){
      DF<-Y#initialize the data frame that will be returned
    } else {
      DF<-merge(DF,Y)#Merge the data frames, I only want one DF
    }
    
  }
}

#Don't use the above function
MD<-read.table("./FREDdata.txt", header=TRUE)
head(MD)
MD<-MD[complete.cases(MD),]#This call eliminates the rows that have NAs in them.
head(MD)
#Now create the real values of the GDP variables
MD$RGDP<-MD$GDP/MD$GDPDef
MD$RC<-MD$C/MD$GDPDef
MD$RI<-MD$I/MD$GDPDef
MD$RG<-MD$G/MD$GDPDef
MD$RNX<-MD$NX/MD$GDPDef
head(MD)

#Create a function for calculating rates of growth between adjacent points.
growth<-function(x){
    n<-length(x)#We must do n-1 interations through the vector
    gr<-vector()
    gr[1]<-NA #because of the method of growth rate calculation we will lose one data point
    x0<-x[1]#Initialize the calculation
    
    for(i in 2:n){
        x1<-x[i]#Get the 'most recent' data point
        temp<-(x1/x0-1)*4#The growth rate calculation annualized
        gr[i]<-temp#updates the vector
        x0<-x1 #uptdate the x0 variable
    }
    gr #send this vector as output
}

#Construct some growth rates
MD$gRGDP<-growth(MD$RGDP)
MD$gRC<-growth(MD$RC)
MD$gRI<-growth(MD$RI)
MD$gRG<-growth(MD$RG)
MD$gRNX<-growth(MD$RNX)
head(MD)

