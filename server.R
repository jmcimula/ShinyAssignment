#Loading libraries
library(shiny)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(Rmisc)#multitplot
library(stringr)

#Loading the date
data("economics")

#Assignment
ecoData <- economics


#Function for CheckBoxGroup
multiXGroup <- function (Z){
  #Space delimiter from CheckBoxGroup
  strG <- unlist(gregexpr(pattern = ' ', Z))
  D <- length(strG)
  
  #Applying the delimiter length to retrieve single value
  if (D > 1){
            #Initializing a new data frame
            DIFrame <- data.frame()   
            for (k in 1 : D){
      
                 H <- Z[k]
                 H <- tolower(H)
                 
                 for (i in 1:dim(ecoData)[2]){
                   #Take one by one chosen items from CheckBoxGroup
                   if (names(ecoData)[i] == H){
                       colData <- ecoData [,i]
                     break;
                   }
                 }
                 #Descriptive statistic
                 H <- summary(colData)
                 mis <- length(which(is.na(colData)))
                 obs <- sum(colData)
                 H <- data.frame(var=Z[k],min=str_trim(str_replace_all(str_replace_all(H[1],"Min.",""),":","")),
                                          max=str_trim(str_replace_all(str_replace_all(H[6],"Max.",""),":","")),
                                          mean=str_trim(str_replace_all(str_replace_all(H[3],"Median",""),":","")),
                                          missing_value=mis,
                                          observations=obs
                                 )
                 DIFrame <- rbind(DIFrame,H) #Populating data frame
            }
  }else{
    H <- Z[1] #Single value selected
    H <- tolower(H)
    
    for (i in 1:dim(ecoData)[2]){
      #Take one by one chosen items from CheckBoxGroup
      if (names(ecoData)[i] == H){
        colData <- ecoData [,i]
        break;
      }
    }
    #Descriptive statistic
    H <- summary(colData)
    mis <- length(which(is.na(colData)))
    obs <- sum(colData)
    DIFrame <- data.frame(var=Z[1],min=str_trim(str_replace_all(str_replace_all(H[1],"Min.",""),":","")),
                          max=str_trim(str_replace_all(str_replace_all(H[6],"Max.",""),":","")),
                          mean=str_trim(str_replace_all(str_replace_all(H[3],"Median",""),":","")),
                          missing=mis,
                          observations=obs
                          )#Simple data frame
    
  }
  #Return the data frame
  return(DIFrame)
}
#Function for EDA
EXplData <- function(Y){
  #Data
  ecoDataOne <- economics
   if (tolower(Y) == "plot" || toupper(Y) =="PLOT"){
    # First plot
    p1 <- ggplot(ecoDataOne, aes(x=pce, y=psavert, colour=uempmed, group=unemploy)) +
      geom_line() +
      ggtitle("Personal Consumption Expenditures and Savings Rate")
    
    # Second plot
    p2 <- ggplot(ecoDataOne, aes(x=date, y=pop, colour=pce)) +
      geom_point(alpha=.3) +
      geom_smooth(alpha=.2, size=1) +
      ggtitle("Growth of Population")
    
    # Third plot
    p3 <- ggplot(subset(ecoDataOne, pop>25000), aes(x=uempmed, colour=unemploy)) +
      geom_density() +
      ggtitle("Median Duration of Unemployment for Population greater than 25,000")
    
    D <- multiplot(p1, p2, p3, cols=2)
    
  }else if(tolower(Y)=="cor" || toupper(Y)=="COR"){
    #Correlation
    D <- corrplot(cor(ecoDataOne[,-1]), method="circle", is.corr=TRUE)
  }else{
    #Default plot
    D <- chart.Correlation(ecoDataOne[,-1], histogram=TRUE, pch=19)
  }
  
  return(D)
}


#Function for the combination
RegR <- function (X){
  
  response <- tolower(X)
  
  if (response == "") { dynamicRegression <- "Please choose a variable"}
  else{
  #Number of colums of the data frame
  getLen <- dim(ecoData)[2]
  
  for (i in 1:getLen){
    #Removing the response variable in the temporary data frame
    if (names(ecoData)[i] == response){
      expVariable <- ecoData [,-i]
      break;
    }
  }
  #Number of colums of the temporary data frame of explanatory variables
  getLenExpVar <- dim(expVariable)[2]
  
  #Initializing a Matrix which will contain all combinations of predictor model
  ExpVarMatrix <- matrix( ncol = getLenExpVar)
  
  #Creating combination
  for (i in 1:getLenExpVar){
    #Combination function
    comb    <- t(combn(names(expVariable),i))
    numbRow <- nrow(comb)
    numbCol <- length(names(expVariable))
    numbRowNA  <- numbRow
    numbColNA  <- numbCol-ncol(comb)
    naMatr   <- matrix(rep(NA, numbRowNA*numbColNA), nrow = numbRowNA, ncol = numbColNA)
    result   <- cbind(comb, naMatr)
    ExpVarMatrix <- rbind(ExpVarMatrix, result)
  }
  #Removing all NA
  ExpVarMatrix <- ExpVarMatrix[-1,]
  
  #Final result of combination between response and explanatory variables
  
  #Setting an empty data frame
  dynamicRegression <- data.frame()
  for (i in 1:nrow(ExpVarMatrix)){
    
    getVal <- na.omit (ExpVarMatrix[i, ])
    mdRegComb <- paste (response, " ~ ", paste (getVal, collapse = " + "), sep = "")
    #print(mdRegComb)
    mdLM <- lm(as.formula(mdRegComb), data = ecoData)
    SMry <- summary(mdLM)
    
    #Diagnostic parameters
    RSqrt  <- SMry[8]   #R-Squared
    RSqrt  <- round(as.double(RSqrt),4)
    AdjRSqrt <- SMry[9] #adj R-Squared
    AdjRSqrt <- round(as.double(AdjRSqrt),4)
    AIC  <- AIC(mdLM)#AIC
    AIC <- round(as.double(AIC),4)
    #Assembling diagnostic parameters per model predictor in Matrix of all combinations
    dFrame <- data.frame(modelReg = mdRegComb, RSquared = RSqrt, AIC = AIC, AdjRSquared = AdjRSqrt)
    
    #Loading data framme
    dynamicRegression <- rbind(dynamicRegression, dFrame)
  }
} 
    return(dynamicRegression)
} 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$ck <- renderPrint({multiXGroup(input$checkGroup)})#Using the function multiXGroup
  output$rd <- renderPrint({RegR(input$radio)}) #Using the function RegR
  output$tx <- renderPlot({EXplData(input$text)}) #Using the function EXplData

})
