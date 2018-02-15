#########################################
##New


##############################################
###LETS READ IN THE HEADER FILE###############

#Delete Later
years <- 30
runs <- 10



#---------------READ IN THE INPUT FILE----------------------#
input <- read.csv(file.choose(), stringsAsFactors = FALSE)

#Create constants array
for (i in 1:nrow(input)) {
  assign(paste("p_",input$Stage_Transition[i], sep=""), c(input$Productivity[i], input$Productivity_SD[i], input$Capacity[i], input$Capacity_SD[i], input$Transition[i], input$Transition_SD[i]))
}

#---------------SET DATA FRAME FOR SIMULATIONS----------------------#
#create a vector for our stages from the input dataframe
stages <- unique(input$Stage1)

#add years, reps and any other placeholders needed to our vector
stages <- c("Year", "Run", stages)

#Create a dataframe equal to years and columns needed
sims <- data.frame(matrix(NaN, nrow=years, ncol=length(stages)))

#Name the columns in our dataframe
names(sims) <- stages

#---------------BEAVERTON HOLT FUNCTION----------------------#
# Takes 2 arguments
# stage1 = N at first stage
# stage1_parms = array vector of parameters

bev_holt <- function (stage1, stage1_parms) {
  stage1/(((1/rnorm(1, mean = stage1_parms[1], sd = stage1_parms[2]))+((1/rnorm(1, mean = stage1_parms[3], sd = stage1_parms[4]))*stage1)))
}



########################################
####     START MODEL LOOPS         #####
########################################

#Loops for runs
for (j in 1:runs) {
  sims$Run <- j
  #Set Fry start at beginning of each run, note this will later be set at input file
  sims$Fry[1] <- 10000

  #Loop for years 
  for (i in 1:years) {
    
    #Iterate years
    sims$Year[i] <- i
    
    
    ##########################################
    #####       START AT YEAR 2         ######
    ##########################################
    
    #--------------Pre Smolts to LGD Smolts----------------#
    #Total smolts at LGD as sum of headwaters and valley pre-smolts
    if (i > 1) {
      sims$LGDSmolt[i] <- 
        (bev_holt(sims$PreSmoltHeadwaters[i], p_PreSmoltHeadwaters_LGDSmolt)
         +
           bev_holt(sims$PreSmoltValley[i], p_PreSmoltValley_LGDSmolt))
    }
    
    ###FRY FROM SMOLT TO TEST TO THIS POINT
    ##This Will end up being LGDsmolts to LGDAge1
    ####Happens first in Year two, so don't do in year 1#######
    if (i > 1) {
      sims$Fry[i] <-  bev_holt(sims$LGDSmolt[i], p_LGDSmolt_LGDAdult1)
    }
    
    
    ##########################################
    #####       START AT YEAR 1         ######
    ##########################################
    
    
    #--------------FRY TO PARR----------------#
    sims$Parr[i] <- bev_holt(sims$Fry[i], p_Fry_Parr)
  
    #--------------PARR TO PRE SMOLTS----------------#
    #Note no Survival Here - they just move####
    #ADD YEAR
    if (i < years) {
      sims$PreSmoltHeadwaters[i + 1] <- sims$Parr[i] * p_PreSmoltHeadwaters_LGDSmolt[5]
      sims$PreSmoltValley[i + 1] <- sims$Parr[i] * p_PreSmoltValley_LGDSmolt[5]
    }
    
    #Print each year iteration
    print(i)
  }
  
  #End year Loops

  #New dataframe on first run, append on successive runs
    if (j == 1) {
    final <- sims
  } else {
    final <- rbind(final, sims)
  }
  
  #Print on each run
  print(paste("Run", j, "was super sweet"))
}





