#########################################
##New


#####################################################
## The Simple Beaverton Holt Function             ###
## Will likely be used at every stage of the model###
## The function will take a handful of arguments ####
## Takes 5 arguments listed in order
# Stage 1 = N at first stage
# prod = productivity term
# prod_sd = standard deviation around sd
# cap = capacity term
# cap_sd = temporal variation on capacity

###The Beaverton Holt Function - i.e. named Bev_Holt

#Will come back to this and make it able to turn on and off stochasticity
bev_holt <- function (stage1, prod, prod_sd, cap, cap_sd) {
  stage1/(((1/rnorm(1, mean = prod, sd = prod_sd))+((1/rnorm(1, mean = cap, sd = cap_sd))*stage1)))
}


###########################################
##    Set Up the Data Frame              ##
###########################################

#Delete Later
years <- 30
runs <- 10


#create a vector for our stages from the input dataframe
stages <- c("Egg", "Fry", "Parr", "PreSmolt")

#add years, reps and any other placeholders needed to our vector
stages <- c("Year", "Run", stages)


#Create a dataframe equal to years and columns needed
sims <- data.frame(matrix(NaN, nrow=years, ncol=length(stages)))

#Name the columns in our dataframe
names(sims) <- stages

#OK, time to initilize the model
#This will pull from inits, probably just set Fry to 10,000

sims$Fry[1] <- 10000
sims$Year[1] <- 1

#OK Let's start populating the model


bev_holt <- function (stage1, prod, prod_sd, cap, cap_sd)

##
p_Fry_Parr <- 0.5 
pSD_Fry_Parr <- 0.025
c_Fry_Parr <- 1000000000
cSD_Fry_Parr <- 0.0
YFP_Fry_Parr <- 0


##
p_Parr_Fry <- 2 
pSD_Parr_Fry <- 0.1
c_Parr_Fry <- 1000000000
cSD_Parr_Fry <- 0.0
YFP_Parr_Fry <- 1


#####START MODEL LOOPS
for (j in 1:runs) {
  sims$Run <- j
  #Set Fry start at beginning of each run
  sims$Fry[1] <- 10000

  
  #####LOOP FOR YEARS############# 
  for (i in 1:years) {
    

    #######Set Years###########
    sims$Year[i] <- i
    
    #######FRY TO PARR#########
    sims$Parr[i] <- bev_holt(sims$Fry[i], p_Fry_Parr, pSD_Fry_Parr, c_Fry_Parr, cSD_Fry_Parr)
  
    ########LETS MOVE THE PARR##################
    ###Note no Survival Here - they just move####
    ##They do however put on a year here, so it needs an if statement##
    if (i != years) {
      sims$HeadwatersPreSmolt[i + 1] <- sims$Parr * m_Parr_HeadwatersPreSmolt
      sims$ValleyPreSmolt[i + 1] <- sims$Parr * m_Parr_ValleyPreSmolt
    }
    
    ######AND ON TO LOWER GRANITE DAM AS SMOLTS###########
    ###Don't do in first year######
    if (i != 1) {
      #####THIS IS SUM OF SURVIVAL FROM EACH OF THE TWO SITES!!!##########
      ###GIVES TOTAL SMOLTS AT LGD###########
      LGDSmolts[i] <- 
        bev_holt(sims$HeadwatersPreSmolt[i], p_HeadwatersPreSmolt_LGDSmolt, pSD_HeadwatersPreSmolt_LGDSmolt, c_HeadwatersPreSmolt_LGDSmolt, cSD_HeadwatersPreSmolt_LGDSmolt)
      +
        bev_holt(sims$ValleyPreSmolt[i], p_ValleyPreSmolt_LGDSmolt, pSD_ValleyPreSmolt_LGDSmolt, c_ValleyPreSmolt_LGDSmolt, cSD_ValleyPreSmolt_LGDSmolt)
    }
    
    
    
    
    
    
  
    
    
    ################################################
    #########START ALL THE NEXT YEAR GUYS HERE######
    
    ##First Break if no transitions needed##
    if (i == years) {
      break 
    ########PARR TO SMOLT########
    sims$Fry[i + YFP_Parr_Fry] <- bev_holt(sims$Parr[i], p_Parr_Fry, pSD_Parr_Fry, c_Parr_Fry, cSD_Parr_Fry)
  
    }
  }

  #####NEW DATAFRAME ON FIRST RUN OR APPEND###########
  if (j == 1) {
    final <- sims
  } else {
    final <- rbind(final, sims)
  }
  
  ###PRINT THAT EVERYTHING WENT FINE##############
  print(paste("Run", j, "was super sweet"))
}





