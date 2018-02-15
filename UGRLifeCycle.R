

#Delete Later
years <- 30
runs <- 20

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
stages <- c("Year", "Run", stages, "Spawner1Egg", "Spawner2Egg", "Spawner3Egg", "WildSpawners")

#Create a dataframe equal to years and columns needed
sims <- data.frame(matrix(0, nrow=years, ncol=length(stages)))

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
    #####       STARTs AT YEAR 5         #####
    ##########################################
    
    #IN YEAR
    #--------------LGD Adult 3 to Trap Adult 3----------------#   
    if (i > 4) {
      sims$TrapAdult3[i] <- bev_holt(sims$LGDAdult3[i], p_LGDAdult3_TrapAdult3)
    }
    
    #--------------Trap Adult 3 to Spawner 3----------------#    
    if (i > 4) {
      sims$Spawner3[i] <- bev_holt(sims$TrapAdult3[i], p_TrapAdult3_Spawner3)
    }
    
    #--------------Spawner 3 to Spawner Egg 3 ----------------#  
    if (i > 4 ) {
      sims$Spawner3Egg[i] <- bev_holt(sims$Spawner3[i], p_Spawner3_Spawner3Egg)
    }
    
    ##########################################
    #####       STARTs AT YEAR 4         #####
    ##########################################
    
    #IN YEAR
    #Age 2 adults return to spawn as 2s and produce eggs
    #--------------LGD Adult 2 to Trap Adult 2----------------#    
    if (i > 3) {
      sims$TrapAdult2[i] <- bev_holt(sims$LGDAdult2[i], p_LGDAdult2_TrapAdult2)
    }
    
    #--------------Trap Adult 2 to Spawner 2----------------#    
    if (i > 3) {
      sims$Spawner2[i] <- bev_holt(sims$TrapAdult2[i], p_TrapAdult2_Spawner2)
    }
    
    #--------------Spawner 2 to Spawner Egg 2 ----------------#  
    if (i > 3 ) {
      sims$Spawner2Egg[i] <- bev_holt(sims$Spawner2[i], p_Spawner2_Spawner2Egg)
    }
    
    #ADD YEAR
    #--------------Ocean Adult 2 to   LGD Adult 3----------------#
    if (i > 3 & i < years) {
      sims$LGDAdult3[i+1] <- bev_holt((sims$OceanAdult2[i] * p_OceanAdult2_LGDAdult3[5]), p_OceanAdult2_LGDAdult3)
    }
    
    ##########################################
    #####       STARTs AT YEAR 3         #####
    ##########################################
    
    #IN YEAR
    #--------------LGD Adult 1 to Trap Adult 1----------------#    
    if (i > 2) {
     sims$TrapAdult1[i] <- bev_holt(sims$LGDAdult1[i], p_LGDAdult1_TrapAdult1)
    }
    
    #--------------Trap Adult 1  to Spawner 1----------------#    
    if (i > 2) {
      sims$Spawner1[i] <- bev_holt(sims$TrapAdult1[i], p_TrapAdult1_Spawner1)
    }
    
    #--------------Spawner 1 to Spawner Egg 1 ----------------#  
    if (i > 2 ) {
      sims$Spawner1Egg[i] <- bev_holt(sims$Spawner1[i], p_Spawner1_Spawner1Egg)
    }
    
    #-------------- Sum Eggs ----------------#  
    if (i > 2 ) {
      sims$Egg[i] <- sims$Spawner1Egg[i] + sims$Spawner2Egg[i]+ sims$Spawner3Egg[i]
    }
    
    #-------------- Sum Wild Spawners ----------------#  
    if (i > 2 ) {
      sims$WildSpawners[i] <- sims$Spawner1[i] + sims$Spawner2[i]+ sims$Spawner3[i]
    }
    
    #ADD YEAR
    #--------------Ocean Adult 1 to Ocean Adult 2----------------#
    if (i > 2 & i < years) {
      sims$OceanAdult2[i+1] <- bev_holt((sims$OceanAdult1[i] * p_OceanAdult1_OceanAdult2[5]), p_OceanAdult1_OceanAdult2)
    }
    
    #--------------Ocean Adult 1 to   LGD Adult 2----------------#
    if (i > 2 & i < years) {
      sims$LGDAdult2[i+1] <- bev_holt((sims$OceanAdult1[i] * p_OceanAdult1_LGDAdult2[5]), p_OceanAdult1_LGDAdult2)
    }
    
    #--------------Egg to Fry----------------#
    if (i > 2 & i < years) {
      sims$Fry[i+1] <- bev_holt(sims$Egg[i], p_Egg_Fry)
    }
  
    ##########################################
    #####       STARTs AT YEAR 2         #####
    ##########################################
    
    #IN YEAR
    #--------------Pre Smolts to LGD Smolts----------------#
    #Total smolts at LGD as sum of headwaters and valley pre-smolts
    if (i > 1) {
      sims$LGDSmolt[i] <- 
        (bev_holt(sims$PreSmoltHeadwaters[i], p_PreSmoltHeadwaters_LGDSmolt)
         +
        bev_holt(sims$PreSmoltValley[i], p_PreSmoltValley_LGDSmolt))
    }
    
    #ADD YEAR
    #--------------LGD Smolts to LGD Adults 1----------------#
    #LGDsmolts * maturation probability
    if (i > 1 & i < years) {
      sims$LGDAdult1[i + 1] <-  bev_holt((sims$LGDSmolt[i] * p_LGDSmolt_LGDAdult1[5]), p_LGDSmolt_LGDAdult1)
    }
  
    #--------------LGD Smolts to Ocean Adults 1----------------#
    #LGDsmolts * stay in ocean probability
    if (i > 1 & i < years) {
      sims$OceanAdult1[i + 1] <-  bev_holt((sims$LGDSmolt[i] * p_LGDSmolt_OceanAdult1[5]), p_LGDSmolt_OceanAdult1)
    }
    
    ##########################################
    #####       STARTs AT YEAR 1         #####
    ##########################################
    
    #IN YEAR
    #--------------FRY TO PARR----------------#
    sims$Parr[i] <- bev_holt(sims$Fry[i], p_Fry_Parr)
  
    #ADD YEAR
    #--------------PARR TO PRE SMOLTS----------------#
    #Note no Survival Here - they just move
    if (i < years) {
      sims$PreSmoltHeadwaters[i + 1] <- sims$Parr[i] * p_Parr_PreSmoltHeadwaters[5]
      sims$PreSmoltValley[i + 1] <- sims$Parr[i] * p_Parr_PreSmoltValley[5]
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





