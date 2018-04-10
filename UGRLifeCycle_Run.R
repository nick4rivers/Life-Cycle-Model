

library(tidyverse) # for data manupulation and some plots
library(VGAM) # postive only normal distribution function


#---------------Establish Constants------------------------#
#Supplementation goal based on population as total brood retention
if (population == "CC") {
  brood_goal = 102
} else {
  brood_goal = 170
}

#Create a big array of all constants from the input file
for (i in 1:nrow(input)) {
  assign(paste("p_",input$Stage_Transition[i], sep=""), c(input$Productivity[i], input$Productivity_SD[i],
                                                          input$Capacity[i], input$Capacity_SD[i],
                                                          input$Transition[i], input$Transition_SD[i],
                                                          input$Natural_Scaler[i], input$Hatchery_Scaler[i], input$Hatchery_Scaler[i]))
}

#---------------SET DATA FRAME FOR SIMULATIONS----------------------#
#create a vector for our stages from the input dataframe
stages <- unique(input$Stage1)


#Add accounting stages we are missing
stages <- c(stages,  "LGDAdult1","LGDAdult2","LGDAdult3", "Brood")

#create hatchery stages
my_hatch <- c()
for (i in 1:length(stages)) {
  my_hatch[i] <- paste("H1_",stages[i], sep="")
}

#add years, reps and hatchery placeholder names
stages <- c("Scenario", "Year", "Run", stages, my_hatch, "TotalBrood")
# Delete HatchRelease Element
stages <- stages[stages != "HatchRelease"]
rm(my_hatch)

#Create a dataframe equal to years and columns needed
sims <- data.frame(matrix(0, nrow=years, ncol=length(stages)))

#Name the columns in our dataframe
names(sims) <- stages

#---------------BEAVERTON HOLT FUNCTION----------------------#
# Takes 2 arguments
# stage1 = N at first stage
# stage1_parms = array vector of parameters

bev_holt <- function (stage1, stage_param) {
  # define productivity and capacity
  prod <-  stage_param[7] * rposnorm(1, mean = stage_param[1], sd = stage_param[2])
  cap <- rposnorm(1, mean = stage_param[3], sd = stage_param[4])
  # the bev hold  
  stage1 / (((1/prod) + ((1/cap)*stage1)))
}

h1_bev_holt <- function (stage1, stage_param) {
  # define productivity and capacity
  prod <-  stage_param[8] * rposnorm(1, mean = stage_param[1], sd = stage_param[2])
  cap <- rposnorm(1, mean = stage_param[3], sd = stage_param[4])
  # the bev hold  
  stage1 / (((1/prod) + ((1/cap)*stage1)))
}

########################################
####     START MODEL LOOPS         #####
########################################

#Loops for runs
for (j in 1:runs) {
  sims$Run <- j
  
  #Seed the model
  sims$Fry[1] <- seed_fry #wild
  sims$H1_LGDSmolt[1] <- seed_hatchery_smolt #hatchery
  
  #Loop for years 
  for (i in 1:years) {
    
    #Iterate years
    sims$Year[i] <- i
    sims$Scenario[i] <- scenario_name
    
    ##########################################
    #####       WITHIN YEAR              #####
    ##########################################
    
    ##########FRY AND PARR###################
    #--------------FRY TO PARR----------------#
    # There's no such thing as a hatchery Fry, they all became wild as eggs! 
    sims$Parr[i] <- bev_holt(sims$Fry[i], p_Fry_Parr)
    
    #--------------Pre Smolts to LGD Smolts----------------#
    #Total smolts at LGD as sum of headwaters and valley pre-smolts after survival to LGD
    sims$LGDSmolt[i] <- 
      (bev_holt(sims$PreSmoltHeadwaters[i], p_PreSmoltHeadwaters_LGDSmolt)
       +
         bev_holt(sims$PreSmoltValley[i], p_PreSmoltValley_LGDSmolt))
    #<<----HATCH----->>#
    #Hatchery releases to LGD smolt
    sims$H1_LGDSmolt[i] <- bev_holt(sims$H1_HatchRelease[i], p_HatchRelease_LGDSmolt)
    

    #######RETURNING ADULTS###########
    #--------------LGD Adult to Trap Adult----------------#    
    sims$TrapAdult[i] <- bev_holt(sims$LGDAdult[i], p_LGDAdult_TrapAdult)
    #<<----HATCH----->>#
    sims$H1_TrapAdult[i] <- h1_bev_holt(sims$H1_LGDAdult[i], p_LGDAdult_TrapAdult)
    
    
    ############# SUPPLEMENTATION SCHEME ############################
    
    if (i < stop_sup) { # Turn off hatchery supplementation in year
      
      if (population == "CC") {
        
        #### CATHERINE CREEK SUP ####
        
        #Set target for wild fish retention
        #Check spawners returning to the trap
        if (sims$TrapAdult[i] + sims$H1_TrapAdult[i] < 250) {
          wild_target <- 0.4 * brood_goal #Low Tier
        } else if (sims$TrapAdult[i] + sims$H1_TrapAdult[i] > 500) {
          wild_target <- 0.1 * brood_goal #High Tier
        } else {
          wild_target <- 0.2 * brood_goal #Middle Tier
        }
        
        #------- RESCALE WILD SPAWNERS ------------#
        # Retain natural adults at the trap
        if (sims$TrapAdult[i] > wild_target) {
          sims$Brood[i] <- wild_target
          sims$TrapAdult[i] <- sims$TrapAdult[i] - sims$Brood[i]
        } else { # or take them all
          sims$Brood[i] <- sims$TrapAdult[i]
          sims$TrapAdult[i] <- 0
        }
        
        #Calculate hatchery needed to reach brood goal given wild retained
        hatchery_target <- brood_goal - sims$Brood[i]
        
        #Try to take what you need, or take them all
        if (sims$H1_TrapAdult[i] > hatchery_target) {
          sims$H1_Brood[i] <- hatchery_target
          sims$H1_TrapAdult[i] <- sims$H1_TrapAdult[i] - sims$H1_Brood[i]
        } else {
          sims$H1_Brood[i] <- sims$H1_TrapAdult[i]
          sims$H1_TrapAdult <- 0
        }
        
        #Set Total Brood
        sims$TotalBrood[i] <- sims$H1_Brood[i] + sims$Brood[i]
        
      } else {
        
        #### UPPER GRANDE RONDE SUP ####  
        
        # set target for wild retention
        wild_target <- sims$TrapAdult[i] * 0.5
        
        # make sure it's not too many
        if (wild_target > brood_goal) {
          wild_target <- brood_goal
        }
        
        # Take the target and rescale trap adults
        sims$Brood[i] <- wild_target
        sims$TrapAdult[i] <- sims$TrapAdult[i] - sims$Brood[i]
        
        # set hatchery target from remaining brood goal
        hatchery_target <- brood_goal - sims$Brood[i]
        
        #Try to take what you need, or take them all
        if (sims$H1_TrapAdult[i] > hatchery_target) {
          sims$H1_Brood[i] <- hatchery_target
          sims$H1_TrapAdult[i] <- sims$H1_TrapAdult[i] - sims$H1_Brood[i]
        } else {
          sims$H1_Brood[i] <- sims$H1_TrapAdult[i]
          sims$H1_TrapAdult[i] <- 0
        }
        
        #Set Total Brood
        sims$TotalBrood[i] <- sims$H1_Brood[i] + sims$Brood[i]
        
      } # END UGR SUP
    } # SUP STOP
    ##################### END SUPPLEMENTATION SCHEME ########################

    #--------------Trap Adult to Spawner----------------#    
    sims$Spawner[i] <- bev_holt(sims$TrapAdult[i], p_TrapAdult_Spawner)
    #<<----HATCH----->>#
    sims$H1_Spawner[i] <- h1_bev_holt(sims$H1_TrapAdult[i], p_TrapAdult_Spawner)
    
    #########SPAWNER AND EGGS###########
    #--------------Spawner to Egg----------------#  
    sims$Egg[i] <- 0.5 * bev_holt(sims$Spawner[i], p_Spawner_Egg)
    #<<----HATCH----->>#
    sims$H1_Egg[i] <- 0.5 * h1_bev_holt(sims$H1_Spawner[i], p_Spawner_Egg)
    
    #-------------- Sum Eggs ----------------#
    #Note this is where H1 is Wild Again!!!!!
    sims$TotalEgg[i] <- sims$Egg[i] + sims$H1_Egg[i]
    
    ##########################################
    #####       NEXT YEAR                #####
    ##########################################
    if (i < years) { #Don't do on final year
    
      #########EGGS TO FRY###########
      #Note, there's no such thing as a hatchery egg, all eggs are wild
      sims$Fry[i+1] <- bev_holt(sims$TotalEgg[i], p_TotalEgg_Fry)
      
      #########PARR AND PRESMOLT###########
      #--------------PARR TO PRE SMOLTS----------------#
      #Note no Survival Here - they just move
      sims$PreSmoltHeadwaters[i + 1] <- sims$Parr[i] * p_Parr_PreSmoltHeadwaters[5]
      sims$PreSmoltValley[i + 1] <- sims$Parr[i] * p_Parr_PreSmoltValley[5]
    
      #########OCEAN ADULTS###########
      #--------------LGD Smolts to LGD Adults 1----------------#
      #LGDsmolts * maturation probability
      sims$LGDAdult1[i + 1] <-  bev_holt((sims$LGDSmolt[i] * p_LGDSmolt_LGDAdult1[5]), p_LGDSmolt_LGDAdult1)
      #<<----HATCH----->>#  
      sims$H1_LGDAdult1[i + 1] <-  h1_bev_holt((sims$H1_LGDSmolt[i] * p_LGDSmolt_LGDAdult1[5]), p_LGDSmolt_LGDAdult1)
      
      #--------------LGD Smolts to Ocean Adults 1----------------#
      #LGDsmolts * stay in ocean probability
      sims$OceanAdult1[i + 1] <-  bev_holt((sims$LGDSmolt[i] * p_LGDSmolt_OceanAdult1[5]), p_LGDSmolt_OceanAdult1)
      #<<----HATCH----->>#
      sims$H1_OceanAdult1[i + 1] <-  h1_bev_holt((sims$H1_LGDSmolt[i] * p_LGDSmolt_OceanAdult1[5]), p_LGDSmolt_OceanAdult1)
      
      #--------------Ocean Adult 1 to Ocean Adult 2----------------#
      sims$OceanAdult2[i+1] <- bev_holt((sims$OceanAdult1[i] * p_OceanAdult1_OceanAdult2[5]), p_OceanAdult1_OceanAdult2)
      #<<----HATCH----->>#
      sims$H1_OceanAdult2[i+1] <- h1_bev_holt((sims$H1_OceanAdult1[i] * p_OceanAdult1_OceanAdult2[5]), p_OceanAdult1_OceanAdult2)
      
      #--------------Ocean Adult 1 to   LGD Adult 2----------------#
      sims$LGDAdult2[i+1] <- bev_holt((sims$OceanAdult1[i] * p_OceanAdult1_LGDAdult2[5]), p_OceanAdult1_LGDAdult2)
      #<<----HATCH----->>#
      sims$H1_LGDAdult2[i+1] <- h1_bev_holt((sims$H1_OceanAdult1[i] * p_OceanAdult1_LGDAdult2[5]), p_OceanAdult1_LGDAdult2)
      
      #--------------Ocean Adult 2 to   LGD Adult 3----------------#
      sims$LGDAdult3[i+1] <- bev_holt((sims$OceanAdult2[i] * p_OceanAdult2_LGDAdult3[5]), p_OceanAdult2_LGDAdult3)
      #<<----HATCH----->>#
      sims$H1_LGDAdult3[i+1] <- h1_bev_holt((sims$H1_OceanAdult2[i] * p_OceanAdult2_LGDAdult3[5]), p_OceanAdult2_LGDAdult3)
    
      #########SUM LGD ADULTS###########
      #--------------Natural Adults----------------#
      sims$LGDAdult[i+1] <- sims$LGDAdult1[i+1] + sims$LGDAdult2[i+1] + sims$LGDAdult3[i+1]
      #--------------Natural Adults----------------#
      sims$H1_LGDAdult[i+1] <- sims$H1_LGDAdult1[i+1] + sims$H1_LGDAdult2[i+1] + sims$H1_LGDAdult3[i+1]
  
    } #END YEAR TRANSITIONS
    
    ##########################################
    #####       TWO  YEARs                #####
    ##########################################
    # This is just populating HatchRelease from AllBrood
    if (i < years - 1) { #Don't do in final two years
      #Dump hatchery fish into valley
      sims$H1_HatchRelease[i + 2] <- sims$TotalBrood[i] * 1470 #PreSmolts per brooder 
    }
    
  #Print each year
  print(paste("Year", i))
  }
  #-----------------------------End year Loops------------------------------#
  #New dataframe on first run, append on successive runs
    if (j == 1) {
    final <- sims
  } else {
    final <- rbind(final, sims)
  }
  
  #Reset simulation to 0's
  if (i < years) {
  sims[,] <- 0
  }
  #Print on each run
  print(paste("Run", j, "was super frickin' sweet"))
}


# write out a single summarization
# change to your directory of choice
# write.csv(sims, file="/Users/nick/Desktop/final.csv")






