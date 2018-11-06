library(tidyverse) # for data manipulation and graphics

#---------------Establish Constants------------------------#

# Create variables from settings file
for (i in 1:nrow(settings)) {
    if (settings$setting_type[i] == "int") {
        assign(settings$setting_name[i], as.integer(settings$setting_value[i]))
    } else {
        assign(settings$setting_name[i], as.character( settings$setting_value[i]))
    }
}

# check stochasticity setting and set to 0
if (stochasticity == 'OFF') {
    input$Productivity_SD = 0
    input$Capacity_SD = 0
    input$Life_History_SD = 0
    input$Fit_SD = 0
}


# Create list for parameters from in input file
for (i in 1:nrow(input)) {
    assign(paste("p_",input$Stage_Transition[i], sep=""), list( "stage1" = input$Stage1[i],
                                                                "stage2" = input$Stage2[i],
                                                                "productivity" = input$Productivity[i],
                                                                "productivity_sd" = input$Productivity_SD[i],
                                                                "capacity"= input$Capacity[i],
                                                                "capacity_sd" = input$Capacity_SD[i],
                                                                "fit_sd" = input$Fit_SD[i],
                                                                "life_history" = input$Life_History[i],
                                                                "life_history_sd" = input$Life_History_SD[i],
                                                                "natural_prod_scaler" = input$Natural_Prod_Scaler[i],
                                                                "hatchery_prod_scaler" = input$Hatchery_Prod_Scaler[i],
                                                                "natural_cap_scaler" = input$Natural_Cap_Scaler[i],
                                                                "hatchery_cap_scaler" = input$Hatchery_Cap_Scaler[i]))
}

# Supplementation goal based on population as total brood retention
if (population == "CC") {
    brood_goal = 102
} else {
    brood_goal = 170
}

#---------------SET DATA FRAME FOR SIMULATIONS----------------------#
# Create a vector for our stages from the input dataframe
stages <- unique(c(input$Stage1, input$Stage2))


# Add accounting stages we are missing that need accounting and hatchery stage
stages <- append(stages, "Brood")

# Create hatchery stages
my_hatch <- c()
for (i in 1:length(stages)) {
    my_hatch[i] <- paste("H1_",stages[i], sep="")
}

# Add model run qualifiers
stages <- c("ModelName", "Model", "Rep", "Year", "Run", stages, my_hatch)
# Delete any stages not needed
stages <- stages[stages != "HatchRelease"]


# Create a dataframe equal to years and columns needed
sims <- data.frame(matrix(0, nrow=years, ncol=length(stages)))

# Name the columns in our dataframe
names(sims) <- stages

# Clean up
rm(my_hatch)
rm(input)
rm(settings)
rm(stages)

########################################
####     START MODEL LOOPS         #####
########################################



# Loops for runs
for (j in 1:runs) {
    sims$Run <- j
    
    # Seed the model
    sims$Egg[1] <- seed_egg
    sims$LGDSmolt[1] <- seed_natural_smolt
    sims$H1_LGDSmolt[1] <- seed_hatchery_smolt
    
    # Loop for years 
    for (i in 1:years) {
    
        # Run years and fill model qualifiers
        sims$Year[i] <- i
        sims$ModelName[i] <- model_name
        sims$Model[i] <- model
        
        
        ##########################################
        #####        WITHIN YEAR             ##### 
        ##########################################
    
        #--------------PreSmolts to LGD Smolts----------------#
        # Total smolts at LGD as sum of headwaters and valley pre-smolts after survival to LGD
        # Don't do in first year to allow seeding
        if (i > 1) {
        sims$LGDSmolt[i] <-
            (bev_holt('NO', sims$PreSmoltHeadwaters[i], p_PreSmoltHeadwaters_LGDSmolt) +
            bev_holt('NO', sims$PreSmoltValley[i], p_PreSmoltValley_LGDSmolt))
        #<<----HATCH----->>#
        # Hatchery releases to LGD smolt
        sims$H1_LGDSmolt[i] <- bev_holt('HO', sims$H1_HatchRelease[i], p_HatchRelease_LGDSmolt)
        }
        
        #--------------Trap Adult Age Classes---------------#
        sims$TrapAdult[i] <- sims$TrapAdult1[i] + sims$TrapAdult2[i] + sims$TrapAdult3[i]
        #<<----HATCH----->>#
        sims$H1_TrapAdult[i] <- sims$H1_TrapAdult1[i] + sims$H1_TrapAdult2[i] + sims$H1_TrapAdult3[i]
    
        ############# SUPPLEMENTATION SCHEME ############################
        if (i < stop_sup) { # Turn off hatchery supplementation in year i
      
            if (population == "CC") {
        
                #### CATHERINE CREEK SUP ####
        
                # Set target for wild fish retention
                # Check spawners returning to the trap
                if (sims$TrapAdult[i] + sims$H1_TrapAdult[i] < 250) {
                    wild_target <- floor(0.4 * brood_goal) #Low Tier
                } else if (sims$TrapAdult[i] + sims$H1_TrapAdult[i] > 500) {
                    wild_target <- floor(0.1 * brood_goal) #High Tier
                } else {
                    wild_target <- floor(0.2 * brood_goal) #Middle Tier
                }
        
                #------- RESCALE WILD SPAWNERS ------------#
                # Retain natural adults at the trap
                if (sims$TrapAdult[i] > wild_target) {
                    sims$Brood[i] <- wild_target
                    sims$PassedAdult[i] <- sims$TrapAdult[i] - sims$Brood[i]
                } else { # or take them all
                    sims$Brood[i] <- sims$TrapAdult[i]
                    sims$PassedAdult[i] <- 0
                }
        
                # Calculate hatchery needed to reach brood goal given wild retained
                hatchery_target <- brood_goal - sims$Brood[i]
        
                # Try to take what you need, or take them all
                if (sims$H1_TrapAdult[i] > hatchery_target) {
                    sims$H1_Brood[i] <- hatchery_target
                    sims$H1_PassedAdult[i] <- sims$H1_TrapAdult[i] - sims$H1_Brood[i]
                } else {
                    sims$H1_Brood[i] <- sims$H1_TrapAdult[i]
                    sims$H1_PassedAdult[i] <- 0
                }
        
                # Set Total Brood
                sims$TotalBrood[i] <- floor(sims$H1_Brood[i] + sims$Brood[i])
        
            } else {
        
            #### UPPER GRANDE RONDE SUP ####  
        
                # Set target for wild retention
                wild_target <- floor(sims$TrapAdult[i] * 0.5)
        
                # Make sure it's not too many
                if (wild_target > brood_goal) {
                    wild_target <- brood_goal
                }
        
                # Take the target and rescale trap adults as passed adults
                sims$Brood[i] <- wild_target
                sims$PassedAdult[i] <- sims$TrapAdult[i] - sims$Brood[i]
        
                # Set hatchery target from remaining brood goal
                hatchery_target <- brood_goal - sims$Brood[i]
        
                # Try to take what you need, or take them all
                if (sims$H1_TrapAdult[i] > hatchery_target) {
                    sims$H1_Brood[i] <- hatchery_target
                    sims$H1_PassedAdult[i] <- sims$H1_TrapAdult[i] - sims$H1_Brood[i]
                } else {
                    sims$H1_Brood[i] <- sims$H1_TrapAdult[i]
                    sims$H1_PassedAdult[i] <- 0
                }
        
                # Set Total Brood
                sims$TotalBrood[i] <- sims$H1_Brood[i] + sims$Brood[i]
        
            } # END UGR SUP
        } # SUP STOP
    
        #---------------------Trap Adult to Passed Adult--------------------------#
        # Passess all adults when/if supplementation is shut down
        if (i >= stop_sup) {
            # Natural
            sims$PassedAdult[i] <- sims$TrapAdult[i]
            # Hatchery
            sims$H1_PassedAdult[i] <- sims$H1_TrapAdult[i]
        }
    
        ##################### END SUPPLEMENTATION SCHEME ###############################
        
        #--------------Passed Adult to Spawner----------------#    
        sims$Spawner[i] <- bev_holt('NO', sims$PassedAdult[i], p_PassedAdult_Spawner)
        #<<----HATCH----->>#
        sims$H1_Spawner[i] <- bev_holt('HO', sims$H1_PassedAdult[i], p_PassedAdult_Spawner)
        
        #----------------Sum Spawners-------------------------#
        sims$TotalSpawner[i] <- sims$Spawner[i] + sims$H1_Spawner[i]
        
        #########SPAWNER AND EGGS###########
        #--------------Spawner to Egg----------------#
        # don't do in first year to allow seeding the model
        if (i > 1) {
        sims$Egg[i] <- bev_holt('NO',  floor(0.5 * sims$TotalSpawner[i]), p_TotalSpawner_Egg)
        }
        ##########################################
        #####       NEXT YEAR                #####
        ##########################################
        if (i < years) { #Don't do on final year
            
            #########EGGS TO PARR###########
            # Note, there's no such thing as a hatchery egg, all eggs gone wild
            sims$Parr[i+1] <- bev_holt('NO', sims$Egg[i], p_Egg_Parr)
        
            #########PARR AND PRESMOLT###########
            #--------------PARR TO PRESMOLTS----------------#
            # Note no Survival Here - they just move
            choice_probs <- life_history_choices(p_Parr_PreSmoltHeadwaters)
            sims$PreSmoltHeadwaters[i + 1] <- floor(sims$Parr[i] * choice_probs$prob1)
            sims$PreSmoltValley[i + 1] <- ceiling(sims$Parr[i] * choice_probs$prob2)
        
            #########OCEAN ADULTS###########
            #--------------LGD Smolts to Adult Age 1----------------#
            choice_probs <- life_history_choices(p_LGDSmolt_TrapAdult1)
            sims$TrapAdult1[i + 1] <-  bev_holt('NO', (sims$LGDSmolt[i] * choice_probs$prob1), p_LGDSmolt_TrapAdult1)
            sims$OceanAdult1[i + 1] <-  bev_holt('NO', (sims$LGDSmolt[i] * choice_probs$prob2), p_LGDSmolt_OceanAdult1)
            #<<----HATCH----->>#  
            sims$H1_TrapAdult1[i + 1] <-  bev_holt('HO', (sims$H1_LGDSmolt[i] * choice_probs$prob1), p_LGDSmolt_TrapAdult1)
            sims$H1_OceanAdult1[i + 1] <-  bev_holt('HO', (sims$H1_LGDSmolt[i] * choice_probs$prob2), p_LGDSmolt_OceanAdult1)
        
            #--------------Ocean Adult 1 to Adult Age 2----------------#
            choice_probs <- life_history_choices(p_OceanAdult1_TrapAdult2)
            sims$TrapAdult2[i+1] <- bev_holt('NO', (sims$OceanAdult1[i] * choice_probs$prob1), p_OceanAdult1_TrapAdult2)
            sims$OceanAdult2[i+1] <- bev_holt('NO', (sims$OceanAdult1[i] * choice_probs$prob2), p_OceanAdult1_OceanAdult2)
            #<<----HATCH----->>#
            sims$H1_TrapAdult2[i+1] <- bev_holt('HO', (sims$H1_OceanAdult1[i] * choice_probs$prob1), p_OceanAdult1_TrapAdult2)
            sims$H1_OceanAdult2[i+1] <- bev_holt('HO', (sims$H1_OceanAdult1[i] * choice_probs$prob2), p_OceanAdult1_OceanAdult2)
      
            #--------------Ocean Adult 2 to LGD Adult 3----------------#
            sims$TrapAdult3[i+1] <- bev_holt('NO', (sims$OceanAdult2[i] * p_OceanAdult2_TrapAdult3$life_history), p_OceanAdult2_TrapAdult3)
            #<<----HATCH----->>#
            sims$H1_TrapAdult3[i+1] <- bev_holt('HO', (sims$H1_OceanAdult2[i] * p_OceanAdult2_TrapAdult3$life_history), p_OceanAdult2_TrapAdult3)
  
        } # END YEAR TRANSITIONS
    
        ##########################################
        #####       TWO  YEARs               #####
        ##########################################
        # This is just populating HatchRelease from AllBrood
        if (i < years - 1) { #Don't do in final two years
            # Dump hatchery fish into valley
            sims$H1_HatchRelease[i+2] <- bev_holt('HO', sims$TotalBrood[i], p_TotalBrood_HatchRelease)
        }
    
        # Print each year
        print(paste("Year", i))
    }
    #-----------------------------End year Loops------------------------------#
    # New dataframe on first run, append on successive runs
    if (j == 1) {
        final <- sims
    } else {
        final <- rbind(final, sims)
    }
  
    # Reset simulation to 0's
    if (i < years) {
        sims[,] <- 0
    }
    # Print on each run
    print(paste("Run", j, "was super frickin' sweet"))
}

# Some final accounting

final <- final %>%
    mutate(TotalTrap = TrapAdult + H1_TrapAdult)