
library(tidyverse)
library(scales)

summary <- final %>%
    select(ModelName, Model, Year, Run, Egg, Parr, LGDSmolt, H1_LGDSmolt, TrapAdult, H1_TrapAdult, Brood, TotalTrap, H1_Brood, Spawner, H1_Spawner, TotalSpawner, Parr) %>%
    group_by(ModelName, Model, Year) %>%
    summarise(
    # TotalEgg = natural plus hatchery
    
        # Eggs Natural + Hatchery
        EggMin = min(Egg),
        Egg25 = quantile(Egg, 0.25),
        EggMed = median(Egg),
        Egg75 = quantile(Egg, 0.75),
        EggMax = max(Egg),
    
        # Parr
        ParrMin = min(Parr),
        Parr25 = quantile(Parr, 0.25),
        ParrMed = median(Parr),
        Parr75 = quantile(Parr, 0.75),
        ParrMax = max(Parr),
    
        # Natural LGDSmolt
        LGDSmoltMin = min(LGDSmolt),
        LGDSmolt25 = quantile(LGDSmolt, 0.25),
        LGDSmoltMed = median(LGDSmolt),
        LGDSmolt75 = quantile(LGDSmolt, 0.75),
        LGDSmoltMax = max(LGDSmolt),
    
        # Hatchery LGDSmolt
        H1_LGDSmoltMin = min(H1_LGDSmolt),
        H1_LGDSmolt25 = quantile(H1_LGDSmolt, 0.25),
        H1_LGDSmoltMed = median(H1_LGDSmolt),
        H1_LGDSmolt75 = quantile(H1_LGDSmolt, 0.75),
        H1_LGDSmoltMax = max(H1_LGDSmolt),
    
        # Natural Returns
        TrapAdultMin = min(TrapAdult),
        TrapAdult25 = quantile(TrapAdult, 0.25),
        TrapAdultMed = median(TrapAdult),
        TrapAdult75 = quantile(TrapAdult, 0.75),
        TrapAdultMax = max(TrapAdult),
    
        # Hatchery Returns
        H1_TrapAdultMin = min(H1_TrapAdult),
        H1_TrapAdult25 = quantile(H1_TrapAdult, 0.25),
        H1_TrapAdultMed = median(H1_TrapAdult),
        H1_TrapAdult75 = quantile(H1_TrapAdult, 0.75),
        H1_TrapAdultMax = max(H1_TrapAdult),
    
        # Total Returns
        TotalTrapMin = min(TotalTrap),
        TotalTrap25 = quantile(TotalTrap, 0.25),
        TotalTrapMed = median(TotalTrap),
        TotalTrap75 = quantile(TotalTrap, 0.75),
        TotalTrapMax = max(TotalTrap),
    
        # Natural Brood
        BroodMin = min(Brood),
        Brood25 = quantile(Brood, 0.25),
        BroodMed = median(Brood),
        Brood75 = quantile(Brood, 0.75),
        BroodMax = max(Brood),
    
        # Hatchery Brood
        H1_BroodMin = min(Brood),
        H1_Brood25 = quantile(H1_Brood, 0.25),
        H1_BroodMed = median(H1_Brood),
        H1_Brood75 = quantile(H1_Brood, 0.75),
        H1_BroodMax = max(H1_Brood),
    
    
        # Natural Spawners
        SpawnerMin = min(Spawner),
        Spawner25 = quantile(Spawner, 0.25),
        SpawnerMed = median(Spawner),
        Spawner75 = quantile(Spawner, 0.75),
        SpawnerMax = max(Spawner),
    
        # Hatchery Spawners
        H1_SpawnerMin = min(H1_Spawner),
        H1_Spawner25 = quantile(H1_Spawner, 0.25),
        H1_SpawnerMed = median(H1_Spawner),
        H1_Spawner75 = quantile(H1_Spawner, 0.75),
        H1_SpawnerMax = max(H1_Spawner),
        
        # Total Spawners
        TotalSpawnerMin = min(TotalSpawner),
        TotalSpawner25 = quantile(TotalSpawner, 0.25),
        TotalSpawnerMed = median(TotalSpawner),
        TotalSpawner75 = quantile(TotalSpawner, 0.75),
        TotalSpawnerMax = max(TotalSpawner)
        
    )

# TODO refactor into a plotting function
# RIBBON PLOTS
# Eggs


ribbon_plot <- function(data, stage_min, stage_max, stage_25, stage_75, stage_med, fill_color, y_label, plot_title) {
    ggplot(data) +
        geom_ribbon(aes(x = Year, ymin = stage_min, ymax = stage_max),alpha = 0.5, fill = fill_color) +
        geom_ribbon(aes(x = Year, ymin = stage_25, ymax = stage_75),  alpha = 0.5) +
        geom_line(aes(x = Year, y = stage_med), lwd = 1, alpha = 0.7) +
        labs(y = y_label, title = plot_title, subtitle = paste(model_name)) +
        theme(axis.text = element_text(size=12,face="bold"),
              axis.title = element_text(size=14,face="bold"),
              plot.title = element_text(size=18, face="bold"),
              plot.subtitle = element_text(size=14, face="bold", color = "darkgray")) +
        scale_y_continuous(labels = comma)
}

# Eggs
ribbon_plot(summary, summary$EggMin, summary$EggMax, summary$Egg25, summary$Egg75, summary$EggMed, "coral3", "Eggs", "Natural + Hatchery Eggs")

# Parr
ribbon_plot(summary, summary$ParrMin, summary$ParrMax, summary$Parr25, summary$Parr75, summary$ParrMed, "dodgerblue", "Parr", "Late Summer Parr")

# Smolt
ribbon_plot(summary, summary$LGDSmoltMin, summary$LGDSmoltMax, summary$LGDSmolt25, summary$LGDSmolt75, summary$LGDSmoltMed, "dodgerblue", "Natural Smolt", "Natural Smolt at LGD")

# Smolt
ribbon_plot(summary, summary$H1_LGDSmoltMin, summary$H1_LGDSmoltMax, summary$H1_LGDSmolt25, summary$H1_LGDSmolt75, summary$H1_LGDSmoltMed, "deeppink3", "Hatchery Smolt", "Hatchery Smolt at LGD")

# Natural Trap Returns
ribbon_plot(summary, summary$TrapAdultMin, summary$TrapAdultMax, summary$TrapAdult25, summary$TrapAdult75, summary$TrapAdultMed, "dodgerblue", "Natural Adults", "Natural Adults to Trap")

# Hatchery Trap Returns
ribbon_plot(summary, summary$H1_TrapAdultMin, summary$H1_TrapAdultMax, summary$H1_TrapAdult25, summary$H1_TrapAdult75, summary$H1_TrapAdultMed, "deeppink3", "Hatchery Adults", "Hatchery Adults to Trap")

# Natural Brood
ribbon_plot(summary, summary$BroodMin, summary$BroodMax, summary$Brood25, summary$Brood75, summary$BroodMed, "dodgerblue", "Natural Adults", "Natural Brood Retained")

# Hatchery Brood
ribbon_plot(summary, summary$H1_BroodMin, summary$H1_BroodMax, summary$H1_Brood25, summary$H1_Brood75, summary$H1_BroodMed, "deeppink3", "Hatchery Adults", "Hatchery Brood Retained")

# Natural Spawners
ribbon_plot(summary, summary$SpawnerMin, summary$SpawnerMax, summary$Spawner25, summary$Spawner75, summary$SpawnerMed, "dodgerblue", "Natural Adults", "Natural Spawners")

# Hatchery Spawners
ribbon_plot(summary, summary$H1_SpawnerMin, summary$H1_SpawnerMax, summary$H1_Spawner25, summary$H1_Spawner75, summary$H1_SpawnerMed, "deeppink3", "Hatchery Adults", "Hatchery Spawner")



# Comparison Plots
