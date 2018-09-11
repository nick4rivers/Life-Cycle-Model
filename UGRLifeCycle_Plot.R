
library(tidyverse)



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


# RIBBON PLOTS


# Eggs
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = EggMin, ymax = EggMax),alpha = 0.5, fill = "coral3") +
    geom_ribbon(aes(x = Year, ymin = Egg25, ymax = Egg75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = EggMed), lwd = 1, alpha = 0.7) +
    labs(y = "Eggs", title = "Natural + Hatchery Eggs", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))


# Parr
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = ParrMin, ymax = ParrMax),alpha = 0.5, fill = "dodgerblue3") +
    geom_ribbon(aes(x = Year, ymin = Parr25, ymax = Parr75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = ParrMed), lwd = 1, alpha = 0.7) +
    labs(y = "Parr", title = "Late Summer Parr", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))


# Smolt
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = LGDSmoltMin, ymax = LGDSmoltMax),alpha = 0.5, fill = "dodgerblue3") +
    geom_ribbon(aes(x = Year, ymin = LGDSmolt25, ymax = LGDSmolt75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = LGDSmoltMed), lwd = 1, alpha = 0.7) +
    labs(y = "LGD Smolt", title = "Natural Smolt at LGD", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Hatchery Smolt
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = H1_LGDSmoltMin, ymax = H1_LGDSmoltMax),alpha = 0.5, fill = "deeppink3") +
    geom_ribbon(aes(x = Year, ymin = H1_LGDSmolt25, ymax = H1_LGDSmolt75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = H1_LGDSmoltMed), lwd = 1, alpha = 0.7) +
    labs(y = "Hatcyery LGD Smolt", title = "Hatchery Smolt at LGD", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))


# Natural Trap Returns
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = TrapAdultMin, ymax = TrapAdultMax),alpha = 0.5, fill = "dodgerblue3") +
    geom_ribbon(aes(x = Year, ymin = TrapAdult25, ymax = TrapAdult75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = TrapAdultMed), lwd = 1, alpha = 0.7) +
    labs(y = "Natural Returns", title = "Natural Returns to Trap", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Hatchery Trap Returns
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = H1_TrapAdultMin, ymax = H1_TrapAdultMax),alpha = 0.5, fill = "deeppink3") +
    geom_ribbon(aes(x = Year, ymin = H1_TrapAdult25, ymax = H1_TrapAdult75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = H1_TrapAdultMed), lwd = 1, alpha = 0.7) +
    labs(y = "Hatchery Returns", title = "Hatchery Returns to Trap", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Total Trap Returns
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = TotalTrapMin, ymax = TotalTrapMax),alpha = 0.5, fill = "green3") +
    geom_ribbon(aes(x = Year, ymin = TotalTrap25, ymax = TotalTrap75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = TotalTrapMed), lwd = 1, alpha = 0.7) +
    labs(y = "Total Returns", title = "Total Returns to Trap", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))


# Natural Brood
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = BroodMin, ymax = BroodMax),alpha = 0.5, fill = "dodgerblue3") +
    geom_ribbon(aes(x = Year, ymin = Brood25, ymax = Brood75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = BroodMed), lwd = 1, alpha = 0.7) +
    labs(y = "Natural Brood", title = "Natural Brood Retained", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Hatchery Brood
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = H1_BroodMin, ymax = H1_BroodMax),alpha = 0.5, fill = "deeppink3") +
    geom_ribbon(aes(x = Year, ymin = H1_Brood25, ymax = H1_Brood75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = H1_BroodMed), lwd = 1, alpha = 0.7) +
    labs(y = "Hatchery Brood", title = "Hatchery Brood Retained", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Natural Spawners
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = SpawnerMin, ymax = SpawnerMax),alpha = 0.5, fill = "dodgerblue3") +
    geom_ribbon(aes(x = Year, ymin = Spawner25, ymax = Spawner75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = SpawnerMed), lwd = 1, alpha = 0.7) +
    labs(y = "Natural Spawners", title = "Natural Spawners", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Hatchery Spawners
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = H1_SpawnerMin, ymax = H1_SpawnerMax),alpha = 0.5, fill = "deeppink3") +
    geom_ribbon(aes(x = Year, ymin = H1_Spawner25, ymax = H1_Spawner75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = H1_SpawnerMed), lwd = 1, alpha = 0.7) +
    labs(y = "Hatchery Spawners", title = "Hatchery Spawners", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Total Spawners
ggplot(summary) +
    geom_ribbon(aes(x = Year, ymin = TotalSpawnerMin, ymax = TotalSpawnerMax),alpha = 0.5, fill = "green3") +
    geom_ribbon(aes(x = Year, ymin = TotalSpawner25, ymax = TotalSpawner75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = TotalSpawnerMed), lwd = 1, alpha = 0.7) +
    labs(y = "Total Spawners", title = "Total Spawners", subtitle = paste(model_name,"-", model)) +
    theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))
