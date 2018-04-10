
library(tidyverse)



summary <- final %>%
  select(Scenario, Year, Run, TotalEgg, Parr, LGDSmolt, H1_LGDSmolt, Spawner, H1_Spawner, Parr) %>%
  group_by(Scenario, Year) %>%
  summarise(
    # TotalEgg = natural plus hatchery
    
    # Eggs Natural + Hatchery
    TotalEggMin = min(TotalEgg),
    TotalEgg25 = quantile(TotalEgg, 0.25),
    TotalEggMed = median(TotalEgg),
    TotalEgg75 = quantile(TotalEgg, 0.75),
    TotalEggMax = max(TotalEgg),
    
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
    
    # Natural Spawners
    SpawnerMin = min(Spawner),
    Spawner25 = quantile(Spawner, 0.25),
    SpawnerMed = median(Spawner),
    Spawner25 = quantile(Spawner, 0.25),
    Spawner75 = quantile(Spawner, 0.75),
    SpawnerMax = max(Spawner),
    
    # Hatchery Spawners
    H1_SpawnerMin = min(H1_Spawner),
    H1_Spawner25 = quantile(H1_Spawner, 0.25),
    H1_SpawnerMed = median(H1_Spawner),
    H1_Spawner25 = quantile(H1_Spawner, 0.25),
    H1_Spawner75 = quantile(H1_Spawner, 0.75),
    H1_SpawnerMax = max(H1_Spawner)
    )


# RIBBON PLOTS


# Total Eggs
ggplot(summary) +
  geom_ribbon(aes(x = Year, ymin = TotalEggMin, ymax = TotalEggMax),alpha = 0.5, fill = "coral3") +
  geom_ribbon(aes(x = Year, ymin = TotalEgg25, ymax = TotalEgg75),  alpha = 0.5) +
  geom_line(aes(x = Year, y = TotalEggMed), lwd = 1, alpha = 0.7) +
  labs(y = "Eggs", title = "Natural + Hatchery Eggs", subtitle = scenario_name) +
  theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))


# Parr
ggplot(summary) +
  geom_ribbon(aes(x = Year, ymin = ParrMin, ymax = ParrMax),alpha = 0.5, fill = "dodgerblue3") +
  geom_ribbon(aes(x = Year, ymin = Parr25, ymax = Parr75),  alpha = 0.5) +
  geom_line(aes(x = Year, y = ParrMed), lwd = 1, alpha = 0.7) +
  labs(y = "Parr", title = "Late Summer Parr", subtitle = scenario_name) +
  theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))


# Smolt
ggplot(summary) +
  geom_ribbon(aes(x = Year, ymin = LGDSmoltMin, ymax = LGDSmoltMax),alpha = 0.5, fill = "dodgerblue3") +
  geom_ribbon(aes(x = Year, ymin = LGDSmolt25, ymax = LGDSmolt75),  alpha = 0.5) +
  geom_line(aes(x = Year, y = LGDSmoltMed), lwd = 1, alpha = 0.7) +
  labs(y = "LGD Smolt", title = "Natural Smolt at LGD", subtitle = scenario_name) +
  theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Hatchery Smolt
ggplot(summary) +
  geom_ribbon(aes(x = Year, ymin = H1_LGDSmoltMin, ymax = H1_LGDSmoltMax),alpha = 0.5, fill = "deeppink3") +
  geom_ribbon(aes(x = Year, ymin = H1_LGDSmolt25, ymax = H1_LGDSmolt75),  alpha = 0.5) +
  geom_line(aes(x = Year, y = H1_LGDSmoltMed), lwd = 1, alpha = 0.7) +
  labs(y = "Hatcyery LGD Smolt", title = "Hatchery Smolt at LGD", subtitle = scenario_name) +
  theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))


# Natural Spawners
ggplot(summary) +
  geom_ribbon(aes(x = Year, ymin = SpawnerMin, ymax = SpawnerMax),alpha = 0.5, fill = "dodgerblue3") +
  geom_ribbon(aes(x = Year, ymin = Spawner25, ymax = Spawner75),  alpha = 0.5) +
  geom_line(aes(x = Year, y = SpawnerMed), lwd = 1, alpha = 0.7) +
  labs(y = "Natural Spawners", title = "Natural Spawners", subtitle = scenario_name) +
  theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Hatchery Spawners
ggplot(summary) +
  geom_ribbon(aes(x = Year, ymin = H1_SpawnerMin, ymax = H1_SpawnerMax),alpha = 0.5, fill = "deeppink3") +
  geom_ribbon(aes(x = Year, ymin = H1_Spawner25, ymax = H1_Spawner75),  alpha = 0.5) +
  geom_line(aes(x = Year, y = H1_SpawnerMed), lwd = 1, alpha = 0.7) +
  labs(y = "Hatchery Spawners", title = "Hatchery Spawners", subtitle = scenario_name) +
  theme(axis.text = element_text(size=12,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))