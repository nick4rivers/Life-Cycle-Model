# Ribon plots for final reporting
library(gridExtra)


# get summary output for base model simulation
ugr_summary <- read.csv(file.choose(), stringsAsFactors = FALSE)
cc_summary <- read.csv(file.choose(), stringsAsFactors = FALSE)




ribbon_plot <- function(data, stage_5, stage_95, stage_25, stage_75, stage_med, fill_color, plot_title, x_label, y_label, y_min, y_max) {
    ggplot(data) +
        geom_ribbon(aes(x = Year, ymin = stage_5, ymax = stage_95),alpha = 0.5, fill = fill_color) +
        geom_ribbon(aes(x = Year, ymin = stage_25, ymax = stage_75),  alpha = 0.5) +
        geom_line(aes(x = Year, y = stage_med), lwd = 1, alpha = 0.7) +
        coord_cartesian(ylim = c(y_min, y_max)) +
        labs(title = plot_title, x = x_label, y = y_label) +
        theme(axis.text = element_text(size=12,face="bold"),
              axis.title = element_text(size=14,face="bold"),
              plot.title = element_text(size=18, face="bold"),
              plot.subtitle = element_text(size=14, face="bold", color = "darkgray")) +
        scale_y_continuous(labels = comma)
}


# UGR Natural Spawners
ugr_nat_spawn <- ribbon_plot(ugr_summary, ugr_summary$Spawner5, ugr_summary$Spawner95, ugr_summary$Spawner25, ugr_summary$Spawner75, ugr_summary$SpawnerMed, "steelblue", "Upper Grande Ronde", " ", "Natural Spawners", 0, 1300)

# UGR Natural Spawners
cc_nat_spawn <- ribbon_plot(cc_summary, cc_summary$Spawner5, cc_summary$Spawner95, cc_summary$Spawner25, cc_summary$Spawner75, cc_summary$SpawnerMed, "seagreen", "Catherine Creek", " ", " ", 0, 1300)

# UGR Hatchery Spawners
ugr_h1_spawn <- ribbon_plot(ugr_summary, ugr_summary$H1_Spawner5, ugr_summary$H1_Spawner95, ugr_summary$H1_Spawner25, ugr_summary$H1_Spawner75, ugr_summary$H1_SpawnerMed, "steelblue", " ", " ", "Hatchery Spawners", 0, 2200)

# UGR Hatchery Spawners
cc_h1_spawn <- ribbon_plot(cc_summary, cc_summary$H1_Spawner5, cc_summary$H1_Spawner95, cc_summary$H1_Spawner25, cc_summary$H1_Spawner75, cc_summary$H1_SpawnerMed, "seagreen", " ", " ", " ", 0, 2200)

# UGR Hatchery Natural Smolt
ugr_nat_smolt <- ribbon_plot(ugr_summary, ugr_summary$LGDSmolt5, ugr_summary$LGDSmolt95,ugr_summary$LGDSmolt25, ugr_summary$LGDSmolt75, ugr_summary$LGDSmoltMed, "steelblue", " ", "Year", "Natural LGD Smolt", 0, 32000)

# cc Natural Smolt
cc_nat_smolt <- ribbon_plot(cc_summary, cc_summary$LGDSmolt5, cc_summary$LGDSmolt95, cc_summary$LGDSmolt25, cc_summary$LGDSmolt75, cc_summary$LGDSmoltMed, "seagreen", " ", "Year", " ", 0, 32000)


# exporting at 1000 x 800
grid.arrange(ugr_nat_spawn, cc_nat_spawn, ugr_h1_spawn, cc_h1_spawn, ugr_nat_smolt, cc_nat_smolt, nrow = 3)

