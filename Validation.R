# VALIDATION
library(tcltk) # for folder selection
library(tidyverse)
library(gridExtra)
library(scales)


# get some raw data
validation_folder <- tk_choose.dir()
raw_data <- read.csv(paste(validation_folder, "/output_raw.csv", sep=""), stringsAsFactors = FALSE)

# select single set of stable years and validation stages
validation_dataset <- raw_data %>%
    # filter run and years
    filter(Year == 50) %>%
    select(Year, LGDSmolt, H1_LGDSmolt, TrapAdult, H1_TrapAdult, Parr, H1_HatchRelease) %>%
    gather(key = Stage, value = Estimate, LGDSmolt, H1_LGDSmolt, H1_HatchRelease, TrapAdult, H1_TrapAdult, Parr, -Year) %>%
    mutate(Source = 'Modelled') %>%
    select(Source, Year, Stage, Estimate)

validation_dataset <- rbind(validation_dataset, ugr.validation)


# plot comparisons

box_comp <- function(stage, label, y_axis, box_color) {
    # Trap Adults Natural
    validation_dataset %>% filter(Stage == stage) %>%
        ggplot(aes(y = Estimate, x = Source)) +
        geom_jitter(width = 0.3, alpha = 0.3, size = 1) +
        geom_boxplot(fill = box_color, alpha = 0.7) +
        labs(y = y_axis, title = label) +
        theme(axis.text = element_text(size=12, face="bold"),
              axis.title.y = element_text(size=14,face="bold"),
              axis.title.x = element_blank(),
              plot.title = element_text(size=16, face="bold")) +
        scale_y_continuous(labels = comma)
}

# natural adults
b1 <- box_comp('Parr', 'Late Summer Parr', 'Parr', 'dodgerblue')
b2 <- box_comp('LGDSmolt', 'Natural LGD Smolt', 'LGD Smolt', 'dodgerblue')
b3 <- box_comp('TrapAdult', 'Natural Adults to Trap', 'Natural Adults', 'dodgerblue')
b4 <- box_comp('H1_HatchRelease', 'Hatchery Smolt Released', 'Hatchery Release', 'deeppink3')
b5 <- box_comp('H1_LGDSmolt', 'Hatchery LGD Smolt', 'Hatchery LGD Smolt', 'deeppink3')
b6 <- box_comp('H1_TrapAdult', 'Hatchery Adults to Trap', 'Hatchery Adults', 'deeppink3')

grid.arrange(b1, b2, b3, b4, b5, b6, nrow = 2)
