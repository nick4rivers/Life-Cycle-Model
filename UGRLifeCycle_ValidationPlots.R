# VALIDATION
library(tcltk) # for folder selection
library(tidyverse)
library(gridExtra)
library(scales)


# bring in observed data for population
validation_observed <- read.csv(file.choose(), stringsAsFactors = FALSE)

# get some raw validation run with stochasticity
validation_folder <- tk_choose.dir()
raw_data <- read.csv(paste(validation_folder, "/output_raw.csv", sep=""), stringsAsFactors = FALSE)
summary_data <- read.csv(paste(validation_folder, "/output_summary.csv", sep=""), stringsAsFactors = FALSE)


# select single set of stable years and validation stages
validation_dataset <- raw_data %>%
    # filter run and years
    filter(Year == 50) %>%
    select(Year, LGDSmolt, H1_LGDSmolt, TrapAdult, H1_TrapAdult, Parr, H1_HatchRelease) %>%
    gather(key = Stage, value = Estimate, LGDSmolt, H1_LGDSmolt, H1_HatchRelease, TrapAdult, H1_TrapAdult, Parr, -Year) %>%
    mutate(Source = 'Modelled') %>%
    select(Source, Year, Stage, Estimate)

# stack up data
validation_dataset <- rbind(validation_dataset, validation_observed)

# bring in deterministic data
# get some raw data
deterministic_folder <- tk_choose.dir()
deterministic_data <- read.csv(paste(deterministic_folder, "/output_raw.csv", sep=""), stringsAsFactors = FALSE)


# select single set of stable years and validation stages
deterministic_dataset <- deterministic_data %>%
    # filter run and years
    filter(Year == 50 & Run == 3) %>%
    select(Year, LGDSmolt, H1_LGDSmolt, TrapAdult, H1_TrapAdult, Parr, H1_HatchRelease) %>%
    gather(key = Stage, value = Estimate, LGDSmolt, H1_LGDSmolt, H1_HatchRelease, TrapAdult, H1_TrapAdult, Parr, -Year) %>%
    mutate(Source = 'Deterministic') %>%
    select(Source, Year, Stage, Estimate)


# plot comparisons

box_comp <- function(stage, label, y_axis, box_color) {
    
    # get reference from deterministic
    deterministic_reference <- deterministic_dataset %>%
        filter(Stage == stage)
    
    deterministic_reference <- deterministic_reference$Estimate
    
    validation_dataset %>% filter(Stage == stage) %>%
        ggplot(aes(y = Estimate, x = Source)) +
        geom_jitter(width = 0.3, alpha = 0.3, size = 1) +
        geom_boxplot(fill = box_color, alpha = 0.6) +
        geom_hline(yintercept = deterministic_reference, size = 1, alpha = 0.5, linetype = 1, col = 'red') +
        labs(y = y_axis, title = label) +
        theme(axis.text = element_text(size=12, face="bold"),
              axis.title.y = element_text(size=12,face="bold"),
              axis.title.x = element_blank(),
              plot.title = element_text(size=12, face="bold")) +
        scale_y_continuous(labels = comma)
}

# natural adults
b1 <- box_comp('Parr', 'Late Summer Parr', 'Parr', 'seagreen')
b2 <- box_comp('LGDSmolt', 'Natural LGD Smolt', 'LGD Smolt', 'seagreen')
b3 <- box_comp('TrapAdult', 'Natural Adults to Tributary', 'Natural Adults', 'seagreen')
b4 <- box_comp('H1_HatchRelease', 'Hatchery Smolt Released', 'Hatchery Release', 'seagreen')
b5 <- box_comp('H1_LGDSmolt', 'Hatchery LGD Smolt', 'Hatchery LGD Smolt', 'seagreen')
b6 <- box_comp('H1_TrapAdult', 'Hatchery Adults to Tributary', 'Hatchery Adults', 'seagreen')


grid.arrange(b1, b2, b3, b4, b5, b6,
             nrow = 2)


# ------------- TABLE OF VALUES ----------------------

# first get population name
pop = raw_data$Population[1]

validation_summary <- validation_dataset %>%
    group_by(Source, Stage) %>%
    summarize(MedianValue = median(Estimate))%>%
    spread(Source, MedianValue) %>%
    mutate(Population = pop,
           Difference = Modelled - Observed,
           PerDiff = Difference / Observed) %>%
    select(Population, Stage, Observed, Modelled, Difference, PerDiff)

write.csv(validation_summary, file=paste(validation_folder, "/",pop,"_validation_table.csv", sep=""))

