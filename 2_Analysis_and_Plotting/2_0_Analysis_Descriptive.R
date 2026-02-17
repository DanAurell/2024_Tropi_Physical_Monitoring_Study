#### Analysis of Tropilaelaps distribution

## Sample size
# Methods: We analyzed data from 29 colonies. 
# For all colonies, we performed the sticky board test, cell uncapping, 
# bump test, and powdered sugar test. For 26 of 29 we performed alcohol washes. 
# Tropilaelaps mites were assessed for all test methods, whereas Varroa were 
# assessed only for cell uncapping, powdered sugar shake, and alcohol wash.


## Bee population FOB

datum %>%
  filter(core == 1) %>% 
  summarise(
    fob_min = min(fob),
    fob_max = max(fob),
    fob_mean = mean(fob),
    fob_sd = sd(fob),
    n = n()
  )

# Visually estimated adult bee population ranged 
# from 1.5-5.5 frames of bees (mean = 3.9; SD = 1.1).



## Tropi sticky

datum %>% 
  filter(core == 1) %>% 
  summarise(
    tropi_min = min(tropi_sticky),
    tropi_max = max(tropi_sticky),
    tropi_mean = mean(tropi_sticky),
    tropi_sd = sd(tropi_sticky),
    n = n()
  )

# Results: For the 26 colonies, the Tropilaelaps mite fall on sticky boards 
# ranged from 1-750 (mean = 51.7, SD = 145.9). 

## Tropi uncap

datum %>% 
  filter(core == 1) %>% 
  summarize(
    tropi_min = min(tropi_infest100),
    tropi_max = max(tropi_infest100),
    tropi_mean = mean(tropi_infest100),
    tropi_sd = sd(tropi_infest100),
    n = n()
  )

# Results: For the 26 colonies, the Tropilaelaps cell infestations 
# ranged from 0-27% (mean = 6.0%, SD = 7.8%). 


## Tropi bump

datum %>% 
  filter(core == 1) %>% 
  summarise(
    tropi_min = min(tropi_bump),
    tropi_max = max(tropi_bump),
    tropi_mean = mean(tropi_bump),
    tropi_sd = sd(tropi_bump),
    n = n()
  )

# Results: For the 26 colonies, the Tropilaelaps detected in the bump test
# ranged from 0-99% (mean = 7.1%, SD = 19.3%). 


## Tropi alc

datum %>% 
  filter(core == 1) %>% 
  summarise(
    tropi_min = min(tropi_alc),
    tropi_max = max(tropi_alc),
    tropi_mean = mean(tropi_alc),
    tropi_sd = sd(tropi_alc),
    n = n()
  )

# Results: For the 26 colonies, the number of Tropilaelaps collected in alcohol wash
# ranged from 0-29 (mean = 1.8, SD = 5.8). 

## Tropi powsug

datum %>% 
  filter(core == 1) %>% 
  summarise(
    tropi_min = min(tropi_powsug),
    tropi_max = max(tropi_powsug),
    tropi_mean = mean(tropi_powsug),
    tropi_sd = sd(tropi_powsug),
    n = n()
  )

# Results: For the 26 colonies, the Tropilaelaps collected in the powdered sugar shake
# ranged from 0-25% (mean = 1.7, SD = 5.0). 

## Varroa powsug


datum %>% 
  filter(core == 1) %>% 
  summarise(
    varroa_min = min(varroa_powsug),
    varroa_max = max(varroa_powsug),
    varroa_mean = mean(varroa_powsug),
    varroa_sd = sd(varroa_powsug),
    n = n()
  )

# Results: For the 26 colonies, the Varroa _
# ranged from 0-54 (mean = 10.2, SD = 14.6). 


## Varroa alc wash

datum %>% 
  filter(core == 1) %>% 
  summarise(
    varroa_min = min(varroa_alc),
    varroa_max = max(varroa_alc),
    varroa_mean = mean(varroa_alc),
    varroa_sd = sd(varroa_alc),
    n = n()
  )

# Results: For the 26 colonies, the Varroa _
# ranged from 0-57% (mean = 9, SD = 13.9). 


## Varroa uncap

datum %>% 
  filter(core == 1) %>% 
  summarise(
    varroa_min = min(varroa_infest100),
    varroa_max = max(varroa_infest100),
    varroa_mean = mean(varroa_infest100),
    varroa_sd = sd(varroa_infest100),
    n = n()
  )
# Results: For the 26 colonies, the Varroa cell infestations 
# ranged from 0-9% (mean = 1.8%, SD = 2.8%).



# Pearson correlations

library(GGally)
 
ggpairs(datum2,
        columns = c("tropi_infest200", "tropi_sticky", "tropi_powsug", "tropi_alc", "tropi_bump"),
        upper = list(continuous = wrap("cor", size = 3)), 
        lower = list(continuous = wrap("smooth")),
        # diag = list(continuous = wrap("density")),
        title = "Tropilaelaps monitoring methods")

ggsave("./2_Analysis_and_Plotting/outputs/pearson_plot_tropi_2026-02-08.tiff", width = 8, height = 8, units = "in")

ggpairs(datum2,
        columns = c("varroa_infest200", "varroa_powsug", "varroa_alc"),
        upper = list(continuous = wrap("cor", size = 3)), 
        lower = list(continuous = wrap("smooth")),
        # diag = list(continuous = wrap("density")),
        title = "Varroa monitoring methods")

ggsave("./2_Analysis_and_Plotting/outputs/pearson_plot_varroa_2026-02-08.tiff", width = 5, height = 5, units = "in")




