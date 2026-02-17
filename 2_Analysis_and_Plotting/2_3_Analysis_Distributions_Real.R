#### Analysis of Tropilaelaps distribution

library(lme4)
library(performance)
library(VCA)
library(ggpubr)

names(datum)




datum2 <- datum %>% 
  filter(core == 1) %>% 
  mutate(

    sum_all = tropi_infest200,
    
    # If uncapped 100 cells from one frame
    
    sum1 = tropi_1_50 + tropi_51_100,
    sum2 = tropi_101_150 + tropi_151_200,
    
    # If uncapped 100 cells (50 cells on each frame)
    
    sumA = tropi_1_50 + tropi_101_150,
    sumB = tropi_51_100 + tropi_151_200,
    
    sumAB = tropi_1_50 + tropi_151_200,
    sumBA = tropi_51_100 + tropi_101_150,

    # If uncapped all
    
    detect_all = if_else(sum_all >0, 1, 0),
    
    # If uncapped 100 cells from one frame
    
    detect1 = if_else(sum1 >0, 1, 0),
    detect2 = if_else(sum2 >0, 1, 0),
    
    # If uncapped 100 cells (50 cells on each frame)
    
    detectA = if_else(sumA >0, 1, 0),
    detectB = if_else(sumB >0, 1, 0),
    detectAB = if_else(sumAB >0, 1, 0),
    detectBA = if_else(sumBA >0, 1, 0),

    # If uncapped 50 cells only on one side
    
    detect1A = if_else(tropi_1_50 >0, 1, 0),
    detect1B = if_else(tropi_51_100 >0, 1, 0),
    detect2A = if_else(tropi_101_150 >0, 1, 0),
    detect2B = if_else(tropi_151_200 >0, 1, 0),
    
         )



sum(datum2$detect_all) # 24/26 = 92.3%

sum(datum2$detect1) # 23/26
sum(datum2$detect2) # 21/26
# 22/26 = 84.6%

sum(datum2$detectA) # 21/26
sum(datum2$detectB) # 23/26
sum(datum2$detectAB) # 23/26
sum(datum2$detectBA) # 22/26
(21+23+23+22)/4
# 22.25/26 = 85.6%

# All sets of 100 cells
(23+21+21+23+23+22)/6
# 22.16667/26 = 85.3%

sum(datum2$detect1A) # 19/26
sum(datum2$detect1B) # 19/26
sum(datum2$detect2A) # 17/26
sum(datum2$detect2B) # 18/26

(19+19+17+18)/4

18.25/26 # = 70.2%

# Further data prep

df <- tibble(cells_uncapped = c(50, 100, 200), detection = c(73, 86, 93))

df %>% 
  ggplot(aes(x = cells_uncapped, y = detection)) +
  geom_point() +
  scale_y_continuous(limits = c(0,100))
  


## Side vs. side

datum_sidevside <- datum_dist %>% 
  select(-uncap_subset) %>% 
  pivot_wider(names_from = side, names_prefix = "infested",
              values_from = count_infested)

datum_sidevside %>% 
  mutate(detectA = if_else()
         
         )


## Frame vs. frame

datum_framevframe <- datum_dist %>% 
  select(-uncap_subset) %>% 
  pivot_wider(names_from = frame, names_prefix = "infested",
              values_from = count_infested)



# Scatterplots

## side v side

paneA <- datum_sidevside %>% 
  ggplot(aes(x = infestedA, y = infestedB)) +
  geom_jitter(alpha = 0.3, height = 0.2, width = 0.2, size = 2) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.3, linetype = "dashed") +
  scale_x_continuous(limits = c(-1,30)) +
  scale_y_continuous(limits = c(-1,30)) +
  labs(x = "Count, side A", y = "Count, side B") +
  theme_classic(base_size = 18)
  

r = cor(datum_sidevside$infestedA, datum_sidevside$infestedB)
r^2
# R-squared = 0.74

datum_sidevside_infpt_rm <- datum_sidevside %>%
  filter(infestedB < 25)

r = cor(datum_sidevside_infpt_rm$infestedA, datum_sidevside_infpt_rm$infestedB)
r^2
# R-squared = 0.88

## frame v frame


paneB <- datum_framevframe %>% 
  ggplot(aes(x = infested1, y = infested2)) +
  geom_jitter(alpha = 0.3, height = 0.2, width = 0.2, size = 2) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.3, linetype = "dashed") +
  scale_x_continuous(limits = c(-1,30)) +
  scale_y_continuous(limits = c(-1,30)) +
  labs(x = "Count, frame 1", y = "Count, frame 2") +
  theme_classic(base_size = 18)

paneA

r = cor(datum_framevframe$infested1, datum_framevframe$infested2)
r^2
# R-squared = 0.67


# Put them together

ggarrange(paneA, paneB, ncol = 2, nrow = 1, labels = c("A", "B"))



ggsave("./2_Analysis_and_Plotting/outputs/tropi_corplot_2026-02-06.tiff", width = 8, height = 4, units = "in")


datum_dist$colony_id <- as.factor(datum_dist$colony_id)
datum_dist$frame <- as.factor(datum_dist$frame)
datum_dist$side <- as.factor(datum_dist$side)

# datum_dist$frame_cont <- rep(1:58, each = 2)
# datum_dist$side_cont <- rep(1:116)

## Correlation between adult bees and phoretic infestation

datum_phoretic <- datum %>% 
  filter(core == 1) %>%
  mutate(adult_detect = tropi_alc + tropi_powsug)

cor(datum_phoretic$fob, datum_phoretic$adult_detect)
(0.1685651)^2




