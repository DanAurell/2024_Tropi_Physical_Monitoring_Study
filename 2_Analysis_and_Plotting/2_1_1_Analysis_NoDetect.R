#### Analysis of Tropilaelaps distribution

# Setup

library(lme4)
library(emmeans)

## Sample size
# Methods: We analyzed these data for the core set of 26 colonies. 

## Tropi sticky
## Tropi uncap
## Tropi bump
## Tropi alc
## Tropi powsug

## Varroa powsug
## Varroa alc wash
## Varroa uncap

names(datum_long)

datum_tropi <- datum_long %>% 
  filter(core == 1) %>% 
  filter(target == "tropi")

unique(datum_long$test)

datum_varroa <- datum_long %>% 
  filter(core == 1) %>% 
  filter(target == "varroa") %>% 
  filter(test %in% c("powsug", "alc", "infest200"))

# Confirm sample size is n=26
# Mathematical mean number of detections

datum_tropi %>% 
  group_by(test) %>% 
  summarise(
    mean = mean(mites_num),
    n=n()
    )


datum_varroa %>% 
  group_by(test) %>% 
  summarise(
    mean = mean(mites_num),
    n=n()
  )


# Formal analyses - Tropi

m.no.0 <- glmer.nb(mites_num ~ 1 + (1|colony_id), data = datum_tropi)
m.no.1 <- glmer.nb(mites_num ~ test + (1|colony_id), data = datum_tropi)

summary(m.no.1)

anova(m.no.0, m.no.1, test = "Chisq")
anova(m.no.0, m.no.1)
# Results: Monitoring method had a significant impact on the number of  
# Tropilaelaps detections
# (Chisq = 149.7, df = 4, P < 0.001)

emmeans(m.no.1, pairwise ~ test, type = "response")$contrasts
emmeans(m.no.1, ~test, type = "response")


# Results: 
# (expressed in CLD)
# sticky ^ a
# infest200 ^ b 
# bump ^ c
# powsug ^ d 
# alc ^ d

simulateResiduals(m.no.1, plot = T) # OK
testDispersion(m.no.1) # There is evidence of overdisperson, but P>0.05; OK
testZeroInflation(m.no.1) # OK

# Nice Tropi pairwise comparisons table with gt

paircomp_notropi <- 
  tidy(emmeans(m.no.1, pairwise ~ test, type = "response")$contrasts)

tbl.paircomp_notropi <- gt(paircomp_notropi) %>% 
  fmt_number(
    columns = c("adj.p.value"),
    decimals = c(6))

tbl.paircomp_notropi <- tbl.paircomp_notropi %>%
  as_rtf()

my_conn <- file("./2_Analysis_and_Plotting/outputs/paircomp_notropi_2026-02-08.RTF", "w")
writeLines(tbl.paircomp_notropi, my_conn)
close(my_conn)


# Varroa
m.no.varroa.0 <- glmer.nb(mites_num ~ 1 + (1|colony_id), data = datum_varroa)
m.no.varroa.1 <- glmer.nb(mites_num ~ test + (1|colony_id), data = datum_varroa)

summary(m.no.varroa.1)

anova(m.no.varroa.0, m.no.varroa.1, test = "Chisq")
anova(m.no.varroa.0, m.no.varroa.1)

# Results: Monitoring method had a significant impact on the number of  
# Varroa detections
# (Chisq = 47.5, df = 2, P < 0.001)

emmeans(m.no.varroa.1, pairwise ~ test, type = "response")$contrasts
emmeans(m.no.varroa.1, ~test, type = "response")


# powsug ^ a
# alc ^ a
# infest200 ^ b

simulateResiduals(m.no.varroa.1, plot = T) # OK
testDispersion(m.no.varroa.1) # OK
testZeroInflation(m.no.varroa.1) # OK


paircomp_novarroa <- 
  tidy(emmeans(m.no.varroa.1, pairwise ~ test, type = "response")$contrasts)

tbl.paircomp_novarroa <- gt(paircomp_novarroa) %>% 
  fmt_number(
    columns = c("adj.p.value"),
    decimals = c(6))

tbl.paircomp_novarroa <- tbl.paircomp_novarroa %>%
  as_rtf()

my_conn <- file("./2_Analysis_and_Plotting/outputs/paircomp_novarroa_2026-02-08.RTF", "w")
writeLines(tbl.paircomp_novarroa, my_conn)
close(my_conn)
