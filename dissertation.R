### MULTIVARIATE ANALYSIS OF HERITABILITY OF LITERACY ACROSS THE DISTRIBUTION
### DISSERTATION BY VALTTERI VUORIO
### SUPERVISED BY PROFESSOR MICHELLE LUCIANO
### THE UNIVERSITY OF EDINBURGH
### 2023-2024


# Load Libraries
library(tidyverse)
library(psych)
library(polycor)
library(readxl)
library(broom) #currently used for pairwise.t.test
### Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# PREPARE DATA -----------------------------------------------------------------

# Load Data
df_read <- read_xlsx("./reading.xlsx")
df_id <- read_xlsx("./Master_IDs.xlsx")
df_iq <- read_xlsx("./RD-TM_IQ_data_20220407.xlsx", sheet = 1, na = c("NA", "#N/A", "NULL"))
df_digit <- read_xlsx("./digitspan.xlsx", sheet = 1, na = c("NA", "#N/A"))
#df_crt <- read_xlsx("./CRT.xlsx", sheet = "taps_CRT_summary_all") # NOT USED

### Put the datasets together
df <- inner_join(df_read,
                 inner_join(df_iq[c(1,2,8,9,10)],
                            inner_join(df_digit, df_id[c(1:2,4:5)])))

df <- df %>%
  transmute(ID = RD_twid,
            SEX = SEX,
            ZYGOSITY = factor(case_when(ZYGOSITY == 1 ~ "MZFF",
                                        ZYGOSITY == 2 ~ "MZMM",
                                        ZYGOSITY == 3 ~ "DZFF",
                                        ZYGOSITY == 4 ~ "DZMM",
                                        ZYGOSITY == 5 ~ "DZFM",
                                        ZYGOSITY == 6 ~ "DZMF",
                                        TRUE ~ "SIB"),
                              levels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZFM", "DZMF", "SIB")),
            FAMID = FAM_ID,
            AGE1 = `RD1-age`,
            AGE2 = `RD2-age`,
            MABAGE = `MAB-age`,
            FIQ = FIQ,
            PIQ = PIQ,
            VIQ = VIQ,
            DIGITSYM = DIGITSYM,
            LNS = `RD-LETNUMSC`,
            DSB = `RD-DIGSPBSC`,
            DSF = `RD-DIGSPFSC`,
            READIRR = (READING_IRREGULAR_CORRECT-READING_IRREGULAR_MISSING)/40,
            READREG = (READING_REGULAR_CORRECT-READING_REGULAR_MISSING)/40,
            READNW = (READING_NONWORD_CORRECT-READING_NONWORD_MISSING)/40,
            SPELLIRR = (SPELLING_IRREGULAR_CORRECT-SPELLING_IRREGULAR_MISSING)/18,
            SPELLREG = (SPELLING_REGULAR_CORRECT-SPELLING_REGULAR_MISSING)/18,
            SPELLNW = (SPELLING_NONWORD_CORRECT-SPELLING_NONWORD_MISSING)/18,
            NWGB = (NONWORDREP_GB_CORRECT-NONWORDREP_GB_MISSING)/40,
            NWDC = (NONWORDREP_DC_CORRECT-NONWORDREP_DC_MISSING)/20
  ) %>%
  mutate(across(.cols = -c(1:7), function(x) ifelse(x <= 0, NA, x))) #for people with too many missing values

### Remove twins "8584050" and "8591850" due to high missing values, "8901501" due to erroneous ZYGOSITY

for(i in 1){
  set.seed(666) #for random numbers
  missing_remove <- c("8584050", "8591850", "8901501")

#Keep these families; there are two or less twins in the family, no nontwin families
keep_families <- df %>%
  group_by(FAMID) %>%
  mutate(twins = sum(ZYGOSITY != "SIB")) %>%
  filter(twins == 2 | twins == 1) %>%
  ungroup() %>%
  .$FAMID

quadruplets <- df %>%
  group_by(FAMID) %>%
  mutate(twins = sum(ZYGOSITY != "SIB")) %>%
  filter(twins == 4) %>%
  ungroup() %>%
  .$FAMID

triplets <- df %>%
  group_by(FAMID) %>%
  mutate(twins = sum(ZYGOSITY != "SIB")) %>%
  filter(twins == 3) %>%
  ungroup() %>%
  .$FAMID

df_impute <- df %>%
  group_by(FAMID) %>%
  mutate(twins = sum(ZYGOSITY != "SIB")) %>%
  filter(twins == 1, ZYGOSITY != "SIB") %>%
  group_by(FAMID, ZYGOSITY) %>%
  mutate(rnd = case_when(ZYGOSITY == "DZMF" & SEX == "F" | ZYGOSITY == "DZFM" & SEX == "M" ~ 0,
                         ZYGOSITY == "DZFM" & SEX == "F" | ZYGOSITY == "DZMF" & SEX == "M" ~ 1,
                         TRUE ~ sample(0:1, 1))) %>%
  group_modify(~ add_row(.x, .after=.x$rnd)) %>%
  select(-twins, -rnd) %>%
  ungroup()

twins_only <- df %>%
  group_by(FAMID) %>%
  mutate(twins = sum(ZYGOSITY != "SIB")) %>%
  filter(twins == 2) %>%
  select(-twins) %>% ### HUOM! dplyr::select menee joskus sekaisin MASS::select kanssa
  .$ID

### Discard/keep participants found on lists above

set.seed(666)

df <- rbind(df, df_impute) %>%
  filter(!ID %in% missing_remove & (FAMID %in% keep_families | FAMID %in% quadruplets | FAMID %in% triplets)) %>%
  mutate(torder = factor(case_when(ZYGOSITY == "DZMF" & SEX == "F" | ZYGOSITY == "DZFM" & SEX == "M" ~ "Second",
                                   ZYGOSITY == "DZMF" & SEX == "M" | ZYGOSITY == "DZFM" & SEX == "F" ~ "First",
                                   ZYGOSITY == "SIB" ~ "Third",
                                   TRUE ~ sample(c("First", "Second"), 1)),
                         levels = c("First", "Second", "Third"))) %>%
  arrange(FAMID, ZYGOSITY, torder) %>%
  ungroup() %>%
  select(-torder) %>%
  distinct(ID, FAMID, .keep_all = TRUE)

}

################################################################################

for(i in 1){
df <- df %>%
  select(ID, FAMID, ZYGOSITY, SEX, AGE1, AGE2, MABAGE, PIQ, VIQ, READIRR, LNS, DSB, DIGITSYM, DSF, READREG, SPELLIRR, SPELLREG, READNW, SPELLNW, NWGB, NWDC)

### Correct for skewness
### Jos jossakin näkyy c(1:8), se muuttui muotoon c(1:7) koska poistin "SINGLETON" muuttujan

df <- df %>%
  mutate(READIRR = asin(READIRR), #-0.651 asin(x) -> ~0
         READNW = asin(READNW), #-1.11 asin(x) -> ~0
         READREG = asin(READREG), #-1.31 asin(x) -> ~0
         SPELLREG = asin(SPELLREG),
         NWGB = asin(NWGB),
         LNS = sqrt(LNS),
         DSB = sqrt(DSB)) %>% #-1.17 asin(x) -> ~0
  mutate(across(.cols = -c(1:7), function(x) scale(x, center = T, scale = T)))

# df <- df %>%
#   mutate(RAB = scale(READREG+READIRR+SPELLREG+SPELLIRR, scale = T, center = T),
#          SPELLING = scale(SPELLREG+SPELLIRR, scale = T, center = T),
#          READING = scale(READREG + READIRR, scale = T, center = T),
#          PA = scale(READNW+SPELLNW, scale = T, center = T),
#          VSTM = DSF,
#          PrS = DIGITSYM,
#          WM = scale(LNS+DSB, scale = T, center = T),
#          PSTM = scale(NWGB+NWDC, scale = T, center = T)
#   ) %>%
#   select(ID, FAMID, SEX, ZYGOSITY, AGE1, AGE2, MABAGE, PIQ, VIQ, PrS, WM, VSTM, PSTM, PA, READING, SPELLING, RAB)

df <- df %>%
  group_by(ID) %>% #jotta keskiarvot tulevat henkilön eikä kokonaisuuden mukaan
  mutate(RAB = mean(c(READREG,READIRR,SPELLREG,SPELLIRR), na.rm = T),
         SPELLING = mean(c(SPELLREG, SPELLIRR), na.rm = T),
         READING = mean(c(READREG, READIRR), na.rm = T),
         Decode = READNW,
         Encode = SPELLNW,
         PSTM = mean(c(NWGB,NWDC), na.rm = T),
         WM = mean(c(LNS,DSB), na.rm = T)) %>%
  ungroup() %>%
  mutate(VSTM = DSF,
         PercSpd = DIGITSYM
  ) %>%
  select(ID, FAMID, SEX, ZYGOSITY, AGE1, AGE2, MABAGE, PIQ, VIQ, PercSpd, WM, VSTM, PSTM, Decode, Encode, READING, SPELLING, RAB)
 }

# EXTRACT RESIDUALS ------------------------------------------------------------
for(i in 1){
residualisation <- c("PIQ", "VIQ", "PercSpd", "WM", "VSTM", "PSTM", "Decode", "Encode", "READING", "SPELLING", "RAB")
ages <- c("MABAGE", "MABAGE", "MABAGE", "AGE2", "AGE2", "AGE2", "AGE1", "AGE1", "AGE1", "AGE1", "AGE1")

# Function cycles through all residual and corresponding age-variables, and residualises
# all variables, then returns a dataframe with all new and lovely values

results <- list()
varlength <- length(residualisation)

# This part creates a linear model of all variables, residualising for corresponding age and sex
# It places individual tibbles on a list, which will then be bound with the original "df" dataframe

for(i in 1:varlength){
  fml <- reformulate(termlabels = c(ages[i],"SEX"), response = residualisation[i])
  a <- resid(lm(fml, data = df[,c(1:7, 7+i)], na.action = na.exclude))
  results <- append(results, tibble(x = a, .name_repair = ~residualisation[i]))
}

# These new residualised values are bound with ID, FAMID, SEX, ZYGOSITY from df
# dfres <- sapply(residualisation, dfres, simplify = FALSE)

#Binds dfres with ID, FAMID, ZYGOSITY, SEX
# Transforms +/- 4 SD values to the max/min value near the boundary
dfres <- cbind(df[,c(2:4)], results) %>% #ID, FAMID, SEX
  mutate(across(.cols = c(4:14),
                function(x)
                  case_when(x > 4 ~ max(.$x[.$x <= 4]),
                            x < -4 ~ min(.$x[.$x >= -4]),
                            TRUE ~ x)
  )) %>%
  group_by(FAMID) %>%
  mutate(twins = sum(ZYGOSITY != "SIB")) %>%
  ungroup()
}
# Density plots for all new variables
df %>%
  select(RCOMP = RAB, PIQ, PercSpd, WM, VSTM, PSTM, Decode) %>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "value") %>%
  mutate(Var = factor(Var, levels = c("RCOMP", "PIQ", "PercSpd", "WM", "VSTM", "PSTM", "Decode"))) %>%
  ggplot(., aes(x = value, fill = Var)) +
  geom_histogram(color = "black") +
  facet_wrap(~Var, scales = "free", ncol = 4) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Histograms for non-residualised variables\n") +
  xlab("\n Z-score") +
  ylab("Count\n")

dfres %>%
  select(RCOMP = RAB, PIQ, PercSpd, WM, VSTM, PSTM, Decode) %>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "value") %>%
  mutate(Var = factor(Var, levels = c("RCOMP", "PIQ", "PercSpd", "WM", "VSTM", "PSTM", "Decode"))) %>%
  ggplot(., aes(x = value, fill = Var)) +
  geom_histogram(color = "black") +
  facet_wrap(~Var, scales = "free", ncol = 4) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Histograms for residualised variables\n") +
  xlab("\n Z-score") +
  ylab("Count\n")

### Checking for skewness for visually and statistically

dfres %>%
  pivot_longer(cols = -c(1:2, "twins"), names_to = "Var", values_to = "value") %>%
  group_by(Var) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = TRUE),
            Shap = shapiro.test(value)$p.value,
            Skew = skew(value, na.rm = T))

### ----------------------------------------------------------------------------

twinData <- as.data.frame(dfres) %>% #reshape does not want to work if input is not of a type "data.frame"
  filter(twins <= 2) %>%
  arrange(FAMID, ZYGOSITY) %>%
  mutate(id = row_number(), .by = "FAMID",
         ZYGOSITY = factor(case_when(ZYGOSITY == 'MZFF' | ZYGOSITY == 'MZMM' ~ 'MZ',
                                     TRUE ~ 'DZ'),
                           levels = c('MZ', 'DZ')),
         RCOMP = factor(case_when(RAB <= 0 ~ "Poor",
                                  TRUE ~ "Good"),
                        levels = c("Good", "Poor"))) %>%
  select(-c(READING, SPELLING, RAB, VIQ, twins)) %>%
  reshape(direction = "wide",
          v.names = c("ZYGOSITY", "RCOMP", "SEX", "PIQ", "WM", "VSTM", "PSTM", "PercSpd", "Decode", "Encode"),
          idvar = "FAMID",
          timevar = "id",
          sep = "") %>%
  select(zyg = ZYGOSITY1, everything(), -c(ZYGOSITY2, ZYGOSITY3, ZYGOSITY4, ZYGOSITY5)) %>%
  ungroup()

varnames <- c("RCOMP", "zyg", "FAMID",
              "SEX1", "PIQ1", "WM1", "VSTM1", "PSTM1", "PercSpd1", "Decode1", "Encode1", 
              "SEX2", "PIQ2", "WM2", "VSTM2", "PSTM2", "PercSpd2", "Decode2", "Encode2",
              "SEX3", "PIQ3", "WM3", "VSTM3", "PSTM3", "PercSpd3", "Decode3", "Encode3")

Vars <- c("PIQ", "VIQ", "WM", "VSTM", "PSTM", "PercSpd", "Decode", "Encode")
nVar <- length(Vars)

first <- 3:11
second <- 12:20
third <- 21:29
fourth <- 30:38
fifth <- 39:47

for(i in 1){
#Twin 1 and sibling (3rd, 4th, or 5th) without twin 2
subData0 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP2)) & RCOMP1 == RCOMP3) %>%
  mutate(across(.cols = second, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),third)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData1 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP2)) & RCOMP1 == RCOMP4) %>%
  mutate(across(.cols = second, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),fourth)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP4)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData2 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP2)) & RCOMP1 == RCOMP5) %>%
  mutate(across(.cols = second, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),fifth)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(),-c(RCOMP2, RCOMP5)) %>%
  rename_with(.cols = everything(), ~ varnames)

#Twins 1 and 2 with or without third, fourth, fifth, or sixth sibling
subData4 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & RCOMP1 == RCOMP3) %>%
  .[,c(1:((2*nVar)+4),third)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData5 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & (RCOMP1 != RCOMP3 | is.na(RCOMP3))) %>%
  mutate(across(.cols = third, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),third)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData6 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & RCOMP1 == RCOMP4) %>%
  .[,c(1:((2*nVar)+4),fourth)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP4)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData7 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & RCOMP1 == RCOMP5) %>%
  .[,c(1:((2*nVar)+4),fifth)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP5)) %>%
  rename_with(.cols = everything(), ~ varnames)

#Twin 2 with 4th, or 5th sibling as an addition
subData9 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP1)) & RCOMP2 == RCOMP4) %>%
  mutate(across(.cols = first,function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),fourth)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP2, everything(), -c(RCOMP1, RCOMP4)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData10 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP1)) & RCOMP2 == RCOMP5) %>%
  mutate(across(.cols = first,function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),fifth)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP2, everything(), -c(RCOMP1, RCOMP5)) %>%
  rename_with(.cols = everything(), ~ varnames)

#The Cartesian product created some duplicate families, only one will be selected, others discarded
#Fex. in one family all belonged to the same RCOMP group, hence three distinct possibilities!
subData <- rbind(subData0, subData1, subData2, subData4, subData5, subData6, subData7, subData9, subData10) %>%
  arrange(FAMID, RCOMP) %>%
  mutate(rows = row_number(), .by = "FAMID") %>%
  filter(rows == 1) %>%
  select(-rows)

subData00 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP2)) &
           (RCOMP1 != RCOMP3 | is.na(RCOMP3)) &
           (RCOMP1 != RCOMP4 | is.na(RCOMP4)) &
           (RCOMP1 != RCOMP5 | is.na(RCOMP5))) %>%
  filter(!FAMID %in% subData$FAMID) %>% #check if FAMID is in the last subData; if not, these single-twins can be added!
  mutate(across(.cols = second, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),third)] %>%
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)

subData <- rbind(subData, subData00)
}
write.csv(subData, "subData.csv")

### CORRELATIONS ---------------------------------------------------------------

pattern <- "^(\\w+)(\\d)$" #thanks ChatGPT

twinCor <- twinData %>%
  select(-c(FAMID, RCOMP1, RCOMP2, RCOMP3, VIQ1, VIQ2, VIQ3, Encode1, Encode2, Encode3)) %>%
  .[,c(1:19)] %>%
  pivot_longer(cols = 2:19, names_to = c("Var", ".value"),
               names_pattern = "^(\\w+)(\\d)$") %>%
  rename(tw1 = "1", tw2 = "2", tw3 = "3")

twinCor %>%
  group_by(Var, zyg) %>%
  summarise(TwinTwin = cor(x = tw1, y = tw2, use = "complete"),
            SibTwin1 = cor(x = tw1, y = tw3, use = "complete"),
            SibTwin2 = cor(x = tw2, y = tw3, use = "complete")) %>%
  pivot_wider(names_from = "zyg", values_from = c("TwinTwin", "SibTwin1", "SibTwin2")) %>%
  transmute(Var = factor(Var, levels = c("PIQ", "PercSpd", "WM", "VSTM", "PSTM", "Decode")),
            MZFF = TwinTwin_MZFF,
            MZMM = TwinTwin_MZMM,
            DZFF = TwinTwin_DZFF,
            DZMM = TwinTwin_DZMM,
            DZFM = TwinTwin_DZFM,
            DZMF = TwinTwin_DZMF,
            Sib = mean(c(SibTwin1_MZFF, SibTwin1_MZMM, SibTwin1_DZFF, SibTwin1_DZMM, SibTwin1_DZFM, SibTwin1_DZMF,
                         SibTwin2_MZFF, SibTwin2_MZMM, SibTwin2_DZFF, SibTwin2_DZMM, SibTwin2_DZFM, SibTwin2_DZMF))) %>%
  arrange(Var) %>%
  mutate(across(.cols = everything(), function(x) sprintf("%.2f", round(x, 2))))

dfres %>%
  pivot_longer(cols = c("PIQ", "PercSpd", "WM", "VSTM", "PSTM", "Decode"), names_to = "Var", values_to = "value") %>%
  group_by(Var, ZYGOSITY) %>%
  summarise(M = sprintf("%.2f", round(mean(value, na.rm = T),2)),
            SD = sprintf("%.2f", round(sd(value, na.rm = T),2)))


### How many twins in each variable:

twinCor %>%
  mutate(Var = factor(Var, levels = c("PIQ", "PercSpd", "WM", "VSTM", "PSTM", "Decode"))) %>%
  pivot_longer(cols = c("tw1", "tw2", "tw3"), names_to = "Twin", values_to = "value") %>%
  na.omit() %>%
  group_by(Var, Twin) %>%
  count() %>%
  pivot_wider(names_from = "Twin", values_from = "n")

### Means and standard deviations for each family member:

twinCor %>%
  mutate(Var = factor(Var, levels = c("PIQ", "PercSpd", "WM", "VSTM", "PSTM", "Decode"))) %>%
  pivot_longer(cols = c("tw1", "tw2", "tw3"), names_to = "Twin", values_to = "value") %>%
  group_by(Twin, Var) %>%
  summarise("M (SD)" = paste0(sprintf("%.2f", round(mean(value, na.rm = T),2)), " (", sprintf("%.2f", round(sd(value, na.rm = T), 2)), ")")) %>%
  pivot_wider(names_from = "Twin", values_from = "M (SD)")

### Variance

twinCor %>%
  mutate(TZYG = factor(case_when(TZYG == "MZMM" | TZYG == "MZFF" ~ "MZ",
                                 TZYG == "DZFF" | TZYG == "DZMM" | TZYG == "DZFM" | TZYG == "DZMF" ~ "DZ",
                                 TRUE ~ "SIB"),
                       levels = c("MZ", "DZ", "SIB"))) %>%
  group_by(Var, TZYG) %>%
  summarise(Variance = var(x = tw1, y = tw2)) %>%
  pivot_wider(names_from = "TZYG", values_from = "Variance") %>%
  mutate(across(.cols = 1:3, function(x) sprintf("%.2f", round(x, digits = 2))))


# PLOTS ########################################################################

### Plot correlations in MZs and DZs

twinData %>%
  mutate(zyg = ifelse(ZYGOSITY.1 == ZYGOSITY.2 & ZYGOSITY.1 == "MZ", "MZ", ifelse(ZYGOSITY.1 == ZYGOSITY.2 & ZYGOSITY.1 == "DZ", "DZ", "SIB"))) %>%
  group_by(zyg) %>%
  filter(zyg != "SIB") %>%
  ggplot(., aes(x = READIRR.1, y = READIRR.2, group = zyg, color = zyg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x)