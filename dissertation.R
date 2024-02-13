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

################################################################################

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

df <- df %>%
  group_by(ID) %>% #jotta keskiarvot tulevat henkilön eikä kokonaisuuden mukaan
  mutate(RAB = mean(c(READREG,READIRR,SPELLREG,SPELLIRR), na.rm = T),
         SPELLING = mean(c(SPELLREG, SPELLIRR), na.rm = T),
         READING = mean(c(READREG, READIRR), na.rm = T),
         Phon = mean(c(READNW,SPELLNW), na.rm = T),
         PSTM = mean(c(NWGB,NWDC), na.rm = T),
         WM = mean(c(LNS,DSB), na.rm = T)) %>%
  ungroup() %>%
  mutate(VSTM = DSF,
         PercSpd = DIGITSYM
  ) %>%
  select(ID, FAMID, SEX, ZYGOSITY, AGE1, AGE2, MABAGE, PIQ, VIQ, PercSpd, WM, VSTM, PSTM, Phon, READING, SPELLING, RAB)

# EXTRACT RESIDUALS ------------------------------------------------------------
residualisation <- c("PIQ", "VIQ", "PercSpd", "WM", "VSTM", "PSTM", "Phon", "READING", "SPELLING", "RAB")
ages <- c("MABAGE", "MABAGE", "MABAGE", "AGE2", "AGE2", "AGE2", "AGE1", "AGE1", "AGE1", "AGE1")

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

# These new residualised values are bound with ID, FAMID, SEX, ZYGOSITY from df
# dfres <- sapply(residualisation, dfres, simplify = FALSE)

#Binds dfres with ID, FAMID, ZYGOSITY, SEX
# Transforms +/- 4 SD values to the max/min value near the boundary
dfres <- cbind(df[,c(2,4)], results) %>% #ID, FAMID, SEX
  mutate(across(.cols = c(3:12),
                function(x)
                  case_when(x > 4 ~ max(.$x[.$x <= 4]),
                            x < -4 ~ min(.$x[.$x >= -4]),
                            TRUE ~ x)
  )) %>%
  group_by(FAMID) %>%
  mutate(twins = sum(ZYGOSITY != "SIB")) %>%
  ungroup()

### Density plots for all new variables
dfres %>%
  pivot_longer(cols = -c(1:2, "twins"), names_to = "Var", values_to = "value") %>%
  ggplot(., aes(x = value, fill = Var)) +
  geom_density() +
  facet_wrap(~Var, scales = "free")

### Checking for skewness for visually and statistically

dfres %>%
  pivot_longer(cols = -c(1:2), names_to = "Var", values_to = "value") %>%
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
  select(-c(READING, SPELLING, RAB, twins)) %>%
  reshape(direction = "wide",
          v.names = c("ZYGOSITY", "RCOMP", "PIQ", "VIQ", "WM", "VSTM", "PSTM", "PercSpd", "Phon"),
          idvar = "FAMID",
          timevar = "id",
          sep = "") %>%
  select(zyg = ZYGOSITY1, everything(), -c(ZYGOSITY2, ZYGOSITY3, ZYGOSITY4, ZYGOSITY5)) %>%
  ungroup()

tripletData <- as.data.frame(dfres) %>%
  filter(twins == 3) %>%
  arrange(FAMID, ZYGOSITY) %>%
  mutate(id = row_number(), .by = "FAMID",
         ZYGOSITY = factor(case_when(ZYGOSITY == 'MZFF' | ZYGOSITY == 'MZMM' ~ 'MZ',
                                     TRUE ~ 'DZ'),
                           levels = c('MZ', 'DZ')),
         RCOMP = factor(case_when(RAB <= 0 ~ "Poor",
                                  TRUE ~ "Good"),
                        levels = c("Good", "Poor"))) %>%
  select(-c(READING, SPELLING, RAB, twins)) %>%
  reshape(direction = "wide",
          v.names = c("ZYGOSITY", "RCOMP", "PIQ", "VIQ", "WM", "VSTM", "PSTM", "PercSpd", "Phon"),
          idvar = "FAMID",
          timevar = "id",
          sep = "") %>%
  select(zyg = ZYGOSITY1, everything(), -c(ZYGOSITY2, ZYGOSITY3, ZYGOSITY4, ZYGOSITY5, ZYGOSITY6)) %>%
  ungroup()

quadrupletData <- as.data.frame(dfres) %>%
  filter(twins == 4) %>%
  arrange(FAMID, ZYGOSITY) %>%
  mutate(id = row_number(), .by = "FAMID",
         ZYGOSITY = factor(case_when(ZYGOSITY == 'MZFF' | ZYGOSITY == 'MZMM' ~ 'MZ',
                                     TRUE ~ 'DZ'),
                           levels = c('MZ', 'DZ')),
         RCOMP = factor(case_when(RAB <= 0 ~ "Poor",
                                  TRUE ~ "Good"),
                        levels = c("Good", "Poor"))) %>%
  select(-c(READING, SPELLING, RAB, twins)) %>%
  reshape(direction = "wide",
          v.names = c("ZYGOSITY", "RCOMP", "PIQ", "VIQ", "WM", "VSTM", "PSTM", "PercSpd", "Phon"),
          idvar = "FAMID",
          timevar = "id",
          sep = "") %>%
  select(zyg = ZYGOSITY1, everything(), -c(ZYGOSITY2, ZYGOSITY3, ZYGOSITY4, ZYGOSITY5)) %>%
  ungroup()

varnames <- c("RCOMP", "zyg", "FAMID",
              "PIQ1", "VIQ1", "WM1", "VSTM1", "PSTM1", "PercSpd1", "Phon1",
              "PIQ2", "VIQ2", "WM2", "VSTM2", "PSTM2", "PercSpd2", "Phon2",
              "PIQ3", "VIQ3", "WM3", "VSTM3", "PSTM3", "PercSpd3", "Phon3")

Vars <- c("PIQ", "VIQ", "WM", "VSTM", "PSTM", "PercSpd", "Phon")
nVar <- length(Vars)

for(i in 1){
#Twin 1 and sibling (3rd, 4th, or 5th) without twin 2
subData0 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP2)) & RCOMP1 == RCOMP3) %>%
  mutate(across(.cols = 11:18, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),19:26)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData1 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP2)) & RCOMP1 == RCOMP4) %>%
  mutate(across(.cols = 11:18, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),27:34)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP4)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData2 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP2)) & RCOMP1 == RCOMP5) %>%
  mutate(across(.cols = 11:18, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),35:42)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(),-c(RCOMP2, RCOMP5)) %>%
  rename_with(.cols = everything(), ~ varnames)

#Twins 1 and 2 with or without third, fourth, fifth, or sixth sibling
subData4 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & RCOMP1 == RCOMP3) %>%
  .[,c(1:((2*nVar)+4),19:26)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData5 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & (RCOMP1 != RCOMP3 | is.na(RCOMP3))) %>%
  mutate(across(.cols = 19:26, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),19:26)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData6 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & RCOMP1 == RCOMP4) %>%
  .[,c(1:((2*nVar)+4),27:34)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP4)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData7 <- twinData %>%
  filter(RCOMP1 == RCOMP2 & RCOMP1 == RCOMP5) %>%
  .[,c(1:((2*nVar)+4),35:42)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP5)) %>%
  rename_with(.cols = everything(), ~ varnames)

#Twin 2 with 4th, or 5th sibling as an addition
subData9 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP1)) & RCOMP2 == RCOMP4) %>%
  mutate(across(.cols = 3:10, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),27:34)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP2, everything(), -c(RCOMP1, RCOMP4)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData10 <- twinData %>%
  filter((RCOMP1 != RCOMP2 | is.na(RCOMP1)) & RCOMP2 == RCOMP5) %>%
  mutate(across(.cols = 3:10, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),35:42)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP2, everything(), -c(RCOMP1, RCOMP5)) %>%
  rename_with(.cols = everything(), ~ varnames)

#Quadruplets, take first two twins; Dizygotic triplets T1 and T2 w/o sibling ## NOT USED
subData41 <- quadrupletData %>%
  filter(RCOMP1 == RCOMP2) %>%
  mutate(across(.cols = 19:26, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),19:26)] %>%
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData31 <- tripletData %>%
  filter((RCOMP1 == RCOMP2 & RCOMP1 != RCOMP3)) %>%
  mutate(across(.cols = 19:26, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),19:26)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)
subData32 <- tripletData %>%
  filter((RCOMP1 != RCOMP2 & RCOMP1 == RCOMP4)) %>%
  mutate(across(.cols = 11:18, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),27:34)] %>% #reduce data to only include the necessary family members
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP4)) %>%
  rename_with(.cols = everything(), ~ varnames)

#The Cartesian product created some duplicate families, only one will be selected, others discarded
#Fex. in one family all belonged to the same RCOMP group, hence three distinct possibilities!
subData <- rbind(subData0, subData1, subData2, subData4, subData5, subData6, subData7, subData9, subData10, subData31, subData32, subData41) %>%
  #mutate(RCOMP = factor(RCOMP, levels = c("Poor", "Good", "Average"))) %>%
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
  mutate(across(.cols = 11:26, function(x) x = NA)) %>%
  .[,c(1:((2*nVar)+4),19:26)] %>%
  select(RCOMP = RCOMP1, everything(), -c(RCOMP2, RCOMP3)) %>%
  rename_with(.cols = everything(), ~ varnames)

subData <- rbind(subData, subData00)
}
write.csv(subData, "subData.csv")

### CORRELATIONS ---------------------------------------------------------------

pattern <- "^(\\w+)(\\d)$" #thanks ChatGPT

twinCor <- twinData %>%
  select(-c(RCOMP1, RCOMP2)) %>%
  .[,c(1:16)] %>%
  pivot_longer(cols = 3:16, names_to = c("Var", ".value"),
               names_pattern = "^(\\w+)(\\d)$") %>%
  rename(tw1 = "1", tw2 = "2") %>%
  group_by(Var, zyg) %>%
  summarise(Cor = cor(x = tw1, y = tw2, use = "complete")) %>%
  pivot_wider(names_from = "zyg", values_from = "Cor") %>%
  mutate(across(.cols = 1:2, function(x) as.numeric(sprintf("%.2f", round(x, digits = 2)))),
         D = MZ-2*DZ)
