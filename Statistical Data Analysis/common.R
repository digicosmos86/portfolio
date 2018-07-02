#
# last updated: 11.02.16
#
##########################

#### Common library commands

setwd("~/Documents/Udio 15-16/Descriptive_and_reliability")
options(digits = 5)

library(dplyr, quietly = TRUE)
library(dtplyr, quietly = TRUE)
library(psych, quietly = TRUE)
library(assertthat, quietly = TRUE)
library(data.table, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(ggthemes, quietly = TRUE)
library(lme4)

##### Preparing Datasets:

INPUT_DATA_NAME <- "UdioDataSet_ITEM_11.02.16.csv"
OUTPUT_DATA_SUM <- sub("ITEM", "SUM", INPUT_DATA_NAME)
OUTPUT_DATA_PREPOST <- sub("ITEM", "PREPOST", INPUT_DATA_NAME)

USAGE_DATA_NAME <- "UsageData_09.29.16.csv"

UDIO_DATASET_FULL <- fread(INPUT_DATA_NAME, data.table = TRUE) # Read the dataset without any conversion
UDIO_DATASET <- UDIO_DATASET_FULL[WITHDRAWN==0,,]              # Filter out withdrawn students
for (j in 1:14) UDIO_DATASET[, (j) := as.factor(UDIO_DATASET[[j]])]

USAGE_DATA <- fread(USAGE_DATA_NAME, data.table = FALSE)
USAGE_DATA$ID <- factor(USAGE_DATA$ID)

wc <- read.csv("word_count_1.csv", stringsAsFactors = FALSE)
wc.by.id <- wc %>% rename(ID = Subjectid) %>% group_by(ID) %>% summarise(TTS=sum(Wordcount, na.rm=TRUE)) %>%
  ungroup() %>% mutate(ID = factor(ID))
USAGE_DATA <- USAGE_DATA %>% left_join(wc.by.id)
USAGE_DATA[is.na(USAGE_DATA$TTS), "TTS"] <- 0
USAGE_DATA$TTS_LOG <- log(USAGE_DATA$TTS+1)
rm(wc, wc.by.id)

activity_counts <- read.csv("activities.csv", stringsAsFactors = F, colClasses = c("character", "character", rep("integer", 6)))[,-2]
total_events <- read.csv("total_count.csv", stringsAsFactors = F, colClasses = c("character", "integer"))
USAGE_DATA <- USAGE_DATA %>%
  left_join(activity_counts, by = c("ID" = "user")) %>%
  left_join(total_events, by = c("ID" = "user"))
USAGE_DATA <- as.data.table(USAGE_DATA)
cor(as.data.frame(USAGE_DATA)[,-1], use="pairwise.complete.obs")

levels(UDIO_DATASET$UDIO_CONDITION) = c("Library", "Supported", "Full")
UDIO_DATASET$UDIO_CONDITION = factor(UDIO_DATASET$UDIO_CONDITION, levels = c("Library", "Full", "Supported"))
UDIO_DATASET_RAW <- UDIO_DATASET[UDIO_CONDITION!="Supported",,]
UDIO_DATASET[,`:=`(PRE_WRITING_12 = 5- PRE_WRITING_12,
                    PRE_WRITING_16 = 5- PRE_WRITING_16,
                    PRE_WRITING_17 = 5- PRE_WRITING_17,
                    PRE_WRITING_19 = 5- PRE_WRITING_19,
                    POST_WRITING_12 = 5- POST_WRITING_12,
                    POST_WRITING_16 = 5- POST_WRITING_16,
                    POST_WRITING_17 = 5- POST_WRITING_17,
                    POST_WRITING_19 = 5- POST_WRITING_19), ]  ##### Reverse Coding Survey Items
UDIO_DATASET = UDIO_DATASET[UDIO_CONDITION!="Supported",,]
#summary(UDIO_DATASET)

##### Common Convenience Vectors for Selecting Columns:
COL <- colnames(UDIO_DATASET)
PRE_READING <- grep("^PRE_READING", COL, value=TRUE)
POST_READING <- grep("^POST_READING", COL, value=TRUE)
PRE_WRITING <- grep("^PRE_WRITING", COL, value=TRUE)
POST_WRITING <- grep("^POST_WRITING", COL, value=TRUE)
PRE_RAPID <- grep("^PRE_RAPID", COL, value = TRUE)
POST_RAPID <- grep("^POST_RAPID", COL, value =  TRUE)

R_ALL <- 1:19
R_AD <- c(1, 5, 7, 12, 16)
R_AP <- c(3, 6, 14, 17, 18)
R_RD <- c(4, 10, 15)
R_RP <- c(2, 8, 9, 11, 13)
R_MRQ <- 19:26

list_reading <- list(
  Reading_Overall = R_ALL, 
  Academic_Digial = R_AD,
  Academic_Print = R_AP,
  Recreational_Digital = R_RD, 
  Recreational_Print = R_RP,
  Reading_MRQ = R_MRQ)

W_ALL <- 1:9
W_AD <- c(3, 5, 7)
W_AP <- c(2, 6, 9)
W_RP <- c(1, 4, 8)
W_MRQ <- 10:20

list_writing <- list(
  Writing_Overall = W_ALL,
  Academic_Digital = W_AD,
  Academic_Print = W_AP,
  Recreational_Print = W_RP, 
  Writing_MRQ = W_MRQ)

######### Convenience functions for generating descriptives

mean_sd <- function(col) {
  sprintf("%.2f (%.2f)", mean(col, na.rm = TRUE), sd(col, na.rm = TRUE))
}

########## Work on the average survey scores ################

svy_avg_r_pre <- as.data.table(sapply(list_reading, function(x) {
  rowMeans(select(UDIO_DATASET, contains("PRE_READING"))[,x,with=FALSE], na.rm = TRUE)
}))
colnames(svy_avg_r_pre) <- paste0("PRE_R_", c("ALL", "AD", "AP", "RD", "RP", "MRQ"))

svy_avg_r_post <- as.data.table(sapply(list_reading, function(x) {
  rowMeans(select(UDIO_DATASET, contains("POST_READING"))[,x,with=FALSE], na.rm = TRUE)
}))
colnames(svy_avg_r_post) <- paste0("POST_R_", c("ALL", "AD", "AP", "RD", "RP", "MRQ"))

svy_avg_w_pre <- as.data.table(sapply(list_writing, function(x) {
  rowMeans(select(UDIO_DATASET, contains("PRE_WRITING"))[,x,with=FALSE], na.rm = TRUE)
}))
colnames(svy_avg_w_pre) <- paste0("PRE_W_", c("ALL", "AD", "AP", "RP", "MRQ"))

svy_avg_w_post <- as.data.table(sapply(list_writing, function(x) {
  rowMeans(select(UDIO_DATASET, contains("POST_WRITING"))[,x,with=FALSE], na.rm = TRUE)
}))
colnames(svy_avg_w_post) <- paste0("POST_W_", c("ALL", "AD", "AP", "RP", "MRQ"))

svy_avg <- bind_cols(svy_avg_r_pre, svy_avg_r_post, svy_avg_w_pre, svy_avg_w_post)
rm(svy_avg_r_pre, svy_avg_r_post, svy_avg_w_pre, svy_avg_w_post)

################ Work on QRI sum scores and pct scores ###################

pre_qri_p_6 <- UDIO_DATASET %>%
  select(GRADE, starts_with("PRE_QRI_S")) %>%
  filter(GRADE == "6") %>%
  select(-GRADE) %>%
  apply(2, max, na.rm = TRUE) %>%
  sum()

pre_qri_p_78 <- UDIO_DATASET %>%
  select(GRADE, starts_with("PRE_QRI_S")) %>%
  filter(GRADE != "6") %>%
  select(-GRADE) %>%
  apply(2, max, na.rm = TRUE) %>%
  sum()

pre_qri_sum <- as.data.frame(UDIO_DATASET) %>%
  select(starts_with("PRE_QRI_S")) %>%
  transmute(PRE_QRI_TOTAL = rowSums(.),
         GRADE = UDIO_DATASET[['GRADE']],
         PRE_QRI_PCT = 100*ifelse(GRADE == 6, PRE_QRI_TOTAL/ pre_qri_p_6, PRE_QRI_TOTAL / pre_qri_p_78)) %>%
  select(-GRADE)

post_qri_p_6 <- UDIO_DATASET %>%
  select(GRADE, starts_with("POST_QRI_S")) %>%
  filter(GRADE == "6") %>%
  select(-GRADE) %>%
  apply(2, max, na.rm = TRUE) %>%
  sum()

post_qri_p_78 <- UDIO_DATASET %>%
  select(GRADE, starts_with("POST_QRI_S")) %>%
  filter(GRADE != "6") %>%
  select(-GRADE) %>%
  apply(2, max, na.rm = TRUE) %>%
  sum()

post_qri_sum <- as.data.frame(UDIO_DATASET) %>%
  select(starts_with("POST_QRI_S")) %>%
  transmute(POST_QRI_TOTAL = rowSums(.),
            GRADE = UDIO_DATASET$GRADE,
            POST_QRI_PCT = 100*ifelse(GRADE == 6, POST_QRI_TOTAL/ post_qri_p_6, POST_QRI_TOTAL / post_qri_p_78)) %>%
  select(-GRADE)

qri_sum <- bind_cols(
  pre_qri_sum, post_qri_sum
)

rm(pre_qri_sum, post_qri_sum) # remove intermediate datasets

################ Combine and output the dataset  ##########################

UDIO_DATASET_ANALYTIC <- bind_cols(
  select(UDIO_DATASET, ID:UDIO_CONDITION),  ## Demographic
  svy_avg,                                  ## Summed Survey Scores
  qri_sum,                                  ## QRI Scores
  select(UDIO_DATASET, ends_with("AB"), ends_with("PROB")), ## RAPID SCORES
  select(UDIO_DATASET, PRE_WORDS_J:KEY_MATCH)               ## WIAT and Keyboarding Scores
)

rm(svy_avg, qri_sum, j)

pre_only <- select(UDIO_DATASET_ANALYTIC, ID:UDIO_CONDITION, starts_with("PRE_"), starts_with("KEY"))[,TIME := "PRE",]
post_only <- select(UDIO_DATASET_ANALYTIC, ID:UDIO_CONDITION, starts_with("POST_"))[,TIME:= "POST"]
colnames(pre_only) <- sub("PRE_", "", colnames(pre_only))
colnames(post_only) <- sub("POST_", "", colnames(post_only))

UDIO_DATASET_PREPOST <- bind_rows(pre_only, post_only)[, TIME:=factor(TIME, levels=c("PRE", "POST"))] %>% select(ID:UDIO_CONDITION, TIME, R_ALL:KEY_MATCH)
rm(pre_only, post_only)

##################################################
### Add usage log data
##################################################

UDIO_DATASET_ANALYTIC <- left_join(UDIO_DATASET_ANALYTIC, USAGE_DATA, by="ID")
UDIO_DATASET_PREPOST <- left_join(UDIO_DATASET_PREPOST, USAGE_DATA, by="ID")

##################################################
### Center and normalize data
##################################################

gm_centered <- as.data.frame(UDIO_DATASET_ANALYTIC) %>%
  select(PRE_R_ALL:etotal) %>%
  mutate_all(funs(C = scale(., center = TRUE, scale = FALSE), N = scale(.))) %>%
  select(-PRE_R_ALL:-etotal)

UDIO_DATASET_C <- bind_cols(UDIO_DATASET_ANALYTIC, gm_centered)

rm(gm_centered)

##################################################
### Get ICC from an lmer model
##################################################

ICC <- function (m) {
  # takes an unconditional model and returns the ICC of the model
  vc.vec <- as.data.frame(VarCorr(m))$vcov
  vc.vec[1] / sum(vc.vec)
}

varComp <- function (lstModels, ncol=6) {
  df <- lapply(lstModels, function (x){
    vc.vec <- as.data.frame(VarCorr(x))$vcov
    format(vc.vec, digits = 3, nsmall = 2)
  }) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(df) <- as.character(1:ncol(df))
  df$name <- c("Intercept (ClassID)", "Residual")
  df <- t(df[, c(ncol+1, 1:ncol)])
  list("\\textbf{Random Effects}", df[, 1], df[, 2], "")
}

# write.csv(UDIO_DATASET_PREPOST, OUTPUT_DATA_PREPOST, row.names = FALSE, na = "")
# write.csv(UDIO_DATASET_ANALYTIC, OUTPUT_DATA_SUM, row.names = FALSE, na = "")

projects <- fread("new-project-report.csv")
projects[, ID := as.character(ID),]
projects <- as.data.frame(projects)
UDIO_DATASET_C <- as.data.frame(UDIO_DATASET_C)

UDIO_DATASET_C$PRE_WIAT_COMP <- (UDIO_DATASET_C$PRE_WORDS_SS + UDIO_DATASET_C$PRE_WIAT_SS) / 2
UDIO_DATASET_C$POST_WIAT_COMP <- (UDIO_DATASET_C$POST_WORDS_SS + UDIO_DATASET_C$POST_WIAT_SS) / 2
UDIO_DATASET_C$PRE_WIAT_COMP_C <- as.vector(scale(UDIO_DATASET_C$PRE_WIAT_COMP, scale = FALSE))
UDIO_DATASET_C$POST_WIAT_COMP_C <- as.vector(scale(UDIO_DATASET_C$POST_WIAT_COMP, scale = FALSE))

projects_create <- read.csv("project_create.csv")
projects_create$ID <- as.character(projects_create$ID)

UDIO_DATASET_C <- left_join(UDIO_DATASET_C, ld, by = "ID")
UDIO_DATASET_C <- left_join(UDIO_DATASET_C, projects, by = "ID")
UDIO_DATASET_C <- left_join(UDIO_DATASET_C, projects_create, by = "ID")
UDIO_DATASET_F <- filter(UDIO_DATASET_C, UDIO_CONDITION == "Full")
saveRDS(UDIO_DATASET_C, "dataset1.rds")
saveRDS(UDIO_DATASET_F, "dataset2.rds")