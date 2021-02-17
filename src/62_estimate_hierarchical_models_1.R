#### 0. 前準備 ========================================================
# library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(texreg)
library(broom)

load("~/Data/JAHEAD/Process_Files/data_after_22.rda")

theme_set(theme_bw(base_size = 18, base_family = "HiraKakuProN-W3") +
              theme(axis.title.x=element_blank(),
                    axis.text=element_text(colour="black")))

##### 1. データの絞り込み ====================================
data <- data_united %>% 
    filter(!is.na(do_care_parents))

##### 1. =====================================================
ans_ml_care <- glmer(do_care_parents ~ ch_sex + ch_age 
                     + lim_adl
                     + (1 | id_text),
                     data=data,
                     family=binomial(link = "logit")
)
summary(ans_ml_care)
# 収束せず