##### 0. 前準備 ========================================================
# library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plm)
library(texreg)
library(broom)

load("~/Data/JAHEAD/Process_Files/data_after_14.rda")

theme_set(theme_bw(base_size = 18, base_family = "HiraKakuProN-W3") +
              theme(axis.title.x=element_blank(),
                    axis.text=element_text(colour="black")))
##### 2. 試しに普通のOLSで推定 ======================================
result1 <- data_long %>% 
    filter(lim_adl > 0) %>% 
    lm(exist_helper_adl_l ~ t_gender + t_age + lim_adl + use_dayservice_n, data=.)
screenreg(result1)

##### 固定効果モデルで分析 =========================================
data_panel <- pdata.frame(data_long, index=c("id", "wave"))

# ADLについて固定効果モデルで推定
result_2 <- plm(exist_helper_adl_l ~ use_dayservice_n + lim_adl +  num_hh_member,
                data=data_panel, index=c("id", "wave"), model="within")
screenreg(result_2)

# 作図用のデータを準備
result <- broom::tidy(result_2, conf.int = TRUE) %>%
    # 並べ替え用の数字
    mutate(num = row_number()) %>% 
    # 変数ごとに日本語のラベルを作成
    mutate(term_j = recode(term,
                           `lim_adl` = "ADLの制約度",
                           `use_dayservice_n` = "デイサービスの利用頻度",
                           `num_hh_member` = "世帯人数"
    ))

# キャタピラープロットの作図
p <- ggplot(result) +
    geom_pointrange(aes(x=reorder(term_j, -num), y=estimate,
                        ymax=conf.high, ymin=conf.low),
                    size=1) +
    geom_hline(aes(yintercept = 0), size = .5, linetype = "dashed") +
    coord_flip() +
    labs(x="回帰係数") +
    theme(axis.title.y=element_blank()) #縦軸のラベルを削除
print(p)

quartz(file="~/Downloads/fig_result_within_model_adl.pdf", type="pdf", width=6,height=3)
p
dev.off()


# IADLについての分析
result_3 <- plm(exist_helper_iadl_l ~ use_homehelp_n + lim_iadl + num_hh_member,
                data=data_panel, index=c("id", "wave"), model="within")
screenreg(result_3)

result <- tidy(result_3, conf.int = TRUE) %>%
    #filter(term!="(Intercept)") %>%
    mutate(num = row_number()) %>% 
    mutate(term_j = recode(term,
                           `lim_iadl` = "IADLの制約度",
                           `use_homehelp_n` = "ホームヘルプの利用頻度",
                           `num_hh_member` = "世帯人数"
                           ))

p <- ggplot(result) +
    geom_pointrange(aes(x=reorder(term_j, -num), y=estimate,
                        ymax=conf.high, ymin=conf.low),
                        size=1) +
    geom_hline(aes(yintercept = 0), size = .5, linetype = "dashed") +
    coord_flip() +
    labs(x="回帰係数") +
    theme(axis.title.y=element_blank()) #縦軸のラベルを削除
print(p)

quartz(file="~/Downloads/fig_result_within_model_iadl.pdf", type="pdf", width=6,height=3)
p
dev.off()