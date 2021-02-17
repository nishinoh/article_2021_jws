##### 0. 前準備 ========================================================
# library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(tibble)
library(readr)

# データのファイルパス。各自のパソコンのディレクトリ構造に合わせて書き替える。
DATA_PATH <- "~/Data/JAHEAD/Process_Files/"

# これまでのデータの読み込み
load(str_c(DATA_PATH, "data_after_01.rda"))
data_long <- data_joint_allwaves

# 今回扱う変数名の一覧
items_adl <- c(str_c(rep("adl_sq",6), 1:6))
items_iadl <- c(str_c(rep("iadl_sq",4), 1:4))
items_ability <- c(str_c(rep("ability_sq", 7), 1:7))
# あとで作成するダミー変数の一覧
newitems_adl <- str_c(items_adl, "_b")
newitems_iadl <- str_c(items_iadl, "_b")
newitems_ability <- str_c(items_ability, "_b")

##### 0.1 共通で使うような変数を新たに作成 ==============================
data_long <- data_long %>% 
    mutate(id_text = str_pad(id, width=8, pad="0")) %>% 
    mutate(id_personyear = str_c(id_text, "-W", wave)) %>% 
    select(id, id_text, wave, id_personyear, everything())

##### 1. ADL制約などの変数の水準を揃える =================================
# Waveごとにfactorの水準(レベルの表現)が異なっているので、揃える

# 関数を作成
refactorFunctionalLimitation <- function(data, variable){
    # ファクターの水準がW5,6とW7で違うので、手動で合わせる。
    # あとでunclassして5段階の得点にすることもありうるので、factor型にしておく
    data <- data %>% 
        # !!()と、括弧で囲う方が安全
        mutate(!!(variable) := fct_collapse(.[[variable]],
                                            ぜんぜん難しくない = "ぜんぜん難しくない",
                                            すこし難しい = "すこし難しい",
                                            かなり難しい = "かなり難しい",
                                            非常に難しい = "非常に難しい",
                                            まったくできない = c("まったくできない", "まったく出来ない"),
                                            DKNA = c("わからない", "DK"))) %>% 
        mutate(!!(variable) := fct_relevel(.[[variable]], c("ぜんぜん難しくない", "すこし難しい", "かなり難しい",
                                                            "非常に難しい", "まったくできない", "DKNA"))) %>% 
        mutate(!!(variable) := recode(.[[variable]], `DKNA` = NA_character_))
}

# 上の関数を使って、Functional Limitationの変数を数値型に変えていく
for(item in items_adl){
    data_long <- refactorFunctionalLimitation(data_long, item)
}

for(item in items_iadl){
    data_long <- refactorFunctionalLimitation(data_long, item)
}

# 身体能力のカテゴリ再編のための関数。ADLやIADLと少し項目が違うので別で作成
refactorPhysicalAbility <- function(data, variable){
    # ファクターの水準がW5,6とW7で違うので、手動で合わせる。
    data <- data %>% 
        # !!()と、括弧で囲う方が安全
        mutate(!!(variable) := fct_collapse(.[[variable]],
                                            ぜんぜん難しくない = "ぜんぜん難しくない",
                                            すこし難しい = "すこし難しい",
                                            # これがADLの項目と言葉が違う
                                            かなり難しい = "とても難しい",
                                            まったくできない = "まったくできない",
                                            DKNA = c("わからない", "DK"))) %>% 
        mutate(!!(variable) := fct_relevel(.[[variable]], c("ぜんぜん難しくない", "すこし難しい", "かなり難しい",
                                                            "まったくできない", "DKNA"))) %>% 
        mutate(!!(variable) := recode(.[[variable]], `DKNA` = NA_character_))
}

for(item in items_ability){
    data_long <- refactorPhysicalAbility(data_long, item)
}

##### 2. 行動の難しさをダミー変数へ ================================
# ADLの難しさを、ダミー変数に変換する関数
recodeFunctionalLimitation <- function(data, new_varname, variable){
    data <- data %>% 
        # あとで基準を見直しやすいようcase_when()で指定
        mutate(!!(new_varname) := case_when(.[[variable]] == "ぜんぜん難しくない" ~ 0,
                                            .[[variable]] == "すこし難しい" ~ 1,
                                            .[[variable]] == "かなり難しい" ~ 1,
                                            .[[variable]] == "非常に難しい" ~ 1,
                                            .[[variable]] == "まったくできない" ~ 1))
}

for(item in items_adl){
    new_name <- str_c(item, "_b")
    data_long <- recodeFunctionalLimitation(data_long, new_name, item)
}

for(item in items_iadl){
    new_name <- str_c(item, "_b")
    data_long <- recodeFunctionalLimitation(data_long, new_name, item)
}

for(item in items_ability){
    new_name <- str_c(item, "_b")
    data_long <- recodeFunctionalLimitation(data_long, new_name, item)
}

##### 3. ADLの制約度の合計点を作成 =====================================
# ダミー変数の合計点
data_long <- data_long %>% 
    mutate(lim_adl = select(., one_of(newitems_adl)) %>% 
                                rowSums(na.rm = TRUE),
           lim_iadl = select(., one_of(newitems_iadl)) %>% 
                                rowSums(na.rm = TRUE),
           lim_ability = select(., one_of(newitems_ability)) %>% 
                                rowSums(na.rm = TRUE))

##### 4. ニードの重さの区分 =====================================
# なお、ADL、IADLのどちらもニードなしの場合、介護者の存在は質問されない

data_long <- data_long %>% 
    mutate(needs_type = case_when(lim_adl >= 1 & lim_iadl >= 1 ~ "どちらも必要",
                                  lim_adl >= 1 & lim_iadl == 0 ~ "ADLだけ必要",
                                  lim_adl == 0 & lim_iadl >= 1 ~ "IADLだけ必要",
                                  lim_adl == 0 & lim_iadl == 0 ~ "どちらも不要"))

##### Fin. 作成したファイルを保存 ================================================
# 作成したファイルを保存し、これまで作ったオブジェクトはいったん全て削除
save(data_long, file=str_c(DATA_PATH, "data_after_11.rda"))
rm(list = ls())
