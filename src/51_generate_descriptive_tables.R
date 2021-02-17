##### 0. 前準備 ========================================================
library(tidyverse)
library(xtable)

# データのファイルパス。各自のパソコンのディレクトリ構造に合わせて書き替える。
DATA_PATH <- "~/Data/JAHEAD/Process_Files/"
# 分析結果を保存するファイルパス。
OUTPUT_PATH <- "~/Data/JAHEAD/Results"

# これまでのデータの読み込み
load(str_c(DATA_PATH, "data_after_41_data_frame.rda"))
load(str_c(DATA_PATH, "data_after_14.rda")) #ADLケアとIADLケアの被提供をみるため

##### 1. 記述統計を出力 ========================================================

# 関数を定義
makeDiscriptiveTable <- function(data){
    # このデータは、集計する変数のみで構成すること
    data %>% 
        gather(key=key, value=value) %>% 
        # selectで取り出してきた順番で変数が並ぶように調整
        mutate(num = row_number()) %>% 
        group_by(key) %>% 
        summarise(N = n(),
                  Mean = mean(value),
                  SD = sd(value),
                  Min = min(value),
                  Max = max(value),
                  num = min(num)) %>%
        arrange(num) %>% 
        select(-num)
}

# それぞれの変数に対してテーブルを作成
data_i_level <-  data_complete_cases %>% 
    mutate(care_1 = if_else(careing_numeric == 1, 1, 0),
           care_2 = if_else(careing_numeric == 2, 1, 0),
           care_3 = if_else(careing_numeric == 3, 1, 0)) %>% 
    select(care_1, care_2, care_3, 
           is_chounan, is_other_son, is_daughter, is_chounan_yome, is_other_yome, is_daughters_husband,
           ch_dist_living_l) %>% 
    rename(援助なし = care_1,
               身体的介護を提供 = care_2,
               家事的援助のみ提供 = care_3,
           `続柄(長男)` = is_chounan,
           `続柄(長男以外の息子)` = is_other_son,
           `続柄(娘)` = is_daughter,
           `続柄(長男の妻)` = is_chounan_yome,
           `続柄(長男以外の妻)` = is_other_yome,
           `続柄(娘の夫)` = is_daughters_husband,
           居住場所の距離 = ch_dist_living_l
           )

tab_i_level_total <- data_i_level %>% 
    makeDiscriptiveTable(.)

tab_i_level_male <- data_i_level %>% 
    filter(`続柄(長男)`==1 | `続柄(長男以外の息子)` == 1 | `続柄(娘の夫)` == 1) %>% 
    makeDiscriptiveTable(.)

tab_i_level_female <- data_i_level %>% 
  filter(`続柄(娘)`==1 | `続柄(長男の妻)` == 1 | `続柄(長男以外の妻)` == 1) %>% 
  makeDiscriptiveTable(.)

data_p_level <- data_complete_cases %>% 
    distinct(id_personyear, .keep_all = TRUE) %>% 
    select(lim_functions, living_spouse, use_publicservices_d, t_female, t_age) %>% 
    rename(親の生活動作の困難度 = lim_functions,
           `親が配偶者と同居(同居=1)` = living_spouse,
           `親が公的サービスを利用(利用=1)` = use_publicservices_d,
           `親の性別(女性=1)` = t_female
           )

tab_p_level_total <- data_p_level %>% 
    select(-t_age) %>% 
    makeDiscriptiveTable(.)

##### 2. 前準備 ========================================================
# LaTeX用の出力(提出した原稿では、次のステップのCSVを利用)
discriptive_table <- tab_i_level_total %>% 
    bind_rows(tab_p_level_total) %>% 
    rename(`　` = key) %>% #変数名のところの行名は空白に。
    xtable(., label="tab_discriptive_table", caption = "分析対象の記述統計", digits = c(0,0,0,2,2,0,0))

print(discriptive_table, file=str_c(OUTOUT_PATH, "descriptive_statistics.tex"),
      caption.placement = "top", table.placement = "htbp", include.rownames = FALSE,
      # 二段組でプリントするので、\begin{table*}と*で段組をエスケースしたい。floating.environmentで環境は好きに指定できるらしい。
      floating.environment = "table*")

# 提出する原稿に用いたCSV
write.csv(discriptive_table, file = str_c(OUTOUT_PATH, "descriptive_statistics.csv"), fileEncoding = "CP932")

##### 3. 年齢の記載および、性別ごとの集計 =======================================

data_p_level <- data_complete_cases %>% 
  distinct(id_personyear, .keep_all = TRUE) %>% 
  select(lim_functions, living_spouse, use_publicservices_d, t_female) %>% 
  rename(親の生活動作の困難度 = lim_functions,
                   `親が配偶者と同居(同居=1)` = living_spouse,
                   `親が公的サービスを利用(利用=1)` = use_publicservices_d,
                   `親の性別(女性=1)` = t_female
  )


tab_p_level_male <- data_p_level %>% 
  filter(`親の性別(女性=1)` == 0) %>% 
  makeDiscriptiveTable(.)

tab_p_level_female <- data_p_level %>% 
  filter(`親の性別(女性=1)` == 1) %>% 
  makeDiscriptiveTable(.)

discriptive_table_parent_gender <- tab_p_level_female %>% 
  left_join(tab_p_level_male, by="key")

# 提出する原稿にはCSVを貼り付ける
write.csv(discriptive_table_parent_gender, file = str_c(OUTOUT_PATH, "descriptive_statistics_parent_gender.csv"), fileEncoding = "CP932")


# 年齢の記述統計(報告は注において報告するため、ターミナルで出力)

# 集計対象となる子のIDを抽出
tmp_child_id <- data_complete_cases %>% 
  filter(is_child == "子ども") %>% 
  .$id_personyear_child_d

# 子の年齢について集計
load(str_c(DATA_PATH, "data_after_23.rda"))

sample_child <- data_united %>% 
  filter(id_personyear_child_d %in% tmp_child_id)

summary(sample_child$ch_age)
sd(sample_child$ch_age, na.rm = TRUE)

# 親の年齢について集計
sample_parent <- data_complete_cases %>% 
  distinct(id_personyear, .keep_all = TRUE)

summary(sample_parent$t_age)
sd(sample_parent$t_age)
