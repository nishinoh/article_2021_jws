##### 0. 前準備 ========================================================
# library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(tibble)
library(readr)
library(foreign)

##### 1. データの読み込み =======================================
# 各Waveごとにデータを読み込む
# エンコーディングの都合上havenでなくforeignで

# データの置き場所
ORIGINAL_DATA_PATH <- "~/Data/JAHEAD/Original_Data/"
DATA_PATH <- "~/Data/JAHEAD/Process_Files/"

# 読み込み時に利用する関数
readDataFile <- function(file){
    read.spss(file, to.data.frame = TRUE, reencode = "CP932") %>% 
        as.tibble(.)
}

org_jahead_w1to3 <- readDataFile(str_c(ORIGINAL_DATA_PATH, "0395/0395.sav"))
org_jahead_w4 <- readDataFile(str_c(ORIGINAL_DATA_PATH, "0679/0679.sav"))
org_jahead_w5to6 <- readDataFile(str_c(ORIGINAL_DATA_PATH, "0823/0823.sav"))
org_jahead_w7 <- readDataFile(str_c(ORIGINAL_DATA_PATH, "1185/1185.sav"))


##### 2. CSVファイルにリストを作成し、それを読み込んで共通変数名をつける =================
# あらかじめ作成した変数名の対応表一覧を利用する
var_table <- read_csv("src/00_variable_names.csv")

selectVariables <- function(data, var_table, module){
    # moduleで入力した変数名とcommon_nameをペアにしておく    
    var_pair <- c("common_name", module)
    
    # これからつける共通変数名と、元の変数名のペアのDFを作成
    var_names <- var_table %>%
        # 仮にvar_pair という列名があるとそちらが選ばれるので、安全策として「!!」でunquote
        select(!! var_pair) %>%
        filter(!is.na(.[[module]]))
    
    # データから該当する列を取り出し、名前を共通の変数名に変更
    out <- data %>%
        select(var_names[[module]])
    names(out) <- var_names$common_name
    
    return(out)
}


##### 3. 作成した関数を使って個別のデータを作る =========================
# ループで読み込んでオブジェクトを作った後、
# recovery(jxx0004系の変数)を使って、該当しないケースを落とす。

# さらにfor文を書くのが面倒だったので、ファイルごとにループ
modules_w5to6 <- c("wave5_main", "wave5_proxy", "wave5_miss",
             "wave6_main", "wave6_proxy", "wave6_miss")

for(module in modules_w5to6){
    data <- selectVariables(org_jahead_w5to6, var_table, module=module) %>% 
        filter(!is.na(recovery)) %>% 
        mutate(module = module,
               wave = parse_number(module))
    assign(str_c("data_", module), data)
}

modules_w7 <- c("wave7_main", "wave7_proxy", "wave7_miss")
for(module in modules_w7){
    data <- selectVariables(org_jahead_w7, var_table, module=module) %>% 
        filter(!is.na(recovery)) %>% 
        mutate(module = module,
               wave = parse_number(module))
    assign(str_c("data_", module), data)
}

# 補足: 本調査票・代行調査票・欠票の識別はjxx004系の変数で大丈夫という確認
# org_jahead_w5to6 %>% 
#     count(j5v004, j5p004, j5n004)

##### 4. Waveを縦に結合してlongデータを作成 =================================
# ひたすら縦に接続。調査票ごとに結合してから、全体で結合する。

data_main_ques <- data_wave5_main %>% 
    bind_rows(data_wave6_main, data_wave7_main) %>% 
    mutate(ques_type = "本調査票")

data_proxy_ques <- data_wave5_proxy %>% 
    bind_rows(data_wave6_proxy, data_wave7_proxy) %>% 
    mutate(ques_type = "代行票")

data_missing_ques <- data_wave5_miss %>% 
    bind_rows(data_wave6_miss, data_wave7_miss) %>% 
    mutate(ques_type = "欠票")

# 全て結合したデータを作成
data_joint_allwaves <- data_main_ques %>% 
    bind_rows(data_proxy_ques, data_missing_ques) %>% 
    arrange(id) %>% 
    select(id, wave, ques_type, everything()) #waveと調査票だけ順番を先に持ってくるよう順序を入れ替え

##### Fin. 作成したファイルを保存 ================================================
# 作成したファイルを保存し、これまで作ったオブジェクトはいったん全て削除

save(data_joint_allwaves, file=str_c(DATA_PATH, "data_after_01.rda"))
rm(list = ls())
