#最終更新: 2023年6月7日 15:13
# ライブラリのよみこみ ----
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(stringr)
library(irr)
library(openxlsx)

# データの準備 ----

#参加者のデータの読み込み
raw_dat_expa <- read.xlsx("../data/EXPA_20230412_edited_dat_after_aggregate_v0.1.xlsx")
raw_dat_expb <- read.xlsx("../data/EXPB_20230516_edited_dat_aggregated_v0.1.xlsx")

#必要なデータのみを抽出
# - order
# - ★trial_index
# - ★cwid
# - ★task
# - ★word_pair
# - ★answer
# - FinalRating(expa)
# - FR(expb)

dat_expa <- raw_dat_expa %>%
  dplyr::mutate(FR_A = FinalRating,
                order_A = order,
                answer_A = answer) %>%
  dplyr::select(order_A, trial_index, ID, task, word_pair, answer_A, FR_A)

dat_expb <- raw_dat_expb %>%
  dplyr::mutate(FR_B = FR,
                order_B = order,
                answer_B = answer) %>%
  dplyr::select(order_B, trial_index, ID, task, word_pair, answer_B, FR_B)

#expbにいるexpaのユーザだけを抽出する
#length(unique(dat_expb$cwid))#80名→filter後は,77名しか抽出できなかった
#length(unique(dat_expa$cwid))#100名
target_ids <- unique(dat_expb$ID)
dat_expa <- dat_expa %>%
  dplyr::filter(ID %in% target_ids)
#length(unique(dat_expa$cwid))#100名

#dat_expa/bを結合
# ref: https://www.statology.org/dplyr-join-on-multiple-columns/
dat_expa_expb <- dat_expa %>%
  dplyr::left_join(dat_expb) %>%
  dplyr::filter(task == "sst_answer")

# 分類一致率の計算（項目レベル） ----
# クロス表の作成
cross_tab_dat <- dat_expa_expb %>% group_by(FR_A, FR_B) %>%
  tally %>%
  spread(FR_A, n)

#分類一致率の算出（カテゴリ; kappa）
dat_concat_kappa <- dat_expa_expb[,c("FR_A", "FR_B")]
kappa2(dat_concat_kappa, weight = TRUE)
irr::agree(dat_concat_kappa)
irr::kappam.fleiss(dat_concat_kappa, detail = TRUE)

#分類一致率の算出（連続量; ICC）
dat_calc_reliablity_icc <- dat_expa_expb[,c("FR_A", "FR_B")]
irr::icc(dat_calc_reliablity_icc, model="twoway", type="agreement", unit="average")

# 分類一致率の計算（参加者レベル） ----
# 野崎・子安 (2015)の情動コンピテンス尺度短縮版の信頼性係数の算出に関する研究を参考に
# 参加者ごとのSST得点を求めて、合計点を求めて、相関係数を算出する
# -> r = .62で、まずまずか
dat_expa_expb_part <- dat_expa_expb %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(FR_A_sum = sum(FR_A),
                   FR_B_sum = sum(FR_B))
cor.test(dat_expa_expb_part$FR_A_sum, dat_expa_expb_part$FR_B_sum)
