#最終更新: 2023年10月20日 00:25

# 解析方針 ----
# 回答パターン、合計スコア、1回目のSSTのスコアをまとめたファイルを作成する

# dat_sst:survey_text(trial_type)のFinalRatingの合計点を求める 
# - SumSSTFinalRating(df)
#
# dat_wais_vocab:WAISの回答パターン、合計スコアをまとめる

library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)

# 1. 解析用データの作成（dat_aggregate）----

# 各種データの読み込み ----
dat_sst <- read.xlsx("../data/edited_dat/20230412_edited_dat_after_aggregate_v0.1.xlsx")
dat_wais_vocab <- read.csv("../data/edited_dat/20231019_edited_dat_vocab.csv", fileEncoding = "shift-jis")
dat_common_cwid <- read.csv("../data/edited_dat/20231019_dat_aggregate_73.csv")

# ターゲットキーのみを集計したデータフレーム(dat_aggregate)を作る ----
# クラウドワークスIDがすべてのファイルで共通している73名分のデータのID用
dat_common_cwid_agg <- dat_common_cwid %>%
  dplyr::mutate(ID = as.factor(ID)) %>%
  dplyr::select(ID)

# dat_sst:survey_text(trial_type)のFinalRatingの合計点を求める 
# - SumSSTFinalRating(df)
dat_sst_for_agg <- dat_sst %>%
  dplyr::filter(trial_type == "survey-text") %>% 
  dplyr::mutate(ID = as.factor(ID)) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(SumSSTFinalRating = sum(FinalRating))

# dat_wais_vocab
# まとめたスコア(SumWAISVocabScoreだけでなく、オリジナルの分布の情報も用意する)
#オリジナルの分布用（回答）
dat_wais_vocab_agg_answer <- dat_wais_vocab %>%
  dplyr::filter(trial_type == "survey-multi-choice") %>%
  dplyr::mutate(ID = as.factor(ID)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(SumWAISVocabScore = sum(Score),
                   Stim = stim,
                   Answer = answer) %>%
 tidyr::spread(Stim, Answer)

#オリジナルの分布用（得点）
dat_wais_vocab_agg_score <-dat_wais_vocab %>%
  dplyr::filter(trial_type == "survey-multi-choice") %>%
  dplyr::mutate(ID = as.factor(ID)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(SumWAISVocabScore = sum(Score),
                   Stim = paste(stim,"_s"),
                   Score = Score) %>%
  tidyr::spread(Stim, Score)

#オリジナルの分布の結合（回答と得点）
#分析対象となる73名分のみを抽出
dat_wais_vocab_agg_answer_and_score <- dat_wais_vocab_agg_answer %>%
  dplyr::left_join(dat_wais_vocab_agg_score) %>%
  dplyr::right_join(dat_common_cwid_agg) %>%
  dplyr::left_join(dat_sst_for_agg)

write.csv(x = dat_wais_vocab_agg_answer_and_score,
          file = "../data/for_kusumi/20231019_exp3_dat_wais_vocab_for_reliability_v0.1.csv",
          row.names = FALSE,
          fileEncoding = "shift-jis")
