#最終更新: 2023年10月19日 10:50

# 解析方針 ----
# 1. 解析用データの作成（dat_aggregate）：CrowdworksIDを行に持ち、ターゲットキー（後述）を列に持つ
# 共通のキー: CrowdworksID(demographic), cwid(rpms, sst, vocab_and_tipij)

# dat_demographic:
# - Sex
# - Age
# - AcademicDegree
#
# dat_sst:survey_text(trial_type)のFinalRatingの合計点を求める 
# - SumSSTFinalRating(df)
#
# dat_rpms: survey_likert(trial_type)のCrrectの合計点を求める
# - SumRPMSCorrect(df)
#
# dat_vpcab_and_tipij
# - Vocab_Check_All
# - Vocab_Check_P1
# - Vocab_Check_P2
# - Vocab_Check_P3
# - Extraversion_Agg
# - Agreeableness_Agg
# - AConscientious_Agg
# - Neuroticism_Agg
# - Openess_Agg
#
# 2. 相関分析

library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)

# 1. 解析用データの作成（dat_aggregate）----

# 各種データの読み込み ----
dat_demographic <- read.xlsx("../data/edited_dat/20230313_dat_target_start_to_demographic.xlsx")
dat_rpms <- read.csv("../data/edited_dat/20230313_dat_target_rpms_with_score.csv")
dat_sst <- read.xlsx("../data/edited_dat/20230412_edited_dat_after_aggregate_v0.1.xlsx")
dat_vacob_and_tipij <- read.xlsx("../data/edited_dat/20230313_dat_target_vocab_to_finish.xlsx")

#追記: 20231019
dat_wais_vocab <- read.csv("../data/edited_dat/20231019_edited_dat_vocab.csv", fileEncoding = "shift-jis")
dat_bfs <- read.csv("../data/edited_dat/20231019_edited_dat_bfs.csv", fileEncoding = "shift-jis")

# ターゲットキーのみを集計したデータフレーム(dat_aggregate)を作る ----
# dat_demographic:
# - Sex
# - Age
# - AcademicDegree
dat_demographic_for_agg <- dat_demographic %>%
  dplyr::select(CrowdworksID, Sex, Age, AcademicDegree) %>%
  dplyr::mutate(CrowdworksID = as.factor(CrowdworksID))

# dat_sst:survey_text(trial_type)のFinalRatingの合計点を求める 
# - SumSSTFinalRating(df)
dat_sst_for_agg <- dat_sst %>%
  dplyr::filter(trial_type == "survey-text") %>% 
  dplyr::mutate(CrowdworksID = as.factor(cwid)) %>%
  dplyr::group_by(CrowdworksID) %>%
  dplyr::summarise(SumSSTFinalRating = sum(FinalRating))

# dat_rpms: survey_likert(trial_type)のCrrectの合計点を求める
# - SumRPMSCorrect(df)
dat_rpms_for_agg <- dat_rpms %>% 
  dplyr::filter(trial_type == "survey-likert") %>%
  dplyr::mutate(CrowdworksID = as.factor(cwid)) %>% 
  dplyr::group_by(CrowdworksID) %>%
  dplyr::summarise(SumRPSMCorrect = sum(Correct))

# dat_vpcab_and_tipij
# - Vocab_Check_All
# - Vocab_Check_P1
# - Vocab_Check_P2
# - Vocab_Check_P3
# - Extraversion_Agg
# - Agreeableness_Agg
# - AConscientious_Agg
# - Neuroticism_Agg
# - Openess_Agg
dat_vocab_and_tipij_for_agg <- dat_vacob_and_tipij %>%
  dplyr::mutate(CrowdworksID = as.factor(cwid)) %>% 
  dplyr::select(CrowdworksID, Vocab_Check_All, Vocab_Check_P1, Vocab_Check_P2, Vocab_Check_P3,
                Extraversion_Agg, Agreeableness_Agg, Conscientious_Agg, Neuroticism_Agg, Openess_Agg)

#追記: 20231019
# dat_wais_vocab
dat_wais_vocab_for_agg <- dat_wais_vocab %>%
  dplyr::filter(trial_type == "survey-multi-choice") %>%
  dplyr::mutate(CrowdworksID = as.factor(cwid)) %>% 
  dplyr::group_by(CrowdworksID) %>%
  dplyr::summarise(SumWAISVocabScore = sum(Score))

#追記: 20231019
# dat_bfs
dat_bfs_for_agg <- dat_bfs %>%
  dplyr::mutate(CrowdworksID = as.factor(cwid)) %>%
  dplyr::select(CrowdworksID, Extraversion, Conscientiousness, Neuroticism, Openness, Agreeableness)

# 解析用データの作成（dat_aggregate）
dat_aggregate <- dat_demographic_for_agg %>%
  dplyr::inner_join(dat_sst_for_agg) %>%
  dplyr::inner_join(dat_rpms_for_agg) %>%
  dplyr::inner_join(dat_vocab_and_tipij_for_agg) %>% #以下、20231019追記
  dplyr::inner_join(dat_wais_vocab_for_agg) %>%
  dplyr::inner_join(dat_bfs_for_agg)

#追記: 2023/10/30
#ref: https://qiita.com/swathci/items/d13be09a7ed73c1a8d94
dat_aggregate_sex <- dat_aggregate %>%
  dplyr::group_by(Sex) %>%
  tally()

psych::describe(dat_aggregate$Age)

#追記: 20231020 00:14
write.csv(file = "../data/edited_dat/20231019_dat_aggregate_73.csv",
          x = dat_aggregate,
          row.names = FALSE)

# 2. 相関分析----
source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp") #mycorを持ってくる

# #以下、オリジナルExpAの分析時
# dat_aggregate_cor <- dplyr::select(dat_aggregate, -CrowdworksID) %>%
#   dplyr::mutate(Education = AcademicDegree) %>%
#   dplyr::select(-AcademicDegree) %>%
#   dplyr::select(SumSSTFinalRating, SumRPSMCorrect, Vocab_Check_All, Vocab_Check_P1,
#                 Extraversion_Agg, Agreeableness_Agg, Conscientious_Agg, Neuroticism_Agg, Openess_Agg,
#                 Age, Education)
# mycor(1:11, dat_aggregate_cor, latex = FALSE) #いつもの#相関係数ようのデータ

#以下、実験Cの変数（SumWAISVocabScore, Big Five）とSumSSTFinalRatingの相関係数
#-> SumWAISVocabScoreとSumSSTFinalRatingは.36
dat_aggregate_cor_expc <- dplyr::select(dat_aggregate, -CrowdworksID) %>%
  dplyr::mutate(Education = AcademicDegree) %>%
  dplyr::select(-AcademicDegree) %>%
  dplyr::select(SumSSTFinalRating, SumRPSMCorrect, SumWAISVocabScore, Vocab_Check_All, Vocab_Check_P1, 
                Extraversion, Agreeableness, Conscientiousness, Neuroticism, Openness,
                Age, Education)
mycor(1:12, dat_aggregate_cor_expc, latex = FALSE) #いつもの#相関係数ようのデータ

# 3. 相関表とヒストグラムの可視化 ----
dat_aggregate_panels_expc <- dat_aggregate_cor_expc %>%
  dplyr::select(-Age, -Education, -Extraversion, -Agreeableness, -Conscientiousness, -Neuroticism, -Openness,
                -SumRPSMCorrect, -Vocab_Check_All, -Vocab_Check_P1) %>%
  dplyr::mutate(SumSSTFinalRating = as.integer(SumSSTFinalRating))
psych::pairs.panels(dat_aggregate_panels_expc)

# 4. 記述統計量 ----
#記述統計量
describe(dat_aggregate_cor_expc)
