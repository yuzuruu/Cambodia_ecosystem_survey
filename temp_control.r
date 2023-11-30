#################################################################
# 冷凍フグを運ぶ温度に気をつけましょう
# 2023年4月12日
# 宇都宮　譲（長崎大学経済学部）
library(tidyverse)
library(vroom)
library(khroma)
library(fable)
# 
# ログデータ収集
# 2024年3月以降にロガーをたくさん回収した場合に備えて、
# 複数ログを読み込む設定になってます。
# 発泡内部温度だけならロガーは2つしかないですよ。
# make a list of generated csv files
target_file_list <-
  dir(
    path = "logger",
    pattern = "*.csv"
  )
# ログデータ結合
# 参考
# https://qiita.com/Ringa_hyj/items/434e3a6794bb7ed8ee14
logger_data_combined <-
  vroom::vroom(
    paste0(
      "logger/",
      target_file_list,
      sep = ""
    ),
    id = "filename"
  ) %>%
  dplyr::mutate(
    logger_id = factor(stringr::str_sub(filename, start = 8, end = 9)),
    time = lubridate::ymd_hm(time, tz = "Asia/Bangkok")
  ) %>% 
  dplyr::select(id, logger_id, time, temp)
# 
# 必要な期間を指定するオブジェクトを作成
# UKTを出発して長崎大学に着いた頃まで。
# 時間がバンコク時間であるから注意。
target_interval <- 
  interval(
    start = lubridate::ymd_hms(
      "2023-03-19 10:00:00", 
      tz = "Asia/Bangkok"
      ),
    end = lubridate::ymd_hms(
      "2023-03-20 10:50:00",
      tz = "Asia/Bangkok"
      )
    )
# ログデータから必要な箇所を取り出しながら
# ロガー名称を与える
logger_data_arranged <- 
  logger_data_combined %>% 
  # ロガー21番は測定にしくじったみたい。
  # ゆえに除外する。
  dplyr::filter(
    logger_id != "21" &
    time %within% target_interval) %>% 
  dplyr::mutate(
    logger_id = dplyr::case_when(
      logger_id == "20" ~ "inside",
      logger_id == "13" ~ "outside",
      TRUE ~ "hoge"
    )
  )
# 
# 荷物を預けている時間を塗り分ける区間を設定する
# 預けている時間に変えた。
rects <- 
  data.frame(
    xstart = as.POSIXct(
      c("2023-03-19 20:55:00"), 
      tz = "Asia/Bangkok"
      ),
    xend = as.POSIXct(
      c("2023-03-20 06:00:00"), 
      tz="Asia/Bangkok"
      )
  )
# 原系列折れ線グラフ
line_logger_data <- 
  ggplot(
    data = logger_data_arranged,
    aes(
      x = time,
      y = temp,
      colour = logger_id
    )
  ) +
  geom_line() +
  geom_rect(
    data = rects, 
    aes(xmin = floor_date(xstart, "second"), xmax = xend, ymin = -Inf, ymax = Inf),
    fill="grey",
    alpha = 0.2, 
    inherit.aes = FALSE
    ) + 
  # geom_point() +
  scale_color_okabeito() +
  scale_x_datetime(date_labels = "%d %b %H:%M", date_breaks = "8 hours") +
  labs(
    title = "Temperature inside / outside of cool box",
    subtitle = "Yellow: outside; Black: inside; Grey: Period when we dropped the baggages.",
    x = "Time (Unit: 1 min)",
    y = "Temperature (Unit: Temperature (°C))",
    color = "Loggers' position"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  ) 
# 保存 
ggsave(
  "line_logger_data.pdf",
  plot = line_logger_data,
  height = 150, 
  width = 150,
  units = "mm"
)
# 
# 分析用データ作成
# MCMC収束を容易にするため、系列毎に標準偏差で割った
# 系列をつくる


logger_data_arranged_longer <- 
  logger_data_arranged %>% 
  tidyr::pivot_wider(
    names_from = logger_id,
    values_from = temp
  ) %>% 
  # ここです
  dplyr::mutate(
    outside_scale = outside/sd(outside), # 1.77
    inside_scale = inside/sd(inside), # 4.90
    outside_log = log(outside+10),
    inside_log = log(inside+10),
  )
# Reference: SD of inside / outside
# logger_data_arranged %>% filter(logger_id == "outside") %>% select(temp) %>% summarize(sd= sd(temp)) %>% as.numeric()
# logger_data_arranged %>% filter(logger_id == "inside") %>% select(temp) %>% summarize(sd= sd(temp)) %>% as.numeric()
# 
# 正規化したデータを使った作図用データセット作成
logger_data_arranged_02 <- 
  logger_data_arranged_longer %>% 
  tidyr::pivot_longer(
    cols = c("outside_scale","inside_scale"),
    names_to = "logger_id",
    values_to = "temp_scale"
  )
# 
# 標準化した内部温度・中間温度推移
line_logger_data_scale <- 
  ggplot(
    data = logger_data_arranged_02,
    aes(
      x = time,
      y = temp_scale,
      colour = logger_id
    )
  ) +
  geom_line() +
  geom_rect(
    data = rects, 
    aes(xmin = floor_date(xstart, "second"), xmax = xend, ymin = -Inf, ymax = Inf),
    fill="grey",
    alpha = 0.2, 
    inherit.aes = FALSE
  ) + 
  # geom_point() +
  scale_color_okabeito() +
  scale_x_datetime(date_labels = "%d %b %H:%M", date_breaks = "8 hours") +
  labs(
    title = "Temperature inside / outside of cool box",
    subtitle = "Yellow: outside; Black: inside; Grey: flight period",
    x = "Time (Unit: 1 min)",
    y = "Temperature (Scaled, Unit: Temperature (°C))",
    color = "Loggers' position"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  ) 
# 保存
ggsave(
  "line_logger_data_scale_scale.pdf",
  plot = line_logger_data_scale,
  height = 150, 
  width = 150,
  units = "mm"
)
# 
# 状態空間モデル用データセット
logger_data_arranged_longer_ssm <- 
  logger_data_arranged_longer %>% 
  dplyr::mutate(
    drop = dplyr::case_when(
      time > lubridate::ymd_hms("2023-03-19 20:50:00", tz = "Asia/Bangkok") & 
      time < lubridate::ymd_hms("2023-03-20 06:00:00", tz = "Asia/Bangkok") ~ "1",
      TRUE ~ "0"
    )
  )
# 
# stanにわたすデータ
n <- nrow(logger_data_arranged_longer_ssm)
Y <- logger_data_arranged_longer_ssm$outside
U <- logger_data_arranged_longer_ssm$inside
W <- as.numeric(logger_data_arranged_longer_ssm$drop)
# stanをキックするコード
model_02 <- 
  cmdstanr::cmdstan_model(
    "inside_01.stan", 
    quiet = TRUE
    )
fit <- 
  model_02$sample(
    data = list(
      n = n,
      Y = Y,
      U = U,
      W = W
      ),
    seed = 123,
    # イテレーション回数。
    # ある程度回数が多いほうがいい。
    iter_sampling = 2000,
    iter_warmup = 2000, 
    parallel_chains = 4,
    refresh = 100,
    thin = 1
    )
# 結果要約を保存
fit$save_object("transportation_temp_estimated.rds")
# 
# 結果要旨はこうしたほうが見やすいよ。
write_excel_csv(
  fit$summary(), 
  "transportation_temp.csv"
  )
# 
# 結果を表現する
# ---- temp.est.results ----
# 結果要約ファイルを読み込む
transportation_temp_estimated <- 
  readr::read_csv(
    "transportation_temp.csv"
    )
# 結果要約から必要な箇所を取り出す
transportation_temp_estimated_muu <- 
  transportation_temp_estimated %>% 
  dplyr::filter(stringr::str_detect(variable, "mu_u")) %>% 
  dplyr::select(mean) %>% 
  data.table::setnames("mu_u")
transportation_temp_estimated_uhat <- 
  transportation_temp_estimated %>% 
  dplyr::filter(stringr::str_detect(variable, "uhat")) %>% 
  dplyr::select(mean) %>% 
  data.table::setnames("uhat")
# 
# 推定結果要約と実測値とを組み合わせる
logger_data_arranged_longer_combined <- 
  logger_data_arranged_longer %>% 
  dplyr::bind_cols(transportation_temp_estimated_muu) %>% 
  dplyr::bind_cols(transportation_temp_estimated_uhat) %>% 
  # dplyr::select(outside, inside) %>% 
  tidyr::pivot_longer(
    cols = c("inside","mu_u","uhat"),
    names_to = "condition",
    values_to = "temperature"
  ) %>% 
  dplyr::mutate(
    condition = factor(condition)
  )
# 
# 作図
line_transportation_temp_estimated <- 
  logger_data_arranged_longer_combined %>%
  ggplot2::ggplot(aes(x = time, y = temperature)) +
  geom_line(
    data = dplyr::filter(logger_data_arranged_longer_combined, condition %in% c("inside")),
    aes(x = time,y = temperature, color = condition)
  ) +
  geom_point(
    data = dplyr::filter(logger_data_arranged_longer_combined, condition %in% c("uhat")),
    aes(x = time,y = temperature, colour = condition),
    size = 0.5
    ) + 
  geom_rect(
    data = rects, 
    aes(xmin = floor_date(xstart, "second"), xmax = xend, ymin = -Inf, ymax = Inf),
    fill="grey",
    alpha = 0.2, 
    inherit.aes = FALSE
  ) + 
  scale_x_datetime(date_labels = "%d %b %H:%M", date_breaks = "6 hours") +
  labs(
    x = "Time (ASEAN Common time)",
    y = "Temperature (Unit °C)"
  ) +
  scale_colour_okabeito() +
  theme_classic() +
  theme(
    legend.position = "none"
  )
# 保存
ggsave(
  "line_transportation_temp_estimated.pdf",
  plot = line_transportation_temp_estimated,
  width = 200,
  height = 200, 
  units = "mm"
)

# おしまい
