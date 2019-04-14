# file id -----------------------------------------------------------------
# @author: josh weinflash
# @created: 2019-04-05
# @purpose: 18-19 season animations

# load libraries ----------------------------------------------------------
library("data.table")
library("ggplot2")
library("gganimate")
source("scripts/helper.R")

# load data ---------------------------------------------------------------
l_cols = list(date         = readr::col_date("%m/%d/%y"),
              home_away    = "c",
              opponent     = "c",
              win_loss     = "c",
              team_pts     = "i",
              opponent_pts = "i")

df_logs = readr::read_csv("data/knicks-gamelog.csv", col_types = l_cols)
df_logs = as.data.table(df_logs)

# add some NBA stats ------------------------------------------------------
df_logs = df_logs[order(date), ]
df_logs[, score_diff := (team_pts - opponent_pts)]
df_logs[, net_rating := cumsum(score_diff) / seq_along(score_diff)]

# bin score difference (my_score_bin in helper) ---------------------------
df_logs[, score_bin := my_score_bin(score_diff)]

# add y-coordinate (nth game w/ difference) -------------------------------
df_logs[, n_diff := seq_along(date), by = "score_bin"]

# add titles for plot -----------------------------------------------------
df_logs[, record := sprintf("%i-%i", cumsum(win_loss == "W"), cumsum(win_loss == "L"))]

df_logs[, subtitle := sprintf("%s %i %s %s %i-%i (%s) // Record: %s, Net Rating: %.1f",
                              lubridate::month(date, label = TRUE), mday(date), home_away,
                              opponent, team_pts, opponent_pts, win_loss, record, net_rating)]

# bind duplicate of data to simulate fall ---------------------------------
df_temp = copy(df_logs)
df_temp[, n_diff := 15L]
df_logs = rbind(df_logs, df_temp)

df_logs = df_logs[order(date, -n_diff), ]
df_logs[, frame_no := seq_along(score_diff)]

# build animation ---------------------------------------------------------
a1 = ggplot(df_logs, aes(score_bin, n_diff, group = date, fill = score_bin))
a1 = a1 + geom_point(shape = 21, color = "black", size = 6.0,
                     stroke = 0.50, alpha = 0.75)

a1 = a1 + transition_reveal(frame_no)

a1 = a1 + scale_x_discrete(drop = FALSE)
a1 = a1 + scale_fill_brewer(palette = "RdYlGn", drop = FALSE)

a1 = a1 + labs(x = "Score difference",
               y = "",
               title = "New York Knicks '18-'19 Season",
               subtitle = "{df_logs$subtitle[as.integer(frame_along)]}",
               caption = "Data from @cleantheglass")

a1 = a1 + theme_classic(base_size = 12)

a1 = a1 + theme(legend.position = "none",
                text = element_text(family = "Courier"),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                axis.line.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank())

# write to file -----------------------------------------------------------
anim_save("output/knicks.gif", animation = a1, nframes = 150,
          end_pause = 5, width = 9, height = 5, res = 100, units = "in")
