# Calculating mean VORP and Win Shares for each draft position
data %>%
  group_by(overall_pick) %>%
  summarise(pick_value = mean(value_over_replacement, na.rm = T),
            win_share_value = mean(win_shares, na.rm = T)) -> pick_values

# Plotting Pick Number vs. Average Value Over Replacement
pick_values %>%
  ggplot(aes(overall_pick, pick_value)) +
  geom_point() +
  theme_bw() +
  labs(title = 'Pick Number vs. Average Value Over Replacement', x = 'Pick Number', y = 'Average Value Over Replacement') -> value_plot1

# Plotting Pick Number vs. Log of Average Value Over Replacement
pick_values %>%
  ggplot(aes(overall_pick, log(pick_value))) +
  geom_point() +
  theme_bw() +
  labs(title = 'Pick Number vs. Log of Average Value Over Replacement', x = 'Pick Number', y = 'Log of Average Value Over Replacement') -> value_plot2

# Plotting Pick Number vs. Average Win Shares
pick_values %>%
  ggplot(aes(overall_pick, win_share_value)) +
  geom_point() +
  theme_bw() +
  labs(title = 'Pick Number vs. Average Win Shares', x = 'Pick Number', y = 'Average Win Shares') -> value_plot3

# Plotting Pick Number vs. Log of Average Win Shares
pick_values %>%
  ggplot(aes(overall_pick, log(win_share_value))) +
  geom_point() +
  theme_bw() +
  labs(title = 'Pick Number vs. Log of Average Win Shares', x = 'Pick Number', y = 'Log of Average Win Shares') -> value_plot4

# Running linear regressions on vorp and win shares
lm(log(pick_value) ~ overall_pick, data = pick_values) -> vorp_regression
lm(log(win_share_value) ~ overall_pick, data = pick_values) -> win_share_regression

# Using regression coefficients to calculate expected value and win shares for each draft slot
data %>%
  mutate(expected_value = exp(vorp_regression$coefficients[1] + (vorp_regression$coefficients[2] * overall_pick)),
         expected_win_shares = exp(win_share_regression$coefficients[1] + (win_share_regression$coefficients[2] * overall_pick))) -> data

# Creating dataframe of teams and their average draftee value
data %>%
  group_by(team) %>%
  summarise(average_value = mean(value_over_replacement - expected_value, na.rm = T),
            average_win_shares = mean(win_shares - expected_win_shares, na.rm = T),
            num_players = n()) %>%
  arrange(desc(average_value)) -> teams_over_under_performers
names(teams_over_under_performers) <- c('Team', 'Average Player Value vs. Expected', 'Average Player Win Shares vs. Expected', 'Number of Players')

# Creating dataframe of colleges and their average draftee value
data %>%
  filter(college != '') %>%
  group_by(college) %>%
  summarise(average_value = mean(value_over_replacement - expected_value, na.rm = T),
            average_win_shares = mean(win_shares - expected_win_shares, na.rm = T), 
            num_players = n()) %>%
  arrange(desc(average_value)) %>%
  drop_na(average_value) -> college_over_under_performers
names(college_over_under_performers) <- c('College', 'Average Player Value vs. Expected', 'Average Player Win Shares vs. Expected', 'Number of Players')

# Filtering college draftee value to get rid of small sample size schools
college_over_under_performers %>%
  filter(`Number of Players` > 5) -> college_over_under_performers_filtered
