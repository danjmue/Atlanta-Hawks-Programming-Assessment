## Part A

# Getting NBA team who has drafted the most players who went to Duke and were drafted in or before the 2000 draft
data %>%
  filter(college == 'Duke' & year <= 2000) %>%
  group_by(team) %>%
  summarise(players = n()) %>%
  arrange(desc(players)) %>%
  filter(players == max(players)) -> teams_duke_before_2000
# DAL, MIN, PHO

# Getting NBA team who has drafted the most players who have a first name that begins with D and were drafted in an even year draft
data %>%
  filter(str_sub(player, 1, 1) == 'D' & (year %% 2) == 0) %>%
  group_by(team) %>%
  summarise(players = n()) %>%
  arrange(desc(players)) %>%
  filter(players == max(players)) -> teams_first_letter_D_even_year
# BOS, MIL, SEA

# Describing the relationship between a team’s first round pick slot in one year with their first-round pick slot in the subsequent year
data %>%
  filter(overall_pick <= ifelse(year < 1995, 27, ifelse(year < 2004, 29, 30))) %>% # Accounting for differing number of teams in the NBA over the years
  group_by(team, year) %>%
  summarise(pick_slot = first(overall_pick)) %>% # Assuming that a teams first pick in the draft is their true pick slot
  mutate(next_year_pick_slot = lead(pick_slot),
         change_in_pick_slot = next_year_pick_slot - pick_slot) %>%
  select(team, year, pick_slot, next_year_pick_slot, change_in_pick_slot) -> pick_slot_data

# Plotting pick slot vs. subsequent pick slot
pick_slot_data %>%
  ggplot(aes(x = pick_slot, y = next_year_pick_slot)) +
  geom_point(alpha = .25) + # using transparency of points to show frequency of each combination of pick slot and subsequent pick slot
  geom_line(stat = 'summary', color = 'blue') + # layering the plot with the average subsequent pick slot
  stat_cor(method="pearson", label.x.npc = 'left', label.y.npc = 'top') + # adding correlation statistics
  theme_bw() +
  labs(title = 'Pick Slot vs. Pick Slot in Subsequent Year', x = 'Pick Slot', y = 'Next Year\'s Pick Slot') -> pick_slot_plot1

# Plotting pick slot vs. change in pick slot
pick_slot_data %>%
  ggplot(aes(x = pick_slot, y = change_in_pick_slot)) +
  geom_point(alpha = .25) + # using transparency of points to show frequency of each combination of pick slot and change in pick slot
  geom_line(stat = 'summary', color = 'blue') + # layering the plot with the average change in pick slot
  geom_abline(yintercept = 0, slope = 0, color = 'red') + # adding a line where change in pick slot = 0 to show where teams improve/regress
  stat_cor(method="pearson", label.x.npc = 'right', label.y.npc = 'top', label.x = 20) + # adding correlation statistics
  theme_bw() +
  labs(title = 'Pick Slot vs. Change in Pick Slot in Subsequent Year', x = 'Pick Slot', y = 'Change in Pick Slot') -> pick_slot_plot2

# Plotting pick slot vs. average subsequent pick slot
pick_slot_data %>%
  group_by(pick_slot) %>%
  summarise(average_subsequent_pick_slot = mean(next_year_pick_slot, na.rm = T)) %>%
  ggplot(aes(pick_slot, average_subsequent_pick_slot)) +
  geom_point() +
  geom_smooth(method = lm) +
  stat_cor(method="pearson", label.x.npc = 'left', label.y.npc = 'top') + # adding correlation statistics
  theme_bw() +
  labs(title = 'Pick Slot vs. Average Pick Slot in Subsequent Year', x = 'Pick Slot', y = 'Average Next Year\'s Pick Slot') -> pick_slot_plot3

# Plotting pick slot vs. average change in pick slot
pick_slot_data %>%
  group_by(pick_slot) %>%
  summarise(average_change_in_pick_slot = mean(change_in_pick_slot, na.rm = T)) %>%
  ggplot(aes(pick_slot, average_change_in_pick_slot)) +
  geom_point() +
  geom_smooth() +
  geom_abline(yintercept = 0, slope = 0, color = 'red') + # adding a line where change in pick slot = 0 to show where teams improve/regress
  stat_cor(method="pearson", label.x.npc = 'right', label.y.npc = 'top', label.x = 20) + # adding correlation statistics
  theme_bw() +
  labs(title = 'Pick Slot vs. Average Change in Pick Slot in Subsequent Year', x = 'Pick Slot', y = 'Average Change in Pick Slot') -> pick_slot_plot4

# Getting average pick slot change for the bottom 50% of draft picks
pick_slot_data %>%
  as.data.frame() %>%
  filter(pick_slot < mean(pick_slot)) %>%
  select(change_in_pick_slot) %>%
  colMeans(na.rm = T) -> bottom_half_average_pick_slot_change

# Getting average pick slot change for the top 50% of draft picks
pick_slot_data %>%
  as.data.frame() %>%
  filter(pick_slot > mean(pick_slot)) %>%
  select(change_in_pick_slot) %>%
  colMeans(na.rm = T) -> top_half_average_pick_slot_change
