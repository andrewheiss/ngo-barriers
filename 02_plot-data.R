# Load libraries and data -------------------------------------------------

library(tidyverse)
library(scales)
library(patchwork)  # Install from https://github.com/thomasp85/patchwork

# This was generated with 01_clean-data.R
dcjw <- read_csv("data_clean/dcjw_clean.csv")

# This was hand-typed by Andrew Heiss
dcjw_meta <- read_csv("data_original/dcjw_questions.csv") %>% 
  filter(!ignore_in_index) %>% 
  select(barrier_group = barrier_display, barrier = question_clean, 
         barrier_display = question_display) %>% 
  mutate(barrier_group = paste0("Barriers to ", str_to_lower(barrier_group)))

num_countries <- dcjw %>% distinct(iso3) %>% nrow()


# Wrangle data ------------------------------------------------------------

# Wrangle data so it's plottable with ggplot
total_barriers <- dcjw %>% 
  # Add up the total number of countries in each year with specific NGO laws
  group_by(iso3, year) %>% 
  summarize_at(vars(one_of(dcjw_meta$barrier)), funs(. > 0)) %>% 
  group_by(year) %>% 
  summarize_at(vars(-iso3, -year), funs(sum(.))) %>% 
  gather(barrier, value, -year) %>% 
  # Bring in cleaner names and change all XXXs to line breaks
  left_join(dcjw_meta, by = "barrier") %>% 
  mutate(barrier_display = str_replace(barrier_display, "XXX", "\n")) %>% 
  arrange(desc(value)) %>% 
  # Make sure the barrier names are in the right order
  mutate(barrier_display = fct_inorder(barrier_display))


# Plot data ---------------------------------------------------------------

# Ordinarily we could just plot total_barriers and use facet_wrap() to get all
# three plots, but it's really hard / impossible have separate legends for each
# panel and have different y-axes with a secondary axis, so instead we fake it
# by making three separate plots and then combine them with patchwork
entry_plot <- ggplot(filter(total_barriers, 
                            barrier_group == "Barriers to entry"), 
                     aes(x = year, y = value, 
                         color = barrier_display,
                         linetype = barrier_display)) +
  geom_line(size = 0.5) +
  expand_limits(y = c(0, 60)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / num_countries,
                                         labels = percent_format(accuracy = 1)),
                     expand = c(0, 0)) +
  scale_colour_manual(values = c("black", "grey80", "grey50"), name = NULL) +
  scale_linetype_manual(values = c("solid", "solid", "21"), name = NULL) +
  guides(color = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Number of countries") +
  theme_minimal() + 
  theme(legend.justification = "left",
        legend.position = "bottom") +
  facet_wrap(~ barrier_group)

funding_plot <- ggplot(filter(total_barriers, 
                              barrier_group == "Barriers to funding"), 
                       aes(x = year, y = value, 
                           color = barrier_display,
                           linetype = barrier_display)) +
  geom_line(size = 0.5) +
  expand_limits(y = c(0, 35)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / num_countries,
                                         labels = percent_format(accuracy = 1)),
                     expand = c(0, 0)) +
  scale_colour_manual(values = c("black", "grey80", "grey50", "black", "grey80"), 
                      name = NULL) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "21", "21"), 
                        name = NULL) +
  guides(color = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3)) +
  labs(x = NULL, y = "Number of countries") +
  theme_minimal() + 
  theme(legend.justification = "left",
        legend.position = "bottom") +
  facet_wrap(~ barrier_group)

advocacy_plot <- ggplot(filter(total_barriers, 
                               barrier_group == "Barriers to advocacy"), 
                        aes(x = year, y = value, 
                            color = barrier_display)) +
  geom_line(size = 0.5) +
  expand_limits(y = c(0, 25)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / num_countries,
                                         labels = percent_format(accuracy = 1)),
                     expand = c(0, 0)) +
  scale_colour_manual(values = c("black", "grey80"), name = NULL) +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = NULL, y = "Number of countries") +
  theme_minimal() + 
  theme(legend.justification = "left",
        legend.position = "bottom") +
  facet_wrap(~ barrier_group)

# Combine the three plots
all_barriers_plot <- 
  (entry_plot + funding_plot + advocacy_plot) &
  theme(panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey50", fill = NA, size = 0.15),
        axis.title = element_text(size = rel(0.8), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 3)),
        legend.text = element_text(size = rel(0.6)),
        legend.box.margin = margin(t = -0.5, unit = "lines"),
        strip.text = element_text(size = rel(0.9), hjust = 0, face = "bold"))
all_barriers_plot

# Save final plot
ggsave(all_barriers_plot, filename = "output/all_barriers.pdf", 
       width = 10, height = 4, units = "in")
ggsave(all_barriers_plot, filename = "output/all_barriers.png", 
       width = 10, height = 4, units = "in")

# Save wide data for using in Excel
write_csv(total_barriers, "data_clean/dcjw_plot.csv")
writexl::write_xlsx(total_barriers, "data_clean/dcjw_plot.xlsx")
