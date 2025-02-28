library(tidyverse)
library(withr)


bird <-
  with_seed(
    12345L,
    tibble(
      island = c(rep("smaller", 20), rep("larger", 20)),
      weight = round(rnorm(40, mean = 20, sd = 5), 2)
    )
  )

bird |>
  ggplot(aes(x = island, y = weight)) +
  geom_boxplot(color = "steelblue", fill = "steelblue",
               alpha = 0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.4, width = 0.05, color = "steelblue") +
  theme_bw()

bird |>
  group_by(island) |>
  summarize(mean = mean(weight), median = median(weight)) |>
  ungroup()

bird |>
  group_by(island) |>
  summarize(mean = mean(weight),
            sd = sd(weight),
            N = n(),
            SEM = sd / sqrt(N),
            CI = qt(1 - 0.025, N - 1)) |>
  ungroup() |>
  ggplot(aes(x = island, y = mean, ymin = mean - CI, ymax = mean + CI)) +
  geom_point(color = "steelblue") +
  geom_errorbar(color = "steelblue", width = 0.2) +
  labs(y = "mean and 95% confidence interval") +
  theme_bw()

print(wilcox.test(weight ~ island, data = bird, alternative = "greater"))
print(t.test(weight ~ island, data = bird, alternative = "greater"))

write_csv(bird, "fictive_bird_example.csv")
write_csv(bird, "data/fictive_bird_example.csv")
zip("data/fictive_bird_example.zip", "data/fictive_bird_example.csv")
