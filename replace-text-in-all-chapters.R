library(tidyverse)

tibble(file = Sys.glob("*.qmd")) |>
  mutate(content = map(file, read_lines)) |>
  mutate(content = map(content,
                       \(x) str_replace_all(x, "data-with-R/", "intro-R-and-stats/"))) |>
  mutate(content = walk2(content, file, write_lines))
