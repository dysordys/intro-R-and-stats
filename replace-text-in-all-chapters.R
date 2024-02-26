library(tidyverse)

tibble(file = Sys.glob("*.qmd")) |>
  mutate(content = map(file, read_lines)) |>
  mutate(content = map(content,
                       \(x) str_replace_all(x, "summarise", "summarize"))) |>
  mutate(content = walk2(content, file, write_lines))

tibble(file = Sys.glob("*.qmd")) |>
  mutate(content = map(file, read_lines)) |>
  mutate(hasContent = map_lgl(content, \(x) any(str_detect(x, "autoplot"))))
