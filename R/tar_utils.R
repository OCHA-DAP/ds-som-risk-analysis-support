# Figure out percentile thresholds on both %s and pop exposed at admin 1 level

# Nicer example here: https://www.tidyverse.org/blog/2020/03/dplyr-1-0-0-summarise/
# turning it into a function so i can easily play w/ ranges to look at them
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := quantile(x, q), "{{ x }}_q" := q)
}
