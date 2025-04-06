library(googledrive)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(ggpubr)

drive_deauth()
gs4_deauth()

readdicerolls <- function(name) {
  X <- read_sheet(as_id(name))
  data <- X |> pivot_longer(cols=everything(), names_to = "group")
  ##Make sure value is numeric
  data <- data |> mutate(value=as.numeric(value))
  ##Remove values outside 0:10 or NA
  data <- data |> filter(!is.na(value), value %in% 0:10)
  return(data)
}

n <- c()
mu <- c()
for (k in 1:30) {
  data <- readdicerolls("https://docs.google.com/spreadsheets/d/1n33wF3iJ0NAmItyzEEkR8qLQqUiVQZaQh3NBY1nd4Z0")
  pl.hist <- ggplot(data, aes(x=value, fill=group)) + geom_histogram(binwidth=1, center=0, color="white") + theme_bw() + theme(legend.position="inside", legend.position.inside = c(1,1), legend.justification=c(1,1))
  n <- c(n, nrow(data))
  mu <- c(mu, mean(data$value))
  pl.mean <- ggplot(data.frame(n=n, mu=mu), aes(n, mu)) + geom_line() + theme_bw()
  plot(ggarrange(pl.hist, pl.mean, ncol=2))
  Sys.sleep(10)
}
