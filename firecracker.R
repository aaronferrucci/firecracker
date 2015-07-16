# setwd("C:\\Users\\aaronf\\Documents\\classes\\data_science\\various\\firecracker")
source("firecracker_utils.R")
library(stringr)
library(ggplot2)
library(gridExtra)
library(RCurl)
library(XML)

year <- 2015
the_data <- get_data_from_url(year)
data5k <- the_data[[1]]
data10k <- the_data[[2]]

if (year == 2015) {
  AF <- subset(data10k, Name == "AARON FERRUCCI")
  IF <- subset(data10k, Name == "IAN FERRUCCI")
  EW <- subset(data10k, Name == "EVAN WOLCOTT")
  WL <- subset(data10k, Name == "WILLIAM LENINGTON")
} else {
  AF <- subset(data10k, Name == "AARON FERRUCCI")
  IF <- subset(data5k, Name == "IAN FERRUCCI")
  LM <- subset(data5k, Name == "LORI MATSUMOTO")
}

plot5k <-
  qplot(Age, Time, data=data5k, main="5k", ylim=c(0, NA), col=Gender) +
    geom_point() +
    geom_smooth()
if (year == 2014) {
  plot5k <- plot5k + 
    geom_point(data=LM, color="black") +
    geom_text(data=LM, label="lm", color="black", vjust=1) +
    geom_point(data=IF, color="black") +
    geom_text(data=IF, label="if", color="black", vjust=1)
} else if (year == 2015) {
}

rect_left <- c(0, 18.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5)
rect_right <- c(12.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5)
rectangles <- data.frame(
  xmin = rect_left, xmax = rect_right,
  ymin = -Inf, ymax = Inf
)

#data10k <- subset(data10k, Age < 18.5)

plot10k <-
  ggplot(data10k, aes(x = Age, y = Time, color = Gender)) + 
  annotate(
    "rect",
    xmin=rectangles$xmin,
    xmax=rectangles$xmax, 
    ymin = rectangles$ymin,
    ymax = rectangles$ymax,
    alpha = 1.0,
    fill = 'gray80'
  ) +
  geom_point() +
  stat_smooth(formula = y ~ x) +
  xlim(min(data10k$Age) * 0.9, max(data10k$Age) * 1.1) +
  ylim(0, NA)

if (year == 2014) {
  plot10k <- plot10k + 
    geom_point(data=AF, color="black") +
    geom_text(data=AF, label="af", color="black", vjust=1)
} else if (year == 2015) {
  plot10k <- plot10k + 
    geom_point(data=WL, aes(x = Age, y = Time), color="black") +
    geom_text(data=WL, aes(x = Age, y = Time), label="wl", color="black", vjust=1) +
    geom_point(data=EW, aes(x = Age, y = Time), color="black") +
    geom_text(data=EW, aes(x = Age, y = Time), label="ew", color="black", vjust=1, hjust=0) +
    geom_point(data=IF, aes(x = Age, y = Time), color="black") +
    geom_text(data=IF, aes(x = Age, y = Time), label="if", color="black", hjust=1, vjust=1.1) +
    geom_point(data=AF, aes(x = Age, y = Time), color="black") +
    geom_text(data=AF, aes(x = Age, y = Time), label="af", color="black", vjust=1)
}

# pdf("firecracker2015.pdf", width=6, height=4)
if (year == 2014) {
  grid.arrange(plot5k, plot10k, nrow=2, main=paste(year, "Firecracker"))
} else if (year == 2015) {
  grid.arrange(plot10k, nrow=1, main=paste(year, "Firecracker"))
}
# dev.off()

