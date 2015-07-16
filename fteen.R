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

IF <- subset(data10k, Name == "IAN FERRUCCI")
EW <- subset(data10k, Name == "EVAN WOLCOTT")
WL <- subset(data10k, Name == "WILLIAM LENINGTON")

data10k <- subset(data10k, Age < 18.5)

xmin <- 0.9 * min(data10k$Age)
xmax <- 1.1 * max(data10k$Age)

rect_left <- c(xmin, 14.5)
rect_right <- c(12.5, 16.5)
rectangles <- data.frame(
  xmin = rect_left, xmax = rect_right,
  ymin = -Inf, ymax = Inf
)

plot10k <- 
  ggplot(data10k, aes(x = Age, y = Time, color = Gender)) +
  annotate(
    "rect",
    xmin=xmin,
    xmax=12.5,
    ymin = -Inf,
    ymax = Inf,
    alpha = 1.0,
    fill = 'gray80'
  ) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  xlim(xmin, xmax) +
  ylim(0, NA) +
  scale_x_continuous(breaks = 9:18) +
  geom_point(data=WL, aes(x = Age, y = Time), color="black") +
  geom_text(data=WL, aes(x = Age, y = Time), label="wl", color="black", vjust=1) +
  geom_point(data=EW, aes(x = Age, y = Time), color="black") +
  geom_text(data=EW, aes(x = Age, y = Time), label="ew", color="black", vjust=1, hjust=0) +
  geom_point(data=IF, aes(x = Age, y = Time), color="black") +
  geom_text(data=IF, aes(x = Age, y = Time), label="if", color="black", hjust=-0.2, vjust=0.5)

plot10ka <- 
  ggplot(data10k, aes(x = Age, y = Time, color = Gender)) +
  annotate(
    "rect",
    xmin=rectangles$xmin,
    xmax=rectangles$xmax,
    ymin = -Inf,
    ymax = Inf,
    alpha = 1.0,
    fill = 'gray80'
  ) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  xlim(xmin, xmax) +
  ylim(0, NA) +
  scale_x_continuous(breaks = 9:18) +
  geom_point(data=WL, aes(x = Age, y = Time), color="black") +
  geom_text(data=WL, aes(x = Age, y = Time), label="wl", color="black", vjust=1) +
  geom_point(data=EW, aes(x = Age, y = Time), color="black") +
  geom_text(data=EW, aes(x = Age, y = Time), label="ew", color="black", vjust=1, hjust=0) +
  geom_point(data=IF, aes(x = Age, y = Time), color="black") +
  geom_text(data=IF, aes(x = Age, y = Time), label="if", color="black", hjust=-0.2, vjust=0.5)

# pdf("firecracker2015.pdf", width=6, height=4)
grid.arrange(plot10k, plot10ka, nrow=2, main=paste(year, "Firecracker"))
# dev.off()

