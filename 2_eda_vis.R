#' 2. Data Exploration and Visualization 
#' As part of Stats 551 Final Project

# Library
library(tidyverse)
library(ggplot2)
library(reshape)

# Functions
theme_custom <- function() {
  theme_bw() + # note ggplot2 theme is used as a basis
    theme(plot.title = element_text(size = 10, face = "bold",
                                    hjust = .5,
                                    margin = margin(t = 5, b = 15)),
          plot.caption = element_text(size = 9, hjust = 0, 
                                      margin = margin(t = 15)),
          panel.grid.major = element_line(colour = "grey88"),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(margin = margin(t = 10),
                                      size = 10, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 10),
                                      size = 10, face = "bold"))
}

# Data
us_df = read.csv("us_df.csv")

# Plots
df.m <- melt(us_df, id = c("year"))

ggplot(df.m, aes(year, value)) + 
  geom_line() + 
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_custom()




# Correlation plot

library(corrplot)
library(RColorBrewer)
library(ggpubr)


M <- na.omit(us_df[,-1])
colnames(M) <- c("C02", "Population", "GDP", "GDP Growth", "GDI", "Income", "Urban", "Energy", "Energy Import", "Electricity")
M <-cor(M)

corrplot(M, order = "hclust", type = "lower") + title("Correlation Plot for C02 Emissions Data")



