
# Data Visualization ------------------------------------------------------

#Graphizal tools to interpret patterns: Exploratory visualization & Explanatory visualization

#4 graphics systems in R: Base graphics; Grid graphics; Lattice graphics; ggplot2
# Base: MASS package
library(MASS)
plot(UScereal$sugars,UScereal$calories)
title("Sugars vs. Calories")
# Grid (More power and ability than Base plots)
pushViewport(plotViewport())
pushViewport(dataViewport(x, y))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.points(x, y)
grid.text("UScereal$calories", x = unit(-3, "lines"), rot = 90)
grid.text("UScereal$sugars", y = unit(-3, "lines"), rot = 0)
popViewport(2)