# Textbook exercise EO2HO
# Output: graph
# Galanakis & Hobbs (2021); <i.galanakis@kent.ac.uk>
# ---

library(tidyverse)
library(devtools)
#install_github("andrewheiss/reconPlots") # install if needed
library(reconPlots)

# functions to plot ----
fun.D  <- function(x) 20-x/5
fun.S  <- function(x) 4+x/6
fun.Ss <- function(x) 2+x/12

x_range <- 0:100

# intersections ----
curve_intersectionDS <- curve_intersect(fun.D, fun.S, empirical = FALSE,
                                      domain = c(min(x_range), max(x_range)))
curve_intersectionDSs <- curve_intersect(fun.D, fun.Ss, empirical = FALSE,
                                        domain = c(min(x_range), max(x_range)))
effectiveP <- fun.S(curve_intersectionDSs$x)
plot_labels <- data.frame(label = c("D", "S", "S + subsidy"),
                          x = c(90, 90, 90),
                          y = c(1, 20, 10.5))


# plot ----
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(size = 1, fun = fun.D) +
  stat_function(color = "#81A1C1", size = 1, fun = fun.S) +
  stat_function(color = "#A3BE8C", size = 1, fun = fun.Ss) +
  geom_vline(xintercept = curve_intersectionDS$x, size=.1, linetype = "dotted") +
  geom_hline(yintercept = curve_intersectionDS$y, size=.1, linetype = "dotted") +
  geom_vline(xintercept = curve_intersectionDSs$x, size=.1, linetype = "dotted") +
  geom_hline(yintercept = curve_intersectionDSs$y, size=.1, linetype = "dotted") +
  geom_hline(yintercept = effectiveP, size=.1, linetype = "dotted") +
  scale_x_continuous(limits = c(0,100),expand = c(0, 0),
                     breaks = c(curve_intersectionDS$x,curve_intersectionDSs$x, 100),
                     labels = expression(frac(a-c,b+d), over(2*a-c,2*b+d),frac(a,b))) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(2, 4, curve_intersectionDS$y,curve_intersectionDSs$y, effectiveP,20),
                     labels = expression(frac(c,2),c, c + d*frac(a-c,b+d), frac(1,2)*bgroup("(", c+d*over(2*a-c,2*b+d), ")"), c+d*over(2*a-c,2*b+d),a)) +
  labs(x = "Quantity", y = "Price", title="Eat Out to Help Out: A textbook exercise") +
  geom_text(data = plot_labels,
            aes(x = x, y = y, label = label), parse = TRUE,
            label.size = 7) +
  theme_yannis()

