density.plot.2 <- Q
density.plot.2 <- subset(density.plot.2, density.plot.2$parameter == 2.8)
density.plot.2 <- subset(density.plot.2, density.plot.2$lambda < 0.26)
density.plot.2 <- subset(density.plot.2, density.plot.2$lambda > 0.24)
library(ggplot2)
f1 <- ggplot(density.plot.2 , aes(x = parameter, y = normdistance2, col = type)) +
  stat_smooth(aes(fill = type), method = "loess", alpha = 0.3, size = 1, level = 0.9) +
  xlab("lambda") + ylab("Error") +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "Verdana")
  )
geom_function(fun = dnorm, colour = "red")

l1 <- ggplot(density.plot.2,                    # Change colors of lines & points by group
             aes(x = depth,
                 y = normdistance2,
                 col = type)) +
  geom_point(position = 'jitter') +
  xlab("Depth") + ylab("Error") +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "Verdana")
  )



b1 <- ggplot(density.plot.2, aes(x = parameter, y = normdistance2, col = type)) +
  geom_point(position = 'jitter') +
  geom_boxplot() +
  xlab("Poisson Parameter") + ylab("Error") +
  scale_x_continuous(breaks = seq(min(density.plot.2$parameter), max(density.plot.2$parameter), by = 0.1)) + 
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "Verdana")
  ) +
  facet_wrap(~type, scales = "free_y")  # Facet by the "type" variable
