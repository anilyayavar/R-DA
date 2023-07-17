library(tidyverse)
library(rlang)

my_chart <- function(data, x_var, y_var){

  lmod <- eval(substitute(lm(y_var ~ x_var , data = data)))

  data %>%
    mutate(pred = predict(lmod),
           res = residuals(lmod)) %>%
    ggplot(aes({{x_var}}, {{y_var}})) +
    geom_smooth(method = 'lm', color = 'lightgrey', se = FALSE, formula = 'y ~ x') +
    geom_segment(aes(xend = {{x_var}},  yend = pred), alpha = 0.2, size = 0.9, color = 'seagreen') +
    geom_point(aes(color = abs(res), size = abs(res))) +
    scale_color_continuous(low = 'yellow', high = 'darkred') +
    guides(color = FALSE, size = FALSE) +
    geom_point(aes(y = pred), shape = 1) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

}


my_chart(attitude, rating, complaints)
