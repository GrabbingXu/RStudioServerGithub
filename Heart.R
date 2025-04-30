# # Lite Heart####
# library(ggplot2)
# dat <- data.frame(t = seq(0, 2*pi, by = 0.01))
# x <-  function(t) 16 * sin(t)^3
# y <- function(t) 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)
# 
# dat$y <- y(dat$t)
# dat$x <- x(dat$t)
# 
# heart <- ggplot(dat, aes(x,y)) +
#   geom_polygon(fill = "red", col = "firebrick", alpha = 0.9) +
#   theme_classic()
#   # title("r=a(1-sinθ)")
# 
# print(heart)


# Advanced Heart####
# requires ImageMagick (http://www.imagemagick.org/)
# install.packages("magick")
# install.packages("animation", repos = "http://rforge.net", type = "source")
# install.packages("dplyr")
# install.packages("pryr")
# install.packages("ggplot2")
# install.packages("extrafont")

library(animation)
library(dplyr)
library(pryr)
library(ggplot2)
library(extrafont)
#------------------------------------------------------------------------------#
# heart curve formula
heart <- quote((x^2 + y^2 - 1)^3 - x^2 * y^3)

# formula for heart curve at a given x
heart_at_x <- function(x) {
  function(y) eval(substitute_q(heart, list(x = x)), list(y = y))
}

# trace the heart curve
# by evaluating the heart curve formula at each x, then finding the roots of the
# resulting formula in y; e.g. a x==0, find the roots of (y^2 - 1)^3 = 0
# broken up into upper and lower parts (h_y1 and h_y2)
heart_x <- seq(-1.136, 1.136, 0.001)
heart_y_lower <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(-2, 0.6))$root)
heart_y_upper <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(0.6, 2))$root)

# put together data frame
heart_df <- data.frame(x = rep(heart_x, 2), 
                       y = c(heart_y_lower, heart_y_upper))

# show outline
with(heart_df, plot(x, y))

# create a data frame with one row per x, so we can fill in the heart
heart_df_minmax <- data.frame(x = heart_x,  
                              y_min = heart_y_lower, 
                              y_max = heart_y_upper)

set.seed(20150214)

# fill in the heart by generating random deviates at each x 
# and rejecting those that fall outside the heart curve
heart_full <- apply(heart_df_minmax, 
                    1, 
                    function(w) {
                      x <- w["x"]
                      y_min = w["y_min"]
                      y_max = w["y_max"]
                      y <- rnorm(2, mean = 0.33)
                      y <- y[between(y, y_min, y_max)]
                      x <- x[any(is.finite(y))]
                      data.frame(x, y, row.names = NULL)
                    })

# change from list to data frame
heart_full <- bind_rows(heart_full)

# add random numbers for color and size
heart_full <- heart_full %>% 
  mutate(z1 = runif(n()), 
         z2 = pmin(abs(rnorm(n())), 3), 
         order = runif(n())) %>%
  arrange(order)
# colnames(heart_full) <- c("20240520"," "," "," ","orders")
#------------------------------------------------------------------------------#

# plot the heart
p <- ggplot(heart_full, 
            aes(x, y, color = z1, size = z2)) + 
  geom_point(pch = -1 * as.hexmode(9829)) + 
  scale_color_gradient(limits = c(0, 1), low = "red2", high = "pink") + 
  scale_size(limits = c(0, 3), range = c(0.1, 20)) + 
  theme_bw() +
  labs(title = "吃好喝好 健康快乐 每一天～",  
       x = "Dear Mom",    
       y = "20240617",    
       color = "sth.strength",       
       size = "sth.size") +      
  theme(plot.title = element_text(family = "SimHei", face = "bold", color = "#6495ed", size = 24, hjust = 0.5),
        axis.title.x = element_text(family = "sans", face = "bold", color = "#ff4500", size = 14),
        axis.title.y = element_text(family = "SimHei", face = "bold", color = "#ff4500", size = 14),
        legend.title = element_text(family = "sans", face = "bold", color = "black", size = 11))

print(p)  
# png("valentine.png", 800, 600)
# # pdf("dingdingdang.pdf",width = 10,height = 7)
# print(p) 
# dev.off()

# animated plot
# saveGIF({
#   fill_steps <- 50  # heart fill-in frames
#   float_steps <- 25  # heart float-away frames
#   
#   for (i in seq(fill_steps + float_steps)) {
#     # find the number of hearts to fill in on this step
#     num_hearts <- min(i, fill_steps) * nrow(heart_full) / fill_steps
#     # once the heart is filled in, make the heart float away
#     # by shifting each point up some amount
#     if (i > fill_steps) {
#       j <- i - fill_steps
#       j_scale <- uniroot(function(x) (x * float_steps)^2 - 2.5, c(0, 1))$root
#       y_change <- (j_scale * j)^2
#       heart_full <- mutate(heart_full, y = y + y_change)
#     }
#     # plot the heart
#     p <- ggplot(heart_full[seq(num_hearts), ], 
#                 aes(x, y, color = z1, size = z2)) + 
#       geom_point(pch = -1 * as.hexmode(9829)) + 
#       scale_color_gradient(limits = c(0, 1), low = "red3", high = "pink") + 
#       scale_size(limits = c(0, 3), range = c(0.1, 20)) + 
#       theme_bw() + 
#       coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-1.25, 1.5))
#     print(p)
#   }
# }, 
# movie.name = "valentine.gif", 
# interval = 0.1, 
# nmax = 30, 
# ani.width = 600, 
# ani.height = 400)

