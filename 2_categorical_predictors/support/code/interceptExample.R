### Title:    Intercept Interpretation Example
### Author:   Kyle M. Lang
### Created:  2021-03-23
### Modified: 2021-03-23

rm(list = ls(all = TRUE))

library(ggplot2)

set.seed(235711)

n  <- 500
r2 <- 0.33

x <- sample(-10 : 10, n, TRUE)

s2 <- (var(x) / r2) - var(x)
y  <- 5 + x + rnorm(n, 0, sqrt(s2))

fit <- lm(y ~ x)

int <- as.numeric(coef(fit)[1])
m   <- mean(y[x == 0])

p1 <- ggplot(mapping = aes(x = x, y = y, color = (x == 0))) +
    geom_point() +
    theme_classic() +
    scale_color_manual(values = c("darkgray", "red")) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme(legend.position = "none") +
    xlab("X") +
    ylab("Y")

p1 <- p1 + geom_point(mapping = aes(x = 0, y = m),
                      color   = "black",
                      shape   = 4,
                      size    = 5) +
    geom_hline(yintercept = int, linetype = "solid") +
    geom_hline(yintercept = m, linetype = "dashed")

labs <- c(as.expression(bquote(beta[0] == .(round(int, 2)))),
          as.expression(bquote(bar(italic(Y))[italic(X)==0] == .(round(m, 2))))
          )

p1 + annotate("rect",
              xmin = c(-9, 5),
              xmax = c(-5, 9),
              ymin = c(5.5, -1.5),
              ymax = c(8.5, 1.5),
              fill = "white",
              color = "black") +
    annotate(geom = "text",
             x = c(-7, 7),
             y = c(7, 0),
             label =  labs,
             color = "black",
             fontface = "bold")
