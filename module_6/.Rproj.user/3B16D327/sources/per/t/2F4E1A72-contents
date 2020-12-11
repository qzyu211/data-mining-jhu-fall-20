library(tidyverse); library(e1071); library(cvms); library(tibble); library(broom)

### Problem 1a
q1a <- function(x1, x2) {
  return(1 + 3*x1 - x2)
}

x1s <- seq(-1.5, 1.5, length.out = 1e2)
x2s <- seq(-1.5, 1.5, length.out = 1e2)

# Reference: https://selbydavid.com/2018/01/09/neural-network/
decision_grid <- expand.grid(x1 = x1s, x2 = x2s)
decision_grid_mat <- data.matrix(decision_grid[,c('x1','x2')])
q1a_grid_output <- mapply(q1a, decision_grid_mat[,1], decision_grid_mat[,2])
decision_grid$class_label <- factor(x = q1a_grid_output > 0, labels = c('Blue', 'Red'))

# Reference: https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
theme_update(plot.title = element_text(hjust = 0.5))

# Reference: https://stackoverflow.com/questions/43129280/color-points-with-the-color-as-a-column-in-ggplot2
p1 <- ggplot(data = decision_grid, aes(x = x1, y = x2)) +
  geom_point(colour = decision_grid$class_label, size = 0.1) +
  # Reference: http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
  geom_abline(intercept = 1, slope = 3) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 1a') +
  theme_minimal() + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### Problem 1b
x1s <- seq(-1.5, 1.5, length.out = 1e2)
x2s <- seq(-1.5, 1.5, length.out = 1e2)
q1b <- function(x1, x2) {
  return(-2 + x1 + 2*x2)
}
decision_gridb <- expand.grid(x1 = x1s, x2 = x2s)
decision_grid_matb <- data.matrix(decision_gridb[,c('x1','x2')])
q1b_grid_output <- mapply(q1b, decision_grid_matb[,1], decision_grid_matb[,2])
decision_gridb$class_label <- factor(x = q1b_grid_output > 0, labels = c('Green', 'Purple'))

# Shrink the # of points in the geom_point
x1s <- seq(-1.5, 1.5, length.out = 20)
x2s <- seq(-1.5, 1.5, length.out = 20)
decision_grid <- expand.grid(x1 = x1s, x2 = x2s)
decision_grid_mat <- data.matrix(decision_grid[,c('x1','x2')])
q1a_grid_output <- mapply(q1a, decision_grid_mat[,1], decision_grid_mat[,2])
decision_grid$class_label <- factor(x = q1a_grid_output > 0, labels = c('Blue', 'Red'))

p2 <- ggplot(data = decision_grid, aes(x = x1, y = x2)) +
  # Reference: http://www.math-evry.cnrs.fr/_media/members/cambroise/teaching/tp_r_part1_corrected.pdf
  geom_tile(data = decision_gridb, aes(fill = class_label)) +
  # Reference: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  # Reference: https://stackoverflow.com/questions/25195869/how-to-change-color-palette-of-geom-tile-in-r-ggplot2
  # Reference: https://stackoverflow.com/questions/25176399/scale-fill-discrete-and-scale-fill-manual-legend-options-confusion
  scale_fill_manual(values=c("#99FF33", "#3300FF"), name = 'Class Label') +
  # Reference: https://stackoverflow.com/questions/47023781/how-to-add-a-legend-for-two-geom-layers-in-one-ggplot2-plot
  geom_point(aes(colour = class_label), size = 0.5) +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = 'Class Label') +
  geom_abline(intercept = 1, slope = 3) +
  geom_abline(intercept = 1, slope = -0.5, color = 'red') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme_minimal() + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 1b') +
  guides(shape = FALSE,
         colour = guide_legend(override.aes = list(
           fill = c("#99FF33", "#3300FF"),
           size = c(3, 3),
           shape = c(16, 16))))

### Problem 2
### part (a)
# Reference: https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
plot_circle <- function(center = c(0,0), diameter = 1, npoints = 1e3){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circle_data <- plot_circle(center = c(-1, 2), diameter = 2*2, npoints = 1e3)

p3 <- ggplot(circle_data, aes(x,y)) +
  geom_path() +
  # Reference: https://stackoverflow.com/questions/21294196/how-do-i-make-my-facets-perfectly-square
  theme(aspect.ratio = 1) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 2a') +
  theme_minimal() + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### part (b)
# (-4,5), (2,5), (-4,-1), (1,-1)
fill_area <- data.frame(x1 = c(-4), x2 = c(2),
                        y1 = c(-1), y2 = c(5), class_label = c('Blue'))
circle_data$class_label <- 'Red'

p4 <- ggplot() +
  # Reference: http://sape.inf.usi.ch/quick-reference/ggplot2/geom_rect
  # Reference: https://stackoverflow.com/questions/50343911/remove-border-from-geom-rect-using-ggplot2
  # Reference: https://stackoverflow.com/questions/31599146/ggplot2-change-geom-rect-colour-in-a-stacked-barplot
  geom_rect(data = fill_area,
            mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = '#0099FF',
            color = NA, alpha = 0.5) +
  geom_polygon(data = circle_data, mapping = aes(x, y), fill = "#CC0033", alpha = 0.5) +
  # Reference: https://stackoverflow.com/questions/21294196/how-do-i-make-my-facets-perfectly-square
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 2a') +
  # Reference: https://stackoverflow.com/questions/45346885/center-plot-title-in-ggplot2-using-theme-bw
  theme_minimal() + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(name = 'the colour', 
                      values =c('#0099FF'='black','#CC0033'='red'), labels = c('c2','c1'))

### Problem 3
### part (a)
df <- data.frame(x1 = c(3,2,4,1,2,4,4),
                 x2 = c(4,2,4,4,1,3,1),
                 y = rep(c('Red', 'Blue'), each = 4)[-8])

p5 <- ggplot(data = df, mapping = aes(x = x1, y = x2, colour = y)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(aes(colour = y), size = 1.5) +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = expression(y)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 3a') +
  theme_minimal() + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### part (b)
p6 <- ggplot(data = df, mapping = aes(x = x1, y = x2, colour = y)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(aes(colour = y), size = 1.5) +
  geom_abline(slope = 1, intercept = -0.5) +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = expression(y)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 3b') +
  theme_minimal() + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### part (d)
p7 <- ggplot(data = df, mapping = aes(x = x1, y = x2, colour = y)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(aes(colour = y), size = 1.5) +
  geom_abline(slope = 1, intercept = -0.5) +
  geom_abline(slope = 1, intercept = -1, linetype = 'dashed') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = expression(y)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 3d') +
  theme_minimal() + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### part (e)
# y = x - 0.5
# y - y1 = (x - x1)
# (2,2)
# y - 2 = x - 2
# y = x
d=data.frame(x=c(1,2,5,6,8), y=c(3,6,2,8,7), vx=c(1,1.5,0.8,0.5,1.3), vy=c(0.2,1.3,1.7,0.8,1.4))

margin_points <- data.frame(
  x = c(2, 2, 4, 4),
  y = c(2, 1, 4, 3),
  vx = c(0.25, -0.25, 0.25, -0.25),
  vy = c(-0.25, 0.25, -0.25, 0.25)
)

p8 <- ggplot(data = df, mapping = aes(x = x1, y = x2, colour = y)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(aes(colour = y), size = 1.5) +
  geom_abline(slope = 1, intercept = -0.5) +
  geom_abline(slope = 1, intercept = -1, linetype = 'dashed') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  # Reference: http://sape.inf.usi.ch/quick-reference/ggplot2/geom_segment
  geom_segment(data=margin_points, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=0.5, color="green") +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = expression(y)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 3e') +
  xlim(0, 5) + theme_minimal() +
  theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### part (g)
p9 <- ggplot(data = df, mapping = aes(x = x1, y = x2, colour = y)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(aes(colour = y), size = 1.5) +
  geom_abline(slope = -0.5, intercept = 3.5) +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = expression(y)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 3f') +
  xlim(0, 5) + theme_minimal() +
  theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### part (h)
df2 <- data.frame(x1 = c(3,2,4,1,2,4,4,2),
                 x2 = c(4,2,4,4,1,3,1,3),
                 y = rep(c('Red', 'Blue'), each = 4))

p10 <- ggplot(data = df2, mapping = aes(x = x1, y = x2, colour = y)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(aes(colour = y), size = 1.5) +
  geom_abline(slope = 1, intercept = -0.5) +
  geom_abline(slope = 1, intercept = -1, linetype = 'dashed') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = expression(y)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 3f') +
  xlim(0, 5) + theme_minimal() +
  theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))

### Problem 4
set.seed(111)
x1_class1 <- rnorm(50)
x1_class2 <- rnorm(50, mean = 0.5)
x2_class1 <- 1.5 * x1_class1^(2) + 0.5 + runif(50)
x2_class2 <- 1.5 * x1_class2^(2) + - 0.5 - runif(50)

df <- data.frame(x = c(x1_class1, x1_class2),
                 y = c(x2_class1, x2_class2),
                 z = rep(c(1,2), each = 50))

p11 <- ggplot(df, aes(x, y, color = factor(z))) +
  geom_point() +
  scale_color_manual(values=c("#0099FF", "#CC0033"), name = 'Class Label') +
  theme_minimal() +
  theme(aspect.ratio = 0.55, plot.title = element_text(hjust = 0.5)) +
  labs(x = expression(x[1]), y = expression(x[2]), title = 'Problem 4')

set.seed(666)
train_idx <- sample(seq(1,1e2), 80)

data.train <- data.frame(x = df[train_idx,c('x')],
                         y = df[train_idx,c('y')],
                         z = as.factor(df$z[train_idx]))

data.test <- data.frame(x = df[-train_idx,'x'],
                        y = df[-train_idx,'y'],
                        z = as.factor(df$z[-train_idx]))

svm.linear <- svm(z~., data=data.train, kernel="linear", cost=10)
plot(svm.linear, data.train)
table(df$z[train_idx], predict(svm.linear, data.train))

svm.poly = svm(z~., data=data.train, kernel="polynomial", cost=10)
plot(svm.poly, data.train)
table(df$z[train_idx], predict(svm.poly, data.train))

svm.radial = svm(z~., data=data.train, kernel="radial", gamma=1, cost=10)
plot(svm.radial, data.train)
table(df$z[train_idx], predict(svm.radial, data.train))

plot(svm.linear, data.test)
plot(svm.poly, data.test)
plot(svm.radial, data.test)
table(df$z[-train_idx], predict(svm.linear, data.test))
table(df$z[-train_idx], predict(svm.poly, data.test))
table(df$z[-train_idx], predict(svm.radial, data.test))

linear_cm_train <- tidy(table(tibble("target"=df$z[train_idx],
                               "prediction"=predict(svm.linear, data.train))))
plot_confusion_matrix(linear_cm_train, 
                      targets_col = "target", 
                      predictions_col = "prediction",
                      counts_col = "n")

poly_cm_train <- tidy(table(tibble("target"=df$z[train_idx],
                             "prediction"=predict(svm.poly, data.train))))
plot_confusion_matrix(poly_cm_train, 
                      targets_col = "target", 
                      predictions_col = "prediction",
                      counts_col = "n")

radial_cm_train <- tidy(table(tibble("target"=df$z[train_idx],
                               "prediction"=predict(svm.radial, data.train))))
plot_confusion_matrix(radial_cm_train, 
                      targets_col = "target", 
                      predictions_col = "prediction",
                      counts_col = "n")


linear_cm <- tidy(table(tibble("target"=df$z[-train_idx],
                               "prediction"=predict(svm.linear, data.test))))
plot_confusion_matrix(linear_cm, 
                      targets_col = "target", 
                      predictions_col = "prediction",
                      counts_col = "n")

poly_cm <- tidy(table(tibble("target"=df$z[-train_idx],
                               "prediction"=predict(svm.poly, data.test))))
plot_confusion_matrix(poly_cm, 
                      targets_col = "target", 
                      predictions_col = "prediction",
                      counts_col = "n")

radial_cm <- tidy(table(tibble("target"=df$z[-train_idx],
                               "prediction"=predict(svm.radial, data.test))))
plot_confusion_matrix(radial_cm, 
                      targets_col = "target", 
                      predictions_col = "prediction",
                      counts_col = "n")

