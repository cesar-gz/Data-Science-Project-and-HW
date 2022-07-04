# problem 1, part a
# Load the autompg.csv file and convert cylinders variable to a factor. (code, output of str())
autompg <- read_csv("autompg.csv")
autompg$cylinders <- as.factor(autompg$cylinders)
str(autompg)

# 1, part c, Plot mpg vs. displacement (code, plot)
mpgdata %>% ggplot() + geom_point(mapping=aes( x=displacement, y=mpg))

# 1, d
# Create a linear model called mod_displ of mpg vs. displacement (only one independent variable). What is the R2 value? (code, output of summary(mod_displ), R2 value)
mod_displ <- lm(data=autompg, mpg~displacement)
summary(mod_displ)

# 1, e
# Plot a distribution of the residuals from mod_displ (code, plot)
autompg <- autompg %>% add_residuals(mod_displ)
ggplot(data=autompg) + geom_histogram(mapping = aes(x=resid), bins=35)

# 1, f
# Predict the mpg of three cars with engine displacements 50 cu in, 100 cu in, and 500 cu in each. Which prediction has the tightest bounds? (code, output)
predx <- data.frame(displacement=c(50,100,500))
predict(mod_displ, predx)
predict(mod_displ, newdata=predx, interval = "prediction")
predict(mod_displ, newdata=predx, interval = "confidence")

# 1, g
# Create a linear model called mod_displ_cyl of mpg vs. displacement and cylinders. 
mod_displ_cyl <- lm(data=autompg, formula = mpg~cylinders + displacement)

autompg <- autompg %>% add_predictions(mod_displ_cyl)
ggplot(data=autompg) + geom_point(mapping=aes(y=mpg, x=displacement)) + geom_line(mapping=aes(x=displacement, y=pred, color=cylinders))

# 1, h
# Create a new transformed variable logdispl=log(displacement). Create a linear model called mod_logdispl of mpg vs. logdispl. 
logdispl=log(autompg$displacement)
mod_logdispl <- lm(data=autompg, mpg~logdispl)
summary(mod_logdispl)

# Plot mpg vs. logdispl and overlay the best fit model as a straight line. (code, plot)
autompg <- autompg %>% add_predictions(mod_logdispl)
ggplot(data=autompg) + geom_point(mapping=aes(y=mpg, x=logdispl)) + geom_line(mapping=aes(x=logdispl, y=pred, color=cylinders))

# Give the model equation relating mpg with displacement
lm(data=autompg, formula=mpg~displacement)

# Plot mpg vs. displacement and overlay the best fit model as a curve.
ggplot(data=autompg) + geom_point(mapping=aes(y=mpg, x=displacement)) + geom_line(mapping=aes(x=displacement, y=pred, color=cylinders))

