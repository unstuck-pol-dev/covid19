# COVID-19 New York death prediction model

library(ggplot2)

# Load data
t1 <- 1:16
t2 <- 1:67
deaths_vec <- c(799,777,783,758,671,778,758,606,630,540,
                507,478,481,474,438,422,437,367,337,335,
                330,306,289,299,280,226,230,232,231,216,
                226,207,161,195,166,157,132,157,139,106,
                105,112,105,109, 84,109, 96, 73, 74, 74,
                 67, 67, 56, 54, 58, 49, 52, 42, 35, 45,
                 39, 46, 53, 36, 42, 32, 23)
df1 <- data.frame(t=t1, y=deaths_vec[1:16])
df2 <- data.frame(t=t2, y=deaths_vec)

# Calculate exponential fit
exponential1.model <- lm(log(y) ~t, data=df1)
summary(exponential1.model)
linear1.model <- lm(y ~t, data=df1)
summary(linear1.model)
# lol cubic fit
cubic1.model <- lm(y ~ poly(t, degree=3), data=df1)
summary(cubic1.model)

exponential2.model <- lm(log(y) ~t, data=df2)
summary(exponential2.model)
linear2.model <- lm(y ~t, data=df2)
summary(linear2.model)
# lol cubic fit
cubic2.model <- lm(y ~ poly(t, degree=3), data=df2)
summary(cubic2.model)

# Calculate predicted values
timevalues <- seq(1, 105, 0.1)
prediction1 <- exp(predict(exponential1.model, data.frame(t=timevalues)))
prediction2 <- exp(predict(exponential2.model, data.frame(t=timevalues)))
pdf1 <- data.frame(t=timevalues, y=prediction1)
pdf2 <- data.frame(t=timevalues, y=prediction2)

prediction3 <- predict(linear1.model, data.frame(t=timevalues))
prediction4 <- predict(linear2.model, data.frame(t=timevalues))
pdf3 <- data.frame(t=timevalues, y=prediction3)
pdf4 <- data.frame(t=timevalues, y=prediction4)

prediction5 <- predict(cubic1.model, data.frame(t=timevalues))
prediction6 <- predict(cubic2.model, data.frame(t=timevalues))
pdf5 <- data.frame(t=timevalues, y=prediction5)
pdf6 <- data.frame(t=timevalues, y=prediction6)

#ggplot() + geom_point(data=df2, aes(x=t, y=y)) +
#geom_line(data=data.frame(t=timevalues, y=prediction1), aes(x=t, y=y))
ggplot() + geom_point(data=df2, aes(x=t, y=y)) +
geom_line(data=pdf1, aes(x=t, y=y), color="red") +
  geom_line(data=pdf2, aes(x=t, y=y), color="blue") +
  geom_line(data=pdf3, aes(x=t, y=y), color="red") +
  geom_line(data=pdf4, aes(x=t, y=y), color="blue") +
  geom_line(data=pdf5, aes(x=t, y=y), color="orange") +
  geom_line(data=pdf6, aes(x=t, y=y), color="green") +
  xlab("Days since peak") + ylab("Daily fatalities") +
  xlim(0,60) + ylim(0,800)
