# COVID-19 Massachusetts COVID-19 ostpitalization prediction model

library(ggplot2)

# Load data
t1 <- 1:24
t2 <- 1:24
deaths_vec <- c(3965,3873,3830,3830,3854,3892,3875,3856,3803,3716,
                3601,3617,3539,3542,3562,3436,3349,3229,3128,3102,
                3127,3101,2859,2767)
df1 <- data.frame(t=t1, y=deaths_vec[1:24])
df2 <- data.frame(t=t2, y=deaths_vec)

# Calculate exponential fit
exponential1.model <- lm(log(y) ~t, data=df1)
summary(exponential1.model)
linear1.model <- lm(y ~t, data=df1)
summary(linear1.model)
# lol cubic fit
cubic1.model <- lm(y ~ poly(t, degree=3), data=df1)
summary(cubic1.model)
# lol parabolic fit
parabolic1.model <- lm(y ~ poly(t, degree=2), data=df1)
summary(parabolic1.model)

#exponential2.model <- lm(log(y) ~t, data=df2)
#summary(exponential2.model)
#linear2.model <- lm(y ~t, data=df2)
#summary(linear2.model)
# lol cubic fit
#cubic2.model <- lm(y ~ poly(t, degree=3), data=df2)
#summary(cubic2.model)
# lol parabolic fit
#parabolic2.model <- lm(y ~ poly(t, degree=2), data=df2)
#summary(parabolic2.model)

# Calculate predicted values
timevalues <- seq(1, 105, 0.1)
prediction1 <- exp(predict(exponential1.model, data.frame(t=timevalues)))
#prediction2 <- exp(predict(exponential2.model, data.frame(t=timevalues)))
pdf1 <- data.frame(t=timevalues, y=prediction1)
#pdf2 <- data.frame(t=timevalues, y=prediction2)

prediction3 <- predict(linear1.model, data.frame(t=timevalues))
#prediction4 <- predict(linear2.model, data.frame(t=timevalues))
pdf3 <- data.frame(t=timevalues, y=prediction3)
#pdf4 <- data.frame(t=timevalues, y=prediction4)

prediction5 <- predict(cubic1.model, data.frame(t=timevalues))
#prediction6 <- predict(cubic2.model, data.frame(t=timevalues))
pdf5 <- data.frame(t=timevalues, y=prediction5)
#pdf6 <- data.frame(t=timevalues, y=prediction6)

prediction7 <- predict(parabolic1.model, data.frame(t=timevalues))
#prediction8 <- predict(parabolic2.model, data.frame(t=timevalues))
pdf7 <- data.frame(t=timevalues, y=prediction7)
#pdf8 <- data.frame(t=timevalues, y=prediction8)

#ggplot() + geom_point(data=df2, aes(x=t, y=y)) +
#geom_line(data=data.frame(t=timevalues, y=prediction1), aes(x=t, y=y))
ggplot() + geom_point(data=df2, aes(x=t, y=y)) +
  geom_line(data=pdf1, aes(x=t, y=y), color="red") +
#  geom_line(data=pdf2, aes(x=t, y=y), color="blue") +
  geom_line(data=pdf3, aes(x=t, y=y), color="red") +
#  geom_line(data=pdf4, aes(x=t, y=y), color="blue") +
  geom_line(data=pdf5, aes(x=t, y=y), color="orange") +
#  geom_line(data=pdf6, aes(x=t, y=y), color="green") +
  geom_line(data=pdf7, aes(x=t, y=y), color="orange") +
#  geom_line(data=pdf8, aes(x=t, y=y), color="green") +
  xlab("Days since peak") + ylab("COVID-19 hospitalized cases") +
  xlim(0,60) + ylim(0,4000)
