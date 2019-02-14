stocks_url = "https://mcs.utm.utoronto.ca/~nosedal/data/stocks.txt"
sdata = read.table(stocks_url, header = TRUE, sep = "\t", fill=TRUE)
young = sdata[1:84, 3]
early = sdata[1:131, 1]
late = sdata[1:93, 2]
senior = sdata[1:58, 4]

clean_data = data.frame(y=c(early, late, young, senior),
                   age=factor(rep(c("early", "late", "young", "old"),
                   times=c(length(early), length(late), length(young), length(senior)))))

oneway.test(y~age, data = clean_data,var.equal=TRUE)


boxplot(cbind(Young,EarlyMiddleAge,LateMiddleAge,Senior))



model = lm(y~age-1, data = clean_data)
summary(model)
qqnorm(residuals(model), main =
         "Normal Q-Q Plot of Residuals")
hist(residuals(model), xlab="Residuals", ylab="Frequency", main =
       "Histogram of Residuals")
