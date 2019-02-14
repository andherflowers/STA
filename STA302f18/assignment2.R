#--------------------------------------------------------------
#Problem : Multivariate Linear Regression
#--------------------------------------------------------------

oil_url = "https://mcs.utm.utoronto.ca/~nosedal/datasets/oil-production.txt" #Enter data;
oil_data = read.table(oil_url, header = TRUE) #Import data in R;

summary(lm(oil.production ~ energy.consumption, data = oil_data))
summary(lm(oil.production ~ nuclear.electricity, data = oil_data))
summary(lm(oil.production ~ coal.production, data = oil_data))
summary(lm(oil.production ~ gas.production, data = oil_data))
summary(lm(oil.production ~ fuel.rate, data = oil_data))

#all_reg = lm(oil.production~.-oil.production, data=oil_data)
#(pick energy.consumption as our x1)
x1 = energy.consumption

summary(lm(oil.production ~ x1 + nuclear.electricity, data = oil_data))
summary(lm(oil.production ~ x1 + coal.production, data = oil_data))
summary(lm(oil.production ~ x1 + gas.production, data = oil_data))
summary(lm(oil.production ~ x1 + fuel.rate, data = oil_data))
#(pick fuel.rate as our x2, x1 is still significant, proceed to step 3)
x2 = fuel.rate

summary(lm(oil.production ~ x1 + x2 + nuclear.electricity, data = oil_data))
summary(lm(oil.production ~ x1 + x2 + coal.production, data = oil_data))
summary(lm(oil.production ~ x1 + x2 + gas.production, data = oil_data))

#(pick coal.production as our x3, x1 and x2 are non significant!)
#the final 2 variables model is therefore oil.production = energy.consumption + fuel.rate
