CBDtrain <- read.csv("CBDtrain.csv", row.names = 1)
CBDtest <- read.csv("CBDtestnoY.csv", row.names = 1)
attach(CBDtrain)

newADJOE <- (ADJOE)^(-0.2)
finalmodel <- lm(W.P ~ EFG_O + EFG_D + TORD + DRB + WAB + X500.Level + newADJOE:ADJDE + ADJOE:TOR + ORB:FTR, data = CBDtrain)
summary(finalmodel) #R-squared 80.54%

par(mfrow = c(2, 2))
plot(finalmodel) 

only_numeric <- lm(W.P ~ EFG_O + EFG_D + TORD + DRB + WAB + newADJOE:ADJDE + ADJOE:TOR + ORB:FTR)
plot(only_numeric)
par(mfrow = c(2, 2))

#All assumptions are not violated. 
#The two blots on Residuals vs Fitted and Standardized Residuals plots are due to the categorical variable X500.Level. 
#This has been proven by plotting only_numeric and showing that the points in the plots are random and have no pattern.

testdata <- data.frame(EFG_O = CBDtest$EFG_O, EFG_D = CBDtest$EFG_D, TORD = CBDtest$TORD, 
                       DRB = CBDtest$DRB, WAB = CBDtest$WAB, X500.Level = CBDtest$X500.Level, 
                       newADJOE = (CBDtest$ADJOE)^(-0.2), ADJDE = CBDtest$ADJDE, ADJOE = CBDtest$ADJOE, TOR = CBDtest$TOR, 
                       ORB = CBDtest$ORB, FTR = CBDtest$FTR)
wp_predict <- predict(finalmodel, newdata = testdata)
testing <- data.frame(Ob = 1:length(wp_predict), W.P = wp_predict)
write.csv(testing, file = "testsolution.csv", row.names = FALSE, quote = FALSE)
#Kaggle submission R-squared 80.553%