library(RCPA3)
library(labelled)
load("gvpt201_S24finalsurvey (1).RData")

gvpt201_S24final$Q9_2 #How supportive (1-100) of the US making deep cuts (e.g., billions of dollars) to the amount of money allotted to the military 

gvpt201_S24final$Q17_8 #How supportive (1-100) of the US outlawing civilians from purchasing assault weapons (e.g., high capacity, semi-automatic machine guns)

gvpt201_S24final$military_cuts.n <- as.numeric(gvpt201_S24final$Q9_2)
describeC(gvpt201_S24final$military_cuts.n)



gvpt201_S24final$assault_ban.n <- as.numeric(gvpt201_S24final$Q17_8)
describeC(gvpt201_S24final$assault_ban.n)

gvpt201_S24final$assault_ban <- transformC(type = "cut",
                                       x=assault_ban.n,
                                       data = gvpt201_S24final,
                                       cutpoints = 50,
                                       confirm = F)
levels(gvpt201_S24final$assault_ban)<- c("No", "Yes")
freqC(gvpt201_S24final$assault_ban, plot = F) #transformed/labelled correctly
#looking at how original is distributed
freqC(gvpt201_S24final$assault_ban.n)

freqC(gvpt201_S24final$Q5)

gvpt201_S24final$dems.yes = transformC(type = "dummy",
                                       x= Q5,
                                       data = gvpt201_S24final,
                                       response = "Democrat",
                                       confirm = F)
freqC(gvpt201_S24final$dems.yes, plot = F)
levels(gvpt201_S24final$dems.yes)= c("No", "Yes")
freqC(gvpt201_S24final$dems.yes, plot = F)



freqC(gvpt201_S24final$Q24)
gvpt201_S24final$vote_trump = transformC(type = "dummy",
                                       x= Q24,
                                       data = gvpt201_S24final,
                                       response = "Donald Trump",
                                       confirm = F)
freqC(gvpt201_S24final$vote_trump, plot = F)
levels(gvpt201_S24final$vote_trump)= c("No", "Yes")
freqC(gvpt201_S24final$vote_trump, plot = F)


freqC(gvpt201_S24final$Q1)
gvpt201_S24final$Male = transformC(type = "dummy",
                                         x= Q1,
                                         data = gvpt201_S24final,
                                         response = "Male",
                                         confirm = F)
freqC(gvpt201_S24final$Male, plot = F)
levels(gvpt201_S24final$Male)= c("No", "Yes")
freqC(gvpt201_S24final$Male, plot = F)


histC(x=military_cuts.n, data = gvpt201_S24final, xlab = "R's support (1-100) of the US making deep cuts (e.g., billions of dollars) \n to the amount of money allotted to the military  (0= very against, 100= very for)", main = "Histogram 1", bar.col="lightblue", plot = T)


barplot(table(gvpt201_S24final$assault_ban),
        ylab = "Frequency",
        xlab = "Does R support the US outlawing civilians purchasing assualt weapons?",
        main= "Histogram 2",
        ylim = c( 0 , 1400 ),
        col="brown")


#first model, liner regression, DV is interval
model1 <- regC(military_cuts.n~vote_trump + dems.yes + assault_ban , data = gvpt201_S24final)

#second model, logistic regression, DV is binary
model2 <- logregC(assault_ban~vote_trump + Male + military_cuts.n, data = gvpt201_S24final)

