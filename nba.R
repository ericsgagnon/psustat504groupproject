
shots<-read.csv("shot_logs2.csv", head=TRUE)
shots$made<-ifelse(SHOT_RESULT=="made",1,0) #I thought it would be easier to interpret this way, with a 1 as a make and 0 as miss.

#Game clock is in a hard to use format, not sure how to convert it to make sense at the moment.  Maybe we just ignore this.
#Subsequent models are ignoring game clock, player name
attach(shots)
full.model<-glm(made~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit))
summary(full.model)  # what predictors should we talk about: based on p-values, position, shot number, period, and games played don't seem as important.

#lasso, takes a minute to run
library(glmnet)
factors<-model.matrix(made~SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, data=shots)[,-1]
made<-shots$made

set.seed(1)
cv.out=cv.glmnet(factors,made,alpha=1, family="binomial")
predict(cv.out, type="coefficients", s=cv.out$lambda.1se)
# Lasso shows that we should investigate, shot clock, touch time, shot dist, closest defender, age, and mpg.
# This is similar to the full model using glm and comparing pvalues.


##Exploring some predictors
#shot distance seems to be of two categories, close shots and far shots.  I split it by the mean (~13.5ft).
hist(shots$SHOT_DIST)
hist(SHOT_DIST[SHOT_DIST>mean(SHOT_DIST)])
hist(SHOT_DIST[SHOT_DIST<mean(SHOT_DIST)])
shots$dist<-ifelse(SHOT_DIST>mean(SHOT_DIST), ">AVGft","<AVGft")
detach(shots)

tab1<-table(shots$made, shots$dist)
library(epitools)
epitab(tab1)
#odds of making a shot when further than average distance is about half of that when compared with shots of less than average distance.

#Dribbles wasn't significant in the full model, but there are a large amount of shots with 0 dribbles, 
#so I thought maybe we should analyze that category separately than the rest.

hist(shots$DRIBBLES)
shots.0drib<-subset(shots,shots$DRIBBLES==0)

hist(shots.0drib$SHOT_DIST)
tab.0drib<-table(shots.0drib$made, shots.0drib$dist)
epitab(tab.0drib)
#I think a clear explanation for the drastic improvement in the odds of a shot being a make is that at 0 dribbles, we are probably talking about alley oops wide open lay ups.

#There are like 4000 shots with shot clock = 0.  I'm not sure how that even happens... should we just remove them? ... to be continued



x<-split(shots, shots$pos)
shots.c<-x$C
shots.pf<-x$PF
shots.sg<-x$SG
shots.pg<-x$PG
shots.sf<-x$SF

#based on the graphs it looks like the positions sg, pg, and sf take similar shots, while pf and c take similar shots.
hist(shots.c$SHOT_DIST)
hist(shots.pf$SHOT_DIST)

hist(shots.sf$SHOT_DIST)
hist(shots.sg$SHOT_DIST)
hist(shots.pg$SHOT_DIST)

