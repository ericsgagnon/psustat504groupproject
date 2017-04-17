library(glmnet)
library(epitools)

shots<-read.csv("shot_logs2.csv", head=TRUE)
shots$made<-ifelse(SHOT_RESULT=="made",1,0) #I thought it would be easier to interpret this way, with a 1 as a make and 0 as miss.

#Game clock is in a hard to use format, not sure how to convert it to make sense at the moment.  Maybe we just ignore this.
#Subsequent models are ignoring game clock, player name
#I'm not sure how shot clock=24 when shot is taken or touch time is negative.
shots<-subset(shots, SHOT_CLOCK<24)
shots<-subset(shots, TOUCH_TIME>0)

attach(shots)
full.model<-glm(made~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit))
summary(full.model)  # what predictors should we talk about: based on p-values, position, shot number, period, and games played don't seem as important.


#lasso, takes a minute to run

factors<-model.matrix(made~SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, data=shots)[,-1]
made<-shots$made

set.seed(1)
cv.out=cv.glmnet(factors,made,alpha=1, family="binomial")
predict(cv.out, type="coefficients", s=cv.out$lambda.1se)
# Lasso shows that we should investigate, shot clock, touch time, shot dist, closest defender, age, and mpg.
# This is similar to the full model using glm and comparing pvalues.


##Exploring fewer predictors

#Reduced Logistic Model
reduced.model<-glm(made~LOCATION+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+age+GP+MPG, family=binomial(link=logit), data=shots.24)
summary(reduced.model)
anova(reduced.model)

#further reduced by the massive G^2 only, this model is almost as good as the previous without nearly as many predictors.
reduced.model2<-glm(made~SHOT_CLOCK+SHOT_DIST+CLOSE_DEF_DIST, family=binomial(link=logit), data=shots.24)
summary(reduced.model2)
anova(reduced.model2)

#shot distance seems to be of two categories, close shots and far shots. Splitting it at the free through line (15ft).
hist(shots$SHOT_DIST)
hist(SHOT_DIST[SHOT_DIST>15])
hist(SHOT_DIST[SHOT_DIST<15])
shots$dist<-ifelse(SHOT_DIST>15, ">15ft","<15ft")
tabdist<-table(shots$made, shots$dist)
epitab(tab1)

#odds of making a shot when further than average distance is about half of that when compared with shots of less than average distance.

#Dribbles wasn't significant in the full model, but when you compare shots taken with 0-2 dribbles, compared with 3+ dribbles you see a drastic difference.
#so I thought maybe we should analyze that category separately than the rest.

hist(shots$DRIBBLES)
shots.0drib<-subset(shots,shots$DRIBBLES==0)

hist(shots.0drib$SHOT_DIST) #distribution by distance seems to be approximately the same as the complete data set
tab.0drib<-table(shots.0drib$made, shots.0drib$dist)
epitab(tab.0drib)
#When considering the the odds of making a shot with 0 dribbles only there is a drastic improvement in the odds of a shot being a make (from 1.77 to 2.58 times as likely to make) - likely due to alley oops.

shots.1drib<-subset(shots,shots$DRIBBLES>=1)

tab.1drib<-table(shots.1drib$made, shots.1drib$dist)
tab.1drib
epitab(tab.1drib)
#A player taking 1 or more dribbles still has a higher odds of making a closer shot but the odds are now lower than the overall group.  It is clearly the 0 dribble group that impacts the odds of a shot make versus distance.

#investigating the association between position and shot result.


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

ggplot(data=shots, aes(x=SHOT_DIST, y=pos)) + geom_jitter(aes(color=factor(SHOT_RESULT))) + geom_vline(xintercept=c(15,22),color="black")
#The odds ratios appear to have a similar conclusion, pg, sf, sg all have a similar odds of ~.74 times the odds of a make than a center, whereas a pf is closer to that of center.

tabpos.make<-table(shots$pos, shots$made)
tabpos.make
oddsbypos<-epitab(tabpos.make)
oddsbypos

tabpos.type<-table(pos, PTS_TYPE)
tabpos.type
epitab(tabpos.type)
#This conclusion makes sense as the center's tend to stay closer to the basket and take more two point shots.
#A PF has 7 times the odds of taking a 3 pt shot and PG has 16 times the odds. 


#Reference: https://www.kaggle.com/erikbabb/d/dansbecker/nba-shot-logs/itm-6285-shot-prediction

hist(SHOT_CLOCK)

