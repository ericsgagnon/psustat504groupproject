# code workspace - eric gagnon

Sys.setenv(MAKE = 'make -j 8')
#devtools::install_github("hadley/dplyr")
#devtools::install_github("mdsumner/spdplyr")
library(MASS)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(data.table)
library(lubridate)
library(RPostgreSQL)
library(plotly)
#library(rbokeh)
library(jsonlite)
library(htmltools)
library(glmnet)
library(epitools)
library(broom)
library(lme4)
#################################

# GOALS: 
# Optimize Efficiency vs Effectivess
# Maximize Estimated Value of shot 

# Show Dataset

d.shots <- 
    fread( 'data/shot_logs-1.csv') %>% 
    .[ , Scored := case_when( SHOT_RESULT == 'made' ~ TRUE , 
                              SHOT_RESULT == 'missed' ~ FALSE ) ]

# Update Game clock to represent number of seconds (numeric)
# bug with using some lubridate functions inside data.table - using tmp var as workaround
tmp <- d.shots$GAME_CLOCK %>% ms %>% as.numeric()
d.shots[ , GAME_CLOCK := { tmp } ]

# Player Dataset : 
#   create player_name column to join with shot data
#   manually update player_name column for mismatches between two sources

d.players <- 
    fread('data/players.csv')%>%
    .[ , player_name := Player ] %>% 
    .[ Player == 'allen crabbe' , player_name := 'alan crabbe' ] %>% 
    .[ Player == 'steven adams' , player_name := 'steve adams' ] %>% 
    .[ Player == 'dwyane wade' , player_name := 'dwayne wade' ] %>% 
    .[ Player == 'danilo gallinari' , player_name := 'danilo gallinai' ] %>% 
    .[ Player == 'dirk nowitzki' , player_name := 'dirk nowtizski' ] %>% 
    .[ Player == 'tim hardaway' , player_name := 'time hardaway jr' ] %>% 
    .[ Player == 'beno udrih' , player_name := 'beno urdih' ] %>% 
    .[ Player == 'al-farouq aminu' , player_name := 'al farouq aminu' ] %>% 
    .[ Player == 'jj barea' , player_name := 'jose juan barea' ] %>% 
    .[ Player == 'monta ellis' , player_name := 'mnta ellis' ] %>% 
    .[ Player == 'nerlens noel' , player_name := 'nerles noel' ] %>% 
    .[ Player == 'jimmer fredette' , player_name := 'jimmer dredette' ] %>% 
    .[ Player == 'joe ingles' , player_name := 'jon ingles' ] %>% 
    .[ Player == 'j.j. hickson' , player_name := 'jj hickson' ]

# Confirm no players from the shots data set are missing from the player data
# setdiff( { d[ , unique( player_name ) ] } , { d.players[ , unique( player_name )]})

# Create Working data set

d <- 
    d.shots[ d.players , , on = c( 'player_name' ) , nomatch = 0 ]

## EDA Visuals ########################################################################

# Points per Shot vs Points
d %>% 
    select( player_id , GAME_ID , Position = Pos , PTS , Scored ) %>% 
    group_by( player_id , GAME_ID , Position ) %>% 
    summarise( Points = sum( PTS ) , PointsPerShot = mean( PTS )  ) %>% {
    ggplot( . , aes( Points , PointsPerShot , colour = Position ) ) + 
    geom_point() + 
    geom_density_2d( ) + 
    ggtitle( 'Points Per Shot vs Points' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly

# Baskets per Shot vs Baskets
d %>% 
    select( player_id , GAME_ID , Position = Pos , PTS , Scored ) %>% 
    group_by( player_id , GAME_ID , Position ) %>% 
    summarise( Baskets = sum( Scored ) , BasketsPerShot = mean( Scored )  ) %>% {
    ggplot( . , aes( Baskets , BasketsPerShot , colour = Position ) ) + 
    geom_point() + 
    geom_density_2d( ) + 
    ggtitle( 'Baskets Per Shot vs Baskets' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly

# Baskets vs Shots
d %>% 
    select( player_id , GAME_ID , Position = Pos , PTS , Scored ) %>% 
    group_by( player_id , GAME_ID , Position ) %>% 
    summarise( Baskets = sum( Scored ) , Shots = n()  ) %>% {
    ggplot( . , aes( Shots , Baskets , colour = Position ) ) + 
    geom_point() + 
    geom_smooth( method = 'loess' ) +
    ggtitle( 'Baskets vs Shots' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly

# Effectivness at distances
# d %>% 
#     select( player_id , GAME_ID , Position = Pos , PTS , Scored , ) %>% 
#     group_by( player_id , GAME_ID , Position ) %>% 
#     summarise( Baskets = sum( Scored ) , Shots = n()  ) %>% {
#     ggplot( . , aes( Shots , Baskets , colour = Position ) ) + 
#     geom_point() + 
#     geom_smooth( method = 'loess' ) +
#     ggtitle( 'Baskets vs Shots' , subtitle = 'Per Game - By Position')  } %>% 
#     ggplotly
# 
# d %>% 
#     select( 
#         player_id , 
#         GAME_ID , 
#         Position = Pos , 
#         PTS , 
#         Scored , 
#         SHOT_CLOCK ,
#         DRIBBLES ,
#         TOUCH_TIME ,
#         SHOT_DIST ,
#         PTS_TYPE ,
#         CLOSE_DEF_DIST
#         
#         ) %>% 
#     group_by( player_id , GAME_ID , Position ) %>% 
#     summarise( Baskets = sum( Scored ) , Shots = n()  )

# d %>% melt( id.vars = c( 'player_id' , 'GAME_ID' , 'Pos' ) )
# 
#     ggplot( aes( points ) ) + facet_wrap( ~Pos ) + geom_histogram()
# d[ , .( Scored = { factor( SHOT_RESULT ) } , ShotDistance = SHOT_DIST )] %$%    
#     cdplot( Scored ~ ShotDistance )
# 
# 
# 
#     cdplot( factor( Scored , levels = c('FALSE','TRUE') ) ~ SHOT_DIST )
# 
# 
# 
# 
#     ggplot( aes( points , PointsPerShot , colour = Pos ) ) + geom_density_2d( aes( colour = Pos ))
# 
#     ggplot( aes( points ) ) + facet_wrap( ~Pos ) + geom_histogram()
# d %>% 
#     group_by( player_id , GAME_ID , Pos ) %>% 
#     summarise( points = sum( PTS ) , baskets = sum( Scored ) , PointsPerShot = mean( PTS ) , BasketsPerShot = mean( Scored )  ) %>% 
#d %>% { names(.) %<>% str_replace_all("[:punct:]" , " ") %>% str_to_title() } %>% print

## Efficiency vs Effectiveness ################################################

#k <-
d %>% 
    select( player_id , GAME_ID , Position = Pos , PTS , Scored ) %>% 
    group_by( player_id , GAME_ID , Position ) %>% 
    summarise( PointsPerGame = sum( PTS ) , PointsPerShot = mean( PTS )  ) %$%
    
    select( )
    
    
## Model #######################################################################
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

m <- 
    d[ , .( Points = PTS , Scored , Game = as.factor(GAME_ID) , Player = as.factor( player_name) , 
            Position = as.factor(Pos) , ShotDistance = SHOT_DIST , 
            ClosestDefenderDistance = CLOSE_DEF_DIST , ShotClock = SHOT_CLOCK , 
            Dribbles = DRIBBLES , TouchTime = TOUCH_TIME , GameClock = GAME_CLOCK ,
            ShotType = { as.factor( PTS_TYPE ) } ) ] %>% 
    .[ !is.na( ShotClock ) , ] %$% 
    glmer( 
        Scored ~ ShotType * ShotDistance + ClosestDefenderDistance + ShotClock + ( 1 | Player / Game ) ,
        family = binomial( link = 'logit' )    
    )



        family = poisson(link = "log")    
        Points ~ ShotDistance + ClosestDefenderDistance + ShotClock + ( Pionts | Position / Player / Game ) ,
     #   PTS ~ SHOT_DIST * CLOSE_DEF_DIST * SHOT_CLOCK + ( PTS | Pos / player_id / GAME_ID ) ,
    # select( 
    #     player_id ,
    #     GAME_ID ,
    #     Pos ,
    #     PTS ,
    #     Scored ,
    #     SHOT_CLOCK ,
    #     DRIBBLES ,
    #     TOUCH_TIME ,
    #     SHOT_DIST ,
    #     PTS_TYPE ,
    #     CLOSE_DEF_DIST ,
    #     GAME_CLOCK
    #     ) %$%
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

shots<-read.csv("data/shot_logs2.csv", head=TRUE)

#Game clock is in a hard to use format, not sure how to convert it to make sense at the moment.  Maybe we just ignore this.
#Subsequent models are ignoring game clock, player name


attach(shots)
full.model<-glm(SHOT_RESULT~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit), data=shots)
summary(full.model)  # what predictors should we talk about: based on p-values, position, shot number, period, and games played don't seem as important.
anova(full.model)

#lasso, takes a minute to run

factors<-model.matrix(SHOT_RESULT~SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, data=shots)[,-1]
SHOT_RESULT<-shots$SHOT_RESULT

set.seed(1)
cv.out=cv.glmnet(factors,SHOT_RESULT,alpha=1, family="binomial")
predict(cv.out, type="coefficients", s=cv.out$lambda.1se)
# Lasso shows that we should investigate, shot clock, touch time, shot dist, closest defender, age, and mpg.
# This is similar to the full model using glm and comparing pvalues.


##Exploring fewer predictors

#Reduced Logistic Model
reduced.model<-glm(SHOT_RESULT~LOCATION+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+age+GP+MPG, family=binomial(link=logit), data=shots)
summary(reduced.model)
anova(reduced.model)

#further reduced by the massive G^2 only, this model is almost as good as the previous without nearly as many predictors.
reduced.model2<-glm(SHOT_RESULT~SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+CLOSE_DEF_DIST+age+MPG, family=binomial(link=logit), data=shots)
summary(reduced.model2)
anova(reduced.model2)

reduced.model2 %$%
    pchisq( deviance , df.residual , lower.tail = F )


pchisq()

#further reduced by the massive G^2 only, this model is almost as good as the previous without nearly as many predictors.
reduced.model3<-glm(SHOT_RESULT~SHOT_CLOCK+SHOT_DIST+CLOSE_DEF_DIST, family=binomial(link=logit), data=shots.24)
summary(reduced.model3)
anova(reduced.model3)

#shot distance seems to be of two categories, close shots and far shots. Splitting it at the free through line (15ft).
hist(shots$SHOT_DIST)
hist(SHOT_DIST[SHOT_DIST>=15])
hist(SHOT_DIST[SHOT_DIST<15])
shots$dist<-ifelse(SHOT_DIST<15, "<15ft",">=15ft")
tabdist<-table(shots$SHOT_RESULT, shots$dist)
epitab(tabdist)#odds of making a shot when further than average distance is about half of that when compared with shots of less than average distance.


#Just a thought about looking at subsets of shots based on the bimodal distribution of distances.
shots.near<-subset(shots,SHOT_DIST<15)
ggplot(data=shots.near, aes(x=SHOT_DIST, y=CLOSE_DEF_DIST)) + geom_point(aes(color=factor(SHOT_RESULT))) #can see some wide open shots here

#The full model for the shots less than 15 feet is similar to what we have above with the whole data set except that number of dribbles and touch time and defender distance are much more significant and shot distance(understandably as that was the reason for the split) and shot clock are less significant.
near.model<-glm(SHOT_RESULT~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit), data=shots.near)
summary(near.model)
anova(near.model)

#Reduced
near.model1<-glm(SHOT_RESULT~SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+CLOSE_DEF_DIST+age+MPG, family=binomial(link=logit), data=shots.near)
summary(near.model1)
anova(near.model1)

#Further Reduced
near.model2<-glm(SHOT_RESULT~SHOT_CLOCK+DRIBBLES+SHOT_DIST+CLOSE_DEF_DIST, family=binomial(link=logit), data=shots.near)
summary(near.model2)
anova(near.model2)

#exploring dribbles on near shots
hist(shots.near$DRIBBLES, breaks=25)
shots.near$drib<-ifelse(shots.near$DRIBBLES==0, "0", "1+")
tab.near.drib<-table(shots.near$SHOT_RESULT, shots.near$drib)
tab.near.drib
epitab(tab.near.drib)  #odds of a shot being made based on distance when 0 dribbles are taken compared to 1+ dribbles.

hist(shots.near$CLOSE_DEF_DIST, breaks=20) #not sure what to do with defender distance yet.


shots.far<-subset(shots,SHOT_DIST>=15)
ggplot(data=shots.far, aes(x=SHOT_DIST, y=CLOSE_DEF_DIST)) + geom_point(aes(color=factor(SHOT_RESULT))) + geom_vline(xintercept=22)#22ft is the closest 3 point shot.
#The full model for shots that are 15 feet or further reveal a different set of predictors based on delta G^2 (p-value<0.01).
far.model<-glm(SHOT_RESULT~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit), data=shots.far)
summary(far.model)
anova(far.model)
#Reduced
far.model1<-glm(SHOT_RESULT~PERIOD+SHOT_CLOCK+SHOT_DIST+CLOSE_DEF_DIST+age, family=binomial(link=logit), data=shots.far)
summary(far.model1)
anova(far.model1)

#Dribbles wasn't significant in the full model, but it was in the near model.  Compare shots taken with 0 dribbles, compared with 1 or more dribbles you see a drastic difference.
#so I thought maybe we should include this, the odds of making a shot when 0 dribbles (getting the pass and shooting) are much higher than when dribbling and then shooting.

hist(shots$DRIBBLES)
shots.0drib<-subset(shots,shots$DRIBBLES==0)

hist(shots.0drib$SHOT_DIST) #distribution by distance seems to be approximately the same as the complete data set
tab.0drib<-table(shots.0drib$SHOT_RESULT, shots.0drib$dist)
epitab(tab.0drib)
#When considering the the odds of making a shot with 0 dribbles only there is a drastic improvement in the odds of a shot being a make (from 1.77 to 2.58 times as likely to make) - likely due to passes close to the basket like alley oops.

shots.1drib<-subset(shots,shots$DRIBBLES>=1)

tab.1drib<-table(shots.1drib$SHOT_RESULT, shots.1drib$dist)
tab.1drib
epitab(tab.1drib)
#A player taking 1 or more dribbles still has a higher odds of making a closer shot but the odds are now lower than the overall group.  It is clearly the 0 dribble group that impacts the odds of a shot make versus distance.

#investigating the association between position and shot result which showed some significance in the far model.

#I was curious about splitting by position, maybe it comes in handy. I don't have much evidence of it being worthwile so far though, other than the obvious that center's shoot less three point shots - see odds ratios.
type.pos<-table(shots$pos, shots$PTS_TYPE)
epitab(type.pos)


x<-split(shots.far, shots$pos)
shots.c<-x$C
shots.pf<-x$PF
shots.sg<-x$SG
shots.pg<-x$PG
shots.sf<-x$SF

# For far shots
hist(shots.c$SHOT_DIST)
hist(shots.pf$SHOT_DIST)

hist(shots.sf$SHOT_DIST)
hist(shots.sg$SHOT_DIST)
hist(shots.pg$SHOT_DIST)

#Not a very big difference. The positions have a similar odds increase over center at making the shot.

tabpos.make<-table(shots.far$pos, shots.far$SHOT_RESULT)
tabpos.make
oddsbypos<-epitab(tabpos.make)
oddsbypos


#Reference: https://www.kaggle.com/erikbabb/d/dansbecker/nba-shot-logs/itm-6285-shot-prediction




########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
