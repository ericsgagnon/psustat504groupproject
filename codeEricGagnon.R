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
library(sjPlot)
################################################################################################

# Shot Dataset
  d.shots <- 
      fread( 'data/shot_logs-1.csv') %>% 
      .[ , Scored := case_when( SHOT_RESULT == 'made' ~ TRUE , 
                                SHOT_RESULT == 'missed' ~ FALSE ) ]

# Update Game clock to represent number of seconds (numeric)
# bug with using some lubridate functions inside data.table - using tmp var as workaround
  tmp <- d.shots$GAME_CLOCK %>% ms %>% as.numeric()
  d.shots[ , GAME_CLOCK := { tmp } ]
  rm(tmp)
  
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

tmp.2pt.dist.clusters <- 
  d[ PTS_TYPE == 2  , { kmeans( SHOT_DIST , 10 , iter.max = 20 ) } ] %$%
  centers[ cluster , 1] %>% 
  unname %>% 
  factor

levels(tmp.2pt.dist.clusters) <- 
  tmp.2pt.dist.clusters %>% 
  levels %>% 
  as.numeric %>% 
  signif( digits = 3 ) %>% 
  { paste0( '2 pt: ' , . ) }

tmp.3pt.dist.clusters <- 
  d[ PTS_TYPE == 3  , { kmeans( SHOT_DIST , 10 , iter.max = 20 ) } ] %$%
  centers[ cluster , 1] %>% 
  unname %>% 
  factor

levels(tmp.3pt.dist.clusters) <- 
  tmp.3pt.dist.clusters %>% 
  levels %>% 
  as.numeric %>% 
  signif( digits = 3 ) %>% 
  { paste0( '3 pt: ' , . ) }

tmp.dist.clusters <- 
  d[ , { kmeans( SHOT_DIST , 10 , iter.max = 20 ) } ] %$%
  centers[ cluster , 1] %>% 
  unname %>% 
  factor

levels(tmp.dist.clusters) <- 
  tmp.dist.clusters %>% 
  levels %>% 
  as.numeric %>% 
  signif( digits = 3 )

d %<>% 
    .[ , .( 
      Points = PTS , 
      Scored , 
      Game = as.factor(GAME_ID) , 
      Player = as.factor( player_name) , 
      Position = as.factor(Pos) , 
      ShotDistance = SHOT_DIST , 
      ClosestDefenderDistance = CLOSE_DEF_DIST , 
      ShotClock = SHOT_CLOCK , 
      Dribbles = DRIBBLES , 
      TouchTime = TOUCH_TIME , 
      GameClock = GAME_CLOCK ,
      ShotType = { as.factor( PTS_TYPE ) }
   ) ] %>% 
  .[ ShotType == "2" , ShotTypeDistFactor := tmp.2pt.dist.clusters ] %>% 
  .[ ShotType == "3" , ShotTypeDistFactor := tmp.3pt.dist.clusters ] %>% 
  .[ , ShotDistFactor := tmp.dist.clusters ]

#Cleanup
rm( d.players , d.shots , tmp.dist.clusters , tmp.2pt.dist.clusters , tmp.3pt.dist.clusters )

## EDA Visuals ########################################################################

# Points per Shot vs Points
d %>%
    select( Player , Game , Position , Points , Scored ) %>% 
    group_by( Player , Game , Position ) %>% 
    summarise( PointsPerGame = sum( Points ) , PointsPerShot = mean( Points )  ) %>% {
    ggplot( . , aes( PointsPerGame , PointsPerShot , colour = Position ) ) + 
    geom_point() + 
    geom_density_2d( ) + 
    ggtitle( 'Points Per Shot vs Points Per Game' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly

# Baskets per Shot vs Baskets
d %>% 
    select( Player , Game , Position , Points , Scored ) %>% 
    group_by( Player , Game , Position ) %>% 
    summarise( BasketsPerGame = sum( Scored ) , BasketsPerShot = mean( Scored )  ) %>% {
    ggplot( . , aes( BasketsPerGame , BasketsPerShot , colour = Position ) ) + 
    geom_point() + 
    geom_density_2d( ) + 
    ggtitle( 'Baskets Per Shot vs Baskets Per Game' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly

# Baskets vs Shots
d %>% 
    select( Player , Game , Position , Points , Scored ) %>% 
    group_by( Player , Game , Position ) %>% 
    summarise( Baskets = sum( Scored ) , Shots = n()  ) %>% {
    ggplot( . , aes( Shots , Baskets , colour = Position ) ) + 
    geom_point() + 
    geom_smooth( method = 'loess' ) +
    ggtitle( 'Baskets vs Shots' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly

## Histograms
d %>% 
  .[ , .( Scored , ShotType , ShotDistance , ClosestDefenderDistance , ShotClock , Dribbles = as.numeric( Dribbles ) , TouchTime , GameClock ) ] %>% 
  .[ !is.na( ShotClock ) ] %>% 
  .[ TouchTime >= 0  ] %>% 
  melt( id.vars = c('Scored' , 'ShotType' ) ) %>% 
  .[ , variable := factor( variable ) ] %>% 
  { ggplot( . , aes( value , colour = ShotType , group = ShotType ) ) + facet_wrap(  ~ variable , scales = 'free' ) + geom_histogram( bins = 30 ) } %>% 
  ggplotly

d %>% 
  .[ , .( Scored , ShotType , ShotDistance , ClosestDefenderDistance , ShotClock , Dribbles = as.numeric( Dribbles ) , TouchTime , GameClock ) ] %>% 
  .[ !is.na( ShotClock ) ] %>% 
  .[ TouchTime >= 0  ] %>% 
  melt( id.vars = c('Scored' , 'ShotType' ) ) %>% 
  .[ , variable := factor( variable ) ] %>% 
  { ggplot( . , aes( value , colour = Scored , group = Scored ) ) + facet_wrap(  ~ variable , scales = 'free' ) + geom_histogram( bins = 30 ) } %>% 
  ggplotly

# Model ############################################################################################
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

# glm 

  m.glm <- 
      d %>%  
      .[ !is.na( ShotClock ) , ] %$% 
      glm( 
          Scored ~ ShotDistFactor * ClosestDefenderDistance * Player ,
          family = binomial( link = 'logit' )    
      )
  
  m.glm %>% summary
  m.glm %>% anova
  m.glm %$% pchisq( deviance , df.residual , lower.tail = F )

# glm with random effects 

  m.glmer <- 
    d %>%  
    .[ !is.na( ShotClock ) , ] %$% 
    glm( 
      Scored ~ ShotDistFactor * ClosestDefenderDistance + ( 1 | Player ) ,
      family = binomial( link = 'logit' )    
    )
  
  m.glmer %>% summary
  m.glmer %>% anova
  #m.glmer %$% pchisq( deviance , df.residual , lower.tail = F )


