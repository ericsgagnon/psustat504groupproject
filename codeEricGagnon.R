# code workspace - eric gagnon

# Setup ##############################################################################
#devtools::install_github("hadley/dplyr")
#devtools::install_github("mdsumner/spdplyr")
config <- quote({
Sys.setenv(MAKE = 'make -j 8')
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
library(parallel)
library(car)
library(DescTools)
library(outliers)
library(corrgram)
  })
eval( config )
# Load & Prep Data #######################################################################################

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
      ShotResult = factor( SHOT_RESULT ) ,
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
  .[ , ShotDistFactor := tmp.dist.clusters ] %>% 
  .[ ShotDistance < 15 , ShotDistanceClass := factor( 'Close (<15ft)')] %>% 
  .[ ShotDistance >= 15 , ShotDistanceClass := factor( 'Far (>=15ft)')] %>% 
  .[ , PositionDistClass := factor( paste0( Position , ' - ' , ShotDistanceClass ) ) ]

#Cleanup
rm( d.players , d.shots , tmp.dist.clusters , tmp.2pt.dist.clusters , tmp.3pt.dist.clusters )

## EDA Visuals ########################################################################

# Points per Shot vs Points
grid.newpage()
d %>%
    select( Player , Game , Position , Points , Scored ) %>% 
    group_by( Player , Game , Position ) %>% 
    summarise( PointsPerGame = sum( Points ) , PointsPerShot = mean( Points )  ) %>% {
    ggplot( . , aes( PointsPerGame , PointsPerShot , colour = Position ) ) + 
    geom_point() + 
    geom_density_2d( ) + 
    ggtitle( 'Points Per Shot vs Points Per Game' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly
    ggsave( 
        'eda-pps-ppg.png' , 
        width = 4 , 
        height = 2.5 ,
        scale = 2 ,
        dpi = 300 ,
        path = 'output/'
    )
    
# Baskets per Shot vs Baskets
grid.newpage()
d %>% 
    select( Player , Game , Position , Points , Scored ) %>% 
    group_by( Player , Game , Position ) %>% 
    summarise( BasketsPerGame = sum( Scored ) , BasketsPerShot = mean( Scored )  ) %>% {
    ggplot( . , aes( BasketsPerGame , BasketsPerShot , colour = Position ) ) + 
    geom_point() + 
    geom_density_2d( ) + 
    ggtitle( 'Baskets Per Shot vs Baskets Per Game' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly
    ggsave( 
        'eda-bps-bpg.png' , 
        width = 4 , 
        height = 2.5 ,
        scale = 2 ,
        dpi = 300 ,
        path = 'output/'
    )
    
# Baskets vs Shots
grid.newpage()
d %>% 
    select( Player , Game , Position , Points , Scored ) %>% 
    group_by( Player , Game , Position ) %>% 
    summarise( Baskets = sum( Scored ) , Shots = n()  ) %>% {
    ggplot( . , aes( Shots , Baskets , colour = Position ) ) + 
    geom_point() + 
    geom_smooth( method = 'loess' ) +
    ggtitle( 'Baskets vs Shots' , subtitle = 'Per Game - By Position')  } %>% 
    ggplotly
    ggsave( 
        'eda-baskets-shots.png' , 
        width = 4 , 
        height = 2.5 ,
        scale = 2 ,
        dpi = 300 ,
        path = 'output/'
    )
    

## Histograms
grid.newpage()
d %>% 
    .[ , .( Scored , ShotType , ShotDistance , ClosestDefenderDistance , ShotClock , Dribbles = as.numeric( Dribbles ) , TouchTime , GameClock ) ] %>% 
    .[ !is.na( ShotClock ) ] %>% 
    .[ TouchTime >= 0  ] %>% 
    melt( id.vars = c('Scored' , 'ShotType' ) ) %>% 
    .[ , variable := factor( variable ) ] %>% 
    { ggplot( . , aes( value , colour = ShotType , group = ShotType ) ) + facet_wrap(  ~ variable , scales = 'free' ) + geom_histogram( bins = 30 ) } %>% 
    ggplotly
    ggsave( 
        'eda-hist-by-shot-type.png' , 
        width = 4 , 
        height = 2.5 ,
        scale = 2 ,
        dpi = 300 ,
        path = 'output/'
    )
    

grid.newpage()
d %>% 
    .[ , .( Scored , ShotType , ShotDistance , ClosestDefenderDistance , ShotClock , Dribbles = as.numeric( Dribbles ) , TouchTime , GameClock ) ] %>% 
    .[ !is.na( ShotClock ) ] %>% 
    .[ TouchTime >= 0  ] %>% 
    melt( id.vars = c('Scored' , 'ShotType' ) ) %>% 
    .[ , variable := factor( variable ) ] %>% 
    { ggplot( . , aes( value , colour = Scored , group = Scored ) ) + facet_wrap(  ~ variable , scales = 'free' ) + geom_histogram( bins = 30 ) } %>% 
    ggplotly
    ggsave( 
        'eda-hist-by-scored.png' , 
        width = 4 , 
        height = 2.5 ,
        scale = 2 ,
        dpi = 300 ,
        path = 'output/'
    )
    

grid.newpage()
d %>% 
    .[ , .( Scored , ShotType , ShotDistance , ClosestDefenderDistance , ShotClock , 
            Dribbles = as.numeric( Dribbles ) , TouchTime , GameClock , ShotDistanceClass , PositionDistClass ) ] %>% 
    .[ !is.na( ShotClock ) ] %>% 
    .[ TouchTime >= 0  ] %>% 
    melt( id.vars = c('Scored' , 'ShotType' , 'ShotDistanceClass' , 'PositionDistClass' ) ) %>% 
    .[ , variable := factor( variable ) ] %>% 
    { ggplot( . , aes( value , colour = ShotDistanceClass , group = ShotDistanceClass ) ) + facet_wrap(  ~ variable , scales = 'free' ) + geom_histogram( bins = 30 ) } %>% 
    ggplotly
    ggsave( 
        'eda-hist-by-shot-dist-type.png' , 
        width = 4 , 
        height = 2.5 ,
        scale = 2 ,
        dpi = 300 ,
        path = 'output/'
    )

# Model ############################################################################################
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
save.image( file =  'working.RData' )

# glm 
# 
#   cl <- makeCluster(getOption("cl.cores", { min( detectCores() , 10 ) } ) )
#   l.m.glm <- 
#     parSapply( 
#       cl , 
#       d[ , unique( PositionDistClass ) ] , 
#       function(x) {
#         # prep each environment
#         load( 'working.RData')
#         eval( config )
#         
#         d[ PositionDistClass == x , ] %>%  
#           .[ !is.na( ShotClock ) , ] %$%
#           glm(
#             Scored ~ ShotDistance * ClosestDefenderDistance ,
#             family = binomial( link = 'logit' )
#           )
#       } , simplify = F , USE.NAMES = T )
#   names( l.m.glm ) <- { d[ , unique( PositionDistClass ) ] }
#   save.image( file =  'working.RData' )  
#   stopCluster( cl )
#   rm( cl )
#   
# glm with random effects 

  cl <- makeCluster(getOption("cl.cores", { min( detectCores() , 10 ) } ) )
  l.m.glmer <- 
    parSapply( 
      cl , 
      d[ , unique( PositionDistClass ) ] , 
      function(x) {
        # prep each environment
        load( 'working.RData')
        eval( config )
        
        d[ PositionDistClass == x , ] %>%  
          .[ !is.na( ShotClock ) , ] %$% 
          glmer( 
            Scored ~ ShotDistance * ClosestDefenderDistance + ( ShotDistance  | Player / Game ) +  ( ClosestDefenderDistance  | Player / Game ) ,
            family = binomial( link = 'logit' ) ,
            control = glmerControl(optimizer= c( "bobyqa" , "bobyqa" ) , 
                                   optCtrl=list(maxfun=2e5) )
          )
      } , simplify = F )
  names( l.m.glmer ) <- d[ , unique( PositionDistClass ) ]
  save.image( file =  'working.RData' )  
  stopCluster( cl )
  rm( cl )
  
# Results & Visuals ############################################################################


# output for each model
  l.m.summaries <- 
    l.m.glmer %>% sapply( summary , simplify = F )

# anova for each model 
  l.m.anovas <- 
    l.m.glmer %>% sapply( anova , simplify = F )

# data.frame of model results 
  d.m.results <- 
    l.m.glmer %>% names %>% sapply( function(x) {
      data.frame( PositionShotDistClass = x , tidy( l.m.glmer[[x]] ) )
    } , simplify = F ) %>% 
      bind_rows() %>% 
      mutate( 
        Position = { str_replace_all( PositionShotDistClass , '([:alpha:]{1,2})(.*)' , '\\1')} , 
        ShotDistClass = { str_replace_all( PositionShotDistClass , '([:alpha:]{1,2}) - (.*)' , '\\2')}
      )
  
# data.frame of anova results 
  d.m.anovas <- 
    l.m.glmer %>% names %>% sapply( function(x) {
      data.frame( PositionShotDistClass = x , tidy( anova( l.m.glmer[[x]] ) ) )
    } , simplify = F ) %>% 
      bind_rows() %>% 
      mutate( 
        Position = { str_replace_all( PositionShotDistClass , '([:alpha:]{1,2})(.*)' , '\\1')} , 
        ShotDistClass = { str_replace_all( PositionShotDistClass , '([:alpha:]{1,2}) - (.*)' , '\\2')}
      )
  save.image( file =  'working.RData' )    

# Plot Fixed Effects
  l.p.fe <- 
    l.m.glmer %>% names %>% 
    sapply( function(x){
      sjp.glmer( l.m.glmer[[x]] , type = 'fe' , title = paste0( x , ': Fixed Effects' ) , prnt.plot = F )
      } , simplify = F )
  names(l.p.fe) <- paste0( names( l.p.fe ) , ': Fixed Effects' )

# Plot Fixed Effects Slopes  
  l.p.fe.slope <- 
    l.m.glmer %>% names %>% 
    sapply( function(x){
      sjp.glmer( l.m.glmer[[x]] , type = 'fe.slope' , title = paste0( x , ': Fixed Effects Slopes' ) , prnt.plot = F )
      } , simplify = F )
  names(l.p.fe.slope) <- paste0( names( l.p.fe.slope ) , ': Fixed Effects Slopes' )
  
  # Make combined list of ggplots
  l.p <- 
    c(
      l.p.fe %>% names %>% sapply( function(x){ 
          l.p.fe[[x]][['plot']]
      } , simplify = F )
    ,  
      l.p.fe.slope %>% names %>% sapply( function(x){ 
          l.p.fe.slope[[x]][['plot']]
      } , simplify = F )
    )


  d[ , unique( Position ) ] %>% as.character %>% 
    sapply( function(x) {
      l <- list( 
        { l.p[[ paste0( x , ' - Close (<15ft): Fixed Effects') ]] } ,
        { l.p[[ paste0( x , ' - Far (>=15ft): Fixed Effects') ]] + 
            theme( axis.text.y = element_blank() ,
                   axis.ticks.y = element_blank() )
        }
        )
      
      grid.newpage()
      p <- marrangeGrob( l , nrow = 1 , ncol = 2 , top = NULL )
      ggsave( paste0( x , '-Fixed-Effects.png' ) , 
              p ,
              width = 6 , 
              height = 3 ,
              scale = 2 ,
              dpi = 300 ,
              path = 'output/'
      )
      # FE Slopes
      l <- list( 
        { l.p[[ paste0( x , ' - Close (<15ft): Fixed Effects Slopes') ]] } ,
        { l.p[[ paste0( x , ' - Far (>=15ft): Fixed Effects Slopes') ]] + 
            theme( axis.text.y = element_blank() ,
                   axis.ticks.y = element_blank() )
        }
        )
      
      grid.newpage()
      p <- marrangeGrob( l , nrow = 1 , ncol = 2 , top = NULL )
      ggsave( paste0( x , '-Fixed-Effects-Slopes.png' ) , 
              p ,
              width = 6 , 
              height = 3 ,
              scale = 2 ,
              dpi = 300 ,
              path = 'output/'
      )
      
    } , simplify = F )
  
  save.image( file =  'working.RData' )    
  
  
# Save Tabled Outputs to png  
  x <- 
  d.m.results %>% 
    filter( !is.na( p.value ) ) %>% 
    select( PositionShotDistClass , Term = term , Estimate = estimate , PValue = p.value , Position , DistanceGroup = ShotDistClass ) %>% 
    mutate( Estimate = sprintf( '%.3f' , Estimate  ) ) %>% 
    dcast( DistanceGroup + Term ~ Position , value.var = 'Estimate' )
  png("estimates.png" , width = 7.6 , height = 2.5 , units = 'in' , res = 100 , pointsize = 6 )
  p<-tableGrob(x , rows = NULL )
  grid.arrange(p)
  dev.off()
  
  x <- 
    d.m.results %>% 
    filter( !is.na( p.value ) ) %>% 
    select( PositionShotDistClass , Term = term , Estimate = estimate , PValue = p.value , Position , DistanceGroup = ShotDistClass ) %>% 
    mutate( PValue = sprintf( '%.3f' , PValue  ) ) %>% 
    dcast( DistanceGroup + Term ~ Position , value.var = 'PValue' )
  png("pvalues.png" , width = 7.6 , height = 2.5 , units = 'in' , res = 100 , pointsize = 6 )
  p<-tableGrob(x , rows = NULL )
  grid.arrange(p)
  dev.off()  
  
  
  
  
  
  
