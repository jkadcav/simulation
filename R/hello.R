
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
##Bayes Thoroughbreds
##Factors are Rating (Prior), Matrix, J&T, DSLR, Finishing Speed, Track Work, Career
options(stringsAsFactors=F)

tryCatch(library(RPostgreSQL),error=function(e){
  install.packages("RPostgreSQL", repos="http://cran.rstudio.com/")
  library(RPostgreSQL)})

tryCatch(library(tidyjson),error=function(e){
  install.packages("jsonlite", repos="http://cran.rstudio.com/")
  library(tidyjson)})

tryCatch(library(dplyr),error=function(e){
  install.packages("dplyr", repos="http://cran.rstudio.com/")
  library(dplyr)})

tryCatch(library(dplyr),error=function(e){
  install.packages("plyr", repos="http://cran.rstudio.com/")
  library(plyr)})

tryCatch(library(httr),error=function(e){
  install.packages("httr", repos="http://cran.rstudio.com/")
  library(httr)})

tryCatch(library(doParallel),error=function(e){
  install.packages("doParallel", repos="http://cran.rstudio.com/")
  library(doParallel)})

tryCatch(library(devtools),error=function(e){
  install.packages("devtools", repos="http://cran.rstudio.com/")
  library(devtools)})

tryCatch(library(calculateRBM),error=function(e){
  install_github("jkadcav/calculateRBM",force=T)
  library(calculateRBM)})

tryCatch(library(ggplot2),error=function(e){
  install.packages("ggplot2", repos="http://cran.rstudio.com/")
  library(ggplot2)})





# Simulation of Current Model in RACELAB

# PARAMETERS
# - Day (Mon-Fri)
# - Region (Country etc.)
# - Code (TBD, GRY, HRN)
# - Track (Very Long List)
# - Grade (0.5 - 6.5)
# - Conf. Level (A - E & All)
# - Pool
# - Bet Type
# - WP Market
# - EX Market
# - Sigma
# - Classifier
# - From Date
# - To Date
# - From Going
# - All Going
# - From Distance
# - To Distance
# - Trading Strategy
# - Pool %
# - Trade Timing
# - Market Fusion
# - Min Exp.
# - Max Exp
# - Min P(A)
# - Max P(A)
# - Min Stake
# - Max Stake
# - Rebate
# - Comission


dwConnect<-function(){
  library("RPostgreSQL")
  dbHost<-"localhost"
  dbPort<-6432
  if (!is.na(Sys.getenv("IS_PRODUCTION", NA))) {     # master, only accessible inside DW data center
    dbHost<-"dw-staging.cjza6pmqs6im.ap-southeast-2.rds.amazonaws.com"
    dbPort<-5432
  }
  else if (!is.na(Sys.getenv("IS_DEVELOPMENT", NA))) {
    dbHost<-"dw-staging-read-1.cjza6pmqs6im.ap-southeast-2.rds.amazonaws.com"
    dbPort<-5432
  }
  pg <- DBI::dbDriver("PostgreSQL")
  con<-DBI::dbConnect(pg, user="betia_staging", password="poT5oT4Ayct0Eef5vin2Arb7owG3oo",
                      host=dbHost, port=dbPort, dbname="dw_staging")
  return(con)
}


format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

fetchData<-function(dfrom,dto,animal,country){
  con<-dwConnect()
  if(country=="Australia" & animal=="THOROUGHBRED") lvl<-"AND venues.level=\'M\' "
  else lvl<-""
  dat<-dbGetQuery(con,paste("SELECT meetings.id AS meeting_id,
                            meetings.venue_id as venue_id,
                            events.id AS event_id,
                            event_competitors.id AS event_competitor_id,
                            competitors.id AS competitor_id,
                            trainers.id AS trainer_id,
                            venues.name AS venue_name,
                            meeting_date,
                            countries.name AS country_name,
                            events.number AS event_number,
                            competitors.name AS competitor_name,
                            event_competitor_race_data.program_number ,
                            event_competitor_race_data.barrier,
                            event_competitor_race_data.weight,
                            event_competitor_race_data.finish_time,
                            event_competitor_race_data.finish_position,
                            event_race_data.distance,
                            event_race_data.race_class,
                            event_competitor_race_data.win_margin,
                            (select venue_grade_bases.base from venue_grade_bases where venues.id = venue_grade_bases.venue_id AND venue_grade_bases.grade = (select event_class_grades.grade from event_class_grades where countries.id = event_class_grades.country_id AND event_class_grades.venue_type_id = venues.venue_type_id AND race_class = event_race_data.race_class) ) as grade_base,
                            (SELECT market_json::json->'prices'->event_competitor_race_data.number-1
                            FROM markets
                            WHERE market_name = 'SP'
                            AND markets.meeting_id = meetings.id
                            AND markets.event_number = events.number LIMIT 1) AS sp_price,
                            ( SELECT (markets.market_json -> 'volume_prices'::text) -> (event_competitor_race_data.number - 1) -> 0 -> 'price'::text
                            FROM markets
                            WHERE markets.market_name::text = 'BACK'::text AND provider = 'betfair' AND markets.meeting_id = meetings.id AND markets.event_number = events.number
                            LIMIT 1) AS betfair_back_price,
                            array_to_string(array
                            (SELECT POSITION
                            FROM event_competitor_sectional_data
                            WHERE event_competitor_sectional_data.event_competitor_id = event_competitors.id
                            ORDER BY section_index limit 1),' ') AS pir,
                            (select analysis_json::json->'bmrk'  from market_analyses where market_name = 'pre_race_factors' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1) as benchmark,
                            (select analysis_json::json->event_competitor_race_data.number::text->'nruns' from market_analyses where market_name = 'XMODEL' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1) as nruns,
                            (select analysis_json::json->event_competitor_race_data.number::text->'iruns' from market_analyses where market_name = 'XMODEL' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1) as iruns,
                            (select analysis_json::json->event_competitor_race_data.number::text->'prime_rating' from market_analyses where market_name = 'XMODEL' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1) as prime_rating,
                            coalesce((select analysis_json::json->event_competitor_race_data.number::text->'prime_rating'  from market_analyses where market_name = 'analyst_master' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1),'{}') as prime_rating_analyst,
                            coalesce((select analysis_json::json->event_competitor_race_data.number::text->'acceleration'  from market_analyses where market_name = 'analyst_master' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1),'{}') as acceleration,
                            coalesce((select analysis_json::json->event_competitor_race_data.number::text->'bias'  from market_analyses where market_name = 'analyst_master' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1),'{}') as bias,
                            coalesce((select analysis_json::json->event_competitor_race_data.number::text->'yard'  from market_analyses where market_name = 'analyst_master' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1),'{}') as yard,
                            coalesce((select analysis_json::json->event_competitor_race_data.number::text->'xa_jnt'  from market_analyses where market_name = 'analyst_master' and market_analyses.meeting_id = meetings.id and market_analyses.event_number = events.number limit 1),'{}') as jnt,
                            (select market_json::json->'prices'->event_competitor_race_data.number-1 from markets where market_name = 'WIN' and markets.provider = 'racingandsports' and markets.meeting_id = meetings.id and markets.event_number = events.number limit 1) as rs,
                            (Select count(*) from event_competitors where events.id = event_competitors.event_id) as runners
                            FROM meetings
                            LEFT OUTER JOIN venues ON venues.id = meetings.venue_id
                            LEFT OUTER JOIN countries ON countries.id = venues.country_id
                            LEFT OUTER JOIN venue_types ON venue_types.id = venues.venue_type_id
                            LEFT OUTER JOIN events ON events.meeting_id = meetings.id
                            LEFT OUTER JOIN event_competitors ON event_competitors.event_id = events.id
                            LEFT OUTER JOIN event_competitor_sectional_data ON event_competitor_sectional_data.id = event_competitors.id
                            LEFT OUTER JOIN competitors ON event_competitors.competitor_id = competitors.id
                            LEFT OUTER JOIN event_competitor_race_data ON event_competitor_race_data.id = event_competitors.event_competitor_race_datum_id
                            LEFT OUTER JOIN jockeys ON jockeys.id = event_competitor_race_data.jockey_id
                            LEFT OUTER JOIN trainers ON trainers.id = event_competitor_race_data.trainer_id
                            LEFT OUTER JOIN event_race_data ON event_race_data.id = events.event_race_datum_id
                            LEFT OUTER JOIN track_types ON track_types.id = event_race_data.track_type_id
                            LEFT OUTER JOIN events_with_benchmarks ON events.id = events_with_benchmarks.event_id
                            LEFT OUTER JOIN competitor_race_data on competitors.competitor_race_datum_id = competitor_race_data.id
                            LEFT OUTER JOIN competitors as sire_competitors on competitor_race_data.sire_id = sire_competitors.id
                            LEFT OUTER JOIN competitors as dam_competitors on competitor_race_data.dam_id = dam_competitors.id
                            LEFT OUTER JOIN markets ON markets.meeting_id = meetings.id and markets.event_number = events.number and markets.provider = venues.host_market and markets.market_name = 'WIN'
                            LEFT OUTER JOIN rail_positions ON event_race_data.rail_position_id = rail_positions.id
                            WHERE meeting_date>=\'",dfrom,"\' and meeting_date<=\'",dto,"\' and venue_types.name = \'",animal,"\' and countries.name = \'",country,"\' and scratched=FALSE ",lvl,"
                            ORDER BY meeting_date,venue_name,event_number,program_number ASC;",sep=""))
  dbDisconnect(con)
  return(dat)
}

zWaldScore<-function(rg){
  startTime<-Sys.time()
  rg$nruns<-as.numeric(rg$nruns)
  rg$iruns<-as.numeric(rg$iruns)
  #if(grepl("/",data$date[1])) data$date<-as.Date(data$date,"%d/%m/%Y")
  #if(grepl("/",rg$date[1])) rg$date<-as.Date(rg$date,"%d/%m/%Y")

  races<-rg[!duplicated(rg[c("meeting_id","event_id")]),]
  total<-nrow(races)

  for (i in 1:total){
    #a - working data
    afilter<-rg$meeting_id==races$meeting_id[i] & rg$event_id==races$event_id[i]
    a<-rg[afilter,]


    #confidence level + lms
    conf<-0.99
    for (j in 1:nrow(a)){
      if(is.finite(a$barrier[j]) && a$iruns[j]<4) conf<-conf-0.2
      else if(is.finite(a$barrier[j]) && a$iruns[j]<7) conf<-conf-0.05
    }

    if(conf>0.8) lvl<-1
    else if(conf>0.6) lvl<-2
    else if(conf>0.4) lvl<-3
    else if(conf>0.2) lvl<-4
    else lvl<-5

    rg[afilter,"conf"]<-lvl

    tt<-round(difftime(Sys.time(),startTime,units="mins"),digits=2)
    message(paste("Z Wald Scores: ",i," of ",total," (",tt," minutes elapsed) Est. Remaining: ",round(tt/i*total-tt,digits=2)," mins", sep=''))
    flush.console()
  }
  return(rg)
}

distBands<-function(country,distt){
  if(country=="United Kingdom"){
    if(is.na(distt)) return(NA)
    else if(distt>=804 & distt<1814) return(1)
    else if(distt>=1814 & distt<2855) return(2)
    else if(distt>=2855 & distt<3856) return(3)
    else if(distt>=3856 & distt<4467) return(4)
    else if(distt>=4467) return(5)
  }
}
#pricing formula
pricingPrior<-function(rating,base){
  if(is.na(rating)) return(NA)
  diff<-rating-base
  a<-exp((diff-4.75)/3)
  return(a)
}

#normalizes pricing for both adjusted & non-adjusted ratings
normalizePricing<-function(data){
  races<-data[is.finite(data$event_id) & !duplicated(data[c("event_id")]),c("event_id")]
  for(i in 1:length(races)){
    filter<-data$event_id==races[i] & is.finite(data$event_id)
    a<-data$prob_est[filter]

    prices<-1/(a/sum(a,na.rm=T))

    data$prob_est[filter]<-prices

    flush.console()
  }
  return(data)
}

calcProf<-function(fp,price,stake){
  if(is.na(fp)) return(NA)
  else if(fp==1) return((price-1)*stake)
  else if(fp!=1) return(-stake)

}

simStaking<-function(df,staking,market){
  if(staking==0) {
    df$stake<-100
  }
  else if(staking==1){
    df$stake<-100/((df$prob_est)-1)
  }
  df$pl<-mapply(calcProf,as.numeric(df$finish_position),as.numeric(df[,c(market)]),df$stake)
  return(df)
}

calculateJTD<-function(df){
  meets<-unique(df$meeting_id)

  x<-list()


  for(i in 1:length(meets)){
    filter<-meets[i]==df$meeting_id
    a<-calculateRBM::masterRate(meets[i],0,1)
    x[[i]]<-a[,c('event_competitor_id','jtd')]


  }
  x<-dplyr::bind_rows(x)
  df<-join(df,x,type='left')
  return(df)
}

tradeSimulation<-function(dfrom,dto,animal,country,odds_l,odds_u,staking,mkt_l,mkt_u,exp_l,exp_u,conf_l,conf_u,pricing,sumType,market){
  df<-fetchData(dfrom,dto,animal,country)
  if(animal=="THOROUGHBRED") df<-calculateJTD(df)

  if(country=="Hong Kong")  df$prime_rating<-as.numeric(df$prime_rating_analyst)+as.numeric(df$jnt)+as.numeric(df$bias)+as.numeric(df$acceleration)+as.numeric(df$yard)
  if(pricing=="analyst" & animal=="THOROUGHBRED") df$prob_est<-mapply(pricingPrior,as.numeric(df$prime_rating_analyst),as.numeric(df$jtd))
  else if(pricing=="model" & animal=="THOROUGHBRED") df$prob_est<-mapply(pricingPrior,as.numeric(df$prime_rating),as.numeric(df$jtd))
  else if(pricing=="R&S Prices") df$prob_est<-as.numeric(df$rs)
  else df$prob_est<-mapply(pricingPrior,as.numeric(df$prime_rating),as.numeric(df$grade_base))


  df<-normalizePricing(df)

  if(animal=="GREYHOUND") {
    df<-zWaldScore(df)
    df<-df[df$conf>=conf_l & df$conf<=conf_u,]
  }


  df$dist_band<-mapply(distBands,df$country_name,as.numeric(df$distance))
  df<-df[df$prob_est>=odds_l & df$prob_est<=odds_u & is.finite(df$prob_est),]

  if(market=="Betfair") market<-'betfair_back_price'
  else if(market=="SP") market<-'sp_price'


  df$exp<-(as.numeric(df[,c(market)])-1)/(df$prob_est-1)
  df<-df[df$exp>=exp_l & df$exp<=exp_u & is.finite(df$exp),]
  df$date_day<-weekdays(as.Date(df$meeting_date))

  if(pricing=='analyst') df$diff<-as.numeric(df$prime_rating_analyst)-as.numeric(df$jtd)

  #df<-strikeRateRating(df,pricing)
  df<-simStaking(df,staking,market)

  #if(sumType=="Trading") df<-daySummary(df,dfrom,dto)
  #else df<-chiSquareClassic(df,'prob_est')

  return(df)
}

trackSummary<-function(df){
  venues<-unique(df[,c('venue_name')])
  res<-as.data.frame(matrix(NA,length(venues),5))
  colnames(res)<-c('trades','turnover','ret','roi','profit')

  for(i in 1:length(venues)){
    filter<-venues[i]==df$venue_name
    a<-df[filter,]
    n<-res$trades[i]<-nrow(a)
    t<-res$turnover[i]<-sum(a$stake,na.rm=T)
    p<-res$profit[i]<-sum(a$pl,na.rm=T)
    r<-res$ret[i]<-sum(t,p)
    res$roi[i]<-r/t
  }
  res<-round(res,2)
  res$trades<-round(res$trades,0)
  res$venue<-venues
  #res$turnover<-format.money(res$turnover)
  #res$ret<-format.money(res$ret)
  #res$profit<-format.money(res$profit)



  res<-res[,c('venue','trades','turnover','ret','profit','roi')]
  #colnames(res)<-c('Venue','Trades','Turnover','Return','Profit','ROI%')
  res<-list(split(res[,c('turnover','ret','roi','profit')],res$venue))
  return(res)
}

raceclassSummary<-function(df){
  classes<-unique(df[,c('race_class')])
  res<-as.data.frame(matrix(NA,length(classes),5))
  colnames(res)<-c('trades','turnover','ret','roi','profit')

  for(i in 1:length(classes)){
    filter<-classes[i]==df$race_class
    a<-df[filter,]
    n<-res$trades[i]<-nrow(a)
    t<-res$turnover[i]<-sum(a$stake,na.rm=T)
    p<-res$profit[i]<-sum(a$pl,na.rm=T)
    r<-res$ret[i]<-sum(t,p)
    res$roi[i]<-r/t
  }
  res<-round(res,2)
  res$trades<-round(res$trades,0)
  res$race_class<-classes
  #res$turnover<-format.money(res$turnover)
  #res$ret<-format.money(res$ret)
  #res$profit<-format.money(res$profit)



  res<-res[,c('race_class','trades','turnover','ret','profit','roi')]
  #colnames(res)<-c('Venue','Trades','Turnover','Return','Profit','ROI%')
  res<-list(split(res[,c('turnover','ret','roi','profit')],res$race_class))
  return(res)
}

daySummary<-function(df,dfrom,dto){
  dates<-seq.Date(as.Date(dfrom),as.Date(dto),'1 days')
  res<-as.data.frame(matrix(NA,length(dates),4))
  colnames(res)<-c('turnover','ret','roi','profit')

  for(i in 1:length(dates)){
    filter<-dates[i]==df$meeting_date
    a<-df[filter,]

    t<-res$turnover[i]<-sum(a$stake,na.rm=T)
    p<-res$profit[i]<-sum(a$pl,na.rm=T)
    r<-res$ret[i]<-sum(t,p)
    res$roi[i]<-r/t
  }
  res$meeting_date<-dates
  res$rolling_pl<-cumsum(res[, 4])
  res<-list(split(res[,c('turnover','ret','roi','profit')],res$meeting_date))
  return(res)
}

distSummary<-function(df){
  combos<-unique(df[,c('dist_band','date_day')])
  res<-as.data.frame(matrix(NA,nrow(combos),4))
  colnames(res)<-c('turnover','ret','roi','profit')

  for(i in 1:nrow(combos)){
    filter<-combos$dist_band[i]==df$dist_band & combos$date_day[i]==df$date_day
    a<-df[filter,]

    t<-res$turnover[i]<-sum(a$stake,na.rm=T)
    p<-res$profit[i]<-sum(a$pl,na.rm=T)
    r<-res$ret[i]<-sum(t,p)
    res$roi[i]<-r/t
  }
  res[,c('dist_band','day')]<-combos[,c('dist_band','date_day')]


  #still needs some
  res<-lapply(split(res, res$dist_band), function(x) split(x[,c('turnover','ret','roi','profit')], x$day))

  return(res)
}

## Statistical Diagnostics
factorRank<-function(data,fct){
  #if(grepl("/",data$meeting_date[1])) data$meeting_date<-as.Date(data$meeting_date,"%d/%m/%Y")
  data$rating_rank<-ave(-as.numeric(data[,c(fct)]),interaction(data$event_id),FUN=rank)
  data$rating_rank[is.na(data$rating_rank)]<-NA
  return(data)
}

chiSquareClassic<-function(data,market,ind=1){

  lows<-c(0.0000001,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5)
  ups<-c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,1)

  res<-as.data.frame(matrix(NA,length(lows),7))
  colnames(res)<-c('Lower','Upper','W','N','Act','Exp','Chi')


  data[,c(market)]<-as.numeric(data[,c(market)])

  if(market=='citibet') market<-'citibet_price'
  mkt<-as.numeric(data[,c(market)])


  res$Lower<-lows
  res$Upper<-ups
  for(i in 1:nrow(res)){
    u<-(1/res$Upper[i])
    l<-(1/res$Lower[i])
    filter<-mkt>=u & mkt<l & is.finite(mkt)
    aa<-data[filter,]
    bb<-aa[aa$finish_position==1 & is.finite(aa$finish_position),]
    total<-res$N[i]<-nrow(aa)
    w<-res$W[i]<-nrow(bb)
    act<-res$Act[i]<-w/total
    exp<-res$Exp[i]<-(1/mean(aa[,c(market)],na.rm=T))
    #exp<-res$Exp[i]<-(res$Upper[i]+res$Lower[i])/2
    res$Chi[i]<-((w-(exp)*total)^2)/(exp*total)
    flush.console()
  }
  res<-res[order(-res$Lower),]
  res$Lower<-round(res$Lower,2)
  res_chi<-round(sum(res$Chi,na.rm=T),2)
  colnames(res)<-tolower(colnames(res))
  #res$upper<-round(1/res$upper,1)
  #res$lower<-round(1/res$lower,1)
  #res$lower[nrow(res)]<-1000.0
  res[,5:7]<-round(res[,5:7],2)
  res<-res[,c('upper','lower','w','n','act','exp','chi')]
  if(ind==1) {
    res<-list(split(res[,c('w','n','act','exp','chi')],res$lower))
    return(res)
  }
  else{
    res<-sum(res$chi,na.rm=T)
    return(res)
  }
}

byExpectation<-function(df){
  mins<-c(0,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.4,1.6,1.8,2)
  maxs<-c(0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.4,1.6,1.8,2,100)

  res<-as.data.frame(matrix(NA,length(mins),7))
  colnames(res)<-c('min','max','n','w','act','exp','roi')
  res$min<-mins
  res$max<-maxs
  for(i in 1:nrow(res)){
    filter<-df$exp>=res$min[i] & df$exp<res$max[i] & is.finite(df$exp)
    n<-df[filter,]
    w<-n[n$finish_position==1 & is.finite(n$finish_position),]

    res$w[i]<-nrow(w)
    res$n[i]<-nrow(n)
    res$act[i]<-nrow(w)/nrow(n)
    res$exp[i]<-1/mean(n$prob_est,na.rm=T)
    res$roi[i]<-(sum(n$pl,na.rm=T)/sum(n$stake,na.rm=T))+1
  }
  res<-list(split(res[,c('n','w','act','exp','roi')],res$min))
  return(res)
}

strikeRateRating<-function(df,model){
  if(model=='analyst') fct<-'prime_rating_analyst'
  else fct<-'prime_rating'

  df[,c(fct)]<-as.numeric(df[,c(fct)])
  df<-df[!is.na(df[,c(fct)]),]
  df<-factorRank(df,'diff')
  fp_max<-max(df$finish_position[df$finish_position<50],na.rm=T)

  res<-as.data.frame(matrix(NA,fp_max,fp_max))
  colnames(res)<-seq(1,fp_max)

  for(i in 1:nrow(res)){
    for(j in 1:ncol(res)){
      n<-is.finite(df$finish_position) & is.finite(df$rating_rank)
      nn<-df[n,]
      w<-df$finish_position==i & df$rating_rank==j & is.finite(df$finish_position) & is.finite(df$rating_rank)
      ww<-df[w,]
      res[i,j]<-nrow(ww)/nrow(nn)
    }
  }
  return(res)
}

rSqPseudo<-function(data,price){

  filter<-data$finish_position==1 & is.finite(data$finish_position)
  #filter<-data$finish_order==1 & data[,paste(price)]>0 & data[,paste(price)]<=21 & is.finite(data[,paste(price)]) & is.finite(data$BOX) & data$CONF!='D' & data$CONF!='E'
  w<-data[filter,]
  data[,c(price)]<-as.numeric(data[,c(price)])
  data$logMkt<-log(1/data[,paste(price)])/log(exp(1))
  data$logStr<-log(1/data$runners)/log(exp(1))

  chance<-sum(data$logMkt,na.rm=T)
  measure<-sum(data$logStr,na.rm=T)
  rsq<-(1-chance/measure)
  return(rsq)
}

spearmanModel<-function(data,price){
  data$price_rank<-ave(as.numeric(data[,c(price)]),interaction(data$event_id),FUN=rank)
  data$price_rank[is.na(data[,c('price_rank')])]<-NA

  a<-as.numeric(cor.test(data$price_rank,data$finish_position,method="pearson")[4])
  return(a)
}

statBreakdown<-function(data){
  res<-as.data.frame(matrix(NA,3,3))
  colnames(res)<-c('type','public','handicap')
  res$type<-c('chi','rsq','spearman')

  res$public[1]<-chiSquareClassic(data,'sp_price',0)
  res$public[2]<-abs(rSqPseudo(data,'sp_price'))
  res$public[3]<-spearmanModel(data,'sp_price')

  res$handicap[1]<-chiSquareClassic(data,'prob_est',0)
  res$handicap[2]<-abs(rSqPseudo(data,'prob_est'))
  res$handicap[3]<-spearmanModel(data,'prob_est')

  res<-list(split(res[,c('public','handicap')],res$type))
  return(res)
}

effPlace<-function(data){
  x<-list()
  for(k in 1:2){
    if(k==1) price<-'prob_est'
    else price<-'sp_price'


    for(j in 1:3){
      perc<-c(.5,.45,.4,.35,.3,.25,.2,.15,.1,.05,.025,0)
      percu<-c(1,.5,.45,.4,.35,.3,.25,.2,.15,.1,.05,.025)
      n_col<-rep(NA,length(perc))
      res<-data.frame(perc,percu,n_col,n_col,n_col)
      colnames(res)<-c('perc','percu','n','Exp','Act')
      data[,c(price)]<-as.numeric(data[,c(price)])
      data$focusProb<-(1/data[,paste(price)])
      for (i in 1:nrow(res)){
        upper<-percu[i]
        lower<-perc[i]
        filter<-data$focusProb>lower & data$focusProb<=upper & is.finite(data[,c(price)])
        a<-data[filter,]
        b<-data[filter & data$finish_position==j,]
        res$n[i]<-nrow(a)
        res$Exp[i]<-sum(a$focusProb)/nrow(a)
        std.dev<-sd(a$focusProb)
        res$Act[i]<-nrow(b)/nrow(a)
        res$Z[i]<-(res$Act[i]-res$Exp[i])/std.dev
        flush.console()
      }
      res$percu<-NULL
      if(k==1) pricing<-'model'
      else pricing<-'market'
      x[[paste(pricing,'_efficiency_',j,sep="")]]<-list(split(res[,c('n','Exp','Act','Z')],res$perc))
    }
  }
  return(x)
}

masterSimulation<-function(dfrom,dto,animal,country,odds_l,odds_u,staking,mkt_l,mkt_u,exp_l,exp_u,conf_l,conf_u,pricing,sumType,market){
  df<-tradeSimulation(dfrom,dto,animal,country,odds_l,odds_u,staking,mkt_l,mkt_u,exp_l,exp_u,conf_l,conf_u,pricing,sumType,market)
  if(sumType=='Trading'){
    a<-trackSummary(df)
    b<-daySummary(df,dfrom,dto)
    c<-raceclassSummary(df)
    d<-distSummary(df)
    res<-jsonlite::toJSON(list(by_track=a,by_day=b,by_class=c,by_distance=d),pretty=TRUE)
  }
  if(sumType=='Stats'){
    a<-list(chiSquareClassic(df,'prob_est'),effPlace(df))
    b<-strikeRateRating(df,pricing)
    c<-byExpectation(df)
    d<-statBreakdown(df)
    res<-jsonlite::toJSON(list(by_chi=a,strike_rate_rating=b,by_exp=c,stat_summary=d),pretty=TRUE)
  }
  return(res)
}

#x<-masterSimulation('2018-02-01','2018-02-10','THOROUGHBRED','United Kingdom',1,100,0,1,100,1,100,1,3,'analyst','Trading','Betfair')
