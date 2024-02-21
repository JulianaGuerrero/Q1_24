## Peer code review
## transportation cost analysis - data cleaning script
## Author: Juliana Guerrero
## Date: 02/21/2024

## Purpose: Clean and prepare data set for analysis on transportation cost


## data cleaning

## load data sets

## Baseline trips
bsl_tr_trips = read.csv(file.path(dropbox,
                                  'Rwanda Roads Data/Primary data/Trucker Experiment/Raw/Trucker Baseline/long format/Trucker Baseline-survey-transport-last_trips.csv'))
## Baseline truckers information
bsl_tr = read.csv(file.path(dropbox,
                            'Rwanda Roads Data/Primary data/Trucker Experiment/Raw/Trucker Baseline/long format/Trucker Baseline.csv'))


## endline truckers information
end_tr = read.csv(file.path(dropbox,
                            'Rwanda Feeder Roads/Trucker experiment/2023C/HFC/Truckers/Trucker endline survey.csv'))

## endline preload for truck types
end_pre = read.csv(file.path(dropbox,
                             'Rwanda Feeder Roads/Trucker experiment/2023C/preloads/Trucker_endline/eligible_truckers.csv'))


## endline trips
end_tr_trips = read.csv(file.path(dropbox,
                                  'Rwanda Feeder Roads/Trucker experiment/2023C/HFC/Truckers/Trucker endline survey-survey-consented-still_own_truck-recent_trips-last_trips.csv'))




## Willingness to accept data
## trips information
wta_b = read.csv(file.path(dropbox,
                           'Rwanda Feeder Roads/Trucker experiment/2023B/Monitor_data/WTA 23B/Trucker WTA 23B-survey-consented-recent_trips-last_trips.csv'))

## truckers information
wta_b_tru = read.csv(file.path(dropbox,
                               'Rwanda Feeder Roads/Trucker experiment/2023B/Monitor_data/WTA 23B/Trucker WTA 23B.csv'))


################################################################################
## cleaning and transformations
################################################################################

#### remove atypical values for analysis

# remove high prices in baseline survey
bsl_tr_trips = bsl_tr_trips %>% 
  filter(last_trip_price<1000000)

## filter end when qty is missing
# filter when qutity is larger than 10 tons
end_tr_trips = end_tr_trips %>% 
  filter(last_trip_quant!=0) %>% 
  filter(last_trip_quant<10000)


### add truck size/type

## baseline
bsl_tr_trips = bsl_tr_trips %>% 
  left_join(bsl_tr %>% select(truck_size,truck_type,KEY),
            by=c('PARENT_KEY'='KEY'))

## wta
wta_b = wta_b %>% 
  left_join(wta_b_tru %>% select(truck_type,KEY),
            by=c('PARENT_KEY'='KEY'))


## endline
## add trucker id and then add truck type
end_tr_trips = end_tr_trips %>% 
  left_join(end_tr %>% select(trucker_id,KEY),
            by=c('PARENT_KEY'='KEY')) %>% 
  left_join(end_pre %>% select(trucker_id,truck_type),
            by='trucker_id')




## consolidate price variable with revealed and stated values
# baseline price
bsl_tr_trips = bsl_tr_trips %>% 
  mutate(price_c = ifelse(!is.na(last_trip_price_hypo),last_trip_price_hypo,
                          last_trip_price))

## endline price
end_tr_trips = end_tr_trips %>% 
  mutate(price_c = ifelse(!is.na(last_trip_price_hypo),last_trip_price_hypo,
                          last_trip_price))

## wta price
wta_b = wta_b %>% 
  mutate(price_c = ifelse(!is.na(last_trip_price_hypo),last_trip_price_hypo,
                          last_trip_price))



##  standardized price by capacity, quantity per ton

#baseline
bsl_tr_trips = bsl_tr_trips %>% 
  mutate(price_ton=ifelse(is.na(last_trip_quant),
                          price_c/truck_size,
                          price_c/(last_trip_quant/907))) # 1 ton-->907kg

#endline
end_tr_trips = end_tr_trips %>% 
  mutate(price_ton=price_c/(last_trip_quant/907))

#wta
wta_b = wta_b %>% 
  mutate(price_ton=price_c/(last_trip_quant/907))


## replace nas with zeros,
## feeder dist standardized in percentages of total distance
## no feeder road distance

# baseline
bsl_tr_trips = bsl_tr_trips %>% 
  mutate(feed_dist= ifelse(is.na(last_trip_dist_fr),0,last_trip_dist_fr)) %>% 
  mutate(feed_dist_per = 100*feed_dist/last_trip_distance,
         nofeed_dist = last_trip_distance - feed_dist) 
# endline
end_tr_trips = end_tr_trips %>% 
  mutate(feed_dist= ifelse(is.na(last_trip_dist_feeder),0,last_trip_dist_feeder)) %>% 
  mutate(feed_dist_per = 100*feed_dist/last_trip_distance,
         nofeed_dist = last_trip_distance - feed_dist) 

# wta
wta_b = wta_b %>% 
  mutate(feed_dist= ifelse(is.na(last_trip_dist_fr),0,last_trip_dist_fr)) %>% 
  mutate(feed_dist_per = 100*feed_dist/last_trip_distance,
         nofeed_dist = last_trip_distance - feed_dist) 



# road type as paved and unpaved
# 1	Feeder road
# 2	Paved/tarmacked road
# 3	Damaged road/unpaved/no laterite

# baseline
bsl_tr_trips = bsl_tr_trips %>% 
  mutate(unpaved=ifelse(last_trip_road_type_1==1 | last_trip_road_type_3==1,
                        'unpaved','paved'))

# endline
end_tr_trips = end_tr_trips %>% 
  mutate(unpaved=ifelse(last_trip_road_type_1==1 | last_trip_road_type_3==1,
                        'unpaved','paved'))

# wta
wta_b = wta_b %>% 
  mutate(unpaved=ifelse(last_trip_road_type_1==1 | last_trip_road_type_3==1,
                        'unpaved','paved'))

## truck type as order factor

# filter truck types 5 and 6 in baseline, 
# these are not included in wta or endline
bsl_tr_trips = bsl_tr_trips %>% 
  filter(truck_type %in% c(1,2,3,4))

# transform the truck type as factor
bsl_tr_trips$truck_type = as.factor(bsl_tr_trips$truck_type)

wta_b$truck_type = as.factor(wta_b$truck_type)

end_tr_trips$truck_type = as.factor(end_tr_trips$truck_type)

### price per trip

# standardized by 1000 factor prices
# 1000 rwandan francs --> 1 usd

bsl_tr_trips = bsl_tr_trips %>% 
  mutate(price_s =price_c/1000)

wta_b = wta_b %>% 
  mutate(price_s =price_c/1000)

end_tr_trips = end_tr_trips %>% 
  mutate(price_s =price_c/1000)

## complete data set 
df_t = bind_rows(bsl_tr_trips %>% 
                   select(price_ton,price_c, price_s,unpaved,last_trip_distance,
                          truck_type,feed_dist,feed_dist_per,nofeed_dist,last_trip_quant) %>% 
                   mutate(sea = 'Baseline'),
                 wta_b %>% 
                   select(price_ton,price_c, price_s,unpaved,last_trip_distance,
                          truck_type,feed_dist,feed_dist_per,nofeed_dist,last_trip_quant) %>% 
                   mutate(sea = 'WTAB'),
                 end_tr_trips %>% 
                   select(price_ton,price_c, price_s,unpaved,last_trip_distance,
                          truck_type,feed_dist,feed_dist_per,nofeed_dist,last_trip_quant) %>% 
                   mutate(sea = 'Endline'))

## save outputs for replication
saveRDS(bsl_tr_trips %>% 
          select(price_ton,price_c, price_s,unpaved,last_trip_distance,
                 truck_type,feed_dist,feed_dist_per,nofeed_dist,last_trip_quant) %>% 
          mutate(sea = 'Baseline'),
        file.path(github,"outputs/trips_bsl.RDS"))
saveRDS(wta_b %>% 
           select(price_ton,price_c, price_s,unpaved,last_trip_distance,
                  truck_type,feed_dist,feed_dist_per,nofeed_dist,last_trip_quant) %>% 
           mutate(sea = 'WTAB'),
        file.path(github,"outputs/trips_wta.RDS"))
saveRDS(end_tr_trips %>% 
           select(price_ton,price_c, price_s,unpaved,last_trip_distance,
                  truck_type,feed_dist,feed_dist_per,nofeed_dist,last_trip_quant) %>% 
           mutate(sea = 'Endline'),
        file.path(github,"outputs/trips_end.RDS"))

saveRDS(df_t,
        file.path(github,"outputs/trips_df.RDS"))
