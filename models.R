## Peer code review
## transportation cost analysis - models
## Author: Juliana Guerrero
## Date: 02/21/2024

## Purpose: Identify relevant factors that influence the cost of transportation

## load dataset
df_t = readRDS(file.path(github,
                         "outputs/trips_df.RDS"))


## descriptives
df_t %>% group_by(truck_type) %>% 
  summarise(trips = n(),
            total = mean(last_trip_distance,na.rm=T),
            feeder = mean(feed_dist,na.rm=T),
            nonfeeder= mean(nofeed_dist,na.rm=T),
            price_usd = mean(price_s,na.rm=T), # usd
            price_ton_rf = mean(price_ton/1000,na.rm=T))


################################################################################
## models
################################################################################

# response variable: price per trip

mt_1 = feols(price_s~truck_type+last_trip_distance,df_t)
print(etable(mt_1))


mt_2 = feols(price_s~truck_type+feed_dist+nofeed_dist,df_t)
etable(mt_2)

mt_3 = feols(price_s~truck_type+feed_dist+nofeed_dist+truck_type*feed_dist,df_t)
etable(mt_3)

mt_4 = feols(price_s~truck_type+feed_dist+nofeed_dist+truck_type*feed_dist+
                truck_type*nofeed_dist,df_t)
etable(mt_4)


mt_5 = feols(price_s~truck_type+last_trip_distance+truck_type*last_trip_distance,
              df_t)
etable(mt_5)



## price per trip in log scale

mt_1l = feols(log(price_s)~truck_type+last_trip_distance,df_t)
etable(mt_1l)


mt_2l = feols(log(price_s)~truck_type+feed_dist+nofeed_dist,df_t)
etable(mt_2l)

mt_3l = feols(log(price_s)~truck_type+feed_dist+nofeed_dist+truck_type*feed_dist,df_t)
etable(mt_3l)

mt_4l = feols(log(price_s)~truck_type+feed_dist+nofeed_dist+truck_type*feed_dist+
                truck_type*nofeed_dist,df_t)
etable(mt_4l)


mt_5l = feols(log(price_s)~truck_type+last_trip_distance+truck_type*last_trip_distance,
              df_t)
etable(mt_5l)


