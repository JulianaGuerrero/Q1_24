# models



################################################################################
## models
################################################################################

### price per ton

m1_bsl = feols(price_ton~unpaved + feed_dist,bsl_tr_trips)
etable(m1_bsl)

m2_bsl = feols(price_ton~unpaved + last_trip_distance,bsl_tr_trips)
etable(m2_bsl)

m3_bsl = feols(log(price_ton)~unpaved + last_trip_distance,bsl_tr_trips)
etable(m3_bsl)

m4_bsl = feols(log(price_ton)~unpaved + log(last_trip_distance),bsl_tr_trips)
etable(m4_bsl)


m5_bsl = feols((price_c/1000)~truck_type+unpaved +(last_trip_distance/1000),bsl_tr_trips)
etable(m5_bsl)


m5_bsl = feols(price_s~truck_type+unpaved +dist_s,bsl_tr_trips)
etable(m5_bsl)

# m6_bsl = feols(price_c~truck_type+unpaved + last_trip_distance+feed_dist,bsl_tr_trips)
# etable(m6_bsl)

m7_bsl = feols(price_s~truck_type + dist_s+feed_dist_per,bsl_tr_trips)
etable(m7_bsl)

m8_bsl = feols(price_s~truck_type + feed_ds,bsl_tr_trips)


### Endline
m1_end = feols(price_ton~unpaved + feed_dist,end_tr_trips)
etable(m1_end)

m2_end = feols(price_ton~unpaved + last_trip_distance,end_tr_trips)
etable(m2_end)

m3_end = feols(log(price_ton)~unpaved + last_trip_distance,end_tr_trips)
etable(m3_end)

m4_end = feols(log(price_ton)~unpaved + log(last_trip_distance),end_tr_trips)
etable(m4_end)


# price per trip
m5_end = feols(price_s~truck_type+unpaved +dist_s,end_tr_trips)
etable(m5_end)

# m6_end = feols(price_c~unpaved + last_trip_distance+feed_dist,end_tr_trips)
# etable(m6_end)


m7_end = feols(price_s~truck_type + dist_s+feed_dist_per,end_tr_trips)
etable(m7_end)

m8_end = feols(price_s~truck_type + feed_ds,end_tr_trips)
etable(m8_end)


m10_end = feols(price_s~truck_type + feed_ds+unpaved+unpaved:truck_type,end_tr_trips)
etable(m10_end)
## wta b
m3_wta = feols(log(price_ton)~unpaved + last_trip_distance,wta_b)
etable(m3_wta)

m4_wta = feols(log(price_ton)~unpaved + log(last_trip_distance),wta_b)
etable(m4_wta)

# price per trip
m5_wta = feols(price_s~truck_type+unpaved +dist_s,wta_b)
etable(m5_wta)

# m6_wta = feols(price_c~unpaved + last_trip_distance+feed_dist,wta_b)
# etable(m6_wta)


m7_wta = feols(price_s~truck_type + dist_s+feed_dist_per,wta_b)
etable(m7_wta)

m8_wta = feols(price_s~truck_type + feed_ds,wta_b)
etable(m8_wta)

m10_wta = feols(price_s~truck_type + feed_ds+unpaved+unpaved:truck_type,wta_b)
etable(m10_wta)



###################################################
## models by trip
###################################################
mt_1 = feols(price_c~truck_type+last_trip_distance,df_t)
etable(mt_1)

mt_2 = feols(price_s~truck_type+last_trip_distance,df_t)
etable(mt_2)


mt_3 = feols(price_s~truck_type+feed_dist+nofeed_dist,df_t)
etable(mt_3)

mt_4 = feols(price_s~truck_type+feed_dist+nofeed_dist+truck_type*feed_dist,df_t)
etable(mt_4)

mt_5 = feols(price_s~truck_type+feed_dist+nofeed_dist+truck_type*feed_dist+
               truck_type*nofeed_dist,df_t)
etable(mt_5)


mt_6 = feols(price_s~truck_type+last_trip_distance+truck_type*last_trip_distance,
             df_t)
etable(mt_6)


m7_wta = feols(price_s~truck_type + dist_s+feed_dist_per,wta_b)
etable(m7_wta)
