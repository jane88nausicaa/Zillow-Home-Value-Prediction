library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

properties <- fread('properties_2016.csv')
transactions <- fread('train_2016_v2.csv')
sample_submission <- fread('sample_submission.csv')

properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid,
  no_fip = fips,
  latitude = latitude,
  longitude = longitude,
  s_h = pooltypeid10,
  pool_sh = pooltypeid2,
  pool_nosh = pooltypeid7,
  rawcensus = rawcensustractandblock,
  census = censustractandblock
)

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  error = logerror,
  date = transactiondate
)

properties <- properties %>%
  mutate(tax_delinquency = ifelse(tax_delinquency == "Y", 1,0),
         flag_fireplace = ifelse(flag_fireplace == "TURE", 1,0),
         flag_tub = ifelse(flag_tub == "TURE", 1,0))
         
# look at the distribution of our outcome (logerror)
pdf("fig/logerror_distribution.pdf", width = 7, height = 4)
transactions %>% 
  ggplot(aes(x = error)) + 
  geom_histogram(bins = 400, fill = "blue")+
  theme_bw() + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))+
  ylab("Count") + coord_cartesian(x = c(-0.5,0.5))
dev.off()

# deal with missing values
pdf("fig/missing_values.pdf", width = 7, height = 9)
missing_values <- properties %>% summarize_each(funs(sum(is.na(.)) / n()))
missing_values <- gather(missing_values, key = "feature", value="missing_prop")
missing_values %>% 
  ggplot(aes(x = reorder(feature, -missing_prop), y = missing_prop)) +
  geom_bar(stat = "identity",fill = "blue")+
  coord_flip() + theme_bw()
dev.off()
  
# choose good features
good_features <- filter(missing_values, missing_prop<0.80)

good_features

pdf("fig/good_features.pdf", width = 7, height = 8)
good_features %>%
  ggplot(aes(x = reorder(feature,-missing_prop), y = missing_prop)) +
  geom_bar(stat = "identity", fill = "red")+
  coord_flip() + theme_bw()
dev.off()


vars <- good_features$feature[str_detect(good_features$feature,'num_')]
cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"error")))
num_cor <- cor(tmp, use="complete.obs")

pdf("fig/num_cor.pdf", width = 9, height = 7)
corrplot(num_cor,type="lower", method="number")
#corrplot(cor(tmp, use="complete.obs"),type="lower",method="square")
dev.off()


vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- cor_tmp %>%  select(one_of(c(vars,"error")))
area_cor <- cor(tmp, use="complete.obs")

pdf("fig/area_cor.pdf", width = 9, height = 7)
corrplot(area_cor,type="lower", method="number")
#corrplot(cor(tmp, use="complete.obs"),type="lower",method="square")
dev.off()


vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))
tmp <- cor_tmp %>%  select(one_of(c(vars,"error")))
tax_cor <- cor(tmp, use="complete.obs")

pdf("fig/tax_cor.pdf", width = 8, height = 6)
corrplot(tax_cor,type="lower", method="number")
#corrplot(cor(tmp, use="complete.obs"),type="lower",method="square")
dev.off()



