# load packages
library(tidyverse)
require(readstata13)

## Read in the .dta files
# For Proxy Industrialization: population growth
# hornung_dt_cs <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-cross-section.dta")
hornung_dt_ps <- readstata13::read.dta13("../data/Population&Rail/hornung-rail-panel-city.dta")

# Read in the .dta files
# For Proxy Nationalism Turner in Prussia
turnen <- read.csv("../data/Proxy Nationalism/Turnvereine Prussia/Turnvereine Prussia.csv", sep = ";")

readstata13::read.dta13("../data/Population&Rail/hornung-rail-panel-city.dta")

# Select important variables for Turner
turn <- turnen %>% dplyr::select(
  city, Jahr, Gesamt, Beruf_Handwerk,
  Beruf_Gelehrte, Beruf_Kaufleute
)


# Replace missing values with na
turn[turn == "na"] <- "Missing Value"


# Select important variables for Population census. Kreiskey1849 is particularly
# important to match Turncities to their county, to then later
# match them with other data sets containing data only on county level.

dat <- hornung_dt_ps %>%
  dplyr::select(townkey1849, city, year, popcivil, kreiskey1849) %>%
  dplyr::filter(year %in% c(1864, 1843))


# This is a function to check mismatched names between two arrays.
# First, get a unique array of names,


match_checker <- function(a, b) {
  require(tidyverse)
  if (length(a) > length(b)) {
    longer_list <- a
    shorter_list <- b
  } else {
    longer_list <- b
    shorter_list <- a
  }
  out <- data.frame(name = shorter_list, is_matched = NA)
  for (i in 1:dim(out)[1])
  {
    out$is_matched[i] <- out$name[i] %in% longer_list
  }
  return(out)
}


a <- unique(dat$city)
b <- unique(turn$city)

# Then, run the match_checker.

matches <- match_checker(a, b)

# if you print matches, it will return the list with T and F.

# if you run this, you can see how many remain:

print("cases matched:")
sum(matches$is_matched)
print("cases remaining:")
sum(!matches$is_matched)

write.csv(dplyr::filter(
  matches,
  is_matched == FALSE
),
"../data/Proxy Nationalism/matches_reference.csv",
sep = ";"
)


matchmaker <- read.csv("../data/Proxy Nationalism/Turnvereine Prussia/name_matches.csv", sep = ";")

for (i in 1:length(matchmaker$Old)) {
  turn$city <- gsub(matchmaker$Old[i], matchmaker$New[i], turn$city)
}

turn <- turn %>% filter(!(city == ""))

# Now manually check the city names of Turner and Hornung cities in csv.file
# and rename them to then match them again.
# Run the process again so all possible matches are matched.



# Merge the corrected version of Turner to Hornung cities for full match.

maindf <- left_join(dat, turn, by = "city")




# Replace NAs with 0s, as missing values mean no turnvereien present.
maindf$Gesamt <- ifelse(is.na(maindf$Gesamt), 0, maindf$Gesamt)
maindf$Beruf_Handwerk <- ifelse(is.na(maindf$Beruf_Handwerk), 0, maindf$Beruf_Handwerk)
maindf$Beruf_Gelehrte <- ifelse(is.na(maindf$Beruf_Gelehrte), 0, maindf$Beruf_Gelehrte)
maindf$Beruf_Kaufleute <- ifelse(is.na(maindf$Beruf_Kaufleute), 0, maindf$Beruf_Kaufleute)


maindf[maindf == "Missing Value"] <- NA


# Change the Turner variables to integrals, since they were characters.

maindf <- maindf %>% mutate(
  Gesamt = as.integer(Gesamt),
  Beruf_Handwerk = as.integer(Beruf_Handwerk),
  Beruf_Gelehrte = as.integer(Beruf_Gelehrte),
  Beruf_Kaufleute = as.integer(Beruf_Kaufleute)
)


# We want to sum "Gesamt" Turner in same city to calculate Turner share on population.

maindf <- as_tibble(maindf)

# Processing maindf to summarize total number of turner per town, not per turnverein.
# The 64 at the end of the variable name is designed to highlight the fact that the number of
# turner is only for the year 1864.The mean is to not sum up the population
# of a city in which are more than one Turnverein.

maindf <- maindf %>%
  group_by(townkey1849, city, year, kreiskey1849) %>%
  summarise(
    popcivil = mean(popcivil),
    Gesamt64 = sum(Gesamt),
    Beruf_Handwerk_64 = sum(Beruf_Handwerk),
    Beruf_Gelehrte_64 = sum(Beruf_Gelehrte),
    Beruf_Kaufleute_64 = sum(Beruf_Kaufleute)
  )

# Calculate Population Growth from 1843 to 1864.

## pop_growth <- maindf %>%
##   group_by(townkey1849) %>%
##   summarise(pop_growth_rate = (popcivil / lag(popcivil) - 1)) %>%
##   filter(!is.na(pop_growth_rate))

## # Calculate the share of Turnern in Population in 1864.
## turnshare <- maindf %>%
##   filter(year == 1864) %>%
##   group_by(townkey1849) %>%
##   summarize(turner_share = Gesamt64 / popcivil)

# Join variable pop_growth and turnshare to main data set

## maindf <- left_join(maindf, turnshare)
## maindf <- left_join(maindf, pop_growth)

# Two things achieved: First we now can analyse the role of population growth
# on Turner. Second we have data set with Turner matched to their county (Kreiskey)
# and city (townkey) as they are used in other data sets.


# Since all Industrialisation Data is on county (Kreis) level, we group all Cities
# to their Kreiskey to then merge with Industrialisation Data


maindf[is.na(maindf)] <- 0

maindf <- maindf %>%
  filter(year == 1864) %>%
  group_by(kreiskey1849) %>%
  summarise(
    popcivil = sum(popcivil),
    Gesamt64 = sum(Gesamt64),
    Beruf_Handwerk_64 = sum(Beruf_Handwerk_64),
    Beruf_Gelehrte_64 = sum(Beruf_Gelehrte_64),
    Beruf_Kaufleute_64 = sum(Beruf_Kaufleute_64)
  )

# The Data set of Hornung with which we merged the Turner misses 5 kreiskeys. 
# These kreiskeys are from 5 Landkreise which are also a big City (Cologne, Königsberg, etc.),
# thus the missing Kreiskeys of these Landkreise are allocated to their City.
# This is done by change the Kreiskey of the Landkreis to the same of the City.
# To remember even though Hornung Data set is not perfect, it was good to merge Turner
# because it contained nearly all Turncities, which no other data set did.
# This list is to help choose which kreiskeys are merged into one.

kreiskeychanger <- data.frame(oldkk = c(4, 40, 65, 249, 286), newkk = c(3, 39, 64, 246, 281))


# Read in file delienating the changes in the kreiskey over time. As the kreiskeys in 1849
# are not the same as in 1864. We need to merge the keys of 1864 to 1849.

mergerfile <- read.csv("../data/ipehd_merge_county.csv", sep = ";") %>%
  select(kreiskey1864, kreiskey1849)

# merge with main dataset.
maindf <- left_join(maindf, mergerfile)

# This function changes the kreiskeys depnding on the old-new data frame provided earlier.
# This needs to be given the exact column names. TODO make generalizable
kreiskeycleaner <- function(x, y) {
  for (i in 1:length(x$kreiskey1864)) {
    for (j in 1:length(y$oldkk)) {
      if (x$kreiskey1864[i] == y$oldkk[j]) {
        x$kreiskey1864[i] <- y$newkk[j]
      }
    }
  }
  return(x)
}


# reading in the data set providing the population by age bracket. we use this to get
# a proxy for the working population.

pop15to65 <- read.csv("../data/Data Proxy Industrializtaion/Religion&Population/ipehd_1864_pop_demo (1).csv",
  sep = ",") %>% select(kreiskey1864, pop1864_tot_15to65)

pop15to65 <- kreiskeycleaner(pop15to65, kreiskeychanger)


# Here we are just grouping by doubled or trippled kreiskeys and summing the values
# to get a single value per kreiskey
# Every variable added from the population data set needs to be summarized
pop15to65 <- pop15to65 %>%
  group_by(kreiskey1864) %>%
  summarize(pop1864_tot_15to65 = sum(pop1864_tot_15to65))

maindf <- left_join(maindf, pop15to65)

# Because popcivil of Hronung Data set only provides data for population in a county,
# who live in cities, we read in full data on poulation. Including denomination,
# which might be useful for later analysis. 
# Reading in religious data in 1849 to get: 1) pop data at county level, 2) denomiation data.

countypop1849 <- read.csv("../data/Data Proxy Industrializtaion/Religion&Population/ipehd_1849_rel_deno.csv",
  sep = ","
) %>%
  mutate(
    countypop1849 = (rel1849_pro +
                    rel1849_cat +
                    rel1849_greek +
                    rel1849_menno+
                    rel1849_jew),
    
    rel1849_other = (rel1849_greek +
                rel1849_menno +
                rel1849_jew)) %>%

select(countypop1849, 
       rel1849_pro, 
       rel1849_cat, 
       rel1849_other,
       kreiskey1849)


maindf <- left_join(maindf, countypop1849)

# Doing the same thing for 1864, plus we use the Kreiskeychanger 
# to merge Kreiskeys of 1864 to 1849.

countypop1864 <- read.csv("../data/Data Proxy Industrializtaion/Religion&Population/ipehd_1864_rel_deno (1).csv",
                          sep = ",") %>% mutate(countypop1864 = (rel1864_pro +
                                                                   rel1864_cat + 
                                                                   rel1864_jew +
                                                                   rel1864_oth),
                                                rel1864_other = (rel1864_jew +
                                                                   rel1864_oth)) %>%
  select(kreiskey1864,
         countypop1864,
         rel1864_pro,
         rel1864_cat,
         rel1864_other)



countypop1864 <- kreiskeycleaner(countypop1864, kreiskeychanger)

countypop1864 <- countypop1864 %>%
group_by(kreiskey1864) %>%
  summarise(
    countypop1864 = sum(countypop1864),
    rel1864_pro = sum(rel1864_pro),
    rel1864_cat = sum(rel1864_cat),
    rel1864_other = sum(rel1864_other))


# Merge grouped countypop1864 to Main file.

maindf <- left_join(maindf, countypop1864)



# Read in Industry worker data from 1864, remember to use Kreiskeychanger

Indu1864 <- read.csv("../data/Data Proxy Industrializtaion/Occupation/Industry 1864_occ_indu (1).csv",
                     sep = ",") %>% select(kreiskey1864, occ1864_ind)

Indu1864 <- kreiskeycleaner(Indu1864, kreiskeychanger)


# Replace NAs with 0s, as missing values mean worker are count to other county.
# Indu1864$occ1864_ind <- ifelse(is.na(Indu1864$occ1864_ind), 0, Indu1864$occ1864_ind)


# one kreiskey has an NA a the city level but not the kreis level, so we set it 
# to zero so adding it up giving a sum value. 
Indu1864$occ1864_ind[3] <- 0

# Group Kreiskeys together for single observation
Indu1864 <- Indu1864 %>%
group_by(kreiskey1864) %>%
  summarise(
  occ1864_ind = sum(occ1864_ind))



maindf <- left_join(maindf, Indu1864)

# Read in Number of students in 1864 for measuring Education
# Sum up all students in school, exclude Kindergarten and nursery.

edu1864 <- read.csv("../data/Data Proxy Industrializtaion/Education/ipehd_1864_edu_stud.csv") %>%
  transmute(kreiskey1864,
            county,
            students1864 = edu1864_pub_ele_stud + edu1864_pub_mim_stud_m + edu1864_pub_mif_stud_f 
            + edu1864_pub_rea_stud + edu1864_pub_pgy_stud + edu1864_pub_gym_stud 
            + edu1864_pub_sem_par + edu1864_pri_ele_stud + edu1864_pri_hsm_stud_m
            + edu1864_pri_hsf_stud_f + edu1864_pri_voc_stud + edu1864_pri_sun_stud)%>%

select(kreiskey1864, students1864)

edu1864 <- kreiskeycleaner(edu1864, kreiskeychanger)

edu1864 <- edu1864 %>%
  group_by(kreiskey1864) %>%
  summarise(
    students1864 = sum(students1864))

maindf <- left_join(maindf, edu1864)

# Here is where we stopped and ran the first regressions
# Read in Industrialization variables from 1849 starting with Industry worker


factorywork1849 <- read.csv("../data/Data Proxy Industrializtaion/Occupation/Factory 1849_occ_fac Kopie.csv",
                     sep = ",")  %>%
 transmute(kreiskey1849,
    textileworker1849 = fac1849_spin_woolyarn_workers + fac1849_spin_combyarn_workers
    + fac1849_spin_cotton_workers + fac1849_spin_flax_workers + fac1849_spin_tow_workers
    + fac1849_looms_silk_masters + fac1849_looms_silk_assistants + fac1849_looms_cotton_masters
    + fac1849_looms_cotton_assistants + fac1849_looms_linen_masters + fac1849_looms_linen_assistants
    + fac1849_looms_wool_masters + fac1849_looms_wool_assistants + fac1849_looms_hosiery_masters
    + fac1849_looms_hosiery_assistants + fac1849_looms_band_masters + fac1849_looms_band_assistants
    + fac1849_looms_others_masters + fac1849_looms_others_assistants + fac1849_fact_thread_workers
    + fac1849_fact_silk_ao_workers + fac1849_wool_cloth_workers + fac1849_halfwool_cloth_workers
    + fac1849_cotton_workers + fac1849_linen_workers +fac1849_silk_workers
    + fac1849_shawl_workers + fac1849_band_workers + fac1849_carpet_workers
    + fac1849_passement_workers + fac1849_hosiery_workers + fac1849_laces_workers
    + fac1849_bleach_unit_workers + fac1849_bleach_yarn_workers + fac1849_dye_turkeyred_workers
    + fac1849_dye_silk_workers + fac1849_dye_others_workers + fac1849_print_workers,
    
    metal_miningworker1849 = fac1849_ironworks_workers +  fac1849_wireworks_workers
    + fac1849_rake_workers +  fac1849_sewingneedle_workers +  fac1849_fixingpin_workers
    + fac1849_ironware_workers + fac1849_steel_workers + fac1849_steelware_workers
    + fac1849_copperhammer_workers + fac1849_brass_workers + fac1849_smeltery_workers
    + fac1849_bronzeware_workers + fac1849_engines_workers + fac1849_glassworks_workers
    + fac1849_grindery_workers + fac1849_mirrorglass_workers +  fac1849_porcelain_workers
    + fac1849_earthenware_workers + fac1849_chemicals_workers + fac1849_potboilery_workers
    + fac1849_limekiln_workers + fac1849_brickworks_workers + fac1849_tarfurnace_workers,
    
    other_factoryworker1849 = mill1849_water_masters + mill1849_water_assistants
    + mill1849_post_mills_masters + mill1849_post_mills_assistants
    + mill1849_dutchmills_masters + mill1849_dutchmills_assistants
    + mill1849_animals_workers + mill1849_steam_workers + mill1849_oil_workers
    + mill1849_fulling_workers + mill1849_tan_workers + mill1849_saw_german_workers
    + mill1849_saw_dutch_workers + mill1849_saw_circular_workers + mill1849_other_workers,
    
    all_factoryworker1849 = textileworker1849 + metal_miningworker1849
    + other_factoryworker1849)%>%
  
  select(kreiskey1849, textileworker1849, metal_miningworker1849, 
         other_factoryworker1849, all_factoryworker1849)

  
maindf <- left_join(maindf, factorywork1849)

# Reading in people working on farms full time and subsidiary in 1849
# Test if this is negatively correlated with Turner

farmer1849 <- read.csv("../data/Data Proxy Industrializtaion/Occupation/Farmer 1849_occ_agri.csv",
                            sep = ",")  %>% 
  mutate(kreiskey1849,
         farmer1849 = (occ1849_farming_main_occu +
                       occ1849_farming_subsidiary_occu +
                       occ1849_servant_m_farm +
                       occ1849_servant_f_farm)) %>%
  
  select(kreiskey1849, farmer1849)

maindf <- left_join(maindf, farmer1849)

# Reading in all steam powered machines and engines


steamengines <- read.csv("../data/Data Proxy Industrializtaion/Industrialization/1849_indu_tec.csv",
                             sep = ",") %>%
  
  transmute(kreiskey1849, 
            steamengines1849 = mill1849_steam_grindingstones + steam1849_spinnery_engines
            + steam1849_spinnery_horsepower + steam1849_weaving_engines
            + steam1849_weaving_horsepower + steam1849_fulling_engines
            + steam1849_fulling_horsepower + steam1849_engineworks_engines
            + steam1849_engineworks_horsepower + steam1849_flourmill_engines
            + steam1849_flourmill_horsepower + steam1849_cuttingmill_engines
            + steam1849_cuttingmill_horsepower + steam1849_othermills_engines
            + steam1849_othermills_horsepower + steam1849_mining_engines
            + steam1849_mining_horsepower + steam1849_shipping_engines
            + steam1849_shipping_horsepower + steam1849_metal_fab_engines
            + steam1849_metal_fab_horsepower + steam1849_railway_engines
            + steam1849_railway_horsepower + steam1849_others_engines +
            steam1849_others_horsepower)%>%
  
  select(kreiskey1849, steamengines1849)

maindf <- left_join(maindf, steamengines)

view(steamengines)

# Reading in Craftsmen in 1849, since its the highest share of occupational class
# within the Turner. Craftsmen will not be categorized, taking craftsmen in general.

craftsmen <- read.csv("../data/Data Proxy Industrializtaion/Occupation/Craftsmen 1849_occ_craft Kopie.csv",
                         sep = ",")
  
  
craftsmenlength <- dim(craftsmen)[2]
craftsmenlengthonlydata <- select(craftsmen, !c(kreiskey1849, county, rb))

for(i in 1:dim(craftsmenlengthonlydata)[1]) {
  craftsmen$crafters[i] <- sum(craftsmenlengthonlydata[i,])
}

craftsmen <- select(craftsmen, kreiskey1849, crafters)


maindf <- left_join(maindf, craftsmen)

# Read in Education for school enrollment rate in 1849

edu1849 <- read.csv("../data/Data Proxy Industrializtaion/Education/ipehd_1849_edu_stud.csv") %>%
  transmute(kreiskey1849,
            students1849 = edu1849_pub_ele_stud_m + edu1849_pub_ele_stud_f
            + edu1849_pub_mim_stud_m + edu1849_pub_mif_stud_f 
            + edu1849_pub_high_stud_m + edu1849_pub_pgy_stud_m 
            + edu1849_pub_gym_stud_m + edu1849_pub_sem_stud)%>%
  
  select(kreiskey1849, students1849)

maindf <- left_join(maindf, edu1849)


maindf <- distinct(maindf)

# save file for future procesing
saveRDS(maindf, file = "../data/turner_share.RDS")
