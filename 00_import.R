# Run this script first to prepare data for analysis

# clear environment
rm( list = ls() )

# list packages to be used
pkgs <- c("here","tidyverse")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare data folders for the outcomes
sapply( "_data", function(i) if( !dir.exists(i) ) dir.create(i) )


# DATA READ ----

# the anamnesis
d0 <- read.csv( here( "_raw","0_anamneza.csv" ), sep = "\t" )

# outcome data
d1 <-
  
  lapply(
    
    setNames( list.files("_raw")[-1], sub( ".csv", "", list.files("_raw")[-1] ) ),
    function(i)
      read.csv( here("_raw",i), sep = "\t" )
    
  )

# extract variables object
v <- read.csv( here("_raw","vars.csv"), sep = ";" )

# manually sort the data where needed
for ( i in names(d1)[ grepl("wasi",names(d1)) ] ) d1[[i]] <- NULL
d1$vars <- NULL
d1$OKAMZITE_2B_PRECLUSTEROVANO <- NULL
d1$`Kopie 2B_pribehy_ODDALENE_PRECL` <- NULL
d1$`2B_pamet_na_pribehy_OKAMZITE)` <- NULL
d1$`2B_pamet_na_pribehy_ODDALENE` <- NULL
d1$`2_pamet_na_pribehy_OKAMZITE` <- NULL
d1$`2_pamet_na_pribehy_ODDALENE` <- NULL

# rename list names
names(d1)[ grepl( "ribehy", names(d1) ) ] <- paste0( "2_pamet_na_pribehy_", c("ODDALENE","OKAMZITE") )

# rename variables so they are aligned with 'v'
d1$`2_pamet_na_pribehy_ODDALENE` <-
  d1$`2_pamet_na_pribehy_ODDALENE` %>%
  rename( "pribeh1_odd_sum1" = "pribeh1_ok_sum", "pribeh2_odd_sum" = "pribeh2_ok_sum" ) %>%
  mutate( sum_1_2 = pribeh1_odd_sum1 + pribeh2_odd_sum )

d1$`2_pamet_na_pribehy_OKAMZITE` <- d1$`2_pamet_na_pribehy_OKAMZITE` %>% mutate( sum_1_2 = pribeh1_ok_sum + pribeh2_ok_sum )
d1$`6_trideni` <- d1$`6_trideni` %>% filter( poradi_zadanych_testu %in% c("A","B") )
d1$`8_pracovni_pamet` <- d1$`8_pracovni_pamet` %>% rename( "sumaHS" = "suma_HS" )
d1$`9_verbalni_fluence` <- d1$`9_verbalni_fluence` %>% mutate( KV_sum = K_sum + V_sum )
d1$`16_zrakove_motoricka_presnost` <- d1$`16_zrakove_motoricka_presnost` %>% rename( "cas_adm (s)" = "cas_adm..s." )
d1$`17_zrakove_vnimani` <- d1$`17_zrakove_vnimani` %>% rename( "cas_adm (s)" = "cas_adm..s." )
d1$`19_orientace_v_prostoru` <- d1$`19_orientace_v_prostoru` %>% rename( "cas_adm (s)" = "cas_adm..s." )


# change variables file appropriately
v <-
  
  v %>%
  
  mutate(
    variable = case_when(
      variable == "n_new_rules...2" ~ "n_new_rules",
      variable == "n_correct_rules...3" ~ "n_correct_rules",
      .default = variable
    ),
    type = ifelse( variable == "KV_sum", "cont", type ),
    test = ifelse( variable == "KV_sum", "9_verbalni_fluence", test )
  ) %>%
  
  filter( !grepl("err_",variable) ) %>% # emotion labeling errors
  filter( test != "3_neverbalni_pamet" ) %>% # get rid of original nonverbal memory variables
  add_row( variable = "P1.4_HS", type = "cont", test = "3_neverbalni_pamet", name = NA ) %>% # return nonverbal memory
  filter( !( variable == "HS" & test == "5_zrakova_pozornost" ) ) %>%
  filter( type != "cat" )

# check the variables are A-OK
lapply(
  
  setNames( names(d1), names(d1) ),
  function(i)
    cbind(
      c( na.omit( with( v, variable[ test == i ] ) ) ),
      na.omit( with( v, variable[ test == i ] ) ) %in% colnames(d1[[i]])
    )

)

# prepare final data.frame
d2 <-
  
  lapply(
    
    unique(v$test)[-1],
    function(i) {
      
      print( paste0("pre-processing  ", i," ...") ) # printing to diagnose mistakes
      
      d1[[i]] %>%
      select( all_of( c( "kod_ditete", "poradi_zadanych_testu", "verze_testu", with( v, variable[test == i] ) ) ) ) %>%
      mutate( test = i ) %>%
      pivot_longer(
        cols = all_of( with( v, variable[test == i] ) ),
        names_to = "index",
        values_to = "score"
      )

    }
  ) %>%
  
  # put it all together
  do.call( rbind.data.frame, . ) %>%
  left_join( d0[ d0$verze_testu == "A", c("kod_ditete","vek_roky","gender","trida") ], by = "kod_ditete" ) %>%
  
  # format it
  rename(
    "id" = "kod_ditete",
    "counterbalancing" = "poradi_zadanych_testu",
    "version" = "verze_testu",
    "age_years" = "vek_roky",
    "sex" = "gender",
    "class" = "trida"
  ) %>%
  
  # finish it
  mutate( sex = case_when( sex == "1=zena" ~ "female", sex == "0=muz" ~ "male" ) ) %>%
  mutate( index = paste0( sub( "_.*", "", test ), "_", index ) ) %>%
  pivot_wider( names_from = version, values_from = score )

# save the data
write.table( x = d2, file = here("_data","df.csv"), sep = ",", row.names = F, quote = F )
write.table( x = v, file = here("_data","vars.csv"), sep = ";", row.names = F, quote = F )
