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

# prepare a data folder for the outcomes
if( !dir.exists("_data") ) dir.create("_data")


# IN-HOUSE FUNCTIONS ---


# DATA READ ----

# variables mapping
v <-
  
  read.csv( here("_raw","vars.csv"), sep = ";" ) %>%
  
  # manually re-code variables that are different from the main Validacni studie data set
  mutate( type = case_when( variable == "sum_1_2" ~ NA, .default = type ) ) %>%
  mutate( variable = case_when( grepl("2_",test) ~ sub("_odd","_ok",variable), .default = variable ) ) %>%
  mutate( variable = ifelse( variable == "pribeh1_ok_sum1", "pribeh1_ok_sum", variable ) ) %>%
  mutate( variable = ifelse( grepl("_rules",variable), substr( variable, 1, (nchar(variable)-4) ), variable ) ) %>%
  mutate( variable = ifelse( variable == "sumaHS" & test == "8_pracovni_pamet", "suma_HS", variable ) ) %>%
  
  # administration time has different name in almost every data set
  mutate(
    variable =
      case_when(
        variable == "cas_adm (s)" & test == "16_zrakove_motoricka_presnost" ~ "cas_adm..s.",
        variable == "cas_adm (s)" & test == "17_zrakove_vnimani" ~ "cas_adm..s.",
        variable == "cas_adm (s)" & test == "19_orientace_v_prostoru" ~ "cas_adm..s.",
        .default = variable
      )
  ) %>%
  
  # keep only variables with all info needed
  filter( complete.cases(type) ) %>%
  filter( test != "3_neverbalni_pamet" ) %>% # drop non-verbal memory as it is not prepared
  filter( !(variable == "HS" & test == "5_zrakova_pozornost") ) # missing HS in "zrakova pozornost"

# read 
d0 <-
  
  lapply(
    setNames( unique(v$test), unique(v$test) ),
    function(i) {
      
      print(i)
      
      read.csv( here( "_raw", paste0(i,".csv") ), sep = "\t" ) %>%
        select( all_of( c( "kod_ditete", "poradi_zadanych_testu", "verze_testu", na.omit( v[ v$test == i, "variable" ] ) ) ) )

    }
  )
