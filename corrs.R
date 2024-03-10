# Run this script first to prepare data for analysis

# clear environment
rm( list = ls() )

# list packages to be used
pkgs <- c("here","tidyverse","rstatix")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare data folders for the outcomes
sapply( c("_data","tabs","figs"), function(i) if( !dir.exists(i) ) dir.create(i) )


# IN-HOUSE FUNCTIONS ---

# round and print
rprint <- function(x,dec=2) sprintf( paste0("%.",dec,"f"), round( x , dec) )

# get rid of leading zero
zerolead <- function(x) sub( "0.", ".", x, fixed = T )

# compute correlations
corrcomp <- function(d,meth="pearson") {
  
  # calculate it
  d %>%
    group_by( test, index ) %>%
    cor_test( A, B, method = meth, conf.level = .95, use = "pairwise.complete.obs" ) %>%
    
    # format it
    mutate( test = factor( test, levels = unique(v$test), ordered = T ) ) %>%
    arrange( test )
  
}


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
  filter( type != "cat" ) %>%
  filter( !(test %in% c("0_anamneza","3_neverbalni_pamet") ) ) %>% # drop non-verbal memory as it is not prepared
  filter( !(variable == "HS" & test == "5_zrakova_pozornost") ) %>% # missing HS in "zrakova pozornost"
  filter( variable != "n_new_rules" ) # no data in the data set

# read the anamesis
d0 <- read.csv( here( "_raw","0_anamneza.csv" ), sep = "\t" )

# read the data
d1 <-
  
  lapply(
    
    setNames( unique(v$test), unique(v$test) ),
    function(i) {
      
      print( paste0(" loading ", i," ...") ) # printing to diagnose mistakes

      # load it
      read.csv( here( "_raw", paste0(i,".csv") ), sep = "\t" ) %>%
        select( all_of( c( "kod_ditete", "poradi_zadanych_testu", "verze_testu", with( v, variable[test == i] ) ) ) ) %>%
        mutate( test = i ) %>%
        pivot_longer(
          cols = all_of( with( v, variable[test == i] ) ),
          names_to = "index",
          values_to = "score"
        )

    }) %>%
  
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
  
  # drop rows that cannot be analysed now
  filter( id != 12 ) %>% # unclear what was A and what was B
  filter( !( grepl("2_",test) ) ) %>% # stories test is not prepared
  
  # finish it
  mutate( sex = case_when( sex == "1=zena" ~ "female", sex == "0=muz" ~ "male" ) ) %>%
  mutate( index = paste0( sub( "_.*", "", test ), "_", index ) ) %>%
  pivot_wider( names_from = version, values_from = score )

# save the data
write.table( x = d1, file = here("_data","df.csv"), sep = ",", row.names = F, quote = F )


# ANALYSIS ----

# calculate Pearson's correlations
corr <-
  
  # calculate it
  corrcomp(d1,"pearson") %>%
  left_join( corrcomp(d1,"spearman"), by = c("test","index") ) %>%
  
  # format it
  mutate( test = factor( test, levels = unique(v$test), ordered = T ) ) %>%
  arrange( test )

# plot correlations
corr %>%
  
  ggplot() +
  aes( x = cor.x, y = reorder(index,cor.x), xmin = conf.low, xmax = conf.high, colour = test, fill = test ) +
  geom_linerange( linewidth = 3, mapping = aes(alpha = .3) ) +
  geom_point( size = 3 ) +
  geom_vline( xintercept = .8, linewidth = 1, linetype = "dashed", color = "red" ) +
  theme_minimal( base_size = 14 ) +
  facet_wrap( ~ test, scales = "free_y", ncol = 3 ) +
  theme( legend.position = "none" )

# save the plot
ggsave( plot = last_plot(), filename = here("figs","pearson_corr.jpg"), dpi = 300, height = 16.2, width = 15.9 )

# save the table
write.table(
  
  # prepare the file
  corr %>%
    mutate(
      `Pearson's r` = zerolead( rprint(cor.x,2) ),
      `95% CI` = paste0( "[", zerolead( rprint(conf.low,2) ), ", ", zerolead( rprint(conf.high,2) ), "]" ),
      `t-statistic` = rprint(statistic.x,3),
      `p-value (r)` = ifelse( p.x < .001, "< .001", zerolead( rprint(p.x,3) ) ),
      `Spearman's rho` = zerolead( rprint(cor.y,2) ),
      `S statistic` = rprint(statistic.y,1),
      `p-value (rho)` = ifelse( p.y < .001, "< .001", zerolead( rprint(p.y,3) ) )
    ) %>%
    select(test,index,`Pearson's r`,`95% CI`,`t-statistic`,`p-value (r)`,`Spearman's rho`,`S statistic`,`p-value (rho)`),
  
  # save it
  file = here("tabs","corrs.csv"),
  sep = ";",
  row.names = F,
  quote = F

)

# plot scatter plots
d1 %>%
  
  ggplot() +
  aes( x = A, y = B, colour = test ) +
  geom_point( size = 2, colour = "black" ) +
  geom_smooth( method = lm, size = 2 ) +
  theme_minimal( base_size = 12 ) +
  facet_wrap( ~ index, scales = "free" ) +
  theme( legend.position = "bottom" )

# save the plot
ggsave( plot = last_plot(), filename = here("figs","scatters.jpg"), dpi = 300, height = 16.2, width = 15.9 )


# SESSION INFO ----
capture.output( sessionInfo(), file = "corrs_envir.txt" )
