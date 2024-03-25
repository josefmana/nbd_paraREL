# Calculate correlations and comparisons.

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

v <- read.csv( here( "_data","vars.csv" ), sep = ";" )
df <- read.csv( here( "_data","df.csv" ), sep = "," )
d2 <- df %>% pivot_longer( cols = c("A","B"), names_to = "version", values_to = "score" ) # long format data for difference stats


# CORRELATIONS ----

# calculate Pearson's correlations
corr <-
  
  # calculate it
  corrcomp(df,"pearson") %>%
  left_join( corrcomp(df,"spearman"), by = c("test","index") ) %>%
  
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
df %>%
  
  ggplot() +
  aes( x = A, y = B, colour = test ) +
  geom_point( size = 2, colour = "black" ) +
  geom_smooth( method = lm, size = 2 ) +
  theme_minimal( base_size = 12 ) +
  facet_wrap( ~ index, scales = "free" ) +
  theme( legend.position = "bottom" )

# save the plot
ggsave( plot = last_plot(), filename = here("figs","scatters.jpg"), dpi = 300, height = 16.2, width = 15.9 )


# DIFFERENCES ----

diffs <-
  
  # extract mean ± SDs
  df %>%
  group_by( test, index ) %>%
  get_summary_stats( c(A,B), type = "mean_sd") %>%
  mutate( msd = paste0( rprint(mean,2), " ± ", rprint(sd,2) ) ) %>%
  select( test, index, variable, msd ) %>%
  pivot_wider( names_from = variable, values_from = msd ) %>%
  
  left_join(
    
    # paired t-test
    d2 %>%
      group_by( test, index ) %>%
      t_test( score ~ version, paired = T, detailed = T ) %>%
      mutate(
        `diff [95% CI]` = paste0( rprint(estimate,2), " [", rprint(conf.low,2), ", ", rprint(conf.high,2), "]" ),
        `t-value` = rprint( statistic, 3 ),
        `p-value (t-test)` = ifelse( p < .001, "< .001", zerolead( rprint(p,3) ) )
      ) %>%
      select( test, index, `diff [95% CI]`, `t-value`, df, `p-value (t-test)` ),
    by = c("test","index")
  ) %>%
  
  # Cohen's d
  left_join(
    
    d2 %>%
      group_by( test, index ) %>%
      cohens_d( score ~ version, paired = T ) %>%
      mutate( `Cohen's d` = rprint(effsize,2) ) %>%
      select( test, index, `Cohen's d` ),
    
    by = c("test","index")
  ) %>%
  
  # Wilcoxon test
  left_join(
    
    d2 %>%
      group_by( test, index ) %>%
      wilcox_test( score ~ version, paired = T, detailed = F ) %>%
      mutate(
        `V value` = rprint(statistic,1),
        `p-value (Wilcoxon)` = ifelse( p < .001, "< .001", zerolead( rprint(p,3) ) )
      ) %>%
      select( test, index, `V value`, `p-value (Wilcoxon)` ),
    
    by = c("test","index")
    
  ) %>%
  
  # Wilcoxon test effect size
  left_join(
    
    d2 %>%
      group_by( test, index ) %>%
      wilcox_effsize( score ~ version, paired = T ) %>%
      mutate( `r value` = rprint(effsize,2) ) %>%
      select( test, index, `r value` ),
    
    by = c("test","index")
    
  )

# save it
write.table( diffs, here("tabs","diffs.csv"), sep = ";", row.names = F, quote = F )


# SESSION INFO ----
capture.output( sessionInfo(), file = "stats_envir.txt" )
