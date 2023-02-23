
rm(list=ls())

{#load

library("tictoc")
library("readr")
library("psych")
library("dplyr")          
library("evd")           
#library("apollo")
library("tidyr")
library("kableExtra")
#library("tidylog")
library("gridExtra")
library("stringr")
library("mixl")
library("furrr")
library("purrr")
library("ggplot2")
library("formula.tools")
  source("functions.R")
}

resps =330  # number of respondents
nosim=5  # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"
basc = 0.5
btilapia = -0.2
bcichlids = -0.3
btoxin = 0.2
bkisumu = 0.1
bprice = -0.1


#place your utility functions here
u<-list(
v1 =V.1~ basc + btilapia*alt1.tilapia + bcichlids * alt1.cichlids + btoxin * alt1.toxin + bkisumu * alt1.origin + bprice * alt1.price,
v2 =V.2~ basc + btilapia*alt2.tilapia + bcichlids * alt2.cichlids + btoxin * alt2.toxin + bkisumu * alt2.origin + bprice * alt2.price,
v3 =V.3~ 0)


mnl_U <-paste(map_chr(u,as.character,keep.source.attr = TRUE),collapse = "",";") %>%
  str_replace_all( c("~" = "=", "\\." = "_" , " b" = " @b_" , "V."="U_", "alt"="$alt"))

  

### in case you want the estimated function to differ from the actual function, use this code: 

# mnl_U <- "U_1 = @b_asc + @b_tilapia * $alt1_tilapia + @b_cichlids * $alt1_cichlids + @b_toxin * $alt1_toxin + @b_kisumu * $alt1_origin + @b_price * $alt1_price ;
#           U_2 = @b_asc + @b_tilapia * $alt2_tilapia + @b_cichlids * $alt2_cichlids + @b_toxin * $alt2_toxin + @b_kisumu * $alt2_origin + @b_price * $alt2_price ;
#           U_3 = 0 ;
#   "







designfile<-list.files("Designs/",full.names = F)
designname <- str_remove_all(designfile,"(.ngd|_)")  ## Make sure it designnames to not contain file ending and "_", as the may cause issues when replace





#plan(multisession, workers = 8)
#plan(sequential)
tictoc::tic()

all_designs<- purrr::map(list.files("designs/",full.names = T), sim_choice,no_sim= nosim,respondents = resps, mnl_U = mnl_U) %>% 
  setNames(designname)


#all_designs<- furrr::future_map(list.files("designs/",full.names = T), sim_choice,no_sim= nosim,respondents = resps, mnl_U = mnl_U, .options = furrr_options(seed = T)) %>% 
#setNames(designname) ### Issue is somewhere here

time <- tictoc::toc()

print(time)



powa <- map(all_designs, ~ .x$power)




summaryall <- as.data.frame(map(all_designs, ~.x$summary)) %>% 
  select(!ends_with("vars")) %>% 
  relocate(ends_with(c(".n", "mean","sd", "min" ,"max", "range" , "se" )))

coefall <- map(all_designs, ~ .x$coefs)

pat<-paste0("(",paste(designname,collapse = "|"),").") # needed to identify pattern to be replaced

s<-as.data.frame(coefall) %>% 
  select(!matches("pval|run")) %>%
  rename_with(~ sub("est_b_", "", .x), everything()) %>%
  rename_with( ~ paste0(.,"_",stringr::str_extract(.,pat )), everything() ) %>%   # rename attributes for reshape part 1
  rename_with( ~ stringr::str_replace(.,pattern = pat,replacement=""), everything() )  %>% 
  reshape(varying =1:ncol(.), sep = "_"  , direction = "long" ,timevar = "design", idvar = "run" )


p=list()

for (att in names(select(s,-c("design","run")))) {
  
  p[[att]] <- plot_multi_histogram(s,att,"design")
  
  print(p[[att]])
  
}

do.call(grid.arrange,p)


#saveRDS(all_designs, file = paste0("output/",respondents,"_",no_sim,"runs_4designs_mixl.RDS"))