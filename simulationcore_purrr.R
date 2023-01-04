
rm(list=ls())



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




basc = 0.5
btilapia = -0.2
bcichlids = -0.3
btoxin = 0.2
bkisumu = 0.1
bprice = -0.1


mnl_U_test <- "
  
    U_1 = @b_asc + @b_tilapia*$alt1_tilapia + @b_cichlids * $alt1_cichlids + @b_toxin * $alt1_toxin + @b_kisumu * $alt1_origin + @b_price * $alt1_price ;
    U_2 = @b_asc + @b_tilapia*$alt2_tilapia + @b_cichlids * $alt2_cichlids + @b_toxin * $alt2_toxin + @b_kisumu * $alt2_origin + @b_price * $alt2_price ; 
    U_3 = 0 ;
    
  "  


sim_choice <- function(designfile, no_sim=10, respondents=330, mnl_U ) {

  
 
  
  designs_all <- list()  
  
  
simulate_choices <- function(data=database) {
  
  data <-  data %>% 
    group_by(ID) %>% 
    mutate(
      e_1 = rgumbel(setpp,loc=0, scale=1) ,
      e_2 = rgumbel(setpp,loc=0, scale=1) ,
      e_3 = rgumbel(setpp,loc=0, scale=1) ,
      U_1 = V_1 + e_1 ,
      U_2 = V_2 + e_2 ,
      U_3 = V_3 + e_3 
    )   %>% 
    as.data.frame()
  
  data$CHOICE <- max.col(data[,c("U_1" , "U_2" , "U_3" )])
  
  return(data)
  
} 


estimate_sim <- function(run=1) {         #start loop
  
  cat(run)
  
  database <- simulate_choices() 
  
  
  

 model<-mixl::estimate(model_spec,start_values = est, availabilities = availabilities, data= database)

   return(model)   
   
}



design <- read_delim(designfile,delim = "\t",
                     escape_double = FALSE,
                     trim_ws = TRUE  , 
                     col_select = c(-Design, -starts_with("...")) ,
                     name_repair = "universal") %>% 
  filter(!is.na(Choice.situation)) 



nsets<-nrow(design)        
nblocks<-max(design$Block)
setpp <- nsets/nblocks      # Choice Sets per respondent; in this 'no blocks' design everyone sees all 24 sets
#respondents <- replications*nblocks
replications <- respondents/nblocks

database<- design %>%
  arrange(Block,Choice.situation) %>% 
  slice(rep(row_number(), replications)) %>%    ## replicate design according to number of replications
  mutate(RID = rep(1:respondents, each=setpp)) %>%  # create Respondent ID.
  relocate(RID,`Choice.situation`) %>% 
  mutate( alt1.tilapia=alt1.species==1, alt2.tilapia=alt2.species==1,
          alt1.cichlids=alt1.species==2, alt2.cichlids=alt2.species==2,
    V.1 = basc + btilapia*alt1.tilapia + bcichlids * alt1.cichlids + btoxin * alt1.toxin + bkisumu * alt1.origin + bprice * alt1.price , #Utility of alternative 1
    V.2 = basc + btilapia*alt2.tilapia + bcichlids * alt2.cichlids + btoxin * alt2.toxin + bkisumu * alt2.origin + bprice * alt2.price ,  #Utility of alternative 2
    V.3 = 0 ) %>% # utility of opt out, set to zero
  rename(ID ="RID") %>%
  rename_with(~ stringr::str_replace(.,pattern = "\\.","_"), everything()) %>% 
  as.data.frame()

 

  

database <- simulate_choices() 

model_spec <- mixl::specify_model(mnl_U, database, disable_multicore=F)

est=setNames(rep(0,length(model_spec$beta_names)), model_spec$beta_names)


availabilities <- mixl::generate_default_availabilities(
  database, model_spec$num_utility_functions)


output<- 1:no_sim %>% map(estimate_sim)

coefs<-map(1:length(output),~summary(output[[.]])[["coefTable"]][c(1,8)]  %>%
             tibble::rownames_to_column() %>%
             pivot_wider(names_from = rowname, values_from = c(est, rob_pval0)) ) %>% 
             bind_rows(.id = "run")

output[["summary"]] <-describe(coefs[,-1], fast = TRUE)

output[["coefs"]] <-coefs

pvals <- output[["coefs"]] %>% select(starts_with("rob_pval0"))

output[["power"]] <- 100*table(apply(pvals,1,  function(x) all(x<0.05)))/nrow(pvals)


output[["metainfo"]] <- c(Path = designfile, NoSim = no_sim, NoResp =respondents)


print(kable(output[["summary"]],digits = 2, format = "rst"))


print(output[["power"]])


return(output)  


}


designfile<-list.files("Designs/",full.names = F)
designname <- str_remove_all(designfile,"(.ngd|_)")  ## Make sure it designnames to not contain file ending and "_", as the may cause issues when replace

#go<-sim_choice(designfile = "designs/altscfeff.ngd", respondents = 180 ,no_sim = 20, mnl_U = mnl_U_test)

resps =330

nosim=5000

plan(multisession, workers = 8)
#plan(sequential)
tictoc::tic()

all_designs<- furrr::future_map(list.files("designs/",full.names = T), sim_choice,no_sim= nosim,respondents = resps, mnl_U = mnl_U_test, .options = furrr_options(seed = T)) %>% 
setNames(designname)

time <- tictoc::toc()

print(time)

powa <- lapply(all_designs, "[[", "power") 

unlist(powa)

plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    #geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.5) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
} 


meandata <- lapply(all_designs, "[[", "coefs")

pat<-paste0("(",paste(designname,collapse = "|"),").") # needed to identify pattern to be replaced

s<-as.data.frame(meandata) %>% 
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