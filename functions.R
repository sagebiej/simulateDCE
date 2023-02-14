sim_choice <- function(designfile, no_sim=10, respondents=330, mnl_U,utils=u ) {
  
  
  by_formula <- function(equation){ #used to take formulas as inputs in simulation utility function
    # //! cur_data_all may get deprecated in favor of pick
    # pick(everything()) %>%
    cur_data_all() %>%
      transmute(!!lhs(equation) := !!rhs(equation) )
  } 
  
  

   

  
  simulate_choices <- function(data=database) {  #the part in dataset that needs to be repeated in each run
   
    n=seq_along(1:length(utils))
      
      data <-  data %>% 
      group_by(ID) %>% 
      mutate(
        # e_1 = rgumbel(setpp,loc=0, scale=1) ,
        # e_2 = rgumbel(setpp,loc=0, scale=1) ,
        # e_3 = rgumbel(setpp,loc=0, scale=1) ,
        across(.cols=n,.fns = ~ rgumbel(setpp,loc=0, scale=1), .names = "{'e'}_{n}" ), ## Here, I need to replace the 3 with lenght(utils)
        across(.cols=starts_with("V_"), .fns= ~.x, .names = "{'U2'}_{n}") ,
        U_1 = V_1 + e_1 ,
        U_2 = V_2 + e_2 ,
        U_3 = V_3 + e_3 
      )   %>% 
      as.data.frame()
    
    data$CHOICE <- max.col(data[,c("U_1" , "U_2" , "U_3" )])
    
    print(data)
    
    return(data)
    
  } 
  
  estimate_sim <- function(run=1) {         #start loop
    
    cat(run)
    
    database <- simulate_choices() 
    
    
    
    
    model<-mixl::estimate(model_spec,start_values = est, availabilities = availabilities, data= database,)
    
    return(model)   
    
  }
  
  designs_all <- list() 
  
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
  #browser()
  database<- design %>%
    arrange(Block,Choice.situation) %>% 
    slice(rep(row_number(), replications)) %>%    ## replicate design according to number of replications
    mutate(RID = rep(1:respondents, each=setpp)) %>%  # create Respondent ID.
    relocate(RID,`Choice.situation`) %>% 
    mutate( alt1.tilapia=alt1.species==1, alt2.tilapia=alt2.species==1,
            alt1.cichlids=alt1.species==2, alt2.cichlids=alt2.species==2,
            map_dfc(utils,by_formula)   #our functions to create utility variables. They need to be entered as a formula list as an argument
            # !!lhs(utils[["v1"]]) := !!rhs(utils[["v1"]]) , #Utility of alternative 1
            # !!lhs(utils[["v2"]]) := !!rhs(utils[["v2"]]) ,
            # !!lhs(utils[["v3"]]) := !!rhs(utils[["v3"]]),
           # hgf = basc + btilapia*alt2.tilapia + bcichlids * alt2.cichlids + btoxin * alt2.toxin + bkisumu * alt2.origin + bprice * alt2.price   #Utility of alternative 2
    ) %>% # utility of opt out, set to zero
    rename(ID ="RID") %>%
    rename_with(~ stringr::str_replace(.,pattern = "\\.","_"), everything()) %>% 
    as.data.frame()
  
  
  
  
  
  database <- simulate_choices() 
  
  model_spec <- mixl::specify_model(mnl_U, database, disable_multicore=F)
  
  est=setNames(rep(0,length(model_spec$beta_names)), model_spec$beta_names)
  
  
  availabilities <- mixl::generate_default_availabilities(
    database, model_spec$num_utility_functions)
  
  plan(multisession, workers = 8)
  
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


plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    #geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.5) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", linewidth=1) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
} 