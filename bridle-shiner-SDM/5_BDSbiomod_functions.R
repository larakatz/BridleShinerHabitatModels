prep_env <- function(shp, period, hucsize) {

  # Read in data------------------------------------------------------------------
  points.all <- st_read(shp) # shapefile of presence/absence points
  index <- st_drop_geometry(points.all["huc"]) 
  index$ID <- rownames(index)

  # Data cleaning-----------------------------------------------------------------
  points.env <- 
    points.all %>% 
    st_drop_geometry(.) %>% 
    as.data.frame(.) %>% 
    dplyr::rename(elev = elevation) %>% 
    dplyr::rename(Glac_till_crs = Glacial.t0) %>% # Glacial till coarse
    dplyr::rename(Glac_lk_sed_fin = Glacial.l0) %>% # Glacial lake sediment fine
    dplyr::rename(Glac_out_crs = Glacial.o0) %>% # Glacial outwash coarse
    dplyr::rename(Alluv_cst_sed_fine = Alluvium.0) %>%  # Alluvium and coastal sediment fine
    dplyr::rename(water = Open.Water) %>%  # Open Water (LANDFIRE)
    dplyr::rename(barren = Barren) %>% # Barren-Rock/Sand/Clay
    dplyr::rename(LAc_NHardwd = LAc_NHard0) %>% # Laurentian-Acadian Northern Hardwoods Forest
    dplyr::rename(NAtl_CstPlain_Hardwd = NAtl_Coas0) %>% # Northern Atlantic Coastal Plain Hardwood Forest
    dplyr::rename(LAc_NPineOak = LAc_NPine0) %>%  # Laurentian-Acadian Northern Pine(-Oak) Forest
    dplyr::rename(LAc_PineHemlockHardwd = LAc_PineH0) %>% # Laurentian-Acadian Pine-Hemlock-Hardwood Forest
    dplyr::rename(CAp_Dry_OakPine = CAp_Dry_O0) %>% # Central Appalachian Dry Oak-Pine Forest
    dplyr::rename(Ap_Hemlock_NHardwd = Ap_Hemloc0) %>% # Appalachian (Hemlock-)Northern Hardwood Forest
    dplyr::rename(Ac_LoElev_SpruceFirHardwd = Ac_LoElev0) %>% # Acadian Low-Elevation Spruce-Fir-Hardwood Forest
    dplyr::rename(AcAp_Montane_SpruceFir = AcAp_Mont0) %>% # Acadian-Appalachian Montane Spruce-Fir Forest
    dplyr::rename(CAp_PineOak_Rocky_Wd = CentAp_Pi0) %>% # Central Appalachian Pine-Oak Rocky Woodland
    dplyr::rename(NAtl_CstPln_Mar = NAtl_Coas1) %>% # Northern Atlantic Coastal Plain Maritime Forest
    dplyr::rename(NAtl_CstPln_Dun = NAtl_Coas2) %>% # Northern Atlantic Coastal Plain Dune and Swale
    dplyr::rename(CIntAp_FldplnSys = CentIntAp0) %>% # Central Interior and Appalachian Floodplain Systems
    dplyr::rename(CIntAp_RiparSys = CentIntAp1) %>% # Central Interior and Appalachian Riparian Systems
    dplyr::rename(LAc_FldplnSys = LAc_Flood0) %>% # Laurentian-Acadian Floodplain Systems
    dplyr::rename(Bor_Acid_PeatSys = Bor_Acidi0) %>% # Boreal Acidic Peatland Systems
    dplyr::rename(CIntAp_SwampSys = CentIntAp2) %>% # Central Interior and Appalachian Swamp Systems
    dplyr::rename(GulfAtl_CstPln_SwampSys = GulfAtl_C0) %>% # Gulf and Atlantic Coastal Plain Swamp Systems
    dplyr::rename(GulfAtl_CstPln_TMarshSys = GulfAtl_C1) %>% # Gulf and Atlantic Coastal Plain Tidal Marsh Systems
    dplyr::rename(LAc_ShrubHerb_WetlSys = LAc_Shrub0) %>% # Laurentian-Acadian Shrub-Herbaceous Wetland Systems
    dplyr::rename(NCInt_Wet_Flatwd = NCentInt_0) %>% # North-Central Interior Wet Flatwoods
    dplyr::rename(LAc_SwampSys = LAc_Swamp0) %>% # Laurentian-Acadian Swamp Systems
    dplyr::rename(NeInt_PineBarrens = NeInt_Pin0) %>% # Northeastern Interior Pine Barrens
    dplyr::rename(AcAp_WdHeath = AcAp_WdHe0) %>% # Acadian-Appalachian Subalpine Woodland and Heath-Krummholz
    dplyr::rename(Bor_JackPineBlSpruce = Bor_JackP0) %>% # Boreal Jack Pine-Black Spruce Forest
    dplyr::rename(AcAp_AlpineTundra = AcAp_Alpi0) # Acadian-Appalachian Alpine Tundra
  
  
  if ("Coastal.s0" %in% colnames(points.env)) {
    points.env <- 
      points.env %>% 
      # dplyr::rename(Coast_sed_crs = Coastal.s0)
      dplyr::select(-Coastal.s0) # Remove Coastal Sediment Coarse since it doesn't appear in all outputs
  }
  
  if (period == "Current"){
    points.env <- 
      points.env %>% 
      dplyr::select(-cOccu)
  }
  
  if (period == "Historical"){
    points.env <- 
      points.env %>% 
      dplyr::select(-hOccu)
  }
  
  ## Clean proportion data:
  # points.env[,6:36] <- points.env[,6:36]/100 # percents to proportions
  
  do_not_check <- c("huc", "clay", "sand", "silt", "elev")
  cols_to_check <- subset(colnames(points.env), !(colnames(points.env) %in% do_not_check))
  
  points.env.clean <- 
    points.env %>% 
    mutate(across(all_of(cols_to_check), ~ replace(., . > 100, 100))) # help from ChatGPT
  
  
  ## Remove points with NA values (very small HUCs in the middle of large waterbodies or on the coast: these are missing substrate values):
  points.env.nona <- points.env.clean[stats::complete.cases(points.env.clean),]
  index.nona <- which(index$huc %in% points.env.nona$huc)
  point.coords <- st_coordinates(points.all[index.nona,])
  point.coords <- as.data.frame(cbind(huc = points.all$huc[index.nona], point.coords))
  
  ## Write final csv of environmental covariates and coordinates
  if (hucsize == "huc12") {
    write.csv(point.coords,"./biomod/coords_large_hucs.csv")
    write.csv(points.env.nona,"./biomod/env_large_hucs.csv")
  } else {
    write.csv(point.coords, "./biomod/coords_small_hucs.csv")
    write.csv(points.env.nona, "./biomod/env_small_hucs.csv")
  }
  
  # Prepare model inputs--------------------------------------------------------
  env.huc1 <- left_join(points.env.nona, point.coords, by = "huc")
  
  if (period == "Current") {
    points.all2 <- points.all[index.nona,] %>% 
      dplyr::select(huc, cOccu) %>% 
      st_drop_geometry(.)
  }
  
  if (period == "Historical") {
    points.all2 <- points.all[index.nona,] %>% 
      dplyr::select(huc, hOccu) %>% 
      st_drop_geometry(.)
  }
  
  points.all1 <- env.huc1 %>% left_join(., points.all2, by="huc")
  
  ## Create new column to define pseudoabsences and true presence:
  # Change absences to unknowns because biomod won't work otherwise
  points.all1$pab <- NA
  
  if (period == "Current") {
  points.all1[!is.na(points.all1$cOccu) & points.all1$cOccu == 1,]$pab <- 1
  
  # Calculate number of pseudoabsences needed
  n.pab <- 
    3*nrow(points.all1[!is.na(points.all1$cOccu) & points.all1$cOccu == 1,])
  }
  
  if (period == "Historical") {
    points.all1$pab <- NA
    points.all1[!is.na(points.all1$hOccu) & points.all1$hOccu == 1,]$pab <- 1
    
    # Calculate number of pseudoabsences needed
    n.pab <- 
      3*nrow(points.all1[!is.na(points.all1$hOccu) & points.all1$hOccu == 1,])
  }
  
  prepped_env <- list(points.all1, n.pab)
  names(prepped_env) <- c("points.all1", "n.pab")
  return(prepped_env)
}

model.params <- function(n.cores){
  ## CTA (rpart default options but xval=100 not 5)
  user.cta <- 
    list(
      method = 'class',
      # parms = 'default',
      cost = NULL,
      control = rpart.control(xval = 100, # bigboss default 5, rpart default 10
                              minbucket = 5, # bigboss default 5, rpart default: minsplit/3
                              minsplit = 15, # biomod default 5
                              cp = 0.01, # biomod default 0.001, rpart default 0.01 (the higher the cp, the smaller the tree)
                              maxdepth = 25)
  )
  
  # user.cta.single <- list('allData_allRun' = user.cta)       
  user.cta.list <- list('_allData_RUN1' = user.cta,
                        '_allData_RUN2' = user.cta,
                        '_allData_RUN3' = user.cta,
                        '_allData_RUN4' = user.cta,
                        '_allData_RUN5' = user.cta,
                        '_allData_RUN6' = user.cta,
                        '_allData_RUN7' = user.cta,
                        '_allData_RUN8' = user.cta,
                        '_allData_RUN9' = user.cta,
                        '_allData_RUN10' = user.cta)
  
  ## GBM (bigboss options):
  user.gbm <- 
    list(distribution = 'bernoulli',
          n.trees = 2500,
          interaction.depth = 7,
          n.minobsinnode = 5, # biomod default is 5, gbm default is 10. Kept at 5 to match rpart minbucket=5
          shrinkage = 0.001,
          cv.folds = 3,
          keep.data = FALSE,
          verbose = FALSE,
          n.cores = n.cores) # won't allow us to use multiple cores
  
  # user.gbm.single <- list('allData_allRun' = user.gbm)
  user.gbm.list <- list('_allData_RUN1' = user.gbm,
                        '_allData_RUN2' = user.gbm,
                        '_allData_RUN3' = user.gbm,
                        '_allData_RUN4' = user.gbm,
                        '_allData_RUN5' = user.gbm,
                        '_allData_RUN6' = user.gbm,
                        '_allData_RUN7' = user.gbm,
                        '_allData_RUN8' = user.gbm,
                        '_allData_RUN9' = user.gbm,
                        '_allData_RUN10' = user.gbm)
  
  ## Random forest (bigboss options) ----
  
  user.rf <- list(type = 'classification',
                  ntree = 500,
                  mtry = NULL,
                  strata = factor(c(0,1)),
                  sampsize = NULL,
                  nodesize = 5,
                  maxnodes = NULL) # may need to change
  
  user.rf.list <- list('_allData_RUN1' = user.rf,
                       '_allData_RUN2' = user.rf,
                       '_allData_RUN3' = user.rf,
                       '_allData_RUN4' = user.rf,
                       '_allData_RUN5' = user.rf,
                       '_allData_RUN6' = user.rf,
                       '_allData_RUN7' = user.rf,
                       '_allData_RUN8' = user.rf,
                       '_allData_RUN9' = user.rf,
                       '_allData_RUN10' = user.rf)
  
  ## All model options:
  user.val <- list('CTA.binary.rpart.rpart' = user.cta.list,
                   # 'GAM.binary.mgcv.gam' = user.gam.list,
                   'GBM.binary.gbm.gbm'  = user.gbm.list,
                   # 'GLM.binary.stats.glm' = user.glm.list,
                   'RF.binary.randomForest.randomForest'= user.rf.list)
  
  return(user.val)
}

biomod_loop <- function(i, points, period, hucsize, crs, n.pab, nb.cpu, var.import, 
                        user.val, select.thresh) { 
  # Format input data-----------------------------------------------------------
  
  ## Clear last round of ouputs and create new empty dataframes:
  points$pab <- NA
  if (period == "Current") {
    points[!is.na(points$cOccu) & points$cOccu == 1,]$pab <- 1
    ncases <- nrow(points[!is.na(points$cOccu) & points$cOccu == 1,])
  } # end assign 1 to current presence points
  if (period == "Historical") {
    points[!is.na(points$hOccu) & points$hOccu == 1,]$pab <- 1
    ncases <- nrow(points[!is.na(points$hOccu) & points$hOccu == 1,])
  } # end assign 1 to historic presence points
  
  ## Create empty dataframes to hold outputs:
  new_evals <- as.data.frame(matrix(NA, nrow=3, ncol=7))
  colnames(new_evals) <- c("looprun", "seed","model","meanROC","maxROC", "meanTSS", "maxTSS")
  
  EM.list <- list() # list of ensemble models
  
  new_em_scores <- as.data.frame(matrix(NA, nrow = 0, ncol = 16))
  colnames(new_em_scores) <- c("looprun", "seed", "full.name", "merged.by.PA", 
                               "merged.by.run", "merged.by.algo", "filtered.by", 
                               "algo", "metric.eval", "cutoff", "sensitivity", 
                               "specificity", "calibration", "validation", 
                               "evaluation", "nmodels")
  
  var.imp.new <- as.data.frame(matrix(NA, nrow = 0, ncol=5))
  colnames(var.imp.new) <- c("algo", "expl.var", "looprun", "seed", "mean.var.imp")
  
  new_em_evals <- as.data.frame(matrix(NA, nrow = 1, ncol = 7))
  colnames(new_em_evals) <- c("looprun", "Seed","AUC","TSS","sensitivity",
                              "specificity","threshold")
  
  ## Assign new set of pseudoabsences-------------------------------------------
  set.seed(seeds[i])
  unknown <- points[is.na(points$pab),]
  pab.hucs <- sample(points$huc, n.pab, replace=FALSE)
  pab.index <- which(points$huc %in% pab.hucs)  
  points[pab.index,]$pab <- 0
  
  ## Create indices for combined training (calibration) and validation data-----
  set.seed(seeds[i])
  calval1 <- points %>%
    filter(!is.na(pab)) %>%  # Known presences and absences
    group_by(pab) %>%
    dplyr::sample_frac(size=0.80) # 60% calibration, 20% validation, will be separated by biomod
  calval <- which(points$huc %in% calval1$huc)
  
  ## Create indices for testing (evaluation) data:
  eval1 <- points[-calval,] %>% 
    filter(!is.na(pab))
  eval <- which(points$huc %in% eval1$huc)
  
  ## Create index of HUCs with unknown occupancy:
  new1 <- points %>% 
    filter(is.na(pab)) # Locations to predict occupancy
  new <- which(points$huc %in% new1$huc)
  
  ## Create dataframes of response variables----------------------------------
  ## Known presence/absence to be split into calibration/validation:
  occ.calval1 <- points[calval,] %>% dplyr::select(huc, pab, X, Y)
  rownames(occ.calval1) <- occ.calval1$huc
  occ.calval <- occ.calval1 %>% dplyr::select(-huc) %>%
    st_as_sf(., coords=c("X","Y"), remove=T, crs=st_crs(crs))
  resp.calval <- occ.calval %>% dplyr::select(pab) %>% st_drop_geometry(.) %>%
    as.matrix(.) %>% as.numeric(.)
  index.calval <- cbind(huc=row.names(occ.calval), ID=1:length(resp.calval))
  
  ## Known presence/absence to be used as evaluation (testing):
  occ.eval1 <- points[eval,] %>% dplyr::select(huc, pab, X, Y)
  rownames(occ.eval1) <- occ.eval1$huc
  occ.eval <- occ.eval1 %>% dplyr::select(-huc) %>% 
    st_as_sf(., coords=c("X","Y"), remove=TRUE, crs=st_crs(crs))
  resp.eval <- occ.eval %>% dplyr::select(pab) %>% st_drop_geometry(.) %>% 
    as.matrix(.) %>% as.numeric(.)
  index.eval <- cbind(huc=row.names(occ.eval), ID=1:length(resp.eval))
  
  ## New points at which to predict occupancy:
  occ.new1 <- points[new,] %>% dplyr::select(huc, pab, X, Y)
  rownames(occ.new1) <- occ.new1$huc
  occ.new <- occ.new1 %>% dplyr::select(-huc) %>% 
    st_as_sf(., coords=c("X","Y"), remove=T, crs=st_crs(crs))
  resp.new <- occ.new %>% dplyr::select(pab) %>% st_drop_geometry(.) %>% 
    as.matrix(.) %>% as.numeric(.)
  index.new <- cbind(huc=row.names(occ.new), ID=1:length(resp.new))
  
  ## Create data.frames of coordinates----------------------------------------
  coords.calval <- as.data.frame(st_coordinates(occ.calval))
  coords.eval <- as.data.frame(st_coordinates(occ.eval))
  coords.new <- as.data.frame(st_coordinates(occ.new))
  
  ## Create data.frames of explanatory variables------------------------------
  if (period == "Current"){
    env.calval <- points[calval,] %>% dplyr::select(-huc,-cOccu,-pab,-X,-Y) %>%
      as.data.frame(.)
    env.eval <- points[eval,] %>% dplyr::select(-huc,-cOccu,-pab,-X,-Y) %>% 
      as.data.frame(.)
    env.new <- points[new,] %>% dplyr::select(-huc,-cOccu,-pab,-X,-Y) %>% 
      as.data.frame(.)
  } # end current points creation loop
  if (period == "Historical"){
    env.calval <- points[calval,] %>% dplyr::select(-huc,-hOccu,-pab,-X,-Y) %>%
      as.data.frame(.)
    env.eval <- points[eval,] %>% dplyr::select(-huc,-hOccu,-pab,-X,-Y) %>% 
      as.data.frame(.)
    env.new <- points[new,] %>% dplyr::select(-huc,-hOccu,-pab,-X,-Y) %>% 
      as.data.frame(.)
  } # end historical points creation loop
  
  ## Create formatted data objects--------------------------------------------
  ## Calibration/validation data:
  format <- 
    BIOMOD_FormatingData(
      resp.name = paste0("BDS.", seeds[i]),
      resp.var = resp.calval, # a vector containing binary data (0: absence, 1: presence, NA: indeterminate) for a single species that will be used to build the species distribution model
      expl.var = env.calval, # a data.frame containing the explanatory variables (in columns or layers) that will be used to build the species distribution model(s)
      dir.name = "./biomod", # modeling folder
      resp.xy = coords.calval,
      PA.nb.rep = 0, # an integer corresponding to the number of sets (repetitions) of pseudo-absence points that will be drawn 
      PA.nb.absences = 0, # number of pseudo-absence points that will be selected for each pseudo-absence repetition (true absences included)
      PA.strategy = NULL, # a character defining the strategy that will be used to select the pseudo-absence points
      na.rm = TRUE # A logical value defining whether points having one or several missing values for explanatory variables should be removed from the analysis or not
    )
  
  ## Evaluation data:
  format_eval <- 
    BIOMOD_FormatingData(
      resp.name = paste0("BDS.", seeds[i]),
      resp.var = resp.eval, # a vector containing binary data (0: absence, 1: presence, NA: indeterminate) for a single species that will be used to build the species distribution model
      expl.var = env.eval, # a data.frame containing the explanatory variables (in columns or layers) that will be used to build the species distribution model(s)
      dir.name = "./biomod", # modeling folder
      resp.xy = coords.eval,
      PA.nb.rep = 0, # an integer corresponding to the number of sets (repetitions) of pseudo-absence points that will be drawn 
      PA.nb.absences = 0, # number of pseudo-absence points that will be selected for each pseudo-absence repetition (true absences included)
      PA.strategy = NULL, # a character defining the strategy that will be used to select the pseudo-absence points
      na.rm = TRUE # A logical value defining whether points having one or several missing values for explanatory variables should be removed from the analysis or not
    )
  
  # Define calibration/validation data across cross-validation runs-----------
  cv.table <- bm_CrossValidation(bm.format = format,
                                 strategy = "random",
                                 nb.rep = 10, perc = 0.75)
  
  # Create ModelingOptions object---------------------------------------------
  model_opt <- 
    bm_ModelingOptions(
      data.type = 'binary', 
      models = c('CTA', # Classification Tree Analysis
                 'GBM', # Generalised Boosted Models
                 'RF' # Random Forest
                 # 'GLM', # Generalized Linear Model
                 # 'GAM', # Generalized Additive Model
      ),
      strategy = 'user.defined',
      user.val = user.val,
      user.base = "bigboss", # base parameters to be modified by user.val
      bm.format = format,
      calib.lines = cv.table)
  
  # Run individual models-----------------------------------------------------
  ind_models <- 
    BIOMOD_Modeling(
      bm.format = format, # call in correctly formatted data
      models = c(
        "CTA", 
        # "GAM", 
        "GBM", 
        # "GLM", 
        "RF"), 
      OPT.user = model_opt, # call options that you specified earlier
      CV.strategy = "user.defined", # randomly split data between calibration and validation
      CV.do.full.models = FALSE, # keep calibration and validation data separate
      CV.user.table = cv.table,
      OPT.strategy = "user.defined",
      OPT.user.val = user.val,
      var.import = var.import, # An integer corresponding to the number of permutations to be done for each variable to estimate variable importance
      metric.eval = c("TSS", "ROC"),
      nb.cpu = nb.cpu,
      seed.val = seeds[i]
    )
  
  ## Evaluate individual models using validation dataset------------------------
  roc_by_model <- 
    ind_models %>% 
    get_evaluations(.) %>% 
    filter(metric.eval == "ROC") %>% 
    group_by(algo) %>% 
    summarise(mean_ROC = mean(validation), max_ROC = max(validation))
  tss_by_model <- 
    ind_models %>% 
    get_evaluations(.) %>% 
    filter(metric.eval == "TSS") %>% 
    group_by(algo) %>% 
    summarise(mean_TSS = mean(validation), max_TSS = max(validation))
  over_thresh <- 
    ind_models %>% 
    get_evaluations(.) %>% 
    filter(metric.eval == "TSS") %>% 
    filter(validation > select.thresh) %>% 
    summarise(total = nrow(.))
  # roc_by_model 
  # tss_by_model
  
  new_evals[1:length(unique(roc_by_model$algo)),1] <- i
  new_evals[1:length(unique(roc_by_model$algo)),2] <- seeds[i]
  new_evals[,3] <- roc_by_model$algo
  new_evals[,4] <- roc_by_model$mean_ROC
  new_evals[,5] <- roc_by_model$max_ROC
  new_evals[,6] <- tss_by_model$mean_TSS
  new_evals[,7] <- tss_by_model$max_TSS

  # Ensemble models-----------------------------------------------------------
  if (exists("over_thresh")) {
    if (over_thresh$total > 1) { # more than one individual model retained
      ## Create ensemble model:
      EM.list <- # Save in EM list in slot for seed i
        BIOMOD_EnsembleModeling(
          bm.mod = ind_models,
          models.chosen = 'all',
          em.by = 'all',
          em.algo = c('EMwmean','EMca'),
          metric.select = 'TSS',
          metric.select.thresh = select.thresh, # Good: TSS > 0.6, ROC/AUC > 0.7
          metric.eval = c('TSS', 'ROC'),
          var.import = var.import,
          EMci.alpha = 0.05,
          EMwmean.decay = 'proportional',
          nb.cpu = nb.cpu,
          seed.val = seeds[i]
        )
      new_em_scores <- cbind(looprun = i, 
                             seed = seeds[i],
                             get_evaluations(EM.list), 
                             nmodels = over_thresh$total) # evaluate model
      
      ## Variable permutation importance:
      var.imp.new <- 
        get_variables_importance(EM.list) %>% 
        group_by(algo, expl.var) %>% 
        summarise(looprun = i, seed = seeds[i], mean.var.imp = mean(var.imp), .groups="drop")
    } else {
      if (over_thresh$total == 0) { # if no individual models are selected
        print(paste("All models for seed", seeds[i], "below threshold TSS value of", select.thresh, ". No ensemble model created.", sep = " "))
        new_em_scores <- c(i, seeds[i], rep(NA, (ncol(new_em_scores))-1), 0) # add NAs to df of ensemble model scores
        var.imp.new <- c(NA, NA, i, seeds[i], NA)
      } # end ensemble model loop (no ensemble model created, 0 models)
      
      if (over_thresh$total == 1) { # if 1 individual model is selected
        print(paste("Only one model for seed", seeds[i], "below threshold TSS value of", select.thresh, ". No ensemble model created.", sep = " "))
        new_em_scores <- c(i, seeds[i], rep(NA, (ncol(new_em_scores))-1), 1) # add NAs to df of ensemble model scores
        var.imp.new <- c(NA, NA, i, seeds[i], NA)
      } # end ensemble model loop (no ensemble model created, 1 model)
    } # end create ensemble model loop (over_thresh already exists)
  } else { # R keeps skipping over the over_thresh step for some reason
    over_thresh <- # calculate over_thresh
      ind_models %>% 
      get_evaluations(.) %>% 
      filter(metric.eval == "TSS") %>% 
      filter(validation > select.thresh) %>% # models over TSS threshold
      summarise(total = nrow(.)) # how many models?
    if (over_thresh$total > 1) { # more than one individual model retained
      ## Create ensemble model:
      EM.list <- # Save in EM list in slot for seed i
        BIOMOD_EnsembleModeling(
          bm.mod = ind_models,
          models.chosen = 'all',
          em.by = 'all',
          em.algo = c('EMwmean','EMca'),
          metric.select = 'TSS',
          metric.select.thresh = select.thresh, # Good: TSS > 0.6, ROC/AUC > 0.7
          metric.eval = c('TSS', 'ROC'),
          var.import = var.import,
          EMci.alpha = 0.05,
          EMwmean.decay = 'proportional',
          nb.cpu = nb.cpu,
          seed.val = seeds[i])
      
      new_em_scores <- cbind(looprun = i, 
                             seed = seeds[i], 
                             get_evaluations(EM.list), 
                             nmodels = over_thresh$total) # evaluate model
      
      ## Variable permutation importance:
      var.imp.new <- 
        get_variables_importance(EM.list) %>% 
        group_by(algo, expl.var) %>% 
        summarise(looprun = i, seed = seeds[i], mean.var.imp = mean(var.imp), .groups="drop")
    } else {# end ensemble model creation loop
      if (over_thresh$total == 0) { # if no individual models are selected
        print(paste("All models for seed", seeds[i], "below threshold TSS value of", select.thresh, "no ensemble model created.", sep = " "))
        new_em_scores <- c(i, seeds[i], rep(NA, (ncol(new_em_scores))-1), 0) # add NAs to df of ensemble model scores
        var.imp.new <- c(NA, NA, i, seeds[i], NA)
      } # end ensemble model loop (no ensemble model created, 0 models)
      if (over_thresh$total == 1) { # if 1 individual model is selected
        print(paste("Only one model for seed", seeds[i], "below threshold TSS value of", select.thresh, "no ensemble model created.", sep = " "))
        new_em_scores <- c(i, seeds[i], rep(NA, (ncol(new_em_scores))-1), 1) # add NAs to df of ensemble model scores
        var.imp.new <- c(NA, NA, i, seeds[i], NA)
      } 
    } # end ensemble model loop (no ensemble model created, 1 model)
  } # end create ensemble model loop (over_thresh did not exist)
  
  ## Evaluate ensemble models using evaluation dataset--------------------------
  ## Evaluation folder:
  if (period == "Current") {
    if (hucsize == "huc12") {
      proj.name <- "cL_EM_forecast"
    }
    if (hucsize == "small") {
      proj.name <- "cS_EM_forecast"
    }
  }
  if (period == "Historical") {
    if (hucsize == "huc12") {
      proj.name <- "hL_EM_forecast"
    }
    if (hucsize == "small") {
      proj.name <- "hS_EM_forecast"
    }
  }
  
  ## Evaluate models based on whether they are ensemble or individual models:
  if(exists("over_thresh")) { # do we know how many individual models were retained?
    if (over_thresh$total > 1) { # if an ensemble model was created above
      EM_forecast <- # predict presence across combined calibration + validation data
        BIOMOD_EnsembleForecasting(
          bm.em = EM.list,
          proj.name = proj.name, # folder based on which input data is used
          new.env = format_eval@data.env.var,
          new.env.xy = format_eval@coord,
          models.chosen = 'all',
          metric.binary = 'TSS',
          nb.cpu = nb.cpu) 
      
      predict <- get_predictions(EM_forecast) # get predictions from forecast
      predicted <- # create vector of predictions
        predict %>% 
        filter(filtered.by == "TSS") %>% 
        filter(algo == "EMwmean") %>% 
        dplyr::select(pred) %>% 
        unlist(.) %>% 
        unname(.)
      
      true_occu <- format_eval@data.species # vector of actual occupancy data
      
      ## Check ROC/AUC and TSS (help from ChatGPT):
      roc_curve <- roc(response=true_occu, predicted) # create ROC/AUC curve
      # plot(roc_curve)
      roc_coords <- 
        pROC::coords(
          # find threshold value(s) of TSS (max(sensitivities+specificities))
          roc=roc_curve, 
          x="best") # Several thresholds might be equally optimal.
           
      if (nrow(roc_coords) > 1) { # if there are multiple ROC thresholds
        tss_value.j <- c()
        for (j in 1:length(roc_coords$threshold)) { # find the one that maximizes TSS
          # tss_threshold <- roc_coords$threshold[j] %>% as.numeric(.)
          tss_value.j[j] <- roc_coords$specificity[j] + roc_coords$sensitivity[j] - 1
        }
        tss.value.j <- as.data.frame(cbind(value = tss_value.j, index = 1:length(tss_value.j)))
        tss.j <- tss.value.j[which(tss.value.j$value == max(tss.value.j$value)),]$index
        tss_threshold <- roc_coords$threshold[tss.j] %>% as.numeric(.)
        tss_value <- roc_coords$specificity[tss.j] + roc_coords$sensitivity[tss.j] - 1
        
        for (k in 1:length(tss.j)) { # if there is still more than one best threshold
          new_em_evals[k,1] <- i
          new_em_evals[k,2] <- seeds[i]
          new_em_evals[k,3] <- auc(roc_curve) %>% as.numeric(.)
          new_em_evals[k,4] <- tss_value[k]
          new_em_evals[k,5] <- roc_coords$sensitivity[k]
          new_em_evals[k,6] <- roc_coords$specificity[k]
          new_em_evals[k,7] <- tss_threshold[k]
        }
      }
      
      if (nrow(roc_coords) == 1) { # if there is 1 threshold
        if (!is.na(roc_coords$threshold)) {
          tss_threshold <- roc_coords$threshold %>% as.numeric(.)
          tss_value <- roc_coords$specificity + roc_coords$sensitivity - 1
          
          new_em_evals[,1] <- i
          new_em_evals[,2] <- seeds[i]
          new_em_evals[,3] <- auc(roc_curve) %>% as.numeric(.)
          new_em_evals[,4] <- tss_value
          new_em_evals[,5] <- roc_coords$sensitivity
          new_em_evals[,6] <- roc_coords$specificity
          new_em_evals[,7] <- tss_threshold
        }
        if (is.na(roc_coords$threshold)) { # if there is no best threshold value
          new_em_evals[,1] <- i
          new_em_evals[,2] <- seeds[i] # don't use this model
          new_em_evals[,3] <- NA
          new_em_evals[,4] <- NA
          new_em_evals[,5] <- NA
          new_em_evals[,6] <- NA
          new_em_evals[,7] <- NA
        }
      } # end loop for if an ensemble model with 1 threshold value
    } # end loop for if an ensemble model was created above
    if (over_thresh$total <= 1) { # if only 1 or zero individual models were retained
      new_em_evals[,1] <- i # no ensemble model to evaluate
      new_em_evals[,2] <- seeds[i] 
      new_em_evals[,3] <- NA
      new_em_evals[,4] <- NA
      new_em_evals[,5] <- NA
      new_em_evals[,6] <- NA
      new_em_evals[,7] <- NA
    } # end loop for if no ensemble model was created
  } else { # if over_thresh doesn't exist (even though it should)
    over_thresh <- 
      ind_models %>% 
      get_evaluations(.) %>% 
      filter(metric.eval == "TSS") %>% 
      filter(validation > select.thresh) %>% 
      summarise(total = nrow(.))
    
    if (over_thresh$total > 1) { # if an ensemble model was created above
      EM_forecast <- # predict presence across combined calibration + validation data
        BIOMOD_EnsembleForecasting(
          bm.em = EM.list,
          proj.name = proj.name, # folder based on which input data is used
          new.env = format_eval@data.env.var,
          new.env.xy = format_eval@coord,
          models.chosen = 'all',
          metric.binary = 'TSS',
          nb.cpu = nb.cpu) 
      
      predict <- get_predictions(EM_forecast) # get predictions from forecast
      predicted <- # create vector of predictions
        predict %>% 
        filter(filtered.by == "TSS") %>% 
        filter(algo == "EMwmean") %>% 
        dplyr::select(pred) %>% 
        unlist(.) %>% 
        unname(.)
      
      true_occu <- format_eval@data.species # vector of actual occupancy data
      prev <- ncases / (ncases + n.pab) # the prevalence of positives, or the proportion of positive cases in the population (ncases / (ncontrols + ncases))
      
      ## Check ROC/AUC and TSS (help from ChatGPT):
      roc_curve <- roc(response=true_occu, predicted) # create ROC/AUC curve
      # plot(roc_curve)
      roc_coords <- 
        pROC::coords(
          roc=roc_curve, 
          x="best" # Several thresholds might be equally optimal.
          # best.weights=c(cost, prev) # cost of a false negative is higher than that of a false positive 
        )  
      if (nrow(roc_coords) > 1) { # if there are multiple ROC thresholds
        tss_value.j <- c()
        for (j in 1:length(roc_coords$threshold)) { # find the one that maximizes TSS
          tss_value.j[j] <- roc_coords$specificity[j] + roc_coords$sensitivity[j] - 1
        }
        tss.value.j <- as.data.frame(cbind(value = tss_value.j, index = 1:length(tss_value.j)))
        tss.j <- tss.value.j[which(tss.value.j$value == max(tss.value.j$value)),]$index
        tss_threshold <- roc_coords$threshold[tss.j] %>% as.numeric(.)
        tss_value <- roc_coords$specificity[tss.j] + roc_coords$sensitivity[tss.j] - 1
        
        for (k in 1:length(tss.j)) { # if there is still more than one best threshold
          new_em_evals[k,1] <- i
          new_em_evals[k,2] <- seeds[i]
          new_em_evals[k,3] <- auc(roc_curve) %>% as.numeric(.)
          new_em_evals[k,4] <- tss_value[k]
          new_em_evals[k,5] <- roc_coords$sensitivity[k]
          new_em_evals[k,6] <- roc_coords$specificity[k]
          new_em_evals[k,7] <- tss_threshold[k]
        }
      }
      if (nrow(roc_coords) == 1) { # if there is 1 threshold
        if (!is.na(roc_coords$threshold)) {
          tss_threshold <- roc_coords$threshold %>% as.numeric(.)
          # TPR <- sum(true_occu == 1 & predicted >= tss_threshold) / sum(predicted >= tss_threshold) # true positives/all positives
          # TNR <- sum(true_occu == 0 & predicted < tss_threshold) / sum(predicted < tss_threshold) # true negatives/all negatives
          tss_value <- roc_coords$specificity + roc_coords$sensitivity - 1
          
          new_em_evals[,1] <- i
          new_em_evals[,2] <- seeds[i]
          new_em_evals[,3] <- auc(roc_curve) %>% as.numeric(.)
          new_em_evals[,4] <- tss_value
          new_em_evals[,5] <- roc_coords$sensitivity
          new_em_evals[,6] <- roc_coords$specificity
          new_em_evals[,7] <- tss_threshold
        }
        if (is.na(roc_coords$threshold)) { # if there is no best threshold value
          new_em_evals[,1] <- i 
          new_em_evals[,2] <- seeds[i] # don't use this model
          new_em_evals[,3] <- NA
          new_em_evals[,4] <- NA
          new_em_evals[,5] <- NA
          new_em_evals[,6] <- NA
          new_em_evals[,7] <- NA
        }
      } # end loop for if an ensemble model with 1 threshold value
    }  # end loop for if over_thresh did not exist but is > 1
    if (over_thresh$total <= 1) { # if only 1 or zero individual models were retained
      new_em_evals[,1] <- i # no ensemble model to evaluate
      new_em_evals[,2] <- seeds[i] 
      new_em_evals[,3] <- NA
      new_em_evals[,4] <- NA
      new_em_evals[,5] <- NA
      new_em_evals[,6] <- NA
      new_em_evals[,7] <- NA
    } # end loop for if no ensemble model was created
  } # end loop for if over_thresh needed to be recalculated
  
  # ensemble_evals <- rbind(ensemble_evals, new_em_evals)
  # } # end of for loop
  em_outputs <- list(new_evals, new_em_scores, EM.list, new_em_evals, var.imp.new)
  names(em_outputs) <- c("new_evals", "new_em_scores", "EM.list", "new_em_evals", "var.imp.new")
  return(em_outputs)
} # end of function