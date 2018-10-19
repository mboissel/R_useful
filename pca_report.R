pca_report <- function(
  data,
  design,
  id_var = "Sample_ID",
  technical_vars,
  n_comp = 5,
  fig_n_comp = n_comp
) {

  require(flashpcaR)
  require(scales)
  require(tidyverse)
  require(factoextra) ## to show contrib in PCA axes 
  
  #### function needed ####
  my_flashpca <- function(X, ...) {
    res_pca <- flashpca(X, ...)
    res_pca$var_names <- colnames(X)
    rownames(res_pca$loadings) <- colnames(X)
    return(res_pca)
  }
  
  .get_pca_var_results <- function(var.coord) {
    var.cor <- var.coord
    var.cos2 <- var.cor^2
    comp.cos2 <- colSums(var.cos2)
    contrib <- function(var.cos2, comp.cos2) {
        var.cos2 * 100/comp.cos2
    }
    var.contrib <- t(apply(var.cos2, 1, contrib, comp.cos2))
    colnames(var.coord) <- colnames(var.cor) <- colnames(var.cos2) <- colnames(var.contrib) <- paste0("Dim", 1:ncol(var.coord))
    list(coord = var.coord, cor = var.cor, cos2 = var.cos2, contrib = var.contrib)
  }
  var_cor_func <- function(var.loadings, comp.sdev){var.loadings*comp.sdev}
  
  get_my_contrib <- function(pca, axe = 1 ){
    pca$sdev <- sqrt(pca$values)
    var_cor2 <- t(apply(pca$loadings, 1, var_cor_func, pca$sdev))
    elmt <- .get_pca_var_results(var_cor2)
    
    ## Contrib to Dim1 : 
    contrib1 <- elmt$contrib[, axe, drop = FALSE]
    if(length(axe) > 1) {
      eig <- pca$values
      contrib1 <- t(
        apply(
          contrib1, 
          1, 
          function(var.contrib, pc.eig){var.contrib*pc.eig},
          eig
        )
      )
      contrib1 <- apply(contrib1, 1, sum)/sum(eig)
    }
    return(contrib1)
  }
  
  label_formatter <- function(x) {
    x <- gsub('_', '\n', x)                
  }
  
  #### format ####
  if (!is(design, "data.frame")) {
    design <- as.data.frame(design)
  }
  if (!is(design[, id_var], "character")) {
    design[, id_var] <- as.character(design[, id_var])
  }
  
  keep_technical <- sapply(
    X = design[, technical_vars, drop = FALSE], 
    FUN = function(icol) {
      length(unique(icol))>1 & length(unique(icol))!=length(design[, id_var])
    }
  ) %>% 
    which() %>% 
    names()
  
  if (length(setdiff(technical_vars, keep_technical))!=0) {
    variables_excluded <- setdiff(technical_vars, keep_technical)
    message(
      "The following variables have been excluded (null variances or confounding with samples): ", 
        paste(variables_excluded[-length(variables_excluded)], collapse = ", "), 
        " and ", 
        variables_excluded[length(variables_excluded)]
    )
  }

  data_t <- t(data)
  data_t <- data_t[, colSums(data_t==0)!=nrow(data_t)] ## remove constant

  #### do PCA ####
  pca_res <- my_flashpca(
    X = data_t,
    stand = "sd",
    ndim = n_comp,
    do_loadings=TRUE, 
    verbose=FALSE, 
    return_scale=TRUE
  )
  
  pca_dfxy <- pca_res %>%
    `[[`("projection") %>%
    as.data.frame() %>%
    `colnames<-`(paste0("PC", seq_len(ncol(.)))) %>%
    mutate(Sample_ID = as.character(rownames(data_t))) # %>% 
    # rename(gettext(id_var) = "Sample_ID")
  names(pca_dfxy)[ncol(pca_dfxy)] <- id_var

  pca_dfxy <- pca_dfxy %>% 
    left_join(x = design, y = ., by = id_var)

  #### gg_inertia ####
  # PCA inertia contribution 
  gg_inertia <- data_frame(
    y = (pca_res$values / sum(pca_res$values)), 
    x = sprintf("PC%02d", seq_along(pca_res$values))
  ) %>%
    mutate(cumsum = cumsum(y)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_bar(stat = "identity", width = 1, colour = "white", fill = "#3B528BFF") +
    scale_y_continuous(labels = percent) +
    labs(y = "Inertia", x = "PCA Components")
  # print(gg_inertia)

  #### gg_all_plan & gg_association ####
  if (length(keep_technical)>0) {
    #  PCA factorial planes 
    
    if (length(keep_technical)>1) {    
      # ivar
      test <- do.call("inner_join", lapply(X = keep_technical, FUN = function(ivar) {
        print(ivar)
            
        tmp2 <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(fig_n_comp)), 2)), 1, function(icoord) {
          tmp <- pca_dfxy[, c(ivar, icoord)]
          # tmp[, ivar] <- as.factor(tmp[, ivar])
          tmp[, ivar] <- factor(tmp[, ivar], levels = unique(tmp[, ivar]))
          colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
          tmp[, "X.PC"] <- icoord[1]
          tmp[, "Y.PC"] <- icoord[2]
          return(tmp)
        }))
        return(tmp2)
            
      }))
    } else {
      test <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(fig_n_comp)), 2)), 1, function(icoord) {
          tmp <- pca_dfxy[, c(keep_technical, icoord)]
          tmp[, keep_technical] <- factor(tmp[, keep_technical], levels = unique(tmp[, keep_technical]) )
          colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
          tmp[, "X.PC"] <- icoord[1]
          tmp[, "Y.PC"] <- icoord[2]
          return(tmp)
        }))
    }
    gg_all_plan <- lapply(X = keep_technical, FUN = function(ivar) {
      ggplot(test, aes_string(x = "X", y = "Y", colour = ivar)) +
        geom_hline(aes(yintercept = 0), colour = "black") +
        geom_vline(aes(xintercept = 0), colour = "black") +
        geom_point(shape = 4, size = 2) +
        stat_ellipse(type = "norm") +
        scale_colour_viridis_d() +
        labs(x = NULL, y = NULL) +
        facet_grid(Y.PC ~ X.PC, scales = "free") +
        guides(colour = ifelse(length(unique(pca_dfxy[, ivar])) <= 12, "legend", "none"))
    })
    # print(gg_all_plan)

    # PCA association
    gg_association <- pca_dfxy %>%
      (function(.data) {
        lapply(seq_len(fig_n_comp), function(i) {
          form <- as.formula(paste0("PC", i, " ~ ", paste(keep_technical, collapse = " + ")))
          lm(form, data = .data) %>% 
            anova() %>% 
            rownames_to_column(var = "term") %>% 
            mutate(PC = i)
        }) %>%
          bind_rows()
      }) %>%
      filter(term != "Residuals") %>%
      mutate(term = gsub("factor\\((.*)\\)", "\\1", term)) %>%
      ggplot(aes(x = factor(PC), y = term, fill = `Pr(>F)`)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = scientific(`Pr(>F)`, digits = 2)), colour = "white", size = 3) +
      scale_fill_viridis_c(name = "P-Value", na.value = "grey85", limits = c(0, 0.1)) +
      theme(panel.grid = element_blank()) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(x = "PCA Components", y = NULL)
    # print(gg_association)

  } else {
    gg_all_plan <- NA
    gg_association <- NA
  }
  
  # PCA Contribution 
  
  #### ggDistrContrib1 : Show genes' contribution to Dim-1 ####
  # PCA Contribution to Dim1

  contrib1 <- get_my_contrib(pca_res,1)

  df_contrib1 <- contrib1 %>% 
    as.data.frame() %>% 
    rownames_to_column("x") %>% 
    arrange(desc(Dim1)) %>% 
    mutate(x = factor(x, levels=unique(as.character(x)))) %>% ## to keep values ordered  
    mutate(interval = cut_interval(Dim1, 5))

  ggDistrContrib1 <- ggplot(df_contrib1, aes(interval))+
    geom_bar(
      colour = viridis_pal(begin = 0.5, end = 0.5)(1),
      fill = viridis_pal(begin = 0.5, end = 0.5)(1)
    ) +
    labs(title = paste0("Distribution of contributions"),
       x = "Contribution to Dim1 (in %)")
  # print(ggDistrContrib1)
  
  ## Variation possible : 
  # ggplot(df_contrib1, aes(Dim1)) + 
  #   geom_histogram(bins = 30, color="white", fill=viridis_pal(begin = 0.5, end = 0.5)(1))+
  #   labs(title = paste0("Distribution of contributions"), 
  #        x = "Contribution to Dim1 (in %)")
  
  # ggplot(df_contrib1, aes(Dim1)) + 
  #   geom_density(
  #     aes(y = ..count..),
  #     colour = viridis_pal(begin = 0.5, end = 0.5)(1),
  #     fill = viridis_pal(begin = 0.5, end = 0.5)(1),
  #     alpha = 0.5
  #   ) +
  #   labs(title = paste0("Distribution of contributions"), 
  #        x = "Contribution to Dim1 (in %)")

  # df_contrib1 %>%
  #   head(20) %>%
  #   ggplot(aes(x = x, y = Dim1)) +
  #   geom_bar(stat = "identity", fill = viridis_pal(begin = 0.5, end = 0.5)(1)) +
  #   labs(title = paste0("Contribution of variables to Dim1 (top 20 genes)"),
  #        x = "",
  #        y = "Contributions (%)") +
  #   scale_x_discrete(labels=label_formatter) +
  #   theme(axis.text.x = element_text(size=10, angle=90, vjust = 0.5))
  
  #### ggDistrContrib2 : Show genes' contribution to Dim-2 ####
  # PCA Contribution to Dim2
  
  contrib2 <- get_my_contrib(pca_res,2)
  head(contrib2)

  df_contrib2 <- contrib2 %>% 
    as.data.frame() %>% 
    rownames_to_column("x") %>% 
    arrange(desc(Dim2)) %>% 
    mutate(x = factor(x, levels=unique(as.character(x)))) %>%  ## to keep values ordered 
    mutate(interval2 = cut_interval(Dim2, 5))
    
  ggDistrContrib2 <- ggplot(df_contrib2, aes(interval2))+ 
    geom_bar(
      colour = viridis_pal(begin = 0.5, end = 0.5)(1),
      fill = viridis_pal(begin = 0.5, end = 0.5)(1)
    ) +
    labs(title = paste0("Distribution of contributions"), 
       x = "Contribution to Dim2 (in %)")
  # print(ggDistrContrib2)
  
  #### contrib_toPlan : table of all genes' contribution ####
  # PCA Contribution to Dim1 & Dim2 + to the plan 
  
  contrib_toPlan <- get_my_contrib(pca_res,c(1,2)) %>% 
    as.data.frame() %>% 
    `colnames<-`("Contrib_toPlan") %>% 
    rownames_to_column("x") 

  Allcontrib <- merge(df_contrib1 %>% select(-interval),
                      df_contrib2 %>% select(-interval2),
                      by = "x") %>% 
    merge(.,contrib_toPlan,by="x") %>% 
    `colnames<-`(c("Genes","Contribution to Dim1","Contribution to Dim2","Contribution to Plan"))
  # head(Allcontrib)

  #### return ####
  return(
    list(
      gg_inertia = gg_inertia,
      gg_all_plan = gg_all_plan,
      gg_association = gg_association,
      Allcontrib = Allcontrib, 
      ggDistrContrib1 = ggDistrContrib1, 
      ggDistrContrib2 = ggDistrContrib2
    )
  )
}


#### TEST #### 

# rm(list=ls())

# counts_annotated <- read_rds(path = "/disks/DATATMP/PROJECT_JCS/counts_annotated3.rds")
# rownames(counts_annotated) <- paste0(
#   counts_annotated$ensembl_gene_id, "_",
#   counts_annotated$external_gene_name
# )
# counts_annotated <- as.matrix(counts_annotated[, -c(1, 2)])

# # Covariates
# CovData <- read.table(file = "/disks/DATATMP/PROJECT_JCS/samples_run_type.txt", header = FALSE) %>%
#   `colnames<-`(c("run","samples","type")) %>%
#   mutate(
#     samples_t = paste0(samples,"_",type)
#   )

# data = counts_annotated
# design = CovData
# id_var = "samples"
# technical_vars = "type"
# n_comp = 5
# fig_n_comp = n_comp
# 
# test <- pca_report(
#     data = counts_annotated,
#     design = CovData,
#     id_var = "samples",
#     technical_vars = "type",
#     n_comp = 5,
#     fig_n_comp = 5
# )

# CovData$type2 <- c(rep(1,30),rep(0,29))
# technical_vars =  c("type","type2") 
# test2 <- pca_report(
#     data = counts_annotated,
#     design = CovData,
#     id_var = "samples",
#     technical_vars = c("type","type2"),
#     n_comp = 5,
#     fig_n_comp = 5
# )

## many thanks to mcanouil for this development