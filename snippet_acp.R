# R
### pca simple notes ===================================================================================================
# prcomp() and princomp() [built-in R stats package],
# PCA() [FactoMineR package],
# dudi.pca() [ade4 package],
# and epPCA() [ExPosition package]

# test https://www.datacamp.com/tutorial/pca-analysis-r
# test http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# aussi voir # FAMD
# Factor Analysis for Mixed Data (FAMD) is a principal component method dedicated to explore data with both continuous and categorical variables. It can be seen roughly as a mixed between PCA and MCA.
# More precisely, the continuous variables are scaled to unit variance and the categorical variables are transformed into a disjunctive data table (crisp coding) and then scaled using the specific scaling of MCA. This ensures to balance the influence of both continous and categorical variables in the analysis. It means that both variables are on an equal footing to determine the dimensions of variability.  
# Source: [`Rdocumentation`](https://www.rdocumentation.org/packages/FactoMineR/versions/1.42/topics/FAMD)
# We performe this analysis with the R package `FactoMineR`.  


#### pca report from MC =================================================================================================
# code initially developped by Mickaël Canouil (MC) and here adapted 

# data = all_counts_rlog;
# design = Phenotype %>%
#   dplyr::rename("Sample_ID" = "SampleID");
# id_var = "Sample_ID";
# technical_vars = technical_vars;
# n_comp = min(10, ncol(all_counts_rlog));
# stand_pca = "center";
# fig_n_comp = 4;
# title_level = 3;
# outliers_component = 1:2;
# outliers_threshold = 3;


#' Compute an analysis report using principal component analysis from flashpca tool.
#'
#' The function can be used in a chunk within a Rmarkdown document/script with results="asis" to render the report.
#'
#' @param data A `vector` or `data.frame`. The numeric data on which the PCA has to be performed.
#' @param design A `data.frame`. Additional variables to be used with factorial planes.
#' @param id_var A `character`. The identifier column used to merge the data.
#' @param technical_vars A `vector(character)`. Variables from design to be used with factorial planes.
#' @param n_comp A `numeric`. The number of principal components to be computed.
#' @param fig_n_comp A `numeric`. The number of principal components to be used for figures.
#' @param outliers_threshold A `numeric`. The threshold to define outliers.
#' @param title_level A `numeric`. The markdown title level, *i.e.*, the number of `#` preceding the section.
#' @param stand_pca A `character`. The stand selected in flashpca ("sd" or "center")
#'
#' @return A `data.frame`.
#' @export
#'
#' @import data.table
#' @import ggplot2
#' @import gt
#' @import patchwork
#'
#' @examples
#'
#' if (interactive()) {
#'   pca_report(
#'     data = t(mtcars),
#'     design = as.data.table(mtcars, keep.rownames = "Sample_ID"),
#'     id_var = "Sample_ID",
#'     technical_vars = c("cyl", "gear", "vs"),
#'     n_comp = 5,
#'     fig_n_comp = 5,
#'     outliers_threshold = 3,
#'     title_level = 0
#'   )
#' }
#'
pca_report_modified <- function(
  data,
  design,
  id_var = "Sample_ID",
  technical_vars,
  n_comp = 5,
  fig_n_comp = n_comp,
  outliers_component = 1:2,
  outliers_threshold = 3,
  title_level = 2, 
  stand_pca = "sd" ## center also possible if ever standard
) {
  message_prefix <- "[rain] "
  message(message_prefix, "PCA started ...")

  if (!inherits(design, "data.frame")) stop(message_prefix, '"design" must be a "data.frame"!')

  design <- design %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::all_of(id_var)), as.character) %>%
    dplyr::filter(.data[[id_var]] %in% !!colnames(data))

  keep_technical <- design %>%
    dplyr::summarise_at(
      .vars = dplyr::vars(tidyselect::all_of(technical_vars)),
      .funs = ~ dplyr::n_distinct(.x) > 1 & dplyr::n_distinct(.x) < length(.x)
    ) %>%
    dplyr::select_if(~ all(isTRUE(.x)), identity) %>%
    colnames()

  variables_excluded <- setdiff(technical_vars, keep_technical)
  if (length(variables_excluded)!=0) {
    message(message_prefix,
      "The following variables have been excluded (null variances or confounding with samples): ",
      glue::glue_collapse(variables_excluded, sep = ", ", last = " and ")
    )
  }

  pca_res <- flashpcaR::flashpca(
    X = t(as.matrix(data)),
    stand = stand_pca,
    ndim = n_comp
  )

  pca_dfxy <- pca_res[["projection"]] %>%
    tibble::as_tibble(.name_repair = ~ paste0("PC", seq_len(length(.x)))) %>%
    dplyr::mutate(!!id_var := as.character(colnames(data))) %>%
    dplyr::right_join(y = design, by = id_var)

  cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA inertia contribution {-}\n"))
  p_inertia <- tibble::tibble(
    # y = (pca_res$values / sum(pca_res$values)), ## Estimation de l'inertie --here
    y = pca_res$pve, ## Real % of variance explained ## no exactly the same...
    x = sprintf("PC%02d", seq_along(pca_res$values)),
    cumsum = cumsum(.data[["y"]])
  ) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
    ggplot2::geom_bar(stat = "identity", width = 1, colour = "white", fill = "#3B528BFF", na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.05))) +
    # ggplot2::labs(y = glue::glue("Inertia (% of {n_comp} PCs)"), x = "Principal Components (PCs)")
    ggplot2::labs(
      y = "Proportion of total variance explained", 
      x = "Principal Components (PCs)"
    )
  print(p_inertia)
  cat("\n")

  if (length(keep_technical)>0) {
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA factorial planes {- .tabset}\n"))
    for (ivar in keep_technical) {
      cat(paste0("\n", paste(rep("#", title_level + 1), collapse = ""), " ", ivar, " {-}\n"))
      p_plan <- paste0("PC", 1:fig_n_comp) %>%
        utils::combn(2) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        stats::setNames(c("X.PC", "Y.PC")) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          data = purrr::map2(
            .x = .data[["X.PC"]],
            .y = .data[["Y.PC"]],
            .f = ~ stats::setNames(pca_dfxy[, c(all_of(ivar), .x, .y)], c(all_of(ivar), "X", "Y"))
          )
        ) %>%
        tidyr::unnest(.data[["data"]]) %>%
        dplyr::mutate_at(dplyr::vars(all_of(ivar)), as.character) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = .data[["X"]], y = .data[["Y"]], colour = .data[[ivar]])) +
        ggplot2::geom_hline(yintercept = 0, na.rm = TRUE) +
        ggplot2::geom_vline(xintercept = 0, na.rm = TRUE) +
        ggplot2::geom_point(shape = 4, size = 2, na.rm = TRUE) +
        # ggplot2::stat_ellipse(type = "norm", na.rm = TRUE) +
        ggforce::geom_mark_ellipse(aes(colour = .data[[ivar]])) + #, label = .data[[ivar]])) +
        {
          if (is.numeric(pca_dfxy[[ivar]])) {
            ggplot2::scale_colour_viridis_c(
              name = NULL,
              begin = 0,
              end = 0.75
            )
          } else {
            ggplot2::scale_colour_viridis_d(
              name = NULL,
              begin = if (data.table::uniqueN(pca_dfxy[[ivar]]) == 2) 0.25 else 0,
              end = 0.75,
              guide = ggplot2::guide_legend(override.aes = list(size = 4))
            )
          }
        } + 
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
        ggplot2::scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!ggplot2::sym("Y.PC")),
          cols = ggplot2::vars(!!ggplot2::sym("X.PC")),
          scales = "free"
        ) +
        ggplot2::guides(colour = ifelse(dplyr::n_distinct(pca_dfxy[[ivar]]) <= 12, "legend", "none")) +
        ggplot2::labs(colour = ivar)
      print(p_plan)
      cat("\n")
    }

    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA association {-}\n"))
    if(FALSE) { ## old version when dependant variable are not concidered 
    p_asso <- pca_dfxy %>%
      tidyr:: pivot_longer(names_to = "PC", values_to = "Values", cols = dplyr::num_range("PC", 1:n_comp)) %>%
      dplyr::filter(.data[["PC"]] %in% paste0("PC", 1:fig_n_comp)) %>%
      dplyr::group_by(.data[["PC"]]) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        lm = purrr::map(.data[["data"]], function(data) {
          stats::lm(
            stats::as.formula(paste0("Values ~ ", paste(keep_technical, collapse = " + "))),
            data = data
          ) %>%
            stats::anova() %>%
            tibble::rownames_to_column(var = "term") %>%
            dplyr::filter(.data[["term"]] != "Residuals") %>%
            dplyr::mutate(term = gsub("factor\\((.*)\\)", "\\1", .data[["term"]]))
        })
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data[["PC"]], .data[["lm"]]) %>%
      tidyr::unnest(.data[["lm"]]) %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = factor(.data[["PC"]]),
          y = .data[["term"]],
          fill = .data[["Pr(>F)"]]
        )
      ) +
        ggplot2::geom_tile(colour = "white", na.rm = TRUE) +
        ggplot2::geom_text(
          mapping = ggplot2::aes(label = scales::scientific(.data[["Pr(>F)"]], digits = 2)),
          colour = "white",
          size = 3,
          na.rm = TRUE
        ) +
        ggplot2::scale_fill_viridis_c(name = "P-Value", na.value = "grey85", limits = c(0, 0.1)) +
        ggplot2::theme(panel.grid = ggplot2::element_blank()) +
        ggplot2::scale_x_discrete(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::labs(x = "PCA Components", y = NULL)
    print(p_asso)
    cat("\n")
    
    }
    
    ## adapted form MC code : 
    asso_dt <- reshape2::melt(
      data = pca_dfxy, 
      measure.vars = grep("^PC[0-9]+$", names(pca_dfxy), value = TRUE), 
      variable.name = "pc", 
      value.name = "values"
    ) %>% 
      group_by(pc) %>% 
      nest() %>% 
      mutate(out = map(.x = data, .f = function(.SD) { 
        m <- model.matrix(
          object = as.formula(paste0("values ~ ", paste(keep_technical, collapse = " + "))),
          data = .SD
        )
    
        if (qr(m)$rank == ncol(m)) {
          out <- as.data.table(
            anova(
              lm(
                formula = as.formula(paste0("values ~ ", paste(keep_technical, collapse = " + "))),
                data = .SD
              )
            ),
            keep.rownames = "term"
          )[term != "Residuals"]
        } else {
          out <- rbindlist(lapply(X = keep_technical, .data = .SD, FUN = function(.x, .data) {
            as.data.table(
              anova(
                lm(
                  formula = as.formula(paste0("values ~ ", .x)),
                  data = .SD
                )
              ), 
              keep.rownames = "term"
            )[term != "Residuals"]
          }))
        }
        out[, full_rank := qr(m)$rank == ncol(m)]
        })
      ) %>% 
      ungroup(pc) %>% 
      dplyr::select(-data) %>% 
      unnest(out)
    
    p_asso <- ggplot(
      data = asso_dt,
      mapping = aes(
        x = factor(pc), 
        y = factor(term, levels = sort(unique(term), decreasing = TRUE)), 
        fill = `Pr(>F)`
      )
    ) +
      geom_tile(colour = "white", na.rm = TRUE) +
      ggtext::geom_richtext(
        mapping = aes(
          label = gsub(
            pattern = "(.*)e([-+]*)0*(.*)",
            replacement = "\\1<br>&times;<br>10<sup>\\2\\3</sup>",
            x = scientific(.data[["Pr(>F)"]], digits = 2)
          )
        ),
        colour = "white",
        fill = NA, 
        label.colour = NA,
        size = 2.5,
        na.rm = TRUE
      ) +
      scale_fill_viridis_c(name = "P-Value", na.value = "grey85", end = 0.75, limits = c(0, 0.1)) +
      theme(panel.grid = element_blank()) +
      scale_x_discrete(
        expand = c(0, 0), 
        labels = function(x) {
          paste0(
            x, "<br><i style='font-size:5pt;'>(",
            percent_format(accuracy = 0.01, suffix = " %")(pca_res[["pve"]][as.numeric(gsub("PC", "", x))]), 
            ")</i>"
          )
        }
      ) +
      scale_y_discrete(expand = c(0, 0), labels = toupper) +
      labs(
        x = "Principal Components (PCs)", 
        y = "Variables",
        # title = "Association Tests Between Variables And Principal Components",
        title = "Association Tests Between Variables And PCs",
        caption = ifelse(
          test = all(asso_dt[["full_rank"]]), 
          yes = "Variables are tested against principal components using ANOVA.", 
          no = paste(
            "Variables are independently tested against principal components using ANOVA", 
            "(*i.e.*, model matrix is not full rank)."
          )
        )
      ) +
      theme(
        axis.text.x = ggtext::element_markdown(), 
        axis.text.y = ggtext::element_markdown(), 
        plot.caption = ggtext::element_markdown(face = "italic", size = rel(0.5))
      )
    print(p_asso)
    cat("\n")
  }


  if (!is.null(outliers_component)) {
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA Outliers {-}\n"))
    pcs <- paste0("PC", outliers_component)
    pca_dfxy <- pca_dfxy %>%
      dplyr::mutate(
        dist_centre = sqrt(as.vector(scale(.data[[pcs[1]]]))^2 + as.vector(scale(.data[[pcs[2]]]))^2),
        # high = .data[["dist_centre"]] >=
        #   (stats::median(.data[["dist_centre"]], na.rm = TRUE) +
        #     !!outliers_threshold * stats::IQR(.data[["dist_centre"]], na.rm = TRUE)),
        # low = .data[["dist_centre"]] <=
        #   (stats::median(.data[["dist_centre"]], na.rm = TRUE) -
        #      !!outliers_threshold * stats::IQR(.data[["dist_centre"]], na.rm = TRUE)),
        # bad_samples_bool = .data[["high"]] | .data[["low"]],
        # high = NULL,
        # low = NULL,
        # is_outlier = factor(ifelse(.data[["bad_samples_bool"]], "Yes", "No"), levels = c("Yes", "No")), 
        is_outlier = factor(
          x = dist_centre > (
            stats::quantile(dist_centre, 0.75, na.rm = TRUE) + outliers_threshold * stats::IQR(dist_centre, na.rm = TRUE)
          ),
          levels = c(FALSE, TRUE),
          labels = c("No", "Yes")
        ), 
        dist_centre = NULL
      )
    
    ivar <- "is_outlier"
    p_outliers <- paste0("PC", 1:fig_n_comp) %>%
      utils::combn(2) %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      tibble::as_tibble() %>%
      stats::setNames(c("X.PC", "Y.PC")) %>%
      dplyr::mutate(
        data = purrr::map2(
          .x = .data[["X.PC"]],
          .y = .data[["Y.PC"]],
          .f = ~ stats::setNames(pca_dfxy[, c(!!ivar, .x, .y)], c(!!ivar, "X", "Y"))
        )
      ) %>%
      tidyr::unnest(.data[["data"]]) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::all_of(ivar)), as.character) %>%
      ggplot2::ggplot(mapping = ggplot2::aes(x = .data[["X"]], y = .data[["Y"]], colour = .data[[ivar]])) +
      ggplot2::geom_hline(yintercept = 0, na.rm = TRUE) +
      ggplot2::geom_vline(xintercept = 0, na.rm = TRUE) +
      ggplot2::geom_point(shape = 4, size = 2, na.rm = TRUE) +
      ggplot2::stat_ellipse(type = "norm", na.rm = TRUE) +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!ggplot2::sym("Y.PC")),
        cols = ggplot2::vars(!!ggplot2::sym("X.PC")),
        scales = "free"
      ) +
      ggplot2::guides(colour = ifelse(dplyr::n_distinct(pca_dfxy[[ivar]]) <= 12, "legend", "none")) +
      ggplot2::labs(colour = ivar)
    print(p_outliers)
    cat("\n")
    
    out_tab <- pca_dfxy[pca_dfxy$is_outlier %in% "Yes", c("Sample_ID", technical_vars)]
    if(nrow(out_tab)>0) {
      gt::gt(
        data = out_tab,
        auto_align = "center"
      ) %>%
        gt::tab_header(title = "Samples Identified As Possible Outliers") %>% 
        gt::opt_row_striping() %>%
        gt::opt_all_caps() %>%
        print()
    }

  }

  message(message_prefix, "PCA ended.")

  invisible(pca_dfxy)
}


#### pca report plus ====================================================================================================

pca_report_plus <- function(
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
# counts_annotated <- read_rds(path = "/disks/DATATMP/projecta/counts_annotated3.rds")
# rownames(counts_annotated) <- paste0(
#   counts_annotated$ensembl_gene_id, "_",
#   counts_annotated$external_gene_name
# )
# counts_annotated <- as.matrix(counts_annotated[, -c(1, 2)])

# # Covariates
# CovData <- read.table(file = "/disks/DATATMP/projecta/samples_run_type.txt", header = FALSE) %>%
#   `colnames<-`(c("run","samples","type")) %>%
#   mutate(
#     samples_t = paste0(samples,"_",type)
#   )

# data = counts_annotated
# design = CovData
# id_var = "samples"
# technical_vars = "type"
# # technical_vars =  c("type","type2")
# n_comp = 5
# fig_n_comp = n_comp
# 
# test <- pca_report_plus(
#     data = counts_annotated,
#     design = CovData,
#     id_var = "samples",
#     technical_vars = "type",
#     n_comp = 5,
#     fig_n_comp = 5
# )
# 
# CovData$type2 <- c(rep(1,30),rep(0,29))
# test <- pca_report_plus(
#     data = counts_annotated,
#     design = CovData,
#     id_var = "samples",
#     technical_vars = c("type","type2"),
#     n_comp = 5,
#     fig_n_comp = 5
# )



#### PCA asso test =================================================================================================
# my note about
# Tests d’association aux composantes principales avec et sans indépendances des variables

asso_dt <- melt(
  data = pca_dfxy, 
  measure.vars = grep("^PC[0-9]+$", names(pca_dfxy), value = TRUE), 
  variable.name = "pc", 
  value.name = "values"
)[pc %in% sprintf("PC%02d", 1:n_comp)][,
  {
    m <- model.matrix(
      object = as.formula(paste0("values ~ ", paste(keep_technical, collapse = " + "))),
      data = .SD
    )

    if (qr(m)$rank == ncol(m)) {
      out <- as.data.table(
        anova(
          lm(
            formula = as.formula(paste0("values ~ ", paste(keep_technical, collapse = " + "))),
            data = .SD
          )
        ),
        keep.rownames = "term"
      )[term != "Residuals"]
    } else {
      out <- rbindlist(lapply(X = keep_technical, .data = .SD, FUN = function(.x, .data) {
        as.data.table(
          anova(
            lm(
              formula = as.formula(paste0("values ~ ", .x)),
              data = .SD
            )
          ), 
          keep.rownames = "term"
        )[term != "Residuals"]
      }))
    }
    out[, full_rank := qr(m)$rank == ncol(m)]
  },
  by = "pc"
]

ggplot(
  data = asso_dt,
  mapping = aes(
    x = factor(pc), 
    y = factor(term, levels = sort(unique(term), decreasing = TRUE)), 
    fill = `Pr(>F)`
  )
) +
  geom_tile(colour = "white", na.rm = TRUE) +
  geom_richtext(
    mapping = aes(
      label = gsub(
        pattern = "(.*)e([-+]*)0*(.*)",
        replacement = "\\1<br>&times;<br>10<sup>\\2\\3</sup>",
        x = scientific(.data[["Pr(>F)"]], digits = 2)
      )
    ),
    colour = "white",
    fill = NA, 
    label.colour = NA,
    size = 2.5,
    na.rm = TRUE
  ) +
  scale_fill_viridis_c(name = "P-Value", na.value = "grey85", end = 0.75, limits = c(0, 0.1)) +
  theme(panel.grid = element_blank()) +
  scale_x_discrete(
    expand = c(0, 0), 
    labels = function(x) {
      paste0(
        x, "<br><i style='font-size:5pt;'>(",
        percent_format(accuracy = 0.01, suffix = " %")(pca_res[["pve"]][as.numeric(gsub("PC", "", x))]), 
        ")</i>"
      )
    }
  ) +
  scale_y_discrete(expand = c(0, 0), labels = toupper) +
  labs(
    x = "Principal Components", 
    y = "Variables",
    title = "Association Tests Between Variables And Principal Components",
    caption = ifelse(
      test = all(asso_dt[["full_rank"]]), 
      yes = "Variables are tested against principal components using ANOVA.", 
      no = paste(
        "Variables are independently tested against principal components using ANOVA", 
        "(*i.e.*, model matrix is not full rank)."
      )
    )
  ) +
  theme(
    axis.text.x = element_markdown(), 
    axis.text.y = element_markdown(), 
    plot.caption = element_markdown(face = "italic", size = rel(0.5))
  )

