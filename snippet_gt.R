#### Exemple 1 =====================================================================================
## With gt package, coloration of a domain

x <- c(-2, -1, 0, 1, 2)
y <- c(4, 1, 0, 1, 4)
z <- c(1, 2, 3, 4, NA)
v <- c(1, 2, 3, 4, 5)
res <- as.data.frame(Hmisc::rcorr(cbind(x,y,z,v))$r)
res[["rownames"]] <- rownames(res)

library(gt)
library(scales)

gt(res, rowname_col = "rownames") %>% 
  data_color(
    columns = TRUE,
    colors = col_numeric(
      palette = "dodgerblue",
      domain = c(0.5, 1), 
      na.color = "white"
    )
  )
  
gt(res, rowname_col = "rownames") %>% 
  data_color(
    columns = TRUE,
    colors = col_numeric(
      palette = c("firebrick", "dodgerblue"),
      domain = c(-1, 1), 
      na.color = "white"
    )
  )


#### Exemple 2 =====================================================================================
## Coloration conditionnelle dans gt => tab_style()

library(gt)
set.seed(2020070711)
data.frame(
  Var = letters[1:5],
  beta = rnorm(5),
  p = runif(5, 0, 0.1)
) %>% 
  gt() %>% 
  tab_style(
    style = cell_fill(color = "firebrick2"), 
    locations = cells_body(columns = vars(Var), rows = Var %in% "a")
  ) %>% 
  tab_style(
    style = cell_text(color = "dodgerblue", weight = "bold"), 
    locations = cells_body(columns = everything(), rows = p <= 0.05)
  )

#### Exemple 3 ====
## format number
all_res %>% 
    filter(Gene_Symbol %in% igene & analyse_version %in% "STRICT") %>% 
    arrange(y_name, year) %>% 
    gt() %>% 
    opt_row_striping() %>% 
    gt::tab_header(title = paste(igene, "STRICT")) %>% 
    gt::fmt_number(
      columns = c(starts_with(match = c("score", "pvalue", "Pi_hat", "SE", "P_val"))),
      decimals = 3,
      use_seps = FALSE
    ) 


#### gt table ====

gt::gt(
  data =
    as_tibble(with(pheno_formated, table(pos_fam, Quel_est_votre_lien_de_parente, useNA = "ifany"))) %>% 
    tidyr::pivot_wider(
    names_from = "pos_fam",
    values_from = "n"
  )
) %>% 
  gt::tab_spanner(label = "Quel_est_votre_lien_de_parente ...", columns = 2:6) %>% 
  gt::tab_options(table.font.size = 8)



#### gt style condi ====


all_selection <- fread(
  file = file.path(params[["ewas_directory"]], glue("{project_name}_EWAS_DMP_modelSelection.csv.gz"))
)
all_selection$criterion <- NULL
# best_col <- names(all_selection)[as.numeric(all_selection[all_selection$all_trait%in%"all", ])>5]
best_col <- names(all_selection)[which.max(as.numeric(all_selection[all_selection$all_trait%in%"all", ]))]
best_col <- best_col[!is.na(best_col)]

gt_selection <-   gt(all_selection) %>% 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = all_of(best_col)
    )
  ) %>% 
  tab_options(
    table.font.size = 10, row.striping.include_table_body = TRUE
  )  %>% 
  opt_all_caps() %>%
  tab_header(
    title = html(glue("Selection of models EWAS based on AIC"))
  ) 
print(gt_selection)

#### gt color one line condi ====

thetab <- as_tibble(with(pheno_formated, table(pos_fam, Quel_est_votre_lien_de_parente, useNA = "ifany"))) %>% 
    pivot_wider(
    names_from = "pos_fam",
    values_from = "n"
  )
color_col <- which(thetab[thetab$Quel_est_votre_lien_de_parente %in% "Pere, Mere",] != 0)

gt::gt(
  data = thetab 
) %>% 
  gt::tab_spanner(label = "pos_fam (`PIC00POSFAM`)", columns = 2:(length(unique(pheno_formated$pos_fam)) + 1)) %>% 
  gt::tab_options(table.font.size = 10, row.striping.include_table_body = TRUE) %>% 
  gt::tab_style(
    style = cell_text(color = "firebrick2", weight = "bold"),
    locations = cells_body(
      columns = all_of(color_col), 
      rows = Quel_est_votre_lien_ %in% "Pere, Mere" 
    )
  )

#### gt summary  ====

tab <- all_pheno %>% 
  select(all_of(col_interst)) %>%
  # tbl_summary(by = is_Porteur)
  tbl_summary(
    label = list(FG ~ "Fasting Glucose"),
    statistic = list(all_continuous() ~ "{mean} ± {sd} ({p25}, {p75})"), # LaTeX: $\pm$
    # digits = list(age ~ c(0, 1)),  # you want the mean rounded to 1 decimal place, and the SD to 2 use digits = list(age ~ c(1, 2)).
    by = is_Porteur
  ) %>% 
  add_n() %>% 
  bold_labels() 

#### gt applied in MiST res ====

gt::gt(res[
    ,
    .SD, 
    .SDcols = c(
      "trait_name", 
       "covariates",
      "sample_size_hg19", "sample_size_hg38",
      "S.pi_hg19", "S.pi_hg38", 
      "p.value.S.pi_hg19", "p.value.S.pi_hg38", 
      "S.tau_hg19", "S.tau_hg38", 
      "p.value.S.tau_hg19", "p.value.S.tau_hg38", 
      "p.value.overall_hg19",  "p.value.overall_hg38", 
      "Pi_hat", "OR", "estimate.pi.hat",
      "P_val", "p.value.pi.hat"
    )
]) %>% 
  tab_header(
      title = "MiST Statistics"
  ) %>% 
  tab_options(
    table.font.size = 7, row.striping.include_table_body = TRUE
  ) %>% 
  tab_style(
    style = cell_text(color = "firebrick", weight = "bold"),
    locations = cells_body(columns = c("sample_size_hg19", "sample_size_hg38"), rows = sample_size_hg19 != sample_size_hg38)
  ) %>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(columns = c("sample_size_hg19", "sample_size_hg38"), rows = sample_size_hg19 == sample_size_hg38)
  ) %>% 
  tab_style(
    style = cell_text(color = "green", weight = "bold"), 
    locations = cells_body(columns = c("p.value.S.pi_hg19", "p.value.S.pi_hg38"), rows = (p.value.S.pi_hg19 <= 0.05 & p.value.S.pi_hg38 <= 0.05) | (p.value.S.pi_hg19 > 0.05 & p.value.S.pi_hg38 > 0.05))
  ) %>% 
  tab_style(
    style = cell_text(color = "green", weight = "bold"), 
    locations = cells_body(columns = c("p.value.S.tau_hg19", "p.value.S.tau_hg38"), rows = (p.value.S.tau_hg19 <= 0.05 & p.value.S.tau_hg38 <= 0.05) | (p.value.S.tau_hg19 > 0.05 & p.value.S.tau_hg38 > 0.05))
  ) %>% 
  tab_style(
    style = cell_text(color = "green", weight = "bold"), 
    locations = cells_body(columns = c("p.value.overall_hg19", "p.value.overall_hg38"), rows = (p.value.overall_hg19 <= 0.05 & p.value.overall_hg38 <= 0.05) | (p.value.overall_hg19 > 0.05 & p.value.overall_hg38 > 0.05))
  ) %>% 
  tab_style(
    style = cell_text(color = "firebrick", weight = "bold"), 
    locations = cells_body(columns = c("p.value.overall_hg19", "p.value.overall_hg38"), rows = (p.value.overall_hg19 <= 0.05 & p.value.overall_hg38 > 0.05) | (p.value.overall_hg19 > 0.05 & p.value.overall_hg38 <= 0.05)
  ))  %>% 
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(columns = c("P_val", "p.value.pi.hat"), rows = (P_val <= 0.05 & p.value.pi.hat <= 0.05) | (P_val > 0.05 & p.value.pi.hat > 0.05))
  ) %>%
  tab_style(
    style = cell_text(color = "firebrick", weight = "bold"),
    locations = cells_body(columns = c("P_val", "p.value.pi.hat"), rows = (P_val <= 0.05 & p.value.pi.hat > 0.05) | (P_val > 0.05 & p.value.pi.hat <= 0.05)
  )) %>%
  tab_style(
    style = cell_text(color = "dodgerblue", weight = "bold"),
    locations = cells_body(columns = estimate.pi.hat, rows = p.value.pi.hat <= 0.05)
  ) %>%
  tab_style(
    style = cell_text(color = "dodgerblue", weight = "bold"),
    locations = cells_body(columns = c("Pi_hat", "OR"), rows = P_val <= 0.05)
  ) %>%
  tab_style(
    style = cell_text(color = "dodgerblue", weight = "bold"),
    locations = cells_body(columns =  c("S.pi_hg19", "S.tau_hg19"), rows = p.value.overall_hg19 <= 0.05)
  ) %>% 
  tab_style(
    style = cell_text(color = "dodgerblue", weight = "bold"),
    locations = cells_body(columns =  c("S.pi_hg38", "S.tau_hg38"), rows = p.value.overall_hg38 <= 0.05)
  ) 

