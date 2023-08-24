library(data.table)
# several notes to play with data.table

dt <- as.data.table(mtcars)

dt[j = .SD[.N > 10], by = "cyl"]

dt[j = c("a", "b") := list("a", "b")]

dt[j = `:=`(
  c = "a",
  d = "b"
)][]

## Example of summarize 
dat[, .(count = .N, var = sum(VAR)), by = MNTH]
out <- data[, .(
      N = .N,
      AGE = mean_sd_n(x = AGE, digits = digits),
      BMI = mean_sd_n(x = BMI, digits = digits),
        SEX = paste0(
          "M:", format(sum(SEX == 1), big.mark = ",", scientific = FALSE),
          " / F:", format(sum(SEX == 2), big.mark = ",", scientific = FALSE)
        ),
        FG = mean_sd_n(x = FG, digits = digits)
      ),
      by = Status
    ]
# You can also add these values to your existing dataset by updating your dataset by reference:
dat[, `:=` (count = .N, var = sum(VAR)), by = MNTH]

## update DT with condition  ##
Annotation_qced[MAF > 0.05, Samples_with_mutation := "..."]

## annotation 
genotype_matrix <- data.table::as.data.table(genotype_matrix)
genotype_matrix <- genotype_matrix[var_id %in% annotation_data$var_id, ]
list_vars <- genotype_matrix$var_id
genotype_matrix$var_id <- NULL
list_samples <- names(genotype_matrix)
genotype_matrix <- data.table::transpose(genotype_matrix)
names(genotype_matrix) <- list_vars
genotype_matrix$ID <- list_samples
data.table::setcolorder(DT,c("ID", list_vars))

data_matrix <- data.table::as.data.table(iris[, -5], keep.rownames = "Sample_ID")

df <- data.table::as.data.table(
  iris, keep.rownames = "Sample_ID"
)[, otherSimu := rnorm(n = nrow(iris))][sample(x = seq_len(nrow(iris)), size = 20, replace = FALSE), ]

data_matrix <- as.data.table(
  df[, !c("Species", "Sample_ID")] %>%
    t()  %>% 
    as.matrix() %>% 
    Hmisc::rcorr(., type = "spearman") %>% 
    `[[`("r"), 
)[, Sample_ID := df$Sample_ID]
setnames(data_matrix, names(data_matrix), c(df$Sample_ID, "Sample_ID"))
setcolorder(data_matrix,"Sample_ID")

data_gg <- melt(
  data_matrix, 
  measure.vars = colnames(data_matrix[, -"Sample_ID"])
)[, 
  (c("Sample_ID", "variable")) := 
    list(
      factor(Sample_ID, levels = data_matrix[order.dendrogram(dd_row), Sample_ID]),
      factor(variable, levels = colnames(data_matrix[, -"Sample_ID"])[order.dendrogram(dd_col)])
    )
]

df_tmp <- df[,c("Status2","Status3"):=list(
  factor(x = sample(x = 1:2, size = nrow(df), replace = TRUE), levels = c(1:2), labels = c("oui", "non")),
  factor(x = sample(x = 1:3, size = nrow(df), replace = TRUE), levels = c(1:3), labels = c("oui", "non", "peutetre")))
]


#### Sort rows by group ####
mtcars_dt <- as.data.table(mtcars)
mtcars_dt %>% group_by(cyl) %>% arrange(disp) ## avec le tidy
mtcars_dt[order(drat), ., by = cyl]

#### Merge intervalles ####
out <- merge(
      x = data,# [, -c("chr", "position")], 
      y = annot_regions[
        data,  
        .(
          rowid = unique(rowid), 
          region = paste(region, collapse = ";"), 
          file = paste(file, collapse = ";"), 
          genomic_region = paste(genomic_region, collapse = ";")
        ),
        on = .(chr, start <= position, end >= position), 
        mult = "all",
        by = .EACHI
      ][, .(rowid, region, genomic_region, file)],
      by = "rowid"
    )[, -c("rowid")]

library(data.table)
x = data.table(
  chr = c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
  start = c(5, 10, 25, 50, 100), 
  end = c(15,15,35,60,200), 
  geneid = letters[1:5]
)[, rowid := 1:.N]
y = data.table(
  chr = c("Chr1", "Chr1", "Chr2"), 
  start = c(1, 11, 55),
  end = c(1, 11, 55), 
  GpGid = paste0("CpG", letters[1:3])
)
setkey(y, chr, start, end)
foverlaps(x, y, type = "any", nomatch = NULL,  mult = "all")


library(data.table)
x <- data.table(
  chr = c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
  start = c(5, 10, 25, 50, 100), 
  end = c(15,15,35,60,200), 
  geneid = letters[1:5]
)
y <- data.table(
  chr = c("Chr1", "Chr1", "Chr2"), 
  position = c(1, 11, 55),
  GpGid = paste0("CpG", letters[1:3])
)
x[
  j = `:=`(
    "start_cis_window" = start - 5,
    "end_cis_window" = start + 5
  )
]
y[
  i = x, 
  j = list(geneid, GpGid, dist = start_cis_window - position), 
  on = list(chr, position >= start_cis_window, position <= end_cis_window), 
  by = .EACHI, 
  nomatch = NULL
][j = list(geneid, GpGid, dist)]
#>    geneid GpGid dist
#> 1:      a  CpGa   -1
#> 2:      b  CpGb   -6
#> 3:      d  CpGc  -10


xy <- y[
  i = x[
    j = list(
      geneid,
      chr,
      "start_cis_window" = start - 5,
      "end_cis_window" = start + 5
    )
  ], 
  j = list(geneid, GpGid, dist = start_cis_window - position), 
  on = list(chr, position >= start_cis_window, position <= end_cis_window), 
  by = .EACHI, 
  nomatch = NULL
][, list(geneid, GpGid, dist)]

xy


library(data.table)
x = data.table(
  chr = c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
  start = c(5, 15, 25, 50, 100), 
  end = c(15,20,35,60,200), 
  file_name = paste0("/path/to/file/", letters[1:5])
)[, rowid := 1:.N]
y = data.table(
  chr = c("Chr1", "Chr1", "Chr2"), 
  start = c(1, 11, 55),
  end = c(1, 11, 55), 
  gene_targeter = paste0("gene_", letters[1:3])
)
setkey(y, chr, start, end)
foverlaps(x, y, type = "any", nomatch = NULL,  mult = "all")

#### unnest ####
library(rlang)
unnest_dt <- function(tbl, col) {
  tbl <- as.data.table(tbl)
  col <- ensyms(col)
  clnms <- syms(setdiff(colnames(tbl), as.character(col)))
  tbl <- as.data.table(tbl)
  tbl <- eval(
    expr(tbl[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  colnames(tbl) <- c(as.character(clnms), as.character(col))
  tbl
}

group_list <- data.table(
  group_name = c(
    "group_OPRD1",
    "group_all"
  ),
  gene_in_group = list(
    c("OPRD1_NM_000911.4"),
    c("OPRD1_NM_000911.4",  "GLIS3_NM_001042413.2")
  ), 
  comment = c(
    "...", 
    "..."
  )
)
tmp1 <- unnest_dt(tbl = group_list, col = "gene_in_group")
tmp1
tmp1[, lapply(X = .SD, FUN = function(x) toString(x = unique(x))), by = group_name ]

#### gather / merge multi data
Reduce(function(x,y) merge(x = x, y = y, by = "Gene_name"), list(group_list, acmg_snp, acmg_indel))

#### reformat multi columns #### 
sd_reformate <- names(data)[(grep("NQR008", names(data))):(grep("ENQR042", names(data)))]
new_col_names <- unlist(first_line[sd_reformate])

data <- data[,
   (new_col_names) := lapply(.SD, function(xcol) {
      factor(
        x = xcol, 
        levels = c("Tous les jours", "2-3 fois par semaine", "1 fois par semaine",
                   "2-3 fois par mois", "Environ 1 fois par mois", "Moins souvent" , "N'en consommait pas"),
        labels = c("Tous les jours", "2-3 fois par semaine", "1 fois par semaine",
                   "2-3 fois par mois", "Environ 1 fois par mois", "Moins souvent" , "N'en consommait pas")
      )
    }),     
  .SDcols = sd_reformate
]

#### gather all element by line with stringi ####
catalog[, lapply(X = .SD, FUN = function(x) toString(unique(x))), by = chr_pos_hg38]

## vapply 
ewas$hgnc_symbol <- vapply(strsplit(ewas$ewas_UCSC_RefGene_Name, split = ";"), "[", 1, FUN.VALUE = character(1))

#### summary table results ####
multiomics_table <- data.table(
  file_path = c(
    list.files(
      path = c(
        here("outputs/13-mqtl_hg38/fastqtl_combined"),
        here("outputs/14-eqtl_hg38/genes/qtltools_combined"),
        here("outputs/14-eqtl_hg38/isoforms/qtltools_combined")
      ), 
      pattern = "permutation.txt.gz",
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(
      path = c(
        here("outputs/19-eqtm_hg38")
      ), 
      pattern = "csv.gz",
      recursive = TRUE,
      full.names = TRUE
    )
  )
)[
  , level := ifelse(grepl("gene", file_path), "gene", ifelse(grepl("isoform|transcript", file_path), "transcript", ""))
][
  , analysis := gsub(
    pattern = "(.*)-(.*)_hg38", replacement = "\\2",  
    x = vapply(strsplit(file_path, split = "/", fixed = TRUE), "[", 6, FUN.VALUE = character(1))
  )
]
multiomics_table

multiomics_table_0 <- merge(
  x = multiomics_table,
  y = multiomics_table[, 
    (function(x) {
      results <- fread(x)
      setnames(results, c("p.value", "fdr_storey"), c("pvalue", "fdr"), skip_absent = TRUE)

      if (!"fdr" %in% colnames(results)) results$fdr <- NA
      if (!"permutation_pvalue" %in% colnames(results)) results$permutation_pvalue <- NA
      if (!"downstream_pvalue" %in% colnames(results)) results$downstream_pvalue <- NA

      results[, .(
        pvalue = sapply(c(0.25, 0.1, 0.05, 0.01), function(x) sum(pvalue <= x, na.rm = TRUE)),
        na_pvalues = sum(is.na(pvalue)),
        fdr = ifelse(all(is.na(fdr)), NA, sapply(c(0.25, 0.1, 0.05, 0.01), function(x) sum(fdr <= x, na.rm = TRUE))),
        permutation = ifelse(all(is.na(permutation_pvalue)), NA, sapply(c(0.25, 0.1, 0.05, 0.01), function(x) sum(permutation_pvalue <= x, na.rm = TRUE))),
        downstream = ifelse(all(is.na(downstream_pvalue)), NA, sapply(c(0.25, 0.1, 0.05, 0.01), function(x) sum(downstream_pvalue <= x, na.rm = TRUE))),
        n = .N,
        alpha  = paste("α =", format(c(0.25, 0.1, 0.05, 0.01), nsmall = 2, digits = 2))
      )]
    })(file_path),  
    by = "file_path"
  ],
  by = "file_path"
)

multiomics_table_0

#### convert numeric ####
test_dt[j = (names(test_dt)) := lapply(.SD, as.numeric), .SDcols = names(test_dt)]
res[j = (names(res)) := lapply(.SD, round, 5), .SDcols = names(res)]

#### cbind ####

cbind.fill <- function(...){
    nm <- list(...) 
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(X = nm, FUN = function (x) {
        rbind(x, matrix(, n - nrow(x), ncol(x)))
    })) 
}

writexl::write_xlsx(
  x = list(
    "follow_samples" = as.data.frame(Reduce(
      cbind.fill,
      list(
        data.frame("not_per_protocol" = full_linkage_dt$IID[
            !full_linkage_dt$Le_patient_a_t_il_termine_lessai_conformement_au_protocole %in% "Oui"
          ]
        ), 
        data.frame("not_design" = full_linkage_dt$IID[
            !full_linkage_dt$has_omics_data %in% "Oui"
          ]
        ), 
        data.frame("not_pop_study" = full_linkage_dt$IID[
            !full_linkage_dt$pop_study %in% "Oui"
          ]
        ), 
        data.frame("not_pop_child_study" = full_linkage_dt$IID[
            !full_linkage_dt$pop_child_study %in% "Oui"
          ]
        )
      )
    )
  )), 
  path = follow_exclusion_samples, 
  col_names = TRUE
)


#### to follow each step of QC ####
QC_obj <- list(
  samples_selected = sort(samples), 
  variants_seleced = sort(variants_dt[["var_id"]]), 
  cr_samples_excluded = sort(
    paste0(names(cr_samples_excluded), " (", round(cr_samples_excluded, 3), ")")
  ), 
  cr_variants_excluded = sort(
    paste0(names(cr_variants_excluded), " (", round(cr_variants_excluded, 3), ")")
  ), 
  imputed_threshold = ifelse(impute, rare_threshold, "NotApplicable"), 
  remove_null_variance = sort(snp_no_var)
)
cbind.fill <- function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(X = nm, FUN = function(x) {
        rbind(x, matrix(nrow = n - nrow(x), ncol = ncol(x)))
    }))
}

QC_dt <- setnames(
  x = as.data.table(
    Reduce(
      cbind.fill,
      QC_obj
    )
  ), 
  old = c("V1", "V2", "V3", "V4", "V5", "V6"), 
  new = c(
    "samples_selected", "variants_selected",
    "cr_samples_excluded", "cr_variants_excluded", 
    "imputed_threshold", "remove_null_variance"
  )
)


#### other tips DT ####
# Un bout de code pour transformer les variables d’un data.table, selon une formule ou un vecteur

library(data.table)
phenotypes_dt <- data.table(
  SEX = round(runif(10, 0, 1)),
  AGE = rnorm(10, 25, 2),
  BMI = rnorm(10, 25, 10)
)

# From a formula object
# formula <- ~ factor(SEX, levels = c(0, 1)) + log(AGE) + BMI
# covariates <- unlist(strsplit(as.character(formula)[2], " *\\+ *"))

# From a vector directly
covariates <- c("factor(SEX, levels = c(0, 1))", "log(AGE)", "BMI")

phenotypes_dt2 <- data.table::copy(phenotypes_dt) ## to not modify the original data

expr_call <- str2expression(paste(
  c(
    "phenotypes_dt2 ", # or directly the line above defining "phenotypes_dt2 "
    sapply(
      X = covariates,
      FUN = function(x) {
        if (length(all.names(str2expression(x))) > 1) {
          sprintf("[j = %s := %s]", all.vars(str2expression(x)), x)
        } else {
          ""
        }
      },
      USE.NAMES = FALSE
    )
  ),
  collapse = ""
))
eval(expr_call)

data.table::melt(
  data = data, id.vars = 1:2, variable.name = "cpg_id", value.name = "mvalue"
)

##### Find region ####

ataqseq_dt <- structure(list(
  chr_region = c("chr1", "chr1", "chr1", "chr1", 
  "chr1", "chr1"), 
  start_region = c(6599427L, 6599344L, 6599384L, 
  6599407L, 1325539L, 1325811L), 
  end_region = c(6599730L, 6599527L, 
  6599480L, 6599445L, 1325603L, 1326069L), 
  V4 = c("AAATCCGCATATAGCTGTGTCT", 
  "AAATCCGCATGGAACGTCAGTT", "AAGCAAAGTCAGCCATGAGAAT", "AAGCAAAGTCCGAAGGAAGCCA", 
  "AAGCTTGTGCAGTTTGGAGCAT", "AAGTCCTTAGAAATGCTACGGG"), 
  V5 = c(1L, 
  8L, 6L, 9L, 4L, 1L), 
  V6 = c(".", ".", ".", ".", ".", "."), 
  from_file = c("RtmpGNzNzHGSM5589383_muscle_SM-C1MKW_rep1_fragments.bed.gz", 
  "RtmpGNzNzHGSM5589383_muscle_SM-C1MKW_rep1_fragments.bed.gz", 
  "RtmpGNzNzHGSM5589383_muscle_SM-C1MKW_rep1_fragments.bed.gz", 
  "RtmpGNzNzHGSM5589383_muscle_SM-C1MKW_rep1_fragments.bed.gz", 
  "RtmpGNzNzHGSM5589383_muscle_SM-C1MKW_rep1_fragments.bed.gz", 
  "RtmpGNzNzHGSM5589383_muscle_SM-C1MKW_rep1_fragments.bed.gz")), row.names = c(NA, 
  -6L), class = c("data.table", "data.frame")
)
cpg_info <- structure(list(
  cpg_id = c("cg06520508", "cg06599206", "cg16686458", 
  "cg26607031", "cg22028722", "cg02563469"), 
  cpg_CHR_hg38 = c("chr1", 
  "chr1", "chr1", "chr1", "chr1", "chr1"), 
  cpg_Start_hg38 = c(1325966L, 
  1329624L, 6599461L, 9542642L, 11981794L, 15729657L), 
  position_cpg = c(1325966L, 
  1329624L, 6599461L, 9542642L, 11981794L, 15729657L),
  chr_cpg = c("chr1", 
  "chr1", "chr1", "chr1", "chr1", "chr1")), row.names = c(NA, -6L
  ), class = c("data.table", "data.frame"), sorted = c("cpg_CHR_hg38", 
  "cpg_Start_hg38"))

cpg_info <- setkey(cpg_info, cpg_CHR_hg38, cpg_Start_hg38)

find_region_cpg <- cpg_info[
  i = ataqseq_dt,
  j = list(cpg_id, chr_cpg, position_cpg, chr_region, start_region, end_region, V4, V5, V6, from_file), 
  on = list(cpg_CHR_hg38 = chr_region, cpg_Start_hg38 >= start_region, cpg_Start_hg38 <= end_region),
  by = .EACHI,
  nomatch = NULL
][order(cpg_CHR_hg38, cpg_Start_hg38, cpg_id), 
][
 j = list(
   cpg_id, chr_cpg, position_cpg, chr_region, start_region, end_region, V4, V5, V6, from_file
  )
]
find_region_cpg


## dcast examples ## 
mega_res$Number_byfile_ataq_seq_region <- unlist(lapply(X = seq_len(nrow(mega_res)), FUN = function(i) {
  tmp <- unique(cpg_in_region[cpg_id %in% mega_res$cpg_id[i], .(name_file , Number_byfile_ataq_seq_region)])
  return(paste0("N_", tmp$name_file, "=", tmp$Number_byfile_ataq_seq_region, collapse = "|"))
}))
mega_res$DP_byfile_ataq_seq_region <- unlist(lapply(X = seq_len(nrow(mega_res)), FUN = function(i) {
  tmp <- unique(cpg_in_region[cpg_id %in% mega_res$cpg_id[i], .(name_file , Mean_byfile_DP)])
  return(paste0("DP_", tmp$name_file, "=", round(as.numeric(tmp$Mean_byfile_DP), digits = 3), collapse = "|"))
}))

mega_res$NB <- lapply(X = mega_res$Number_byfile_ataq_seq_region, FUN = function(x){strsplit(x, "|", fixed = TRUE)})
mega_res <- unnest_dt(tbl = mega_res, col = "NB")
mega_res <- tidyr::separate(data = mega_res, col = "NB", into = c("name_sample", "count_region_found"), sep = "=") 
mega_res <- dcast(mega_res, ... ~ name_sample, value.var = "count_region_found")

mega_res$DP <- lapply(X = mega_res$DP_byfile_ataq_seq_region, FUN = function(x){strsplit(x, "|", fixed = TRUE)})
mega_res <- unnest_dt(tbl = mega_res, col = "DP")
mega_res <- tidyr::separate(data = mega_res, col = "DP", into = c("dp_name_sample", "dp_region_found"), sep = "=") 
mega_res <- dcast(mega_res, ... ~ dp_name_sample, value.var = "dp_region_found")

mega_res$Number_byfile_ataq_seq_region <- NULL
mega_res$DP_byfile_ataq_seq_region <- NULL

fwrite(x = mega_res, file = res_file, row.names = FALSE, col.names = TRUE)
message("ok annotation AtaqSeq added")


#### assign values #### 
message("[assign_metabo]", "Select parents and children ... ")
parent_dt <- setDT(metabo_tab_annot)[is_child %in% "Non", ]
enfants_dt <- setDT(metabo_tab_annot)[is_child %in% "Oui", ]

message("[assign_metabo]", "Get metabo val from parents for each chem id ")
p_metabo <- function(p, CHEM_ID) {
  if (is.na(p) | p %in% "0") {
    return(NA_integer_)
  }
  res <- parent_dt[
    parent_dt$IID %in% p & parent_dt$CHEM_ID %in% CHEM_ID
  ][["metabo_val"]]
  if (length(res) == 0) {
    return(NA_integer_)
  }
  return(res)
}
enfants_dt <- enfants_dt[, p_metabo_val := p_metabo(p, CHEM_ID), by = c("p", "CHEM_ID", "IID")]
m_metabo <- function(m, CHEM_ID) {
  if (is.na(m) | m %in% "0") {
    return(NA_integer_)
  }
  res <- parent_dt[
    parent_dt$IID %in% m & parent_dt$CHEM_ID %in% CHEM_ID
  ][["metabo_val"]]
  if (length(res) == 0) {
    return(NA_integer_)
  }
  return(res)
}
enfants_dt <- enfants_dt[, m_metabo_val := m_metabo(m, CHEM_ID), by = c("m", "CHEM_ID", "IID")]
metabo_dt <- rbindlist(list(enfants_dt, parent_dt), use.names = TRUE, fill = TRUE)
