library(data.table)
library(ggplot2)
library(ggrepel)
library(scales) # for trans_new

# param
n_lab <- 10 # number of label wanted

# pval trans is a function called later
pval_trans <- function(alpha = NULL, md = FALSE, prefix = FALSE, colour = "#b22222") {
  trans_new(
    name = "pval",
    domain = c(0, 1),
    transform = function(x) {
      x[x < .Machine$double.xmin] <- .Machine$double.xmin
      -log(x, 10)
    },
    inverse = function(x) 10^-x,
    breaks = (function(n = 5) {
      function(x) {
        max <- floor(-log(min(c(x, alpha), na.rm = TRUE), base = 10))
        if (max == 0) 1 else sort(unique(c(10^-seq(0, max, by = floor(max / n) + 1), alpha)))
      }
    })(),
    format = (function(x, digits = 3) {
      if (md & nchar(system.file(package = "ggtext")) != 0) {
        prefix_text <- if (prefix) "&alpha; = " else ""
        x_fmt <- gsub(
          "^(.*)e[+]*([-]*)0*(.*)$",
          "\\1 &times; 10<sup>\\2\\3</sup>",
          format(x, scientific = TRUE, digits = digits)
        )
        x_fmt[x %in% c(0, 1)] <- x[x %in% c(0, 1)]
        x_fmt <- gsub("^1 &times; ", "", x_fmt)
        alpha_idx <- format(x, scientific = TRUE, digits = digits) ==
          format(alpha, scientific = TRUE, digits = digits)
        x_fmt[alpha_idx] <- paste0("<b style='color:", colour, ";'>", prefix_text, x_fmt[alpha_idx], "</b>")
        x_fmt
      } else {
        prefix_text <- if (prefix) "alpha == " else ""
        x_fmt <- gsub(
          "^(.*)e[+]*([-]*)0*(.*)$",
          "\\1 %*% 10^\\2\\3",
          format(x, scientific = TRUE, digits = digits)
        )
        x_fmt[x %in% c(0, 1)] <- x[x %in% c(0, 1)]
        x_fmt <- gsub("^1 \\%\\*\\% ", "", x_fmt)
        alpha_idx <- format(x, scientific = TRUE, digits = digits) ==
          format(alpha, scientific = TRUE, digits = digits)
        x_fmt[alpha_idx] <- paste0(prefix_text, x_fmt[alpha_idx])
        parse(text = x_fmt)
      }
    })
  )
}


file_ewas <- "path/to/my/results.csv.gz"
dt <- data.table::fread(file_ewas)

## here by gene name (by = "UCSC_RefGene_Name"), we gonna get only the lower pvalue (min(pvalue)) and put the gene label "UCSC_RefGene_Name"
dt <- dt[
  j = gene_label_min := data.table::fifelse(
    test = pvalue == min(pvalue, na.rm = TRUE) &
      !is.na(UCSC_RefGene_Name) &
      !UCSC_RefGene_Name %in% c("", "NA"),
    yes = paste(unique(unlist(strsplit(gsub(",", ";", UCSC_RefGene_Name), ";"))), collapse = ";"),
    no = NA_character_
  ),
  by = "UCSC_RefGene_Name"
][
  i = pvalue > 0.05,
  j = pvalue := NA_real_
][
  j = file := basename(file_ewas)
][
  j = cpg_chr := sub("chr", "", cpg_chr)
][
  j = c("estimate", "cpg_chr", "cpg_pos", "pvalue", "gene_label_min")
][order(pvalue)]
head(dt) ## but here as you can see it is too bad that we have empty label when no gene name...
## let's put cpg name when no gene name available

dt <- data.table::fread(file_ewas) # restart with the whole file
# first clear gene name to use
dt$gene_name_clean <- unlist(lapply(seq_len(nrow(dt)), function(i) {
  ifelse(
    test = dt$UCSC_RefGene_Name[i] %in% c("", "NA") | is.na(dt$UCSC_RefGene_Name[i]),
    yes = dt$CpG[i],
    no = paste0(unique(unlist(strsplit(dt$UCSC_RefGene_Name[i], split = ";"))), collapse = ";") # clean unique gene name
  )
}))
dt <- dt[
  j = gene_label_min := data.table::fifelse(
    test = pvalue == min(pvalue, na.rm = TRUE),
    yes = gene_name_clean, # put the clean label for the min pvalues
    no = NA_character_ # do not put a label for other pvalues
  ),
  by = "gene_name_clean"
][
  i = pvalue > 0.05,
  j = pvalue := NA_real_
][
  j = file := basename(file_ewas)
][
  j = cpg_chr := sub("chr", "", cpg_chr)
][
  j = c("CpG", "estimate", "cpg_chr", "cpg_pos", "pvalue", "gene_label_min", "gene_name_clean")
][order(pvalue)]
head(dt, 15) # check what you got !
# for each gene, you will only label the lowest pvalue get.
# if the gene name is not known, it because the CpG number.

if (is.numeric(dt[["cpg_chr"]])) {
  dt[j = "cpg_chr" := lapply(.SD, as.character), .SDcols = "cpg_chr"]
}

if (dt[!is.na(gene_label_min), .N] > n_lab) {
  dt[which(!is.na(gene_label_min))[-c(1:n_lab)], gene_label_min := NA_character_]
}

alpha <- 0.05 / nrow(dt) # so alpha is boneferoni threshold here

myvolvano <- ggplot2::ggplot(dt) +
  ggplot2::aes(
    x = .data[["estimate"]],
    y = .data[["pvalue"]],
    colour = abs(.data[["estimate"]])
  ) +
  ggplot2::geom_vline(xintercept = 0, linetype = 2) +
  ggplot2::geom_point(size = 0.60) +
  ggplot2::geom_hline(yintercept = alpha, linetype = 2, colour = "#b22222") +
  ggplot2::scale_colour_viridis_c( # beautifull palette of color !
    trans = "sqrt", limits = c(0, NA),
    begin = 0.1, end = 0.95, option = "plasma" # plasma palette selected here
  ) +
  ggplot2::scale_y_continuous(
    trans = pval_trans(md = TRUE), # so put low pvalue with transformation
    expand = ggplot2::expansion(mult = c(0, 0.2))
  ) +
  ggplot2::coord_cartesian(ylim = c(0.05, NA)) + # to zoom
  ggrepel::geom_label_repel( # to show label in box
    mapping = ggplot2::aes(label = .data[["gene_label_min"]]),
    show.legend = FALSE,
    min.segment.length = 0,
    size = 2.5,
    na.rm = TRUE,
    max.overlaps = Inf # to avoid overlab label
  ) +
  ggplot2::labs(
    x = "Estimates",
    y = "P-values",
    colour = "Estimates",
    subtitle = pretty_trait
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(face = "italic"),
    axis.title.x = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown(),
    legend.position = "none"
  )

myvolvano # show it !

ggsave(file = "path/to/save/plot.png", plot = myvolvano)
# specify size, resolution etc...
