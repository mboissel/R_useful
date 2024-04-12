#R 
## Aim : extract all genes existing in some VCF, via bcftools call in R envir

library(data.table)
library(targets)

samples_files <- tar_read("samples_files")
nb_vcf_to_read <- 10

extract_info <- unique(rbindlist(
  lapply(X = seq_len(nb_vcf_to_read), FUN = function(i) {
    unique(rbindlist(list( 
      data.table::setnames(data.table::fread(
        cmd = paste(
          "bcftools",
          "query -f '%CHROM\t%POS\t%ID\t%REF\t%ALT\t%INFO/SYMBOL\n'",
          samples_files$snp_germinal[i]
        )
      ), c("CHROM", "POS", "ID", "REF", "ALT", "SYMBOL"))
      ,
      data.table::setnames(data.table::fread(
        cmd = paste(
          "bcftools",
          "query -f '%CHROM\t%POS\t%ID\t%REF\t%ALT\t%INFO/SYMBOL\n'",
          samples_files$indel_germinal[i]
        )
      ), c("CHROM", "POS", "ID", "REF", "ALT", "SYMBOL"))
    )))
  }),
  use.names = TRUE
))

# head(extract_info, 15)
# unique(extract_info$SYMBOL)

