read_vcf <- function(vcf_file) {
  tmp_scan <- try(
    scan(
      file = vcf_file,
      what = character(), n = 10000, sep = "\n", skip = 0,
      fill = TRUE, na.strings = "", quote = "\"", quiet = TRUE),
    silent = TRUE
  ) ## find the start of the vcf file
  skip.lines <- grep("^#CHROM", tmp_scan)

  geno_mat <- data.table::fread(
    vcf_file, 
    header = TRUE, 
    showProgress = FALSE, 
    skip = skip.lines - 1
  )
  return(geno_mat)
}
