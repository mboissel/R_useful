## snippet to do a meta analyses ##

# result path
output_directory = "/tmp/"

# options(stringsAsFactors = FALSE)

#### load  ---------------------------------------------------------------------
# library(grid)
library(meta)
# library(readxl) 
# library(writexl) 
# library(officer) ## for read_pptx => write pptx 
# library(rvg) ## for ph_with_vg_at
library(tidyverse) ## to use `%>%`, mutate, select ...


#### DATASET -------------------------------------------------------------------

## A real dataset to use : 
dta <- structure(list(
  POP = c(
    "etude1", "etude1", "etude1", "etude1", "etude1", "etude1", "etude1", 
    "etude2", "etude2", "etude2", "etude2", "etude2", "etude2", "etude2",
    "etude3", "etude3", "etude3", "etude3", "etude3", "etude3", "etude3"),
  Gene_Name = c(
    "geneAB", "geneGA4", "geneGA6", "geneGC", "geneH1", "geneH4", "geneK11", 
    "geneGC", "geneAB", "geneH4", "geneGA6", "geneH1", "geneK11", "geneGA4",
    "geneAB", "geneGC", "geneH4", "geneK11", "geneH1", "geneGA4", "geneGA6"),
  Estimate_beta = c(
    0.207925600198783, 1.88567818556954, -4.75992047369255, 
    1.95771721535179, 2.00145743780637, 0.124528656711712, 0.848809580481303, 
    1.59789998110591, 1.64855083051555, -12.8890923907403, 
    0.795758203217314, -0.181625934119027, 0.341094587458617, -10.473897226316,
    1.33532111383067, 1.60732774438519, 0.952408769615033,
    1.81088440774369, -1.25500902905987, 0.222207122098226, 0.143517675541615
  ),
  CI_2.5 = c(
    -0.534048454761081, -0.601036782760681, -9.48657936302997,
    0.802468153792509, 0.954530055954353, -1.26225416123246, -0.292510493937119,
    0.477848745000833, 0.355359502910546, NA,
    -0.599772170653116, -1.25016750631846, -1.45734454639282, NA, 
    0.758935673070754,  0.777851529290706, 0.156776271206201, 
    0.287223470589621, -3.07086716199774, -2.71831815943812, -0.91354322082316),
  CI_97.5 = c(
    0.909675613406493, 5.03863412188904, -0.990170289493387,
    3.30670113474363, 3.27976643072443, 1.40632055899676, 2.00519731543251,
    2.82409933081721, 3.05865416604063, 11.4381014246579, 
    2.08243933847273, 0.76076406306055, 2.142059012374, 29.6700378153818, 
    1.86550165793939, 2.34236439934898, 1.63511122911422,
    3.05014771461879, -0.0806635423557972, 1.97194826680101, 0.964391305436382
  ),
  SE = c(
    0.365645379566775, 1.29917083078154, 2.81727715232674,
    0.624226995071529, 0.578077881973835, 0.659777899226375, 0.575625391530438,
    0.585692882919847, 0.67184114163164, 234.18057580129, 
    0.663807316790707, 0.504827709911626, 0.882461108612321, 196.967684763947,
    0.280702039244763, 0.394530623372692, 0.372889953402636, 
    0.679451979926418, 0.72442343829915, 1.08044361712239, 0.469066126903179
  ),
  P_val = c(
    0.569591170272858, 0.146655305926441, 0.0911147675402399, 
    0.00171137003357648, 0.000535654215356047, 0.850294014467458, 0.140323676892844, 
    0.00636768127788481, 0.014136315991808, 0.956107299195403, 
    0.230613930200858, 0.719013111613838, 0.699106794118713, 0.957591905837756, 
    1.96416613280035e-06, 4.62074569510503e-05, 0.0106454033346911,
    0.00769395203405579, 0.0831979804065563, 0.837054273395551, 0.759631525034932
  ),
  OR = c(
    1.2311215709413, 6.59082272754561, 0.00856629061587152, 
    7.08313931392432, 7.39983304009577, 1.13261447700174, 2.33686334774416,
    4.94264187671269, 5.19943949583682, 2.52544722845588e-06, 
    2.2161206297635, 0.833913220563743, 1.40648627053251, 2.826469026029e-05, 
    3.80121637361168, 4.98946028395243, 2.59194554556803,
    6.11585394968765, 0.285073274269172, 1.24883001137128, 1.1543272139855
  ),
  nb_rare_var = c(
    33, 3, 2, 14, 19, 6, 13,
    10, 7, 2, 5, 19, 4, 1, 
    17, 15, 4, 4, 36, 7, 5
  )
), row.names = c(NA, -21L), class = c("tbl_df", "tbl", "data.frame"))

Common_gene <- unique(dta$Gene_Name)



#### Application 1 =============================================================

#### META R pkg ----------------------------------------------------------------

## do the meta analysis for each gene ... Go 

all_res <- tibble(
  Gene_Name = Common_gene
) %>% 
  mutate(
    meta_object = map(.x = Gene_Name, .f = function(igene) {
      meta::metagen(
        TE = dta$Estimate_beta[dta$Gene_Name %in% igene], 
        seTE = dta$SE[dta$Gene_Name %in% igene], 
        studlab = dta$POP[dta$Gene_Name %in% igene], 
        data = dta[dta$Gene_Name %in% igene, ], 
        sm = "MD"
      )
    }), 
    base = map(.x = meta_object, .f = ~summary(.x)), 
    tmp = map2(.x = base, .y = meta_object, .f = function(base, meta_object){
      tmp <- unlist(base$fixed)
      # NB : utiliser la fonction random quand la pval d'hétérogénéité est inférieure à 0.05. ##
      if (meta_object$pval.Q < 0.05) {
        tmp <- unlist(base$random)
      }
      return(tmp)
    }), 
    my_meta_res = map2(.x = tmp, .y = base, .f = function(tmp, base){
      tmp <- unlist(tmp)
      base <- unlist(base)
      
      beta <- tmp['TE']
      cil <- tmp['lower']
      ciu <- tmp['upper']
      p.value <- tmp['p']
      pv.het <- pchisq(q = base$Q, df = (base$k - 1), lower.tail = FALSE)
      
      # res <- c("Beta" = beta, "cil" = cil, "ciu" = ciu, "p.value" = p.value, "pv.het" = pv.het)
      
      res <- c(beta, cil, ciu, p.value, pv.het)
      names(res)	<- c("BETA", "L95", "U95", "p-value", "Heterogeneity")
      
      return(res)
    })
    
  )

## save it 
all_res
saveRDS(object = all_res, file = file.path(output_directory, "all_metaanalysis_res.RDS"))

## extract meta results ...
all_meta_res <- do.call(
  what = "rbind", all_res$my_meta_res %>% `names<-`(all_res$Gene_Name)
) %>% 
  as.data.frame() %>% 
  rownames_to_column("Gene_Name")
all_meta_res
## to write in xlsx ...
writexl::write_xlsx(
  x = all_meta_res, path = file.path(output_directory, "all_metaanalysis_res.xlsx"),
  col_names = TRUE
)


#### FOREST PLOT ---------------------------------------------------------------

# ?meta::forest ## to see all option and help
meta::forest(x = all_res$meta_object[[1]]) ## test ## ok ! Seems beautiful


## write them in power point ... for instance
# for (i in 1:length(Common_gene)) {
#   
#   ppt <- read_pptx()
#   
#   ppt <- ppt %>%
#     add_slide(layout = "Blank", master = "Office Theme") %>%
#     ph_with_vg_at(
#       code = print(meta::forest(x = all_res$meta_object[[i]])), 
#       width = 9.5, 
#       height = 8, 
#       left = 0.25, 
#       top = 0.25
#     )
#   
#   print(x = ppt, target = paste0(output_directory, "/forestplot_", all_res$Gene_Name[[i]], ".pptx"))
#   
# }


#### Application 2 =============================================================

#### METAFOR R pkg -------------------------------------------------------------

## check here : https://wviechtb.github.io/metafor/

# renv::install("metafor")
library(metafor) # help(metafor)

## Notes : 
# calculate log risk ratios and corresponding sampling variances for the BCG vaccine dataset
# dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg,
#               slab=paste(author, year, sep=", ")) # also add study labels
# dat

# escalc return
# yi	: observed outcomes or effect size estimates.
# vi	: corresponding (estimated) sampling variances.

# res <- rma(yi, vi, data=dat, test="knha")
# res

res <- rma(yi = Estimate_beta, sei = SE, data = dta, test = "knha")
res <- rma(yi = Estimate_beta, sei = SE, data = dta, test = "z")
res

all_res2 <- tibble(
  Gene_Name = Common_gene
) %>% 
  mutate(
    metafor_object = map(.x = Gene_Name, .f = function(igene) {
      # rma(yi = Estimate_beta, sei = SE, data = dta[dta$Gene_Name %in% igene, ], test = "knha")
      rma(yi = Estimate_beta, sei = SE, data = dta[dta$Gene_Name %in% igene, ], test = "z")
    }), 
    base = map(.x = metafor_object, .f = ~summary(.x)), 
    my_meta_res = map(.x = base, .f = function(base){
      
      ## read doc 
      ## --here to do :
      ## NB : utiliser la fonction random quand la pval d'hétérogénéité est inférieure à 0.05. ## ? where here 
      
      beta <- base$beta[[1]] # oui 
      se <- base$se[[1]] # oui 
      cil <- base$beta[[1]] - 1.96 * base$se[[1]] # oui 
      ciu <- base$beta[[1]] + 1.96 * base$se[[1]] # oui 
      p.value <- base$pval[[1]] ##  oui (for test = z)
      # pv.het <- pchisq(q = base$QE, df = (base$k- 1), lower.tail = FALSE)
      pv.het <- base$QEp # oui 
      
      # res <- c("Beta" = beta, "cil" = cil, "ciu" = ciu, "p.value" = p.value, "pv.het" = pv.het)
      
      res <- c(beta, se, cil, ciu, p.value, pv.het)
      names(res)	<- c("BETA", "se", "L95", "U95", "p-value", "Heterogeneity")
      
      return(res)
    })
    
  )

## extract meta results ...
all_meta_res2 <- do.call(
  what = "rbind", all_res2$my_meta_res %>% `names<-`(all_res2$Gene_Name)
) %>% 
  as.data.frame() %>% 
  rownames_to_column("Gene_Name")
all_meta_res2

# show forest plot 
forest(
  x = all_res2$metafor_object[[1]], annotate = TRUE, header = "Studies"
)
## same ! 


#### Application 3 =============================================================

## Real analyses ##
## Do a meta analyses for gene present in replication... ##

options(stringsAsFactors = FALSE)

## load meta & grid
library(grid)
library(meta)

#### DATASET -------------------------------------------------------------------

dta <- data.frame(
  POP = c("etude1", "etudeT", "etude50"),
  Gene_Name = c("geneG3", "geneG3", "geneG3"),
  OR = c(3.90789189, 1.7147, 3.3249),
  # CI_2.5 = c(0.3364722, -0.037772, -0.864078), # IC(beta)
  # CI_97.5 = c(2.4849066, 1.0990283, 3.2519225), # IC(beta)
  SE = c(0.533131226, 0.2877754,  1.0543615), # se(beta) ? see se_from_beta
  P_val = c(0.003, 0.1342847, 0.4351049), 
  se_from_beta = c(TRUE, FALSE, FALSE)
)
dta$beta <- log(dta$OR)

dta 

# dta$upperci_beta <- dta$beta + dta$SE * 1.96 # well 
# dta

##  CI and SE are from BETA ??
## othewise : /!\ 

# get SE of beta ! 
# https://www.researchgate.net/post/How_can_I_calculate_beta_coefficient_and_its_error_from_Odds_Ratio_from_GWAS_summary_Statisitcs
# Upper bound = OR + se(OR) x 1.96
# Lower bound = OR - se(OR) x 1.96
# Log(upper bound of OR) = upper bound of beta
# Log(lower bound of OR) = lower bound of beta
# If you have the 95% C.I of beta, then calculating the SE(beta) :
# for example if beta = 0.5 and the upper C.I is 0.6 then
# upper C.I up of beta = beta + se(beta) x 1.96
# 0.6 = 0.5 + se x 1.96
# Assuming the confidence interval being used is 95%.

# # Log(upper bound of OR) = upper bound of beta
dta$upper_bound_or <- ifelse(!dta$se_from_beta, dta$OR + dta$SE * 1.96, NA)
# dta$upper_bound_beta <- log(dta$`CI_97.5`)
dta$upper_bound_beta <- log(dta$upper_bound_or)
# upper C.I of beta = beta + se(beta) x 1.96
# se(beta) = ( upper C.I of beta - beta ) / 1.96
dta$SE_beta <- ifelse(
  test = !dta$se_from_beta,
  yes = (dta$upper_bound_beta - dta$beta) / 1.96, 
  no = dta$SE # here se provided was ever about beta.
)
dta


#### META ----------------------------------------------------------------------

# NB : Input must be log OR or log hazard ratios, not direct OR or hazard ratios

summary_mesure_nature <- "OR" # "OR"
# or "MD" = Mean difference (to conclude at beta scale) or "OR" to get the summary measure at OR scale.

meta_analyses <- meta::metagen(
  TE = beta, # beta = log(OR)
  seTE = SE_beta, # se(beta)
  studlab = POP, 
  data = dta, 
  sm = summary_mesure_nature
)
meta_analyses
base <- summary(meta_analyses)

tmp <- unlist(base$fixed)
# NB : utiliser la fonction random quand la pval d'hétérogénéité est inférieure à 0.05. ##
if (meta_analyses$pval.Q < 0.05) {
  tmp		<- unlist(base$random)
}
my_meta_res <- local({
  tmp <- unlist(tmp)
  base <- unlist(base)
  
  Estimate <- tmp['TE']
  cil <- tmp['lower']
  ciu <- tmp['upper']
  p.value <- formatC(tmp['p'])
  pv.het <- pchisq(q = base$Q, df = (base$k-1), lower.tail = FALSE)
  res <- c(
    round(Estimate, digits = 3), 
    round(cil, digits = 3), round(ciu, digits = 3), 
    p.value, 
    round(base$Q, digits = 3), round(pv.het, digits = 3), 
    base$I2,
    paste0("summary measure nature sm = ", summary_mesure_nature)
  )
  names(res)	<- c("Estimate", "L95", "U95", "p-value", "Q.Heterogeneity", "pv.Heterogeneity", "I2", "note")
  return(res)
})
my_meta_res


#### FOREST PLOT ---------------------------------------------------------------
# meta::forest(x = meta_analyses)



#### Application 3 =============================================================

## Real analyses ##
## geneP1

#### DATASET -------------------------------------------------------------------

dta <- data.frame(
  POP = c("etudeT", "etude50"),
  OR = c(3.3983, 1.2293),
  SE = c( 0.6161141, 0.1087916),
  P_val = c(0.047091, 0.0577634), 
  N = c(44083, 43125),
  se_from_beta = c(FALSE, FALSE)
)
dta$beta <- log(dta$OR)

dta 

# dta$upperci_beta <- dta$beta + dta$SE * 1.96 # well 
# dta

##  CI and SE are from BETA ??
## othewise : /!\ 

# # Log(upper bound of OR) = upper bound of beta
dta$upper_bound_or <- ifelse(!dta$se_from_beta, dta$OR + dta$SE * 1.96, NA)
# dta$upper_bound_beta <- log(dta$`CI_97.5`)
dta$upper_bound_beta <- log(dta$upper_bound_or)
# upper C.I of beta = beta + se(beta) x 1.96
# se(beta) = ( upper C.I of beta - beta ) / 1.96
dta$SE_beta <- ifelse(
  test = !dta$se_from_beta,
  yes = (dta$upper_bound_beta - dta$beta) / 1.96, 
  no = dta$SE # here se provided was ever about beta.
)
dta


#### META ----------------------------------------------------------------------

# NB : Input must be log OR or log hazard ratios, not direct OR or hazard ratios

summary_mesure_nature <- "OR" # "OR"
# or "MD" = Mean difference (to conclude at beta scale) or "OR" to get the summary measure at OR scale.

meta_analyses <- meta::metagen(
  TE = beta, # beta = log(OR)
  seTE = SE_beta, # se(beta)
  studlab = POP, 
  data = dta, 
  sm = summary_mesure_nature
)
meta_analyses
base <- summary(meta_analyses)

tmp <- unlist(base$fixed)
# NB : utiliser la fonction random quand la pval d'hétérogénéité est inférieure à 0.05. ##
if (meta_analyses$pval.Q < 0.05) {
  tmp		<- unlist(base$random)
}
my_meta_res <- local({
  tmp <- unlist(tmp)
  base <- unlist(base)
  
  Estimate <- tmp['TE']
  cil <- tmp['lower']
  ciu <- tmp['upper']
  p.value <- formatC(tmp['p'])
  pv.het <- pchisq(q = base$Q, df = (base$k-1), lower.tail = FALSE)
  res <- c(
    round(Estimate, digits = 3), 
    round(cil, digits = 3), round(ciu, digits = 3), 
    round(exp(Estimate), digits = 3),
    p.value, 
    round(base$Q, digits = 3), round(pv.het, digits = 3), 
    base$I2,
    paste0("summary measure nature sm = ", summary_mesure_nature)
  )
  names(res)	<- c("Estimate", "L95", "U95", "OR", "p-value", "Q.Heterogeneity", "pv.Heterogeneity", "I2", "note")
  return(res)
})
my_meta_res
