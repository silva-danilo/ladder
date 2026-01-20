
# packs
library(stringi)
library(rvest)

# 01/2026
url <- "http://plsql1.cnpq.br/divulg/RESULTADO_PQ_102003.prc_comp_cmt_links"
url <- paste0(url, "?V_COD_DEMANDA=200310&V_TPO_RESULT=CURSO")
url <- paste0(url, "&V_COD_AREA_CONHEC=10200002&V_COD_CMT_ASSESSOR=MA")

# read
page <- read_html(url)

# tab
tab <- html_elements(page, "table")
tab <- lapply(tab, function(x) html_table(x, fill=T))
tab <- tab[[6]]
tab <- data.frame(tab)

# prep
names(tab) <- tab[2,]
tab <- tab[-c(1:2, nrow(tab)),]

# value (https://bit.ly/3Nlwuq1 based)
value <- function(nivel){
  # new
  if(nivel=="PQ-A") v <- 1500
  if(nivel=="PQ-B") v <- 1300
  if(nivel=="PQ-C") v <- 1100
  
  # old
  if(nivel=="PQ-SR") v <- 1500
  if(nivel=="PQ-1A") v <- 1500
  if(nivel=="PQ-1B") v <- 1400
  if(nivel=="PQ-1C") v <- 1300
  if(nivel=="PQ-1D") v <- 1200
  if(nivel=="PQ-2") v <- 1100
  v
}

# rank
tab$Valor <- sapply(tab$Nível, value) 
tab$Rank <- rank(paste0(tab$Valor, tab$Início))
tab$Pos <- as.integer(factor(-tab$Rank))
View(tab)
