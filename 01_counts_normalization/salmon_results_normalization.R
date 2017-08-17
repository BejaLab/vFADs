library(readr)
library(tximport)
library(edgeR)

# salmon counts import
metadata <- read_csv('../data/TARA_metadata.csv')
files <- file.path('../data/salmon_output/', metadata$dataset, 'quant.sf')
all(file.exists(files))
names(files) <- metadata$dataset

txi <- tximport(files, type = "salmon", txOut = TRUE, importer=read.delim)
SalmonCounts <- txi$counts
SalmonCounts[SalmonCounts<20] <- 0 # replace with 0 read counts < 20

# gene metadata
genes_list <- read_tsv('../data/gene_info.tsv')

#edgeR
cts <- SalmonCounts[,colSums(SalmonCounts) > 0]
normMat <- txi$length
normMat <- normMat[, colSums(SalmonCounts) > 0]
normMat <- normMat/exp(rowMeans(log(normMat)))
o <- log(edgeR::calcNormFactors(cts/normMat)) + log(colSums(cts/normMat))
y <- edgeR::DGEList(cts)
y$offset <- t(t(log(normMat)) + o)

write.table(edgeR::rpkm(y, gene.length=genes_list$len), file = '../data/salmon_tximport_edger_rpkm.tsv', sep = '\t')
