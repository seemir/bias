# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("tikzDevice")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("_helper_func.R")

# ----------------------------------------------------------- #
# Load imputed data
# ----------------------------------------------------------- #
all_data_files <- c("HFpEF", "HFmrEF")
lapply(gsub(" ", "", paste("data_files/", all_data_files, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Principal component analysis
# ----------------------------------------------------------- #
HFpEF_pca <- princomp(as.matrix(HFpEF), cor = T)
HFmrEF_pca <- princomp(as.matrix(HFmrEF), cor = T)

# ----------------------------------------------------------- #
# Explained variance plot
# ----------------------------------------------------------- #
tikz(file="../../../doc/thesis/images/pca_var_plot_HFpEF.tex",
    width = 10)
pca_var_plot(HFpEF_pca, 59, title = "HFpEF")
dev.off()
tikz(file="../../../doc/thesis/images/pca_var_plot_HFmrEF.tex",
    width = 10)
pca_var_plot(HFmrEF_pca,51,title = "HFmrEF")
dev.off()

# ----------------------------------------------------------- #