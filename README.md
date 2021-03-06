# bias 

[![made-with-latex](https://img.shields.io/badge/Made%20with-LaTeX-1f425f.svg)](https://www.latex-project.org/)
[![Project Status: Inactive – The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive) 
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.4-blue.svg)](https://cran.r-project.org/)
[![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)

My final master thesis submitted for the degree of Master of Science in Bioinformatics and Applied Statistics at the Norwegian University of Life Sciences (NMBU). All the processes mentioned in the ML procedure in this thesis are developed using the _R_ statistical programming language (version 3.4.4 - _Someone to Lean On_) with RStudio as the integrated development environment (IDE), version 1.1.423. 

## Abstract
In this thesis, we attempt to investigate how well various clustering algorithms (hierarchical clustering, k-means and expectation–maximization) perform in producing phenotypically distinct clinical patient groups (i.e. phenomapping) with heart failure with preserved ejection fraction (HFpEF) and mid-range ejection fraction (HFmrEF). Furthermore, we evaluate the performance of various classification algorithms (k-nearest neighbours, logistic regression, naive Bayes, linear discriminant analysis, support vector machines and random forest) in predicting patient mortality and readmission. All the algorithms were applied on a data set consisting of 375 patients with symptomatic heart failure (HF) identified at a tertiary hospital in the United Kingdom.

In the cluster analysis, we found that the hierarchical and k-means algorithms show signs of clustering more mutually exclusive patient groups with HF compared to the physicians. By examining the important attributes of the participants enrolled at the start of the study, i.e. the baseline characteristics. We found that the patient groups produced by these algorithms had 62 significantly different baseline characteristics compared to 59 produced by the physicians.

In the classification of mortality and readmission, we found that linear discriminant analysis (LDA) and logistic regression show promising potential. That is, the level of accuracy for which the algorithms predicted mortality and readmission rank high compared to the other algorithms evaluated. LDA predicted mortality with approximately 69.9\% accuracy and readmission with 99.7\%. Logistic regression had similar results with approximately 69.6\% accuracy for mortality and 98.7\% for readmission. Similar results are reported in the literature. Our findings lend support to the idea that the application of such algorithms may help in better understanding the complex nature of a clinical syndrome such as heart failure.

## ML procedure adopted in the thesis
<p align="center">
  <img src = 'https://i.imgur.com/0gwO56h.png' align="centre" height="65%" width="65%"></img>
</p>
