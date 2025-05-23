# ODR (optimization-based dietary recommendation)
This is a R implementation of ODR, as described in our paper:

Wang, X.W., Weiss, S.T. Hu, F. B, and Liu, Y.Y. [Optimization-based dietary recommendations for healthy eating]. 

<p align="center">
  <img src="fig.png" alt="demo" width="700" height="250" style="display: block; margin: 0 auto;">
</p>

## Contents
- [Overview](#overview)
- [Environment](#environment)
- [Repo Contents](#repo-contents)
- [How the use the ODR framework](#How-the-use-the-ODR-framework)

# Overview

Various diet scores have been developed to assess compliance with dietary guidelines. Yet, enhancing those diet scores is very challenging. Here, we tackle this issue by formalizing an optimization problem and solving it with simulated annealing. Our optimization-based dietary recommendation (ODR) approach, evaluated using Diet-Microbiome Association study data, provides efficient and reasonable recommendations for different diet scores. ODR has the potential to enhance nutritional counseling and promote dietary adherence for healthy eating.

# Environment
We have tested this code for R 4.3.1.

# Repo Contents
(1) A small demo dataset to test the ODR framework.

(2) R code to optimize the diet to improve the healthy eating score (HEI2015 as an example).

# How to use the ODR framework
**Prepare the Input Data:** The key step in running the ODR is formatting the input data to be compatible with the R package dietaryindex (see GitHub: https://github.com/jamesjiadazhan/dietaryindex). The required input should include both:

(1) Food Pattern Equivalents for individual foods

(2) Total Nutrient Intakes (for HEI-2015 calculation) derived from ASA24

**Run the Optimization Script:** Execute the R script run_ODR.R to perform the optimization.

**Custom Diet Scores:** Users can optimize other diet scores by supplying their own dietary data and modifying the scoring function within the dietaryindex package accordingly.
