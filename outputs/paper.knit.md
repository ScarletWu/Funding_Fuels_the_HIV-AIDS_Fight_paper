---
title: "Tracing Hidden Struggles of Rural Indian Women during Pandemic"
author: Ruoxian Wu
thanks: "Code and data are available at: https://github.com/ScarletWu/Tracing_Hidden_Struggles_of_Rural_Indian_Women_during_Pandemic.git. Replication on Social Science Reproduction platform is available at: https://www.socialsciencereproduction.org/reproductions/1783/"
date: "April 16, 2024"
date-format: long
abstract: " "
format: pdf
toc: true
number-sections: true
bibliography: references.bib
---



# Introduction

Public health funding significantly impacts public health outcomes, particularly during global health emergencies like the COVID-19 pandemic. In the study "Women’s well-being during a pandemic and its containment" published in the Journal of Development Economics (2022), the authors explore the dual crisis of disease and the containment policies designed to mitigate its spread, focusing specifically on their effects on women in lower-income countries. This paper critically analyzes the methodologies and findings of the original study, aiming to understand the nuanced impacts of such policies on various aspects of women's well-being, including mental health and food security.

In my analysis, I replicate the original study’s research using publicly available data, extending the investigation to assess how containment measures affect the broader socio-economic outcomes beyond what was initially reported. Utilizing methodologies such as difference-in-differences and regression discontinuity designs, this paper evaluates the sensitivity of the findings to different analytical approaches. By doing so, it seeks to validate the original results while stimulating further discussion on interpreting data to assess public health policies' impacts.

This critical analysis endeavors to unravel the complex relationships between public health funding, policy effectiveness, and their socio-economic ramifications. Through a detailed reexamination of the original study’s data and methodology, this paper contributes to a more comprehensive understanding of how public health initiatives can be optimized to better serve vulnerable populations during crises. My analysis keeps a keen eye on the societal, economic, and systemic factors that shape public health outcomes and the operational dynamics of public funding within these contexts.

# Data
My reproduction used the programming language R [@r], the analysis used the following packages: Haven[@rHaven], Dplyr [@rDplyr], Ggplot2 [@rGgplot2], Readr [@rReadr], Here [@rHere], Janitor [@rJanitor], KableExtra [@rKableExtra], Knitr [@rKnitr], Tidyverse [@rTidyverse].




## Source
This critical analysis utilizes replication data associated with the article "Women’s well-being during a pandemic and its containment" from the Journal of Development Economics. This data, along with associated code, was made accessible by the authors to facilitate the replication of key findings such as statistical models and graphical representations. By enabling the reproduction of the authors' analyses, this data contributes to the transparency and credibility of the study's conclusions.
The replication package can be find and downloaded after requesting access from . 

## Variables

The data for this study includes both individual-level and regional-level variables from six states in rural India. Individual-level variables encompass demographic details (age, gender, household head status), economic factors (employment status, income levels), and health-related outcomes (mental health indicators, nutrition data). Regional-level variables cover containment measures, healthcare access, and socio-economic indicators such as the prevalence of COVID-19, public health infrastructure, and local economic conditions.

Regarding data collection, the authors conducted a large phone survey in August 2020, targeting households that were first interviewed in the fall of 2019, thereby providing a pre-pandemic baseline. This longitudinal approach allowed the researchers to examine changes over time attributed to the pandemic and containment policies. The survey data were supplemented with regional health statistics and COVID-19 case data obtained from official public health sources.

The data from the phone survey included detailed questions on mental health using validated psychological scales (PHQ9 and GAD7) and food security questions adapted from national health surveys. This allowed the researchers to construct indices of mental health and nutritional status, crucial for evaluating the impact of containment policies on women’s well-being.

Subsequent to data collection, the data were organized and analyzed using statistical software, with the authors employing advanced econometric techniques such as difference-in-differences and regression discontinuity designs to assess the impact of varying levels of containment. This rigorous analytical approach helps to isolate the effects of public health interventions from other confounding factors.

# Results





::: {#fig-1 .cell layout-ncol="2"}
::: {.cell-output-display}
![Distribution of monthly income in rupees for household heads](paper_files/figure-pdf/fig-1-1.pdf){#fig-1-1}
:::

::: {.cell-output-display}
![Share of households with lower income, fewer meals, and declining female well-being](paper_files/figure-pdf/fig-1-2.pdf){#fig-1-2}
:::

Impact of general economic disruptions on income and women's welfare.
:::

# Discussion
In progress

# Reference
