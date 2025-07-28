---
title: "British Columbia Socio-Economic Index (BC-SEI)"
format: html
---


# British Columbia Socio-Economic Index (BC-SEI)

## Table of Contents

-   [Introduction](#introduction)
-   [Methodology](#methodology)
-   [Index Construction](#index-construction)
-   [Variables](#variables)
-   [Geographic Areas](#geographic-areas)
-   [Interpretation](#interpretation)
-   [Limitations](#limitations)
-   [Use Cases](#use-cases)
-   [References](#references)

## Introduction

The British Columbia Socio-Economic Index (BC-SEI) is a measure of socio-economic advantage and disadvantage across different geographic areas in British Columbia. Following a similar approach to the Australian Bureau of Statistics' Socio-Economic Indexes for Areas (SEIFA), this index summarizes various social and economic conditions of people and households within defined areas.

The BC-SEI provides a method for determining the level of social and economic wellbeing in each area by incorporating multiple dimensions including income, education, employment, housing, and other factors that influence socio-economic conditions.

## Methodology

The BC-SEI are constructed using principal components analysis (PCA), a statistical technique that summarizes the information from a set of correlated variables into a smaller number of components. The first principal component, which explains the largest proportion of the total variation in the data, forms the basis of the index.

Data sources include: 
- Census data 
- Tax data 
- Administrative data from provincial and federal agencies

## Index Construction

The index is constructed through the following steps:

1.  Selection of variables based on their relevance to socio-economic advantage and disadvantage
2.  Standardization of variables (converting to z-scores)
  2.1 impute missing values using KNN
3.  Application of principal components analysis in base year. 
  3.1 removal of co-linear and low impact variables
4.  Extraction of the first principal component as the primary index
5.  Transformation of index scores to a scale with a mean of 1000 and standard deviation of 100
6. Apply model to other years data.

## Variables

The BC-SEI incorporates variables across four main domains, with the following hierarchical structure:

### Updated model


The updated model is only available for 2023 due to many variables are only available for 2023.


The BC-Total Index has been created using four sub-indices: ECON, EDUC, HEALTH, and COMMUNITY. 


```mermaid
graph TD
    A[BC-Total Index - Detailed Model] --> B[Economic]
    A --> C[Education]
    A --> D[Health]
    A --> E[Community]

    %%  Link each category to the FIRST node INSIDE its subgraph
    B --- B1
    C --- C1
    D --- D1
    E --- E1

    subgraph EconBox ["Economic Indicators"]
        direction TB
        B1[Median after-tax income of household from Census] --- B2[Employment Rate from Labor Force Survey]
        B2 --- B3[Median peronal income from tax File]
        B3 --- B4[Employment rate from Census]
        B4 --- B5[Median house value]
        B5 --- B6[Population growth rate]
        B6 --- B7[Ratio of natural and applied sciences occupations in population from Census]
        B7 --- B8[Ratio of health occupations in population from Census]
        B8 --- B9[Ratio of management occupations in population from Census]
        B9 --- B10[Unemployment Rate from Labor Force Survey]
        B10 --- B11[Unemployment rate from Census]
        B11 --- B12[Percentage of dwellings in need of major repairs]
        B12 --- B13["Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%) from Census"]
    end

    subgraph EduBox ["Education Indicators"]
        direction TB
        C1[Proportion of population with postsecondary education] --- C2[Proportion of grade 10-12 students with an A in Science course]
        C2 --- C3[Proportion of grade 10-12 students with an A in English language art course]
        C3 --- C4[Proportion of grade 10-12 students with an A in Social studies course]
        C4 --- C5[Proportion of grade 10-12 students with an A in Math course]
        C5 --- C6[Average FSA score percentage in Grade 7 Numeracy]
        C6 --- C7[Average FSA score percentage in Grade 4 Numeracy]
        C7 --- C8[Average FSA score percentage in Grade 7 Reading]
        C8 --- C9[Proportion of positive responses in student learning survey questions: do you have breakfast ]
        C9 --- C10[Average FSA score percentage in Grade 4 Reading]
        C10 --- C11[High school completion rate by Grade 12]
        C11 --- C12[Proportion of positive responses in student learning survey questions: do you feel welcome at school]
        C12 --- C13[Proportion of positive responses in student learning survey questions: do you like school]
        C13 --- C14[Proportion of population with only a high school diploma]
        C14 --- C15[Proportion of population without a high school diploma]
    end

    subgraph HealthBox ["Health Indicators"]
        direction TB
        D1[Percentage of the population receiving paid healthcare pharmaceutical benefits] --- D2[Prevalence rate of asthma in the population]
        D2 --- D3[Prevalence rate of diabetes in the population]
        D3 --- D4[Prevalence rate of mood and anxiety disorders in the population]
        D4 --- D5[Prevalence rate of osteoarthritis]
        D5 --- D6["Prevalence rate of hypertension (high blood pressure)"]
    end

    subgraph CommunityBox ["Community Indicators"]
        direction TB
        E1[Proportion of single-parent families in the community] --- E2[Percentage of parents who were lone parents at the time of childbirth.]
        E2 --- E3["Children and youth in care per thousand young (0-18) population"]
        E3 --- E4[Proportion of population receiving disability assistance]
        E4 --- E5[Proportion of population receiving income assistance]
        E5 --- E6[All reported crimes excluding traffic, per 100,000 people]
        E6 --- E7[Number of homicides per 100,000 population]
    end

    classDef indexStyle fill:#7C4DFF,stroke:#333,stroke-width:2px,color:#fff
    classDef domainStyle fill:#E8EAF6,stroke:#333,stroke-width:2px
    classDef indicatorStyle fill:#F5F5F5,stroke:#666,stroke-width:1px
    classDef boxStyle fill:#F8F9FA,stroke:#333,stroke-width:2px

    class A indexStyle
    class B,C,D,E domainStyle
    class EconBox,EduBox,HealthBox,CommunityBox boxStyle
    class B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,D1,D2,D3,D4,D5,D6,E1,E2,E3,E4,E5,E6,E7 indicatorStyle
```

The BC-SEI Index has been created using three sub-indices: ECON, EDUC, and COMMUNITY. 



```mermaid
graph TD
    A[BC-SEI Index - Detailed Model] --> B[Economic]
    A --> C[Education]
    A --> E[Community]

    %%  Link each category to the FIRST node INSIDE its subgraph
    B --- B1
    C --- C1
    E --- E1

    subgraph EconBox ["Economic Indicators"]
        direction TB
        B1[Median after-tax income of household from Census] --- B2[Employment Rate from Labor Force Survey]
        B2 --- B3[Median peronal income from tax File]
        B3 --- B4[Employment rate from Census]
        B4 --- B5[Median house value]
        B5 --- B6[Population growth rate]
        B6 --- B7[Ratio of natural and applied sciences occupations in population from Census]
        B7 --- B8[Ratio of health occupations in population from Census]
        B8 --- B9[Ratio of management occupations in population from Census]
        B9 --- B10[Unemployment Rate from Labor Force Survey]
        B10 --- B11[Unemployment rate from Census]
        B11 --- B12[Percentage of dwellings in need of major repairs]
        B12 --- B13["Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%) from Census"]
    end

    subgraph EduBox ["Education Indicators"]
        direction TB
        C1[Proportion of population with postsecondary education] --- C2[Proportion of grade 10-12 students with an A in Science course]
        C2 --- C3[Proportion of grade 10-12 students with an A in English language art course]
        C3 --- C4[Proportion of grade 10-12 students with an A in Social studies course]
        C4 --- C5[Proportion of grade 10-12 students with an A in Math course]
        C5 --- C6[Average FSA score percentage in Grade 7 Numeracy]
        C6 --- C7[Average FSA score percentage in Grade 4 Numeracy]
        C7 --- C8[Average FSA score percentage in Grade 7 Reading]
        C8 --- C9[Proportion of positive responses in student learning survey questions: do you have breakfast ]
        C9 --- C10[Average FSA score percentage in Grade 4 Reading]
        C10 --- C11[High school completion rate by Grade 12]
        C11 --- C12[Proportion of positive responses in student learning survey questions: do you feel welcome at school]
        C12 --- C13[Proportion of positive responses in student learning survey questions: do you like school]
        C13 --- C14[Proportion of population with only a high school diploma]
        C14 --- C15[Proportion of population without a high school diploma]
    end


    subgraph CommunityBox ["Community Indicators"]
        direction TB
        E1[Proportion of single-parent families in the community] --- E2[Percentage of parents who were lone parents at the time of childbirth.]
        E2 --- E3["Children and youth in care per thousand young (0-18) population"]
        E3 --- E4[Proportion of population receiving disability assistance]
        E4 --- E5[Proportion of population receiving income assistance]
        E5 --- E6[All reported crimes excluding traffic, per 100,000 people]
        E6 --- E7[Number of homicides per 100,000 population]
    end

    classDef indexStyle fill:#7C4DFF,stroke:#333,stroke-width:2px,color:#fff
    classDef domainStyle fill:#E8EAF6,stroke:#333,stroke-width:2px
    classDef indicatorStyle fill:#F5F5F5,stroke:#666,stroke-width:1px
    classDef boxStyle fill:#F8F9FA,stroke:#333,stroke-width:2px

    class A indexStyle
    class B,C,D,E domainStyle
    class EconBox,EduBox,HealthBox,CommunityBox boxStyle
    class B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,D1,D2,D3,D4,D5,D6,E1,E2,E3,E4,E5,E6,E7 indicatorStyle
```

### Longitudinal model

The longitudinal model is the version 1 of the BC-SEI index which has been created using data from 2003 to 2023. 

A few variables are available for creating the BC-Total index.


```mermaid
graph TD
    A[BC-Total Index - Longitudinal Model] --> B[Economic]
    A --> C[Education]
    A --> D[Health]
    A --> E[Community]

    %%  Link each category to the FIRST node INSIDE its subgraph
    B --- B1
    C --- C1
    D --- D1
    E --- E1

    subgraph EconBox ["Economic Indicators"]
        direction TB
        B1[Median after-tax income of household from Census] ---  B4[Employment rate from Census]
        B4 --- B5[Ratio of home ownership to rental occupancy in the community]
        B5 --- B11[Unemployment rate from Census]
        B11 --- B12[Percentage of dwellings in need of major repairs]
    end

    subgraph EduBox ["Education Indicators"]
        direction TB
        C1[Proportion of population with postsecondary education] --- C2[Proportion of grade 10-12 students with an A in Science course]
        C2 --- C3[Proportion of grade 10-12 students with an A in English language art course]
        C3 --- C4[Proportion of grade 10-12 students with an A in Social studies course]
        C4 --- C5[Proportion of grade 10-12 students with an A in Math course]
        C5 --- C6[Average FSA score percentage in Grade 7 Numeracy]
        C6 --- C7[Average FSA score percentage in Grade 4 Numeracy]
        C7 --- C8[Average FSA score percentage in Grade 7 Reading]
        C8 ---  C10[Average FSA score percentage in Grade 4 Reading]
        C10 --- C11[High school completion rate by Grade 12]
        C11 --- C14[Proportion of population with only a high school diploma]
        C14 --- C15[Proportion of population without a high school diploma]
    end

    subgraph HealthBox ["Health Indicators"]
        direction TB
        D1[Percentage of the population receiving paid healthcare pharmaceutical benefits] --- D2[Prevalence rate of asthma in the population]
        D2 --- D3[Prevalence rate of diabetes in the population]
        D3 --- D4[Prevalence rate of mood and anxiety disorders in the population]
        D4 --- D5[Prevalence rate of osteoarthritis]
        D5 --- D6["Prevalence rate of hypertension (high blood pressure)"]
    end

    subgraph CommunityBox ["Community Indicators"]
        direction TB
        E1[Proportion of single-parent families in the community] --- E2[Percentage of parents who were lone parents at the time of childbirth.]
        E2 --- E3["Children and youth in care per thousand young (0-18) population"]
        E3 --- E4[Proportion of population receiving disability assistance]
        E4 --- E5[Proportion of population receiving income assistance]
        E5 --- E6[All reported crimes excluding traffic, per 100,000 people]
        E6 --- E7[Number of homicides per 100,000 population]
    end

    classDef indexStyle fill:#7C4DFF,stroke:#333,stroke-width:2px,color:#fff
    classDef domainStyle fill:#E8EAF6,stroke:#333,stroke-width:2px
    classDef indicatorStyle fill:#F5F5F5,stroke:#666,stroke-width:1px
    classDef boxStyle fill:#F8F9FA,stroke:#333,stroke-width:2px

    class A indexStyle
    class B,C,D,E domainStyle
    class EconBox,EduBox,HealthBox,CommunityBox boxStyle
    class B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,D1,D2,D3,D4,D5,D6,E1,E2,E3,E4,E5,E6,E7 indicatorStyle
```

```mermaid
graph TD
    A[BC-SEI Index - Longitudinal Model] --> B[Economic]
    A --> C[Education]
    A --> E[Community]

    %%  Link each category to the FIRST node INSIDE its subgraph
    B --- B1
    C --- C1
    E --- E1

    subgraph EconBox ["Economic Indicators"]
        direction TB
        B1[Median after-tax income of household from Census] ---  B4[Employment rate from Census]
        B4 --- B5[Ratio of home ownership to rental occupancy in the community]
        B5 --- B11[Unemployment rate from Census]
        B11 --- B12[Percentage of dwellings in need of major repairs]
    end

    subgraph EduBox ["Education Indicators"]
        direction TB
        C1[Proportion of population with postsecondary education] --- C2[Proportion of grade 10-12 students with an A in Science course]
        C2 --- C3[Proportion of grade 10-12 students with an A in English language art course]
        C3 --- C4[Proportion of grade 10-12 students with an A in Social studies course]
        C4 --- C5[Proportion of grade 10-12 students with an A in Math course]
        C5 --- C6[Average FSA score percentage in Grade 7 Numeracy]
        C6 --- C7[Average FSA score percentage in Grade 4 Numeracy]
        C7 --- C8[Average FSA score percentage in Grade 7 Reading]
        C8 ---  C10[Average FSA score percentage in Grade 4 Reading]
        C10 --- C11[High school completion rate by Grade 12]
        C11 --- C14[Proportion of population with only a high school diploma]
        C14 --- C15[Proportion of population without a high school diploma]
    end

    subgraph CommunityBox ["Community Indicators"]
        direction TB
        E1[Proportion of single-parent families in the community] --- E2[Percentage of parents who were lone parents at the time of childbirth.]
        E2 --- E3["Children and youth in care per thousand young (0-18) population"]
        E3 --- E4[Proportion of population receiving disability assistance]
        E4 --- E5[Proportion of population receiving income assistance]
        E5 --- E6[All reported crimes excluding traffic, per 100,000 people]
        E6 --- E7[Number of homicides per 100,000 population]
    end

    classDef indexStyle fill:#7C4DFF,stroke:#333,stroke-width:2px,color:#fff
    classDef domainStyle fill:#E8EAF6,stroke:#333,stroke-width:2px
    classDef indicatorStyle fill:#F5F5F5,stroke:#666,stroke-width:1px
    classDef boxStyle fill:#F8F9FA,stroke:#333,stroke-width:2px

    class A indexStyle
    class B,C,D,E domainStyle
    class EconBox,EduBox,HealthBox,CommunityBox boxStyle
    class B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,D1,D2,D3,D4,D5,D6,E1,E2,E3,E4,E5,E6,E7 indicatorStyle
```

## Geographic Areas 

The BC-SEI is calculated for various geographic levels:

-   Community Health Service Delivery Areas
-   Census Subdivisions (municipalities)
-   Dissemination Areas (small areas composed of one or more neighboring blocks)

## Interpretation 

The BC-SEI is designed so that higher scores indicate relative advantage, while lower scores indicate relative disadvantage. The scores are relative within British Columbia, with:

-   A score of 50 representing the provincial average
-   Scores above 50 indicating relative advantage
-   Scores below 50 indicating relative disadvantage

The index represents a continuum from most disadvantaged to most advantaged rather than specific categories or thresholds of disadvantage.

## Limitations 

Key limitations of the BC-SEI include:

-   The index measures relative, not absolute, disadvantage
-   Area-based measures may not reflect the diversity within areas
-   The index represents an average for an area and doesn't identify pockets of advantage or disadvantage
-   Rural and remote areas may have different patterns of advantage/disadvantage that are not well-captured
-   The index should be used alongside other measures for comprehensive analysis

## Use Cases 

The BC-SEI can be used for:

-   Identifying areas of relative socio-economic disadvantage for resource allocation
-   Planning service delivery and community development initiatives
-   Research on health, education, and social outcomes
-   Policy development and evaluation
-   Comparative analysis across different geographic areas

## References 

-   Statistics Canada (2021). Census of Population.
-   BC Stats (2023). British Columbia Socio-Economic Index Technical Documentation.
-   Australian Bureau of Statistics (2018). Socio-Economic Indexes for Areas (SEIFA).
