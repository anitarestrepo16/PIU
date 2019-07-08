# README

[This repository](https://github.com/ChildMindInstitute/PIU-R-scripts) includes the code used in ["Problematic Internet Use in Children and Adolescents: Associations with psychiatric disorders and impairment"](https://docs.google.com/document/d/1d9AVxbdnDkwLLrm9n-tIduo00SbtOyJLe-GjTLP-Dqs).

## Contents

### Dx_of_interest
- 'Or' statements to get Dxes from across 10 columns

### All_Binary
- Add variable for 1 vs 2 caregivers
- Merge DFs
- Odds ratios
  - round to 2 digits
  - note: exponentiate confidence intervals?
  - export to CSV
- Logistic regressions
  - export to CSV
- Linear regressions: PIU predict negative outcomes
  - export to CSV
- note: no MRV participants included, so site is binary

### Demos_Breakdown
_note: Jon run this one to understand better_
- Breakdown by
  - age buckets
  - SES tertiles
  - race buckets
  - site (binary without MRV)
- Odds ratios by demo
  - export to CSV

### Distribution_Plots
- stacked bar plots with age
  - age in buckets
  - IAT score in buckets
  - export image

### Linear_Regressions
- linear regressions
- dimensional regressions

## Dependencies

- apaTables
- base (_installed and loaded with R_)
- broom
- car
- corrplot
- epiR
- lavaan
- paran
- psych
- UpSetR
- venn
- VennDiagram

```R
install.packages(c("apaTables", "broom", "car", "corrplot", "epiR", "lavaan", "paran", "psych", "UpSetR", "venn", "VennDiagram"), dependencies = TRUE)
```
