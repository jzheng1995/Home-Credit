# Predicting Home Credit

## Project description

Welcome to the Home Credit prediction project! This repository contains the code, data, and documentation for a machine learning analysis aimed at predicting whether a customer will continue with their account or close it (i.e., churn). Accurately predicting home credit is crucial for insurance companies to decide whether potential customers are a return on investment.

## Objective

The main objective of this project is to develop a predictive model that can classify customers into two categories: those who will likely default and those who won't. 

![Bank Churn](assets/bank.jpg)

## Dataset

The dataset used in this project belongs to the [Home Credit - Credit Risk Model Stability](https://www.kaggle.com/competitions/home-credit-credit-risk-model-stability/overview) competition. The data includes various features that might influence a customer's creditworthiness, such as:

- Customer demographics (age, gender, etc.)
- Account details (Outstanding credit, active interest rate, etc.)
- External credit details (Number of transactions, credit score, etc)

## Methods

The project has largely been a data extraction and feature selection exercise. 
1. [Data extraction and brief EDA](Extract_EDA.md)
- ~ 1.5 million user rows
- Extensive SQL joins across ~30 tables
2. [Random forest]
- Random forest modeling in R
- h2o cross-validated random grid search
- tidymodels + ranger framework for random forests

