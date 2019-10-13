# Adv_lab6
[![Build Status](https://travis-ci.org/lmxblur/Adv_lab6.svg?branch=master)](https://travis-ci.org/lmxblur/Adv_lab6)
## The 6th assignment of the course Advanced Programming in R.
**Creating three different algorithms for knapsack problem. In order to study the effects of algorithms with different computional complexity and how to speedup R code.
A collaborative work of Group 19.**
  * Dimitriadis Spyridon - spydi472
  * Mengxin Liu - menli358
  
## Microbenckmark Example
Comparison between parallel and non-parallel brute_force_knapsack method using microbenchmark package with times=20.

| Method | Minimum Time | Maximum Time | Median | Mean |
| :---: | :---: | :---: | :---: | :---: |
| Parallel | 7.098s | 8.054s | 7.487s | 7.558s |
| Non-Parallel | 9.845s | 11.846s | 10.916s | 10.916s |
