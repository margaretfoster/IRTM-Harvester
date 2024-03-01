# IRTM-Harvester
This project describes how to use the [IRT-M Package](https://github.com/dasiegel/IRT-M) as a tool to associate latent dimensions across multiple surveys.

IRT-M is a theoretically constrained Bayesian IRT model introduced by [Morucci et al](https://arxiv.org/abs/2111.11979). IRT-M solves the problem that IRT (and similar dimensionality-reduction models, such as PCA) produce underlying dimensions that best fit the data. But the conventional approaches produce dimensions that are not necessarily: (1) related to the question of interest, (2) meaningful, (3) comperable across different data. Morruci et al focuses on how IRT-M addresses the first two concerns. This repository, IRT-M Harvester, focuses on how IRTM can be used to directly link otherwise incompatible sources of data.


Here we focus on one use case: connecting surveys would otherwise represent single snapshots of a place and a time. By identifying and linking latent dimensions of interest, IRT-M permits 
