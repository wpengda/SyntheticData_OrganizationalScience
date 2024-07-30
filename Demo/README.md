# Synthetic Data Generation Demo

## Introduction
Here is a demo for the synthetic data generation by SDV package (https://github.com/sdv-dev/SDV) and Boston Housing Data from Harrison & Rubinfeld (1978) (https://doi.org/10.1016/0095-0696(78)90006-2).

## Prerequisites
To embark on generating synthetic data, ensure you have the following prerequisites in place:
- A virtual environment setup is highly recommended for an isolated development environment. We suggest using Conda for this purpose, which can be installed following the instructions at [Conda's official documentation](https://docs.conda.io/projects/conda/en/latest/user-guide/install/download.html).

## Installation
Begin by cloning this repository to your local environment. Once cloned, all necessary dependencies can be installed through Conda. These dependencies are meticulously listed in either `conda.yml` for Conda.

```bash
conda env create -n vsynth -f conda.yml
conda activate vsynth
```

## Generating Synthetic Data
The notebook to initiate the synthetic data generation process is `demo.ipynb`. This notebook serves as the cornerstone of our process, preparing and sharing the initial dataset across other components of the project.
