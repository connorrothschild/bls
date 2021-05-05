# Protecting Low Wage Workers During COVID19

## Wage and Hour Division / Rice University Social Policy Analysis Capstone

Prepared by [Connor Rothschild](www.connorrothschild.com)

This repository includes the code necessary to replicate the quantitative analyses done for Rice University's SOPA 400/401 research capstone.

### Background

This research took place during the fall of 2020 and the spring of 2021. It occurred against the backdrop of the COVID-19 pandemic, and the project's focus was to analyze COVID-19's impact on 1) the Houston economy, 2) low-wage workers, and 3) the landscape of WHD investigations of labor rights violations. Toward that end, the team surveyed existing literature, interviewed stakeholders in the labor space, and conducted quantitative analyses. The quantitative analysis included an investigation of existing [WHD enforcement priorities](https://enforcedata.dol.gov/views/data_catalogs.php) and also a survey of COVID-19's impact, via the [Quarterly Census of Employment and Wages (QCEW)](https://www.bls.gov/cew/). This repository includes the code necessary to replicate the quantitative analyses done for Rice University's SOPA 400/401 research capstone.

### Repository structure

The repository is structured around three primary folders:

* `prep/` contains all scripts for data retrieval, wrangling, and preparation. It outputs data to the `data/` folder. 
* `analysis/` includes all scripts necessary for analysis and visualization of prepared data.
* `presentation/` visualizes and creates tables that were used in our final report.

Each file in these three folders should be self-documenting and its purpose should be clear. 

Some other folders that are less important include:

* `shiny/` includes files necessary to create a Shiny Web application we used for exploratory purposes and briefly presented to the Houston WHD team. You should be able to run the Shiny app if you desire.
* `archived/` has archived files and data that we have since retired. 
* `data/` contains our data, unsurprisingly.
* `utils/` includes functions that we use throughout, for visualization purposes. For example, our ggplots utilize a custom theme defined in `utils/theme.R`.

### Workflow

You can get started by running `init.R` which lives at the project root. This script will install all necessary packages for our analysis.

Then, the recommended folder to tackle is `analysis/`. Each script in this file will source the necessary preparation script that lives in `prep/`. That is to say, you will never need to run these scripts on your own, but can instead rely on the `source()` functions.

### Other project intricacies

#### File referencing

This project makes use of the `here` package to improve file referencing in a consistent way. As such, it requires analyses be done in a directory with an `.Rproj` file. (One already lives in this repository.) If you delete the existing `.Rproj` file, the file referencing using `here::here()` will no longer work. Alternatively, you can create an empty file called `.here` in your project root.

#### Data 

By default, we have `.gitignore`d the `whd_whisard.csv` dataset retrieved from the WHD Data Catalog. This is because it is nearly 230mb unzipped and cannot be version controlled. Like we wrote above, running the scripts in `prep/` will download any data files you don't already have and rewrite them to the `data/` folder. The `whd_houston.csv` file is a CSV that filters down `whd_whisard.csv` to only include our area of analysis.