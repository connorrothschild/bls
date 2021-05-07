# Protecting Low Wage Workers During COVID19

## Wage and Hour Division / Rice University Social Policy Analysis Capstone

Prepared by Connor Rothschild

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

* `shiny/` includes files necessary to create a [Shiny web application](https://connorrothschild.shinyapps.io/covid-industry-exploration-tool/) we used for exploratory purposes and briefly presented to the Houston WHD team. You should be able to run the Shiny app locally if you would like to.
* `archived/` has archived files and data that we have since retired. 
* `data/` contains our data, unsurprisingly.
* `utils/` includes functions that we use throughout, for visualization purposes. For example, our ggplots utilize a custom theme defined in `utils/theme.R`.

### Workflow

You can get started by running `init.R` which lives at the project root. This script will install all necessary packages for our analysis.

Then, the recommended folder to tackle is `analysis/`. Each script in this file will source the necessary preparation script that lives in `prep/`. That is to say, you will never need to run these scripts on your own, but can instead rely on the `source()` functions.

`presentation/` has some scripts we used to generate one-off tables and visualizations for our report; these scripts are in a separate folder because they are 1) not essential to our analysis and 2) mostly present existing information in new ways. Regardless, you're welcome to run those scripts as well to see the code that generated some of the tables and figures in our report.

### Scripts overview

There are three scripts in `analysis/`.

* `impact.R`: This script analyzes COVID-19's impact on Houston, on an industry-by-industry basis. The script that prepares the data for this analysis lives in `prep/create_qcew.R`. We visualize, for each industry, shifts in employment between Q1 and Q2, and shifts in average weekly wage.
* `enforcement.R`: This script analyzes existing WHD enforcement priorities by exploring where investigations have occurred most frequently. The script that prepares the data for this analysis lives in `prep/create_enforcement.R`. We analyzed enforcement priorities in a variety of ways, such as sum of backwages paid and number of investigations. All analyses filter for our time period of analysis, 2015-2021.
* `impact_vs_enforcement.R` combines the above two analyses into one. In particular, it shows investigations per 1,000 establishments to understand enforcement priorities standardized to industry size. It also highlights the aforementioned *suspect industries* in this view. In sum, this script analyzes enforcement priorities of suspect industries compared to non-suspect ones.

### Other project intricacies

#### File referencing

This project makes use of the `here` package to improve file referencing in a consistent way. As such, it requires analyses be done in a directory with an `.Rproj` file. (One already lives in this repository.) If you delete the existing `.Rproj` file, the file referencing using `here::here()` will no longer work. Alternatively, you can create an empty file called `.here` in your project root.

#### Data 

This repository includes data from 1) the [QCEW](https://www.bls.gov/cew/), and 2) enforcement data from the [WHD data catalog](https://enforcedata.dol.gov/views/data_catalogs.php). There is one extraneous file, `industry-titles.csv`, which includes a lookup table for NAICS codes' human readable name.

By default, we have put the `whd_whisard.csv` dataset retrieved from the WHD Data Catalog in `.gitignore`. This is because it is nearly 230mb unzipped and cannot be version controlled. Like we wrote above, running the scripts in `prep/` will download any data files you don't already have and rewrite them to the `data/` folder. (The `whd_houston.csv` file is a CSV that filters down `whd_whisard.csv` to only include our area of analysis.)

## Questions/Contact

Please direct any questions or concerns to [connor@connorrothschild.com](mailto:connor@connorrothschild.com).
