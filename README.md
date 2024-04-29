# ECindicators

### - a publishing platform for Ecosystem condition indicators.

## Rationale

Ecosystem condition indicators are used in ecosystem condition assessments and accounts to monitor the quality and integrity of nature. These assessments and accounts can be important policy instruments that guide nature governance. When presented in reports aimed for managers and policy makers, indicators are often described just enough for them to be interpretable. However, indicator calculations/designs are often quite technical and includes many steps that cannot be described fully in a rapport that may include several indicators and indices. There is a need to document these workflows in order to validate and review their soundness and relevance for describing ecosystem condition. This documentation will also be a benefit to the ecosystem condition indicator community of practice because it makes it easier to learn from each other and develop the field quicker.

## How to use ECindicators

On this site (this GitHub repository) you can submit technical documentation and ecological justification for your ecosystem condition indicator(s) and have it published on an online web application in a standardised format along side other indicators from anywhere in the world.

As a minimum requirement, the documentation must be renderable as an html, and you must submit a separate metadata file (excel template is provided). However, we encourage, and facilitate, the publication of fully reproducible workflows in the form of quarto files and associated data files.

### Workflow

The submission and publication of indicator documentation is described in the numbered workflow below.

1.  Fork the main branch of this repository (repo)
2.  Fill inn the appropriate template files and rename the files according to our [naming convention](#naming-convention)
3.  Do a pull request (PR) from your forked repo to the `published` branch in this repo.
4.  Administrators of this repo will check that the submission (the PR) is done properly and that all files have been named in the correct way. If you have submitted data and code, this review will also include checking that the code is able to locate the data and run, and that proper code annotation is provided. Revise the PR until it reached the requirements set by the administrators.
5.  The PR will be merged with the `published` branch and the indicator documentation (the rendered html) will be published on an online web application.
6.  The publicly available documentation is now subject to voluntary review, for which there are separate guidelines.
7.  The PR authors can revise their documentation as many time as needed, and in response to review by peers. Major revisions may result in the creation of a new version number for the original indicator.

### Naming convention {#naming-convention}

All indicators are given a unique code:

`CO_NAME_XXX_YYY`

where

-   `CO` = two letter couny code (ISO 3166-1 alpha-2) indicator the country that the indicator applies to, or if it applies to multiple countries, the country of the main affiliation for the main author.

-   `NAME` = four letter code for the indicator name (usually the first four letters of the full name, e.g. ALIE for the the indicator Alien Species)

-   `XXX` = three digit major version number. Major versions include substantial updates to the methodology or data origins behind the indicator. If the same indicator is used more-or-less in the same way for two ecosystem types, version number can also refer to ecosystem type.

-   `YYY` = three digit minor version number. Minor version changes include updates bigger than just an increase in the data (e.g. from a yearly update), but smaller than a major update.
