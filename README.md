[![web application](https://github.com/NINAnor/ecRxiv/blob/main/docs/_ecrxiv_logo_hovedlogo_tekst_under.png?raw=true)](https://ecRxiv.com)

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](docs/code_of_conduct.md)

## To browse ecosystem condition indicators, please visit the [ecRxiv web app](https://ecRxiv.com). 

## Browse source files, please switch to the [main branch](https://github.com/NINAnor/ecRxiv/tree/main/indicators).


## NEWS

- 15.01.2026 We now have a new submission workflow whic is hopefully easier to use and requires less git knowledge
- 19.12.2025 ecRxiv was used to document ecosystem condition indicators for the Norwegian pilot condition account. The report can be found [here](https://www.miljodirektoratet.no/publikasjoner/2025/desember-2025/nasjonal-pilot-for-okologisk-tilstandsregnskap/)
- 19.12.2025 ecRxiv maintainer and inventor [Anders Kolstad](https://github.com/anders-kolstad) recently won an [award for good open science pratices](https://norrn.substack.com/p/norrn-december-newsletter) based on his work with ecRxiv.

## How to use ecRxiv
To browse ecosystem condition indicators, please visit the [ecRxiv web app](https://ecRxiv.com). 
Or, to browse source files (raw data files or scripts), please switch to the [main branch](https://github.com/NINAnor/ecRxiv/tree/main/indicators).
You can also look up the histories for published indicators, such as code reviews and comments, by going to the [issues tab](https://github.com/NINAnor/ecRxiv/issues) and in the _Labels_ filter, search and select for indicatorID's. 

### How to submit indicator documentation to ecRxiv

On this site (this GitHub repository) you can submit technical documentation and ecological justification for your ecosystem condition indicator(s) and have it published on an [online web application](https://ecRxiv.com) in a standardised format along side other indicators from anywhere in the world.

The documentation must be submitted in the form of a quarto file (.qmd) following our template. The indicator metadata is embedded in the same .qmd file, in the top YAML. The .qmd wil render to HTML.

[![Tutorial video](https://img.youtube.com/vi/wLYnORKZ1ZQ/0.jpg)](https://www.youtube.com/watch?v=wLYnORKZ1ZQ)

#### Submission workflow
WROK  IN PROGRESS
(to update an existing indicator, jump to point ?

1.  **Fork the template (default) branch of this repository (repo)**. The main branch contains a lot of files, so we keep the template files in a clean branch of the repo. That way your fork will look a lot cleaner. Eventually your files will go into the main branch. Note that personal users can only fork a repo once (at a time). Organisational users can fork as many times they want, so you can for example have one fork per indicator. An alternative is to have one fork on your personal user, where you ruitinely synchronise the main branch, and use other branches on that fork for individual indicators.  
2.  **Add your work to this fork**. Although it is possible to edit the files in your fork directly through GitHub in the web browser, most users will prefer to make a local copy (called a _clone_)of the repo and edit files in a separate software, such as RStudio or Positron. Now you can either start working on the indicator here, and let this fork be the _home_ for this indicator. Alternatively you can keep you work in a seperate repo and just copy the work over to this fork, altough this requires a bit more manual work.
Smaller data files can be stored in the `data/` folder. Please also adhere to the [recommended terminology](https://github.com/NINAnor/ecRxiv/wiki#recomended-terminology). The metadata is added to the top yaml of the quarto file. Not all the terms are that well defined yet, such as the open scienjce badges, but try and fill it out as best you can.
6.  Render the quarto file to html
7.  Do a pull request (PR) from your forked repo to the `submission` branch in this repo.
8.  Administrators of this repo will check that the submission (the PR) is done properly and that all files have been named in the correct way. If you have submitted data and code, this review will also include checking that the code is able to locate the data and run, and that proper code annotation is provided. Revise the PR until it reached the requirements set by the administrators.
9.  The PR will be merged with the `main` branch and the indicator documentation (the rendered html) will be published on an online web application.
10.  The publicly available documentation is now subject to voluntary review, for which there are separate guidelines.
11.  The PR authors can revise their documentation as many time as needed, and in response to review by peers. Major revisions may result in the creation of a new version number for the original indicator.

### What if I have questions or suggestions?
There is a [Q&A](https://github.com/NINAnor/ecRxiv/discussions/categories/q-a) where you can ask questions if you get stuck. If you already know there is a bug, or you have suggestsion for improving ecRxiv, you may raise an [issue](https://github.com/NINAnor/ecRxiv/issues) using the _FEEDBACK_ form. We are grateful for any contributions, feedback and questions. 

### How to review an indicator
Indicator reviews are handled through GitHub Issues. Got to the [issues tab](https://github.com/NINAnor/ecRxiv/issues) and select *New Issue*. 
If you are adding a complete review, similar to peer reviewing a scientific article, use the *INDICATOR REVIEW FORM*.
If you just want to add a comment and a more simple review, use the *FEEDBACK* form.

### How to review a PR and update the shiny app (for admins)
This is for ecRxiv administrators who first come incontact with a new PR. 
Open a new issue on GitHub and chose the *PR review - checklist* template. Follow the instructions there.


## Rationale

Ecosystem condition indicators are used in ecosystem condition assessments and accounts to monitor the quality and integrity of nature. These assessments and accounts can be important policy instruments that guide nature governance. When presented in reports aimed for managers and policy makers, indicators are often described just enough for them to be interpretable. However, indicator calculations/designs are often quite technical and include many steps that cannot be described fully in a report that may include several indicators and indices. There is a need to document these workflows in order to validate and review their soundness and relevance for describing ecosystem condition. This documentation will also be a benefit to the ecosystem condition indicator community of practice because it makes it easier to learn from each other and develop the field quicker.
