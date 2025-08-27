[![web application](https://github.com/NINAnor/ecRxiv/blob/main/docs/_ecrxiv_logo_hovedlogo_tekst_under.png?raw=true)](https://view.nina.no/ecRxiv/)

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](docs/code_of_conduct.md)

## NEWS

The [ecRxiv web app](https://view.nina.no/ecRxiv/) is now available.


## Rationale

Ecosystem condition indicators are used in ecosystem condition assessments and accounts to monitor the quality and integrity of nature. These assessments and accounts can be important policy instruments that guide nature governance. When presented in reports aimed for managers and policy makers, indicators are often described just enough for them to be interpretable. However, indicator calculations/designs are often quite technical and include many steps that cannot be described fully in a report that may include several indicators and indices. There is a need to document these workflows in order to validate and review their soundness and relevance for describing ecosystem condition. This documentation will also be a benefit to the ecosystem condition indicator community of practice because it makes it easier to learn from each other and develop the field quicker.

## How to use ecRxiv

On this site (this GitHub repository) you can submit technical documentation and ecological justification for your ecosystem condition indicator(s) and have it published on an online web application in a standardised format along side other indicators from anywhere in the world.

As a minimum requirement, the documentation must be renderable as an html, and you must submit a separate metadata file (excel template is provided). However, we encourage, and facilitate, the publication of fully reproducible workflows in the form of quarto files and associated data files.


### How to submit indicator documentation to ecRxiv

[![Tutorial video](https://img.youtube.com/vi/wLYnORKZ1ZQ/0.jpg)](https://www.youtube.com/watch?v=wLYnORKZ1ZQ)

The submission and publication of indicator documentation is also described in the numbered workflow below.

1.  **Fork the main branch of this repository (repo)**. The main branch contains a lot of data files, but creating a fork does not cause create copies of the files themselves, and does not lead to higher data storage requirements. Note that personal users can only fork a repo once (at a time). Organisational users can fork as many times they want, so you can for example have one fork per indicator. An alternative is to have one fork on your personal user, where you ruitinely synchronise the main branch, and use other branched for individual indicators.  
2.  **Make a partial clone of your forked repo**. Although it is possible to edit the files in your fork directly through GitHub in the web browser, most users will prefer to make a local copy of the repo and edit files in a separate software, such as RStudio or Positron. To avoid copying all the data files and git history for all the indicators already on ecRxiv, we highly recommend making a partial clone, and not a full (normal) clone. To do this you need to talk to git via the command line (don't be scared!). As a general solution, open the folder where you want to clone the repo into, right click, and open Git Bash. Alternatively, if working in RStudio, you can write directly in the terminal window:
  

```
git clone --filter=blob:none --no-checkout https://github.com/<USER>/<REPO>
```
This clones just the .git folder, but does not copy down any bigger data files from the web. Now you can choose which directories (folders) you want to clone. This is called sparse checkout. First, move working directory one folder up:

```
cd <REPO>
```

Then initiate sparse checkout based on the root folder:

```
git sparse-checkout init --cone
```

Checkout root folder (downloads blobs (files), some of which you'll need):

```
git checkout main
```

Now, specify additional folders to download. If you are starting to work on a new indicator, and not updating an existing indicator, this is the code you should use:

```
git sparse-checkout set indicators/template_copy_folder_then_rename
```

Leave the git bash window open. 

3.  Copy the `template_copy_folder_then_rename` folder and name the new folder, and files and folders inside it, according to our [naming convention](https://github.com/NINAnor/ecRxiv/wiki#naming-convention). Make sure not to simply rename the `template` folder - make a real copy.
4.  Update the sparse checkout. The sparse checkout is set up to read only the `template_copy_folder_then_rename` folder, but you now want to work in the new folder you just created. Bring up the git bash window again and type this (replacing NEW_FOLDER_NAME with be the indicator ID):
```
git sparse-checkout set indicators/<NEW_FOLDER_NAME>
``` 
5.  Document your indicator using the templates provided. Smaller data files can be stored in the `data/` folder. Fill in `metadata.xlsx` and `README.md`(don't rename these files). Keep your forked repo up to date with your work by routinely pushig your changes. Please also adhere to the [recommended terminology](https://github.com/NINAnor/ecRxiv/wiki#recomended-terminology).
6.  Render the quarto file to html
7.  Do a pull request (PR) from your forked repo to the `main` branch in this repo.
8.  Administrators of this repo will check that the submission (the PR) is done properly and that all files have been named in the correct way. If you have submitted data and code, this review will also include checking that the code is able to locate the data and run, and that proper code annotation is provided. Revise the PR until it reached the requirements set by the administrators.
9.  The PR will be merged with the `main` branch and the indicator documentation (the rendered html) will be published on an online web application.
10.  The publicly available documentation is now subject to voluntary review, for which there are separate guidelines.
11.  The PR authors can revise their documentation as many time as needed, and in response to review by peers. Major revisions may result in the creation of a new version number for the original indicator.

### What if I have questions or suggestions?
There is a [Q&A](https://github.com/NINAnor/ecRxiv/discussions/categories/q-a) where you can ask questions if you get stuck. If you already know there is a bug, or you have suggestsion for improving ecRxiv, you may raise an [issue](https://github.com/NINAnor/ecRxiv/issues) using the _feedback or comments_ form. We are grateful for any contributions, feedback and questions. 

### How to review an indicator
Indicator reviews are handles through GitHub Issues. Got to the [issues tab](https://github.com/NINAnor/ecRxiv/issues) and select *New Issue*. 
If you are adding a complete review, similar to peer reviewing a scientific article, use the *Indicator review - checklist* template.
If you just want to add a comment and a more simple review, use the *Feedback or comments* template.

### How to review a PR and update the shiny app (for admins)
This is for ecRxiv administrators who first come incontact with a new PR. 
Open a new issue on GitHub and chose the *PR review - checklist* template. Follow the instructions there.


