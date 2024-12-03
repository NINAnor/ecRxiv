---
name: PR review checklist
about: A checklist for processing new PRs with indicator documentation
title: '[New PR]: "indicatorID"'
labels: PR-checklist
assignees: anders-kolstad

---

## PR review checklist

This is a checklist for ecRxiv administrators that will have first contact with new PRs.
The list explains how to ensure the PR si following the standards for ecRxiv submissions,
and how to do a code review.

Before you start:

Please add the indicator ID to the title of this issue (replacing the placeholder text that reads "indicatorID").
If there is no indicator ID, figure out what it should be and assign it. The indicator ID should be repeated in the folder name (under `indicators/`), in the `.qmd` file and in `metadata.xlsx`.
Also, under **labels** in the right column, locate and choose the correct indicator ID from the list.  

- [ ] The indicator has an ID that follows the naming convention

### Conflict of interest

- [ ] I confirm that I have conflict of interest that makes me unsuited to review this indicator. If not, I will convert this checkbox into an issue and declare my conflict there.

### Code of Conduct

- [ ] I confirm that I read and will adhere to the [code of conduct](https://github.com/NINAnor/ecRxiv/blob/main/docs/code_of_conduct.md).

### General checks
Open the PR and check the following: 

- [ ] **Modified files:** the number of new files is correct; there are no unreasonably big files or too many files (e.g. html_files, old drafts or cashed files folders). Are any non-relevant files modified? For help with removing files from PR, see below.
- [ ] **File structure**: The folders and files are names correctly and have the right structure (they are not moved around compared to `indicators/template/`)
- [ ] **Meta data:** the metadata file (metadata.xlsx) filled out
- [ ] **Document structure:** look at the quarto file and check that the document structure is reasonably according to the template, including headers and the yaml header.
- [ ] **References:** is there a bibliography file? If not, should there be? (note: hyperlinks break over time)
- [ ] **HTML included:** the HTML is included and looks good. To view it, 
open the html from the PR and view it online by pasting this string to the start of the URL: `http://htmlpreview.github.io/?`

### Too many modified files

It’s common that a PR contain more files than what we actually want to update in the main.
This can be draft documents, old rendered pdf or html outputs, or large data files that should
be handled outside of GitHub.

First, create a backup branch of the original PR branch.

Then, in the PR branch, revert the files you don't want to to include in the PR. 
- https://stackoverflow.com/questions/38743912/remove-a-file-from-a-git-pull-request
- https://stackoverflow.com/questions/2733873/how-can-i-revert-a-single-file-to-a-previous-version?noredirect=1&lq=1

If the author is not comfortable with git, you can ask to be given access to commit to his or her branch. 
- https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/allowing-changes-to-a-pull-request-branch-created-from-a-fork

Alternatively, create a brand new PR from you locally checkout version of the PR (se directly below)


### Test PR locally

- [ ] General checks passed

When these boxes are ticked, you can move to the next part, which is to checkout the PR.
We need to be able to render the HTML's in order to update them. We start by fetching the PR branch and try to render it locally. 

in RStudio, go to th main branch and pull the latest changes. 
Then put this into the terminal:

`git fetch origin pull/ID/head:BRANCH_NAME` 

, where `ID` is the ID for the PR which you can see in the PR's title following a hash tag. It's usually a three or four  digit number.
`BRANCH_NAME` you can make up yourself. A suggestion is to use the indicator ID followed by `code-review`.

The switch to that branch. 

`git switch BRANCH_NAME`

Try to render the quarto file. 

- [ ] The quarto file renders to HTML without errors, and with the correct formatting



### If there are no errors 


If there are no errors when compiling the HTML, then you can go on to test the new contribution on the shiny app,
to make sure the app won't crash when we merge the new indicator to the main branch.

In your locally checkout version, start by merging the main branch to get the latest changes. 

`git merge origin/main`

It could be that rebase would be better for this, but this also works.

Now you should be able to run the shiny app locally and test to see if the indicators shows up with no errors.

- [ ] The shiny app works locally when including the new indicator(s)

### If there are errors

If there are things in the code that cause errors, or just something you want to see fixed,
then there are a few different ways to proceed. You can

1. Add a [review](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/reviewing-changes-in-pull-requests/about-pull-request-reviews) inside the PR
1. Open an issue using the *Community review* template.
1. Fork the PR branch (the and suggest changes directly that the contributor then can look at and chose wheter to merge to their PR branch)
1. Make edits in you locally checkout version and start a new PR. 

The last option is ususlly reserved for when we are unable to reach the original authors of the PR and need to make relativly minor edits.
What we **don't do** is to email or call the person and ask then to fix something. We want to keep the digital trail.

Option 1 is the preferred option. 

- [ ] All of the reviewers suggestions (your suggestions) and have been adressed and the PR has been updated if needed.

### When the file renders and the app don't break

Add as many coomments and suggestions as you feel relevent in the PR.
You can also use the *community review* issue template for this.
When alle your comments are resolved by the author, then we can merge the PR to main. 

Immediately afterwards we create a stable copy of the HTML with the version number 
included in the name (INDICATORID_XXX_version_VVV_vvv; where XXX is the indicator ID number, and VVV and vv is the version number, before and after the perions, respectively).

- [ ] PR merged
- [ ] Stable HTML version is added

### Update the internal and external Shiny app

When new indicators are merged to the main branch,
we need to update the shiny app. First we update the internal version, and check it.

- [ ] Open the github repo on NINAs RShiny Server and pull the lastes changes on the main branch.
- [ ] Test the app on the server (from your personal folder)
- [ ] If OK, move the (modified part of) the app to `/data/shiny/shiny-sites/ecRiv`. 

In the file explorer tab on the RShiny server you can only copy one file at the time, and you cant copy folders. Therefore, click the little box
next to the folder you want to move, and under *more*, select copy. Let the new folder get the suggested name starting with 'CopyOf...'.
Then check the little box next to that folder (uncheck the other one) and select *more - move*. Move this to the correct folder 
under `/data/shiny/shiny-sites/ecRiv` and remove the 'CopyOf'-part.

- [ ] Test the app on `https://internal-shiny.nina.no/ecRxiv/`
- [ ] Contact *Datahjelp* and ask them to update the external app
- [ ] Test the external app on `https://view.nina.no/ecRxiv/`
