# Personal website

This repository contains the code for my website.

## Requirements

### Quarto

### R

#### Packages

Make sure all of these packages are installed:

```{r}
install.packages(c("bookdown", "mice", "tufte"))
```

## Rendering the website

When working on the website, you can get a preview by using:

```
quarto render
```

When the changes is ready, publish it by executing this in the Terminal:

```
. publish.sh
```