## clean up from previous interactive sessions
rm(list=ls())

## vital
library(compositions)

transform_variable = function(dfr, col.names, name.prefix) {
  
  ## ensure that all components are present in the dataset
  i.col = which(names(dfr) %in% col.names)
  if (length(i.col) != length(col.names)) {
    msg = paste('cannot find column combination ', paste(col.names, collapse=' '), collapse=' ')
    stop(msg)
  }
  
  ## Close the composition.
  ### This is in case some parts are missing. 
  ### It may seem counter-intuitive, but it is explained in the 
  ### "Analyzing Compositional Data with R" book.
  varClosed = clo(dfr[, i.col])
  
  ## Transform the variable by projecting it into a Euclidean space.
  ### A D-dimensional comp. variable is projected onto a (D-1)-dimensional variable.
  varIlr = ilr(varClosed)
  
  ## Generate names for the new variables.
  names(varIlr) = paste(name.prefix, seq_len(ncol(varIlr)), sep='')
  
  ## Remove the un-transformed variable components from the dataset...
  dfr = dfr[, -i.col]
  
  ## ... and append the transformed components.
  cbind(dfr, varIlr)
}

###########################################################
## create dummy dataset
### what: for the purposes of this example, we are working with a fictitious dataset
### why: reproducibility as there is reduced dependency on external datasets
numRows = 70

set.seed(0)

## first variable: compositional
a1 = runif(n=numRows)
a2 = runif(n=numRows)
a3 = runif(n=numRows)

## second variable: compositional
b1 = rnorm(n=numRows)
b2 = rnorm(n=numRows)
b3 = rnorm(n=numRows)
b4 = rnorm(n=numRows)

## third variable: continuous & non-compositional
c1 = rpois(n=numRows, lambda=1)

## fourth variable: categorical & non-compositional
d1 = sample(c(0, 1), size=numRows, replace=T)

## dependent variable: categorical, but could work with continuous as well
y = sample(c(0, 1), size=numRows, replace=T)

## construct dataset
dfr = data.frame(
  a1=a1, a2=a2, a3=a3,
  b1=b1, b2=b2, b3=b3, b4=b4,
  c1=c1,
  d1=as.factor(d1),
  y=as.factor(y)
)

cat('schema before transformation:\n')
print(names(dfr))
cat('\n')

dfr = transform_variable(dfr=dfr, col.names=c('a1', 'a2', 'a3'), name.prefix='a_ilr_')
dfr = transform_variable(dfr=dfr, col.names=c('b1', 'b2', 'b3', 'b4'), name.prefix='b_ilr_')

cat('schema after transformation:\n')
print(names(dfr))
cat('\n')

###########################################################
## now simply fit model as if nothing happened
model = glm(formula = y ~ ., data=dfr, family = 'binomial')

## the usual...
plot(model$y, model$fitted.values)
lines(x=c(-0.1, 1.1), y=c(0.5, 0.5), lwd=2, col='red')

num.correct = sum(ifelse(model$fitted.values > 0.5, 1, 0) == model$y)
num.total = length(model$y)
cat('accuracy =', num.correct / num.total, '\n')

## get p-values for the inclusion of individual coefficients
print(summary(model))

## Chi-sq test
model_2 = glm(formula = y ~ a_ilr_1, data=dfr, family='binomial')
print(anova(model, model_2, test='Chisq'))