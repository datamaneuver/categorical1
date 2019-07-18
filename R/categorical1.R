categorical1 <- function(data)
{
  d = data
  categorical_columns = c()
  a = sapply(colnames(d), function(x) class(d[[x]]))
  a=as.matrix(a)

  for (i in 1:ncol(d))
  {
    if (is.factor(d[,i]))
    {
      values = c()
      values = append(values, c(names(d[i])))
      categorical_columns = append(categorical_columns, values, after = length(categorical_columns))
    }
  }

  for (i in 1:length(categorical_columns))
  {
    cat('\nFrequency of Categories for varible',categorical_columns[i])
    j = as.matrix(table(d[categorical_columns[i]]))
    colnames(j) = "value_count"
    print(j)
  }
}


