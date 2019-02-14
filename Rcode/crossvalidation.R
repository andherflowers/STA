do_10cv_knn = function(X,Y,krange){
  n = length(Y) # smaple size

  # permute index set
  permidx = sample(1:n,n)
  X = as.matrix(X)[permidx,]
  Y = Y[permidx]

  # size of fold
  foldsize = floor(n/10)

  # for saving errors
  valset_error = array(0,length(krange))

  for(k in 1:length(krange)){
    K = krange[k]
    # the inner loop will be over the folds
    for(j in 1:10){
     # for fold 1-9 do same as last time
     if(j < 10){
       testidx = foldsize*(j-1) + (1:foldsize)
     # take care with last fold, it might be larger
     # reason: n/10 might not be an integer
     }else{
       testidx = (foldsize*(j-1)):n
     }

     fit = knnreg(as.matrix(X)[-testidx,,drop=FALSE],Y[-testidx],k=K)
     pr = predict(fit, newdata = as.matrix(X)[testidx,,drop=FALSE])
     valset_error[k] = valset_error[k] + mean((Y[testidx] - pr)^2)
    } # end loop over folds
  } # end loop over k

  # the next line will output the result
  return(valset_error)
}
