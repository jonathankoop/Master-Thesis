generate_rem <-function (formula, param1,covar,M) 
{
  j=1
  t <- 0
  dummy <- data.frame(time = 1, actor1 = 1, actor2 = 2)
  rehOut <-remify::remify(edgelist = dummy,model = "tie",actors = covar$name,
                          directed = TRUE,origin = 0)
  
  out <- remstats(reh = rehOut, tie_effects = formula, attr_actors  = covar)
  riskset<-attributes(out)$riskset
  adj <- matrix(0, 1, ncol = nrow(riskset)) # what is this for?
  
  
    param=param1
    
    for (i in 1:M) {
      
      beta <- lapply(param, function(x) {
        if (class(x) == "function") {
          x(t)
        }
        else {
          x
        }
      })
      logLambda <- out[dim(out)[1],,] %*% unlist(beta)
      
      lambda <- exp(logLambda)
      dt <- rexp(1, sum(lambda))
      d <- sample(1:nrow(lambda), 1, prob = lambda/sum(lambda))
      if (i+((j-1)*M) == 1) {
        edgelist <- cbind(time = (t + dt), actor1 = riskset[d, 
                                                            1], actor2 = riskset[d, 2])
      }
      else {
        edgelist <- rbind(edgelist, cbind(time = (t + dt), 
                                          actor1 = riskset[d, 1], actor2 = riskset[d, 
                                                                                   2]))
      }
      edgelist <- as.data.frame(edgelist)
      edgelist$time <- as.numeric(edgelist$time)
      t <- max(edgelist$time)
      adj <- rbind(adj, adj[i+((j-1)*M), ])
      adj[i + 1+((j-1)*M), d] <- adj[i + 1+((j-1)*M), d] + 1
      cat(i+((j-1)*M), "\r")
      if (i < M) {
        dummy$time <- dummy$time + t
        edgelistTemp <- rbind(edgelist, dummy)
        rehOut <-remify::remify(edgelist = edgelistTemp, model = "tie",actors = covar$name,
                                directed = TRUE,origin = 0)
        
        
        
        if ( i+((j-1)*M)<= M ){
          out <- remstats(reh = rehOut, tie_effects = formula, attr_actors  = covar)}
        else{
          stop<-  i+((j-1)*M)
          start<- i+((j-1)*M)-M+1
          
          out <- remstats(reh = rehOut, tie_effects = formula, attr_actors = covar,memory = "full",start =start, stop =stop , memory_value=400 )
        }
      }
    }
  edgelist
}
