# AIC & BIC
'getAIC' <- function(asreml)
{
  vc     <- data.table(summary(asreml)$varcomp)
  loglik <- summary(asreml)$loglik
  n.par  <- dim(vc[constraint!="Fixed" & constraint!="Constrained"])[1]
  out <- data.frame(loglik   = loglik,
                    m2loglik = -2 * loglik,
                    n.par    = n.par,
                    AIC      = -2 * loglik + 2 * n.par)
  out
}

# variance components with confidence interval
'getVC' <- function(asreml)
{  
  if(!require(data.table)){cat("please install package 'data.table'")
  }else{if(!require(nadiv))     {cat("please install package 'nadiv'")
  }else{
    VC1 <- data.table(summary(asreml)$varcomp,  keep.rownames="CovParm")
    VC2 <- data.table(nadiv::aiCI(asreml),      keep.rownames="CovParm")
    VC  <- VC2[VC1, .(CovParm, LCL, estimate, UCL, std.error, constraint), on="CovParm"]
  }
  }
  VC
}

# quick plot vc
'plotVC' <- function(VC)
{  
  if(!require(ggplot2)){cat("please install package 'ggplot2'")
  }else{
    ggplot(data=VC, aes(factor(CovParm,levels=unique(CovParm)), y=estimate)) +
      geom_bar(stat="identity") + 
      ylab("VC estimate with \nconfidence interval") +
      geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.2) +
      theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
            axis.title.x=element_blank(),
            axis.ticks.x=element_blank())
  }
}