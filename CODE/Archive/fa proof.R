# fa proof
dat3 <- fread(paste0("D:/User/pschmidt/Desktop/GitHub/projectX/CODE/",datasets[dat.nr],"_means.txt"), stringsAsFactors = T)
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))

cols <- names(dat)
names(dat3)
i <- 1
cols[i]

df <- as.data.frame(dat)
df3 <- as.data.frame(dat3)

all.equal(df$adj.mean, df3$adjmean)
all.equal(df$r.i, df3$ri)
all.equal(df$t.j, df3$tj)
all.equal(df$Y,   df3$Y)
all.equal(df$L,   df3$L)
all.equal(df$G,   df3$G)
all.equal(df$GG,  df3$GG)
all.equal(df$w,   df3$w)