# Problem 1, part c, using built in esoph dataset in R
esoph[1:5, c(1, 4, 5)]

# 1, d
is.na(esoph$ncontrols)

# 1, e
mean(esoph$ncontrols)

# 1, f
ggplot(data=esoph) + geom_point(mapping=aes(x = ncontrols, y=ncases))

# 1, g
ggplot(data=esoph) + geom_point(mapping=aes(x = ncontrols, y=ncases, color=tobgp))

# 1, h
ggplot(data=esoph) + geom_point(mapping=aes(x =ncontrols, y=ncases, color=tobgp)) 
    + labs(x="# of Controls", y="# of Cases", title="Cesar Gutierrez, Konnor Gutierez, Rhys Julian-Jones") 
    + scale_x_continuous(limits = c(0,80), breaks = c(seq(0,80,by=5))) + scale_y_continuous(limits = c(0,20), breaks = c(0:20))

