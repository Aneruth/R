alist <- c(1,2,3,4,5)
blist <- c(6,7,8,9,10)

clist <- alist + blist
clist
# Similar for other operations 

# If we have a vector smaller than second vector then "R" copies first vector upto required time to perform operation
smallList <- c(14,15,16)
bigList <- c(18,19,20,21)
summationList <- smallList + bigList
summationList # This considers smallList as 14,15,16,{14} : {14} newly copied value

