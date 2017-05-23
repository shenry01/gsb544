data = data.frame(read.csv("pddata.csv"))

#chose a == 0 and b ==1
#payoff from chosing respectivly == pa or pb
#epayoff = probabilty of getting $1 based on choices made


data$Ia[data$choice==0]=1
data$Ia[data$choice==1]=0
data$Ib[data$choice==1]=1
data$Ib[data$choice==0]=0

ID = unique(data$id)

data$propAepo[1] = 0
data$propBepo[1] = 0

data$propApo[1] = 0
data$propBpo[1] = 0

d= 0.7

for(i in 2:length(data[,1])){
  if(data$id[i-1]!=data$id[i]){
    data$propAepo[i] = 0
    data$propBepo[i] = 0
    
    data$propApo[i] = 0
    data$propBpo[i] = 0
  }
  
  if(data$id[i-1]==data$id[i]){
    data$propAepo[i] = d*data$propAepo[i-1]+data$Ia[i-1]*data$epayoff[i-1]
    data$propBepo[i] = d*data$propBepo[i-1]+data$Ib[i-1]*data$epayoff[i-1]
    
    data$propApo[i] = d*data$propApo[i-1]+data$Ia[i-1]*data$payoff[i-1]
    data$propBpo[i] = d*data$propBpo[i-1]+data$Ib[i-1]*data$payoff[i-1]
  }
}

h = 1

data$probApo = exp(h*data$propApo)/(exp(h*data$propApo)+exp(h*data$propBpo))
data$probBpo = exp(h*data$propBpo)/(exp(data$propApo)+exp(h*data$propBpo))

data$probAepo = exp(h*data$propAepo)/(exp(h*data$propAepo)+exp(h*data$propBepo))
data$probBepo = exp(h*data$propBepo)/(exp(h*data$propAepo)+exp(h*data$propBepo))


LLpo = function(h)(-(h*data$Ia*data$propApo+h*data$Ib*data$propBpo - log(exp(data$propApo)+exp(h*data$propBpo))))

ll_r1 = nlminb(rep(1,1000), LLpo)

LLepo = function(h)(-(h*data$Ia*data$propAepo+h*data$Ib*data$propBepo - log(exp(data$propAepo)+exp(h*data$propBepo))))

ll_r2 = nlminb(rep(1,1000), LLepo)


