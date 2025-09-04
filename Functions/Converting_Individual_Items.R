removing_all_na_cols = function(dt){
  all_na = c()
  for(j in 1:ncol(dt)){
    all_na = c(all_na,all(is.na(dt[,j])))
  }
  dt = dt[,!all_na]
  return(dt)
}

cbmiss <- function(itemscore){
  flag <- ifelse(is.na(itemscore),1, 
                 ifelse(itemscore<0,1,0))
  return(flag)
}

cbraw = function(itemscore){
  itemscore <- ifelse(is.na(itemscore),NA,ifelse(itemscore<0,NA,itemscore))
  return(itemscore)
}

itemscore_yesno = function(itemscore){
  itemscore <- ifelse(itemscore=="Yes", 1, ifelse(itemscore=="No",0,NA))
  return(itemscore)
}

sleep_forwardscore = function(itemscore){
  itemscore <- ifelse(itemscore=="Usually"|itemscore=="Falls Asleep"|itemscore==3,3 , ifelse(itemscore=="Sometimes"|itemscore=="Very Sleepy"|itemscore==2,2,ifelse(itemscore=="Rarely"|itemscore=="Missing"|itemscore==1,1,NA)))
  return(itemscore)
}
sleep_reversescore = function(itemscore){
  itemscore <- ifelse(itemscore=="Usually"|itemscore=="Falls Asleep",1,ifelse(itemscore=="Sometimes"|itemscore=="Very Sleepy",2,ifelse(itemscore=="Rarely"|itemscore=="Missing",3,NA)))
  return(itemscore)
}

summing_items_per_row = function(dt,item_list,col_name,TF){
  #dt = data table that contains columns that you want to sum
  #col_name = a crated summed column name(in character value) -> matching with the # of lists
  #item_list = the list of item lists
  #TF = if you want to remove NA when summing or not
  for(l in 1:length(item_list)){
    for(i in 1:nrow(dt)){
      dt[i,col_name[[l]]] = sum(dt[i,item_list[[l]]],na.rm=TF)
    }
  }
  return(dt)
}

seq_factor2scores = function(item,dt){
  item = ifelse(item=="Never/Almost Never",1,
              ifelse(item=="Once in a While",2,
                     ifelse(item=="Sometimes",3,
                            ifelse(item=="Frequently",4,
                                   ifelse(item=="Almost Always/Always",5,NA)))))
}
