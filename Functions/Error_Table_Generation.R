error_table_generation = function(error_dt,dt,error_type,item,item_value,error_desc,priority){
  dt$`Error Type`=error_type
  dt$Item = item
  dt$`Item Value`=item_value
  dt$`Error Description`=error_desc
  dt$`Additional Information/Instructions`=''
  dt$`Error Priority`=priority
  dt$`Fixed (Y/N)`=''
  dt$`Comments or questions from grantees`=''
  error_table = rbind(error_dt,dt)
  return(error_table)
}
