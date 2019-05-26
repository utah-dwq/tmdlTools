### TESTING ####
wqp_file = "//Users//elisehinman//Documents//GitHub//ecoli_tmdl//ecoli_flow_test_data.csv"
trans_wb = "//Users//elisehinman//Documents//GitHub//ecoli_tmdl//wqp_translation.xlsx"

convertWQP <- function(wqp_file, trans_wb, file_path){
  # Read in data
  data = read.csv(wqp_file, stringsAsFactors = FALSE)
  
  # Read in translation workbook
  trans_wb = openxlsx::loadWorkbook(trans_wb)
  trans_units = openxlsx::readWorkbook(trans_wb, sheet = 1)
  # isolate units from wqp file
  data_units = unique(data[,c("CharacteristicName", "ResultMeasure.MeasureUnitCode")])
  all_units = merge(trans_units, data_units, all=TRUE)
  
  
  
  # merge with translation wb
  
  
}