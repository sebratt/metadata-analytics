# # # # # # # # # # # # # # # # # # # 
# Clean the author column 
# Has code for detecting if the PI is in the author list 
# Feb 2023

# Install and load the necessary packages
# install.packages("tidyverse")
# install.packages("splitstackshape")
# install.packages("dplyr")
# install.packages("stringr")

library(tidyverse)
library(splitstackshape)
library(dplyr)
library(stringr)

# Read the csv file into a tibble
d <- read.csv("C:/Users/ual-laptop/Downloads/GenBank Research/Spring 2023 Research/Data/GB_NIH_TOTAL_COST_subset.csv") # read organization file
print(d$authors.y)
head(d$authors.y,5)

# Replace the " and" in the authors.y column with a single comma ","
d$authors.y<- gsub(" and",",", d$authors.y) 
d$authors.y <- gsub(" and ",",", d$authors.y)
d$authors.y <- gsub("and ",",", d$authors.y)

head(d$authors.y,5)

# Split the authors.y column based on the ".," characters to isolate each author
# into a separate column

# This code is a starting place to see if the PI is in the author list

# Sub-setting the tibble to only include the PI_NAMEs and the author columns
d2 <- d[, c("PI_NAMEs","authors.y_001","authors.y_002","authors.y_003","authors.y_004",
            "authors.y_005","authors.y_006","authors.y_007","authors.y_008","authors.y_009",
            "authors.y_010","authors.y_011","authors.y_012","authors.y_013","authors.y_014",
            "authors.y_015","authors.y_016","authors.y_017","authors.y_018","authors.y_019",
            "authors.y_020","authors.y_021","authors.y_022","authors.y_023","authors.y_024",
            "authors.y_025","authors.y_026","authors.y_027","authors.y_028","authors.y_029",
            "authors.y_030","authors.y_031","authors.y_032","authors.y_033","authors.y_034",
            "authors.y_035","authors.y_036","authors.y_037","authors.y_038","authors.y_039",
            "authors.y_040","authors.y_041","authors.y_042","authors.y_043","authors.y_044",
            "authors.y_045","authors.y_046","authors.y_047","authors.y_048","authors.y_049",
            "authors.y_050","authors.y_051","authors.y_052","authors.y_053","authors.y_054",
            "authors.y_055","authors.y_056","authors.y_057","authors.y_058","authors.y_059",
            "authors.y_060","authors.y_061","authors.y_062","authors.y_063","authors.y_064",
            "authors.y_065","authors.y_066","authors.y_067","authors.y_068","authors.y_069",
            "authors.y_070","authors.y_071","authors.y_072","authors.y_073","authors.y_074",
            "authors.y_075","authors.y_076","authors.y_077","authors.y_078","authors.y_079",
            "authors.y_080","authors.y_081","authors.y_082","authors.y_083","authors.y_084",
            "authors.y_085","authors.y_086","authors.y_087","authors.y_088","authors.y_089",
            "authors.y_090","authors.y_091","authors.y_092","authors.y_093","authors.y_094",
            "authors.y_095","authors.y_096","authors.y_097","authors.y_098","authors.y_099",
            "authors.y_100","authors.y_101","authors.y_102","authors.y_103","authors.y_104",
            "authors.y_105","authors.y_106","authors.y_107","authors.y_108","authors.y_109",
            "authors.y_110","authors.y_111","authors.y_112","authors.y_113","authors.y_114",
            "authors.y_115","authors.y_116","authors.y_117","authors.y_118","authors.y_119",
            "authors.y_120","authors.y_121","authors.y_122","authors.y_123","authors.y_124",
            "authors.y_125","authors.y_126","authors.y_127","authors.y_128","authors.y_129",
            "authors.y_130","authors.y_131","authors.y_132","authors.y_133","authors.y_134",
            "authors.y_135","authors.y_136","authors.y_137","authors.y_138","authors.y_139",
            "authors.y_140","authors.y_141","authors.y_142","authors.y_143","authors.y_144",
            "authors.y_145","authors.y_146","authors.y_147","authors.y_148","authors.y_149",
            "authors.y_150","authors.y_151","authors.y_152","authors.y_153","authors.y_154",
            "authors.y_155","authors.y_156","authors.y_157","authors.y_158","authors.y_159",
            "authors.y_160","authors.y_161","authors.y_162","authors.y_163","authors.y_164",
            "authors.y_165","authors.y_166","authors.y_167","authors.y_168","authors.y_169",
            "authors.y_170","authors.y_171","authors.y_172","authors.y_173","authors.y_174",
            "authors.y_175","authors.y_176","authors.y_177","authors.y_178","authors.y_179",
            "authors.y_180","authors.y_181","authors.y_182","authors.y_183","authors.y_184",
            "authors.y_185","authors.y_186","authors.y_187","authors.y_188","authors.y_189",
            "authors.y_190","authors.y_191","authors.y_192","authors.y_193","authors.y_194",
            "authors.y_195","authors.y_196","authors.y_197","authors.y_198","authors.y_199",
            "authors.y_200","authors.y_201","authors.y_202","authors.y_203","authors.y_204",
            "authors.y_205","authors.y_206","authors.y_207","authors.y_208","authors.y_209",
            "authors.y_210","authors.y_211","authors.y_212","authors.y_213","authors.y_214",
            "authors.y_215","authors.y_216","authors.y_217","authors.y_218","authors.y_219",
            "authors.y_220","authors.y_221","authors.y_222","authors.y_223","authors.y_224",
            "authors.y_225","authors.y_226","authors.y_227","authors.y_228","authors.y_229",
            "authors.y_230","authors.y_231","authors.y_232","authors.y_233","authors.y_234",
            "authors.y_235","authors.y_236","authors.y_237","authors.y_238","authors.y_239",
            "authors.y_240","authors.y_241","authors.y_242","authors.y_243","authors.y_244",
            "authors.y_245","authors.y_246","authors.y_247","authors.y_248","authors.y_249",
            "authors.y_250","authors.y_251","authors.y_252","authors.y_253","authors.y_254",
            "authors.y_255","authors.y_256","authors.y_257","authors.y_258","authors.y_259",
            "authors.y_260","authors.y_261","authors.y_262","authors.y_263","authors.y_264",
            "authors.y_265","authors.y_266","authors.y_267","authors.y_268","authors.y_269",
            "authors.y_270","authors.y_271","authors.y_272","authors.y_273","authors.y_274",
            "authors.y_275","authors.y_276","authors.y_277","authors.y_278","authors.y_279",
            "authors.y_280","authors.y_281","authors.y_282","authors.y_283","authors.y_284",
            "authors.y_285","authors.y_286","authors.y_287","authors.y_288","authors.y_289",
            "authors.y_290","authors.y_291","authors.y_292","authors.y_293","authors.y_294",
            "authors.y_295","authors.y_296","authors.y_297","authors.y_298","authors.y_299",
            "authors.y_300","authors.y_301","authors.y_302","authors.y_303","authors.y_304",
            "authors.y_305","authors.y_306","authors.y_307","authors.y_308","authors.y_309",
            "authors.y_310","authors.y_311","authors.y_312","authors.y_313","authors.y_314",
            "authors.y_315","authors.y_316","authors.y_317","authors.y_318","authors.y_319",
            "authors.y_320","authors.y_321","authors.y_322","authors.y_323","authors.y_324",
            "authors.y_325","authors.y_326","authors.y_327","authors.y_328","authors.y_329",
            "authors.y_330","authors.y_331","authors.y_332","authors.y_333","authors.y_334",
            "authors.y_335","authors.y_336","authors.y_337","authors.y_338","authors.y_339",
            "authors.y_340","authors.y_341","authors.y_342","authors.y_343","authors.y_344",
            "authors.y_345","authors.y_346","authors.y_347","authors.y_348","authors.y_349",
            "authors.y_350","authors.y_351","authors.y_352","authors.y_353","authors.y_354",
            "authors.y_355","authors.y_356","authors.y_357","authors.y_358","authors.y_359",
            "authors.y_360","authors.y_361","authors.y_362","authors.y_363","authors.y_364",
            "authors.y_365","authors.y_366","authors.y_367","authors.y_368","authors.y_369",
            "authors.y_370","authors.y_371","authors.y_372","authors.y_373","authors.y_374",
            "authors.y_375","authors.y_376","authors.y_377","authors.y_378","authors.y_379",
            "authors.y_380","authors.y_381","authors.y_382","authors.y_383","authors.y_384",
            "authors.y_385","authors.y_386","authors.y_387","authors.y_388","authors.y_389",
            "authors.y_390","authors.y_391","authors.y_392","authors.y_393","authors.y_394",
            "authors.y_395","authors.y_396","authors.y_397","authors.y_398","authors.y_399",
            "authors.y_400","authors.y_401","authors.y_402","authors.y_403","authors.y_404",
            "authors.y_405","authors.y_406","authors.y_407","authors.y_408","authors.y_409",
            "authors.y_410","authors.y_411","authors.y_412","authors.y_413","authors.y_414",
            "authors.y_415","authors.y_416","authors.y_417")]

# Load the tibble into d3 and d4, where d4 will only contain a sample of the 
# dataset for testing the code written for string manipulations
d3 <- d2
d4 <- d[1:7005,]

# Convert the data type of the PI_NAMEs column to character 
d4$PI_NAMEs <- as.character(d4$PI_NAMEs)

print(d4$PI_NAMEs)

# "MATHER, THOMAS N;" to "Mather,T.N."
# Code for one input (using string manipulations)
# input_str <- "MATHER, THOMAS N;"
# split_string <- strsplit(input_str, ",")[[1]]
# print(split_string)
# 
# last_name <- split_string[1]
# last_name <- tolower(last_name)
# last_name <- str_to_title(last_name)
# first_name <- split_string[2]
# first_name <- strsplit(first_name, " ")[[1]]
# first_name <- first_name[2:3]
# first_name <- paste0(substr(first_name, 1, 1), ".")
# first_name <- paste(first_name, collapse = "")
# 
# print(last_name)
# print(first_name)

# Loop the string manipulation code tested above through the tibble 
for (i in 1:nrow(d4)) {
  
  input_str <- d4[[i,49]]
  split_string <- strsplit(input_str, ",")[[1]]
  
  last_name <- split_string[1]
  last_name <- tolower(last_name)
  last_name <- str_to_title(last_name)
  first_name <- split_string[2]
  first_name <- strsplit(first_name, " ")[[1]]
  first_name <- first_name[2:3]
  first_name <- paste0(substr(first_name, 1, 1), ".")
  first_name <- paste(first_name, collapse = "")
  
  d4[[i,49]] <- paste0(last_name,",",first_name)
  
}

print(head(d4$PI_NAMEs,150))

class(d4[i,49])

# Check if PI_NAMEs is equal to any of the author columns, and generate a new
# binary column "PI_in_author_list", where 1 indicates the PI is part of the 
# author list, and 0 otherwise
d4$PI_in_author_list <- ifelse(d4$PI_NAMEs == d4$authors.y_001 | 
                               d4$PI_NAMEs == d4$authors.y_002 | 
                               d4$PI_NAMEs == d4$authors.y_003 | 
                               d4$PI_NAMEs == d4$authors.y_004 | 
                               d4$PI_NAMEs == d4$authors.y_005 | 
                               d4$PI_NAMEs == d4$authors.y_006 | 
                               d4$PI_NAMEs == d4$authors.y_007 | 
                               d4$PI_NAMEs == d4$authors.y_008 | 
                               d4$PI_NAMEs == d4$authors.y_009 | 
                               d4$PI_NAMEs == d4$authors.y_010 | 
                               d4$PI_NAMEs == d4$authors.y_011 | 
                               d4$PI_NAMEs == d4$authors.y_012 | 
                               d4$PI_NAMEs == d4$authors.y_013 | 
                               d4$PI_NAMEs == d4$authors.y_014 | 
                               d4$PI_NAMEs == d4$authors.y_015 | 
                               d4$PI_NAMEs == d4$authors.y_016 | 
                               d4$PI_NAMEs == d4$authors.y_017 | 
                               d4$PI_NAMEs == d4$authors.y_018 | 
                               d4$PI_NAMEs == d4$authors.y_019 | 
                               d4$PI_NAMEs == d4$authors.y_020 | 
                               d4$PI_NAMEs == d4$authors.y_021 | 
                               d4$PI_NAMEs == d4$authors.y_022 | 
                               d4$PI_NAMEs == d4$authors.y_023 | 
                               d4$PI_NAMEs == d4$authors.y_024 | 
                               d4$PI_NAMEs == d4$authors.y_025 | 
                               d4$PI_NAMEs == d4$authors.y_026 | 
                               d4$PI_NAMEs == d4$authors.y_027 | 
                               d4$PI_NAMEs == d4$authors.y_028 | 
                               d4$PI_NAMEs == d4$authors.y_029 | 
                               d4$PI_NAMEs == d4$authors.y_030 | 
                               d4$PI_NAMEs == d4$authors.y_031 | 
                               d4$PI_NAMEs == d4$authors.y_032 | 
                               d4$PI_NAMEs == d4$authors.y_033 | 
                               d4$PI_NAMEs == d4$authors.y_034 | 
                               d4$PI_NAMEs == d4$authors.y_035 | 
                               d4$PI_NAMEs == d4$authors.y_036 | 
                               d4$PI_NAMEs == d4$authors.y_037 | 
                               d4$PI_NAMEs == d4$authors.y_038 | 
                               d4$PI_NAMEs == d4$authors.y_039 | 
                               d4$PI_NAMEs == d4$authors.y_040 | 
                               d4$PI_NAMEs == d4$authors.y_041 | 
                               d4$PI_NAMEs == d4$authors.y_042 | 
                               d4$PI_NAMEs == d4$authors.y_043 | 
                               d4$PI_NAMEs == d4$authors.y_044 | 
                               d4$PI_NAMEs == d4$authors.y_045 | 
                               d4$PI_NAMEs == d4$authors.y_046 | 
                               d4$PI_NAMEs == d4$authors.y_047 | 
                               d4$PI_NAMEs == d4$authors.y_048 | 
                               d4$PI_NAMEs == d4$authors.y_049 | 
                               d4$PI_NAMEs == d4$authors.y_050 | 
                               d4$PI_NAMEs == d4$authors.y_051 | 
                               d4$PI_NAMEs == d4$authors.y_052 | 
                               d4$PI_NAMEs == d4$authors.y_053 | 
                               d4$PI_NAMEs == d4$authors.y_054 | 
                               d4$PI_NAMEs == d4$authors.y_055 | 
                               d4$PI_NAMEs == d4$authors.y_056 | 
                               d4$PI_NAMEs == d4$authors.y_057 | 
                               d4$PI_NAMEs == d4$authors.y_058 | 
                               d4$PI_NAMEs == d4$authors.y_059 | 
                               d4$PI_NAMEs == d4$authors.y_060 | 
                               d4$PI_NAMEs == d4$authors.y_061 | 
                               d4$PI_NAMEs == d4$authors.y_062 | 
                               d4$PI_NAMEs == d4$authors.y_063 | 
                               d4$PI_NAMEs == d4$authors.y_064 | 
                               d4$PI_NAMEs == d4$authors.y_065 | 
                               d4$PI_NAMEs == d4$authors.y_066 | 
                               d4$PI_NAMEs == d4$authors.y_067 | 
                               d4$PI_NAMEs == d4$authors.y_068 | 
                               d4$PI_NAMEs == d4$authors.y_069 | 
                               d4$PI_NAMEs == d4$authors.y_070 | 
                               d4$PI_NAMEs == d4$authors.y_071 | 
                               d4$PI_NAMEs == d4$authors.y_072 | 
                               d4$PI_NAMEs == d4$authors.y_073 | 
                               d4$PI_NAMEs == d4$authors.y_074 | 
                               d4$PI_NAMEs == d4$authors.y_075 | 
                               d4$PI_NAMEs == d4$authors.y_076 | 
                               d4$PI_NAMEs == d4$authors.y_077 | 
                               d4$PI_NAMEs == d4$authors.y_078 | 
                               d4$PI_NAMEs == d4$authors.y_079 | 
                               d4$PI_NAMEs == d4$authors.y_080 | 
                               d4$PI_NAMEs == d4$authors.y_081 | 
                               d4$PI_NAMEs == d4$authors.y_082 | 
                               d4$PI_NAMEs == d4$authors.y_083 | 
                               d4$PI_NAMEs == d4$authors.y_084 | 
                               d4$PI_NAMEs == d4$authors.y_085 | 
                               d4$PI_NAMEs == d4$authors.y_086 | 
                               d4$PI_NAMEs == d4$authors.y_087 | 
                               d4$PI_NAMEs == d4$authors.y_088 | 
                               d4$PI_NAMEs == d4$authors.y_089 | 
                               d4$PI_NAMEs == d4$authors.y_090 | 
                               d4$PI_NAMEs == d4$authors.y_091 | 
                               d4$PI_NAMEs == d4$authors.y_092 | 
                               d4$PI_NAMEs == d4$authors.y_093 | 
                               d4$PI_NAMEs == d4$authors.y_094 | 
                               d4$PI_NAMEs == d4$authors.y_095 | 
                               d4$PI_NAMEs == d4$authors.y_096 | 
                               d4$PI_NAMEs == d4$authors.y_097 | 
                               d4$PI_NAMEs == d4$authors.y_098 | 
                               d4$PI_NAMEs == d4$authors.y_099 | 
                               d4$PI_NAMEs == d4$authors.y_100 | 
                               d4$PI_NAMEs == d4$authors.y_101 | 
                               d4$PI_NAMEs == d4$authors.y_102 | 
                               d4$PI_NAMEs == d4$authors.y_103 | 
                               d4$PI_NAMEs == d4$authors.y_104 | 
                               d4$PI_NAMEs == d4$authors.y_105 | 
                               d4$PI_NAMEs == d4$authors.y_106 | 
                               d4$PI_NAMEs == d4$authors.y_107 | 
                               d4$PI_NAMEs == d4$authors.y_108 | 
                               d4$PI_NAMEs == d4$authors.y_109 | 
                               d4$PI_NAMEs == d4$authors.y_110 | 
                               d4$PI_NAMEs == d4$authors.y_111 | 
                               d4$PI_NAMEs == d4$authors.y_112 | 
                               d4$PI_NAMEs == d4$authors.y_113 | 
                               d4$PI_NAMEs == d4$authors.y_114 | 
                               d4$PI_NAMEs == d4$authors.y_115 | 
                               d4$PI_NAMEs == d4$authors.y_116 | 
                               d4$PI_NAMEs == d4$authors.y_117 | 
                               d4$PI_NAMEs == d4$authors.y_118 | 
                               d4$PI_NAMEs == d4$authors.y_119 | 
                               d4$PI_NAMEs == d4$authors.y_120 | 
                               d4$PI_NAMEs == d4$authors.y_121 | 
                               d4$PI_NAMEs == d4$authors.y_122 | 
                               d4$PI_NAMEs == d4$authors.y_123 | 
                               d4$PI_NAMEs == d4$authors.y_124 | 
                               d4$PI_NAMEs == d4$authors.y_125 | 
                               d4$PI_NAMEs == d4$authors.y_126 | 
                               d4$PI_NAMEs == d4$authors.y_127 | 
                               d4$PI_NAMEs == d4$authors.y_128 | 
                               d4$PI_NAMEs == d4$authors.y_129 | 
                               d4$PI_NAMEs == d4$authors.y_130 | 
                               d4$PI_NAMEs == d4$authors.y_131 | 
                               d4$PI_NAMEs == d4$authors.y_132 | 
                               d4$PI_NAMEs == d4$authors.y_133 | 
                               d4$PI_NAMEs == d4$authors.y_134 | 
                               d4$PI_NAMEs == d4$authors.y_135 | 
                               d4$PI_NAMEs == d4$authors.y_136 | 
                               d4$PI_NAMEs == d4$authors.y_137 | 
                               d4$PI_NAMEs == d4$authors.y_138 | 
                               d4$PI_NAMEs == d4$authors.y_139 | 
                               d4$PI_NAMEs == d4$authors.y_140 | 
                               d4$PI_NAMEs == d4$authors.y_141 | 
                               d4$PI_NAMEs == d4$authors.y_142 | 
                               d4$PI_NAMEs == d4$authors.y_143 | 
                               d4$PI_NAMEs == d4$authors.y_144 | 
                               d4$PI_NAMEs == d4$authors.y_145 | 
                               d4$PI_NAMEs == d4$authors.y_146 | 
                               d4$PI_NAMEs == d4$authors.y_147 | 
                               d4$PI_NAMEs == d4$authors.y_148 | 
                               d4$PI_NAMEs == d4$authors.y_149 | 
                               d4$PI_NAMEs == d4$authors.y_150, 1, 0)


print(head(d4$PI_in_author_list,150))

options(max.print=1000000)