# 提取汉字拼音首字母
# Converts Chinese characters to the initials of their pinyin representation
# Args:
#   char1: 输入的汉字字符串 Input Chinese characters as a string
#   first5: 是否只取前五个字母 (默认为TRUE) 
#           Whether to take only the first five initials (default is TRUE)
#   to_upper: 是否转换为大写字母 (默认为TRUE) 
#             Whether to convert to uppercase letters (default is TRUE)
# Returns:
#   拼音首字母字符串 The string of pinyin initials
py_ini <- function(char1, first5 = TRUE, to_upper = TRUE) {
  # 检查是否已安装pinyin包
  # Check if the 'pinyin' package is already installed
  if (requireNamespace("pinyin", quietly = TRUE)) {
    # 提示用户是否安装pinyin包
    # Prompt the user if they want to install the 'pinyin' package
    cat("需要包“pinyin”才能运行接下来的代码。The 'pinyin' package is required for this code to run.\n")
    cat("是否安装：Do you want to install it? (Y/N): ")
    
    # 从用户获取输入
    # Get input from the user
    user_input <- readline(prompt = "")
    
    # 如果用户输入Y，则安装pinyin包
    # If the user inputs 'Y', then install the 'pinyin' package
    if (toupper(user_input) == "Y") {
      install.packages("pinyin")
    } else {
      # 如果用户选择不安装'pinyin'包，提醒代码可能不会按预期工作
      # If the user chooses not to install the 'pinyin' package, inform them that the code may not work as expected
      cat("You chose not to install the 'pinyin' package. The code may not work as expected.\n")
    }
  }
  
  # 加载pinyin包
  # Load the 'pinyin' package
  library(pinyin)
  
  # 现在您可以在代码中使用pinyin包的功能
  # Now you can use the functionalities of the 'pinyin' package in the code
  # 默认只取前五个字母；默认字母全部大写
  # Default behavior is to take only the first five initials; default is all uppercase letters.
  
  # 将汉字转换为带数字音调的拼音
  py_char <- py(char1, dic = pydic(dic = c("pinyin2")), other_replace = "")
  
  # 提取首字母的函数
  ext_ini <- function(text) {
    # 使用strsplit函数将字符串分割成单词
    # Use the strsplit function to split the string into words.
    words <- strsplit(text, "_")
    
    # 使用sapply函数提取每个单词的首字母
    # Use the sapply function to extract the first letter of each word.
    initials <- sapply(words, function(word) substr(word, 1, 1))
    
    # 将提取的首字母组合成一个字符串
    # Combine the extracted first letters into a string.
    result <- paste(initials, collapse = "")
    
    if (to_upper) {
      result <- toupper(result)
    }
    
    return(result)
  }
  
  # 提取汉字拼音的首字母
  initials <- sapply(py_char, ext_ini)
  initials <- unname(initials)
  
  if (first5) {
    initials <- substr(initials, 1, 5)
  }
  
  return(initials)
}


# Args:
#   char1: 输入的汉字字符串 Input Chinese characters as a string
#   first5: 是否只取前五个字母 (默认为TRUE) 
#           Whether to take only the first five initials (default is TRUE)
#   to_upper: 是否转换为大写字母 (默认为TRUE) 
#             Whether to convert to uppercase letters (default is TRUE)
# Returns:
#   拼音首字母字符串 The string of pinyin initials
py_ini <- function(char1, first5 = TRUE, to_upper = TRUE) {
  # 检查是否已安装pinyin包
  # Check if the 'pinyin' package is already installed
  if (!requireNamespace("pinyin", quietly = TRUE)) {
    # 提示用户是否安装pinyin包
    # Prompt the user if they want to install the 'pinyin' package
    cat("需要包“pinyin”才能运行接下来的代码。The 'pinyin' package is required for this code to run.\n")
    cat("是否安装：Do you want to install it? (Y/N): ")
    
    # 从用户获取输入
    # Get input from the user
    user_input <- readline(prompt = "")
    
    # 如果用户输入Y，则安装pinyin包
    # If the user inputs 'Y', then install the 'pinyin' package
    if (toupper(user_input) == "Y") {
      install.packages("pinyin")
    } else {
      # 如果用户选择不安装'pinyin'包，提醒代码可能不会按预期工作
      # If the user chooses not to install the 'pinyin' package, inform them that the code may not work as expected
      cat("You chose not to install the 'pinyin' package. The code may not work as expected.\n")
    }
  }
  
  # 加载pinyin包
  # Load the 'pinyin' package
  library(pinyin)
  
  # 现在您可以在代码中使用pinyin包的功能
  # Now you can use the functionalities of the 'pinyin' package in the code
  # 默认只取前五个字母；默认字母全部大写
  # Default behavior is to take only the first five initials; default is all uppercase letters.
  
  # 将汉字转换为带数字音调的拼音
  py_char <- py(char1, dic = pydic(dic = c("pinyin2")), other_replace = "")
  
  # 提取首字母的函数
  ext_ini <- function(text) {
    # 使用strsplit函数将字符串分割成单词
    # Use the strsplit function to split the string into words.
    words <- strsplit(text, "_")
    
    # 使用sapply函数提取每个单词的首字母
    # Use the sapply function to extract the first letter of each word.
    initials <- sapply(words, function(word) substr(word, 1, 1))
    
    # 将提取的首字母组合成一个字符串
    # Combine the extracted first letters into a string.
    result <- paste(initials, collapse = "")
    
    if (to_upper) {
      result <- toupper(result)
    }
    
    return(result)
  }
  
  # 提取所有汉字的首字母
  initials <- sapply(py_char, ext_ini)
  initials <- unname(initials)
  
  if (first5) {
    initials <- substr(initials, 1, 5)
  }
  
  return(initials)
}


