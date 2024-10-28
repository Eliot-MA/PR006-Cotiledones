# install.packages("usethis")
library(usethis)
use_git_config(user.name = "Eliot Mompeán", user.email = "eliot.mompean@uah.es")

URL_DEL_REPOSITORIO <- "https://github.com/Eliot-MA/PR006-Cotiledones.git"
system("git remote remove origin")
system("git remote add origin https://github.com/Eliot-MA/PR006-Cotiledones.git")

system("git init")

system("git remote -v")

system("git push -u origin master")  
