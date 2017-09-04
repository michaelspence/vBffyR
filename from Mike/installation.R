# Installing rstan

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function",
    file = M, sep = "\n", append = TRUE)

cat("\nCC=clang", "CXX=clang++ -arch x86_64 -ftemplate-depth-256",
    file = M, sep = "\n", append = TRUE)

cat("\nCXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations",
    file = M, sep = "\n", append = TRUE)

cat(readLines(M), sep = "\n")

# note: omit the 's' in 'https' if you cannot handle https downloads
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
