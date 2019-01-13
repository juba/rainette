## Test debug fonction find.terminales de CHD.R
## On reconstruit l'arbre suivant :
# 
# 
#       +----2----+
#       |         |
#   +---3---+     |
#   |       |     |
# +-4-+   +-5-+   |
# |   |   |   |   |
# 6   7   8   9   10


list_mere <- list()
list_mere[[6]] <- 4
list_mere[[7]] <- 4
list_mere[[4]] <- 3
list_mere[[8]] <- 5
list_mere[[9]] <- 5
list_mere[[5]] <- 3
list_mere[[3]] <- 2
list_mere[[10]] <- 2
list_fille <- list()
list_fille[[2]] <- c(3,10)
list_fille[[3]] <- c(4,5)
list_fille[[4]] <- c(6,7)
list_fille[[5]] <- c(8,9)

# Premier cas : chaque noeud a un effectif de 1

n1 <- matrix(
  c(3,4,6,
    3,4,7,
    3,5,8,
    3,5,9,
    2,2,10
    ),
  ncol=3,
  byrow=TRUE
)

find.terminales(n1, list_mere, list_fille, 1)
## Problème : dans ce cas on devrait avoir deux classes terminales 4 et 5
find.terminales(n1, list_mere, list_fille, 2)

# Deuxième cas : on met un effectif de 4 au noeud 10

n1 <- matrix(
  c(3,4,6,
    3,4,7,
    3,5,8,
    3,5,9,
    2,2,10,
    2,2,10,
    2,2,10,
    2,2,10
  ),
  ncol=3,
  byrow=TRUE
)

find.terminales(n1, list_mere, list_fille, 2)
## Problème : dans ce cas on devrait avoir 10 et 3 comme terminales
find.terminales(n1, list_mere, list_fille, 3)

## CODE IRAMUTEQ
getfille <- function(nlf, classe, pf) {
  if (classe == 0 || !length(nlf[[classe]])) {
    return(pf)
  } else {
    for (cl in nlf[[classe]]) {
      pf <- c(pf, cl)
      if (cl <= length(nlf)) {
        pf <- getfille(nlf, cl, pf)
      }
    }
  } 
  return(pf)
}
getlength <- function(n1, clnb) {
  colnb <- (clnb %/%2)
  tab <- table(n1[,colnb])
  print(tab)
  eff <- tab[which(names(tab) == as.character(clnb))]
  return(eff)
}
find.terminales <- function(n1, list_mere, list_fille, mincl) {
  tab <- table(n1[,ncol(n1)])
  print("tab")
  print(tab)
  clnames <- rownames(tab)
  terminales <- clnames[which(tab >= mincl)]
  tocheck <- setdiff(clnames, terminales)
  if ("0" %in% terminales) {
    terminales <- terminales[which(terminales != 0)]
  }
  if (length(terminales) == 0) {
    return('no clusters')
  }
  if ("0" %in% tocheck) {
    tocheck <- tocheck[which(tocheck != "0")]
  }
  print("terminales")
  print(terminales)
  print("tocheck")
  print(tocheck)
  print("terminales avant")
  print(terminales)
  while (length(tocheck)!=0) {
    for (val in tocheck) {
      print(val)
      mere <- list_mere[[as.numeric(val)]]
      print('mere')
      print(mere)
      if (mere != 1) {
        ln.mere <- getlength(n1, mere)
        print('ln.mere')
        print(ln.mere)
        filles.mere <- getfille(list_fille, mere, NULL)
        print('fille mere')
        print(filles.mere)
        filles.mere <- filles.mere[which(filles.mere != val)]
        print(filles.mere)
        print(intersect(filles.mere, tocheck))
        if ((ln.mere >= mincl) & (length(intersect(filles.mere, tocheck)) == 0) & (length(intersect(filles.mere, terminales)) == 0 )) {
          print('mere ok')
          terminales <- c(terminales, mere)
          for (f in c(filles.mere, val, mere)) {
            tocheck <- tocheck[which(tocheck != f)]
          }	
        } else if ((ln.mere >= mincl) & (length(intersect(filles.mere, terminales)) == 0) & (length(intersect(filles.mere, tocheck))!=0)){
          print('mere a checke cause fille ds tocheck')
          tocheck <- tocheck[which(tocheck != val)]
          tocheck <- c(mere, tocheck)
          
        } else {
          print('pas ok on vire du check')
          tocheck <- tocheck[which(tocheck != val)]
        }
      } else {
        print('mere == 1')
        tocheck <- tocheck[which(tocheck != val)]
      }
      print('tocheck')
      print(tocheck)
    }
    print(tocheck)
  }
  print("terminales après")
  print(terminales)
  terminales
}