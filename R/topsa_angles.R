# Euler(2.0)

# ishigami.fun <- function(X) {
#   A <- 7
#   B <- 0.1
#   sin(X[, 1]) + A * sin(X[, 2])^2 + B * X[, 3]^4 * sin(X[, 1])
# }
#
# X <- matrix(runif(3*100, -pi, pi), ncol = 3)
# Y <- ishigami.fun(X)
# Xdat <- X
# Ydat <- Y


#Funcion para seccionar los datos por angulo ----
#
Seccion_angulos <-
  function(Yr, Xr, steps = pi / 6) {
    # Reciben vector Y, X_i y el paso
    X_i <- Xr - mean(Xr)
    Y_i <- Yr - mean(Yr)
    angulos <- atan2(Y_i, X_i)


    angulos_data <- list(NULL)
    angulos <- ifelse(angulos >= 0, angulos, angulos + 2 * pi)
    cita <- seq(0, 2 * pi - steps, by = steps)


    for (l in 1:length(cita)) {
      if (cita[l] <= pi) {
        angulos_data[[l]] <-
          cbind(x = X_i[which(angulos > cita[l] &
                                angulos < cita[l] + pi)],
                y = Y_i[which(angulos > cita[l] &
                                angulos < cita[l] + pi)],
                angle = cita[l])
      } else{
        angulos_data[[l]] <-
          cbind(x = X_i[which(angulos >= cita[l] |
                                angulos <= -pi + cita[l])],
                y = Y_i[which(angulos >= cita[l] |
                                angulos <= -pi + cita[l])],
                angle = cita[l])

      }

    }
    #names(angulos_data) <- cita
    angulos_data

  }

pru <- Seccion_angulos(Y, X[, 1], pi / 2)
#ggplot() + geom_point(aes(X[, 1] - mean(X[, 1]), Y - mean(Y)), shape = 2) + geom_point(aes(pru[[1]][, 1], pru[[1]][, 2]))

asd <- topsa(pru[[1]][,2],pru[[1]][,1])$results[[1]]$Symmetric.Diff.Area


topsa_angles <- function(Yr, Xr, steps = pi / 2) {
 ########Etqieutas
   vect_angulos<- seq(0,2*pi,pi/24)#Se hizo con 24
  vect_angulos_caracter<- c('0','pi/24','pi/12','pi/8','pi/6','5*pi/24','pi/4','7*pi/24','pi/3','3*pi/8','5*pi/12','11*pi/24','pi/2','13*pi/24','7*pi/12','5*pi/8','2*pi/3','17*pi/24','3*pi/4','19*pi/24','5*pi/6','7*pi/8','11*pi/12','23*pi/24','pi','25*pi/24','13*pi/12','9*pi/8','7*pi/6','29*pi/24','5*pi/4','31*pi/24','4*pi/3','11*pi/8','17*pi/12','35*pi/24','3*pi/2','37*pi/24','19*pi/12','13*pi/8','5*pi/3','41*pi/24','7*pi/4','43*pi/24','11*pi/6','15*pi/8','23*pi/12','47*pi/24','2*pi')
  cita <- seq(0,2*pi,steps)
  indice <- c()
  for (d in 1:length(cita)) {
    indice[d] <- which(abs(vect_angulos-cita[d])<0.0001)
  }
  cita_caracter <- vect_angulos_caracter[indice]
  #library(rSymPy)
  # cita <- seq(0,2*pi,steps)
  # b <- paste("nsimplify(" ,cita,",[pi], tolerance=0.0000000001)")
  # vector <- lapply(b, sympy)
  # cita_caracter <-unlist(vector)
  # Verficar si lo anterior corre
  upper<- cita_caracter[which((cita-pi)<0)]
  lower <- cita_caracter[which(cita-pi>=0)]
  lower <- lower[which(lower!="2*pi")]
  Etiquetas <- rep(paste(upper,lower,sep = "--"),2)
  #Finaliza etiquetas


  pru <- Seccion_angulos(Yr, Xr, steps)
  df <- suppressWarnings(lapply(X=1:length(pru),function(i) {
  data.frame(Indice=topsa(pru[[i]][,2],pru[[i]][,1])$results[[1]]$Symmetric.Diff.Area)%>%dplyr::mutate(angle_numeric=cita[i])%>%dplyr::mutate(angle=cita_caracter[i]) %>% dplyr::mutate(side=ifelse(angle%in%upper,"upper","lower")) %>% dplyr::mutate(Etiqueta=Etiquetas[i])

  }))
  (dff <- dplyr::bind_rows(df) %>% dplyr::select(-angle_numeric))
}

topsa_angles(Y,X[,1])
