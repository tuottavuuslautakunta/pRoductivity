###########################################
#
# Simple Syntethic Control Algorithm (SSCA)
#
# Author: Jetro Anttonen (jetro.anttonen@etla.fi)
#
###########################################

# J = Number of control regions
# T = Number of timeperiods
# T0 + 1 = Time of intervention (argument 'intervention' in z.prep)

z.prep <- function(longdata,
                   unit_var,
                   value_var,
                   time_var,
                   target_unit,
                   intervention,
                   first_date,
                   last_date,
                   start_date,
                   normalize = "no",
                   drop_countries = c()) {
  #Save the call
  theCall <- list(
    longdata,
    unit_var,
    value_var,
    time_var,
    target_unit,
    intervention,
    first_date,
    last_date,
    start_date,
    normalize,
    drop_countries
  )
  names(theCall) <- c(
    "longdata",
    "unit_var",
    "value_var",
    "time_var",
    "target_unit",
    "intervention",
    "first_date",
    "last_date",
    "start_date",
    "normalize",
    "drop_countries"
  )

  #Time indices
  di <- ts(seq(300), start = first_date, frequency = 1)
  ld_i <- window(di, start = last_date, end = last_date)
  start_i <- window(di, start = start_date, end = start_date)
  int_i <-
    as.numeric(window(di, start = intervention, end = intervention)) - as.numeric(start_i + 1)
  T0 <- as.numeric(int_i - 1)

  #Stacks, cuts and normalizes the Z matrix around the starting value
  units <- levels(as.factor(longdata[, unit_var]))
  Z <-  matrix(NA,
           ncol = length(units),
           nrow = (as.numeric(ld_i) - as.numeric(start_i) + 1))
  colnames(Z) <- units
  for (i in 1:length(units)) {
    temp <- longdata[which(longdata[, unit_var] == units[i]), ]
    #Collect
    if (dim(temp)[1] != 0) {
      sy <- as.numeric(temp[1, time_var])
      temp <- ts(temp[, value_var], start = sy, frequency = 1)
      #Cut
      temp <- window(temp, start = start_date, end = last_date)
      temp <- as.numeric(temp)
      #Normalize
      if (normalize == "start")
        temp <- temp / temp[1]
      if (normalize == "end")
        temp <- temp / temp[T0]
      #Stack
      Z[, i] <- temp
    } else {
      Z[, i] <- NA
    }
  }

  if (length(drop_countries) != 0) {
    dropthese <- which(colnames(Z) %in% drop_countries)
    Z <- Z[, -dropthese]
    #print(paste0("Following countries dropped: ", paste0(drop_countries, collapse = ", ")))
  }

  #Remove countries with NA values
  to_drop <- c()
  c_names <- colnames(Z)
  for (i in 1:dim(Z)[2]) {
    if (sum(is.na(Z[, i])) != 0)
      to_drop <- c(to_drop, i)
  }
  if (length(to_drop) > 0) {
    Z <- Z[, -to_drop]
    print(paste0(
      "Following countries dropped due to missing observations: ",
      paste(c_names[to_drop], collapse = ", ")
    ))
  }

  #New time indices
  di <- ts(seq(300), start = start_date, frequency = 1)
  ld_i <- window(di, start = last_date, end = last_date)
  int_i <- window(di, start = intervention, end = intervention)
  T0 <- int_i - 1

  #Builds the final Z matrices
  Z0_post <- ts(Z[int_i:ld_i, ], start = intervention, frequency = 1)
  Z1_post <- Z0_post[, which(colnames(Z0_post) == target_unit)]
  Z0_post <- Z0_post[, -which(colnames(Z0_post) == target_unit)]

  Z0 <- ts(Z[1:T0, ], start = start_date, frequency = 1)
  Z1 <- Z0[, which(colnames(Z0) == target_unit)]
  Z0 <- Z0[, -which(colnames(Z0) == target_unit)]

  if (normalize == "constant") {
    Z0 <- cbind(Z0, rep(1, dim(Z0)[1]))
    Z0_post <- cbind(Z0_post, rep(1, dim(Z0_post)[1]))
    colnames(Z0)[dim(Z0)[2]] <- "Constant"
  }

  ret <- list(Z0, Z1, Z0_post, Z1_post, theCall)
  names(ret) <- c("Z0", "Z1", "Z0_post", "Z1_post", "theCall")

  #out: list( Z1 (T0x1), Z0 (T0xJ), Z1_post, Z0_post, theCall )
  ret
}

w.opt <- function(z.list) {
  Z1 <- z.list$Z1
  Z0 <- z.list$Z0
  w_len <- dim(Z0)[2]
  c_names <- colnames(Z0)

  # Function to be minimized w.r.t W (Jx1): (Z1 - Z0*W)'*(Z1 - Z0*W)
  # st. sum(W) = 1 and w_i >= 0
  min.fun <- function(W) {
    W <- W / sum(W)
    value <- t(Z1 - Z0 %*% W) %*% (Z1 - Z0 %*% W)
    value
  }

  opt.out <-
    optim(rep(1, w_len),
          min.fun,
          method = "L-BFGS-B",
          lower = rep(0, w_len))
  W_star <- opt.out$par / sum(opt.out$par)
  names(W_star) <- c_names

  ret <- list(opt.out, W_star)
  names(ret) <- c("opt.out", "W_star")

  #out: list(opt.out, W_star)
  ret
}



comp.rmse <- function(z.list, W_star) {
  Z1 <- ts(z.list$Z1, start = 1, frequency = 1)
  dop <- ts(z.list$Z0 %*% W_star, start = 1, frequency = 1)
  Z1 <- (Z1 / Z1[1]) * 100
  dop <- (dop / dop[1]) * 100
  sqrt(mean((dop - Z1) ^ 2)) #RMSE
}

loo.fun <- function(z.list) {
  ret <- list()

  allData <- z.list$theCall$longdata
  countries <-
    levels(as.factor(allData$geo))[-which(levels(as.factor(allData$geo)) == "FI")]
  timevec <-
    z.list$theCall[["start_date"]]:z.list$theCall[["last_date"]]
  for (i in 1:length(countries)) {
    country <- countries[i]
    z.list.temp <- z.prep(
      longdata = z.list$theCall[["longdata"]],
      unit_var = z.list$theCall[["unit_var"]],
      target_unit = z.list$theCall[["target_unit"]],
      value_var = z.list$theCall[["value_var"]],
      time_var = z.list$theCall[["time_var"]],
      intervention = z.list$theCall[["intervention"]],
      first_date = z.list$theCall[["first_date"]],
      last_date = z.list$theCall[["last_date"]],
      start_date = z.list$theCall[["start_date"]],
      normalize = z.list$theCall[["normalize"]],
      drop_countries = country
    )
    w.list.temp <- w.opt(z.list.temp)
    W_star.temp <- w.list.temp$W_star

    temp.obj <- list(z.list.temp, W_star.temp)
    names(temp.obj) <- c("z.list", "W_star")
    ret[[i]] <- temp.obj
  }

  ret
}


ssca_est <- function(data, unit_var, target_unit, value_var, time_var, est_periods, eval_periods){

  z.list <- data %>%
    data.frame() %>%
    z.prep(unit_var = unit_var,
           target_unit = target_unit,
           value_var = value_var,
           time_var = "time",
           intervention = eval_periods[1],
           first_date = est_periods[1],
           last_date = eval_periods[length(eval_periods)],
           start_date = est_periods[1],
           normalize = "no",
           drop_countries = c())


  #Estimating the model
  w.list <- w.opt(z.list)

  #Leave-one-out' robustness check
  loo.obj <- loo.fun(z.list)

  ret <- list(z_list = z.list, w_list = w.list, loo.obj = loo.obj)
  ret
}
