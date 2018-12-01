#========================================================#
# STAT 6021: Linear Models for Data Science              |
# R tutorial for learning unit 9                         |
# R functions                                            |
#========================================================#
#                                                        |
# -------------------------------------------------------+
# R function for stepwise variable selection methods     |
# -------------------------------------------------------+
#                                                        |

get.model.str <- function(var.in, resp.name, reg.names) {
	var.in.idx <- which(var.in)
	model.str <- paste(resp.name, "~")
	first.in <- TRUE
	for (iVAR in var.in.idx) {
		if (first.in) {
			model.str <- paste(model.str, reg.names[iVAR])
			first.in <- FALSE
		} else {
			model.str <- paste(model.str, "+", reg.names[iVAR])
		}
	}
	return(model.str)
}

eval.lm <- function(model.str, data.name) {
	lm.call.str <- paste("reg.lm <- lm(", model.str, ", data=", data.name, ")")
	eval(parse(text=lm.call.str))
	return(reg.lm)
}

forward.step <- function(curr.var.in, alpha.in, resp.name, reg.names, data.name) {
	curr.var.out.idx <- which(!curr.var.in)
	enter.idx <- NA
	if (length(curr.var.out.idx) > 0) {
		k <- length(reg.names)
		pval.seq <- rep(x=Inf, times=k)
		for (iVAR in curr.var.out.idx) {
			cand.var.in <- curr.var.in
			cand.var.in[iVAR] <- TRUE
			cand.model.str <- get.model.str(cand.var.in, resp.name, reg.names)
			cand.model.lm <- eval.lm(cand.model.str, data.name)
			iROW <- which(row.names(summary(cand.model.lm)$coefficients) == reg.names[iVAR])
			pval.seq[iVAR] <- summary(cand.model.lm)$coefficients[iROW,4]
		}
		enter.idx <- which.min(pval.seq)
		if (pval.seq[enter.idx] < alpha.in) {
			print(paste("Variable ", reg.names[enter.idx], " enters the model (pval=", sprintf("%6.4f", pval.seq[enter.idx]), ")", sep=""))
		} else {
			print("No variables enter the model")
			enter.idx <- NA
		}
	} else {
		print("No variables available to enter the model")
	}
	return(enter.idx)
}

backward.step <- function(curr.var.in, alpha.out, resp.name, reg.names, data.name) {
	curr.var.in.idx <- which(curr.var.in)
	leave.idx <- NA
	if (length(curr.var.in.idx) > 0) {
		k <- length(reg.names)
		pval.seq <- rep(x=-Inf, times=k)
		curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
		curr.model.lm <- eval.lm(curr.model.str, data.name)
		for (iVAR in curr.var.in.idx) {
			iROW <- which(row.names(summary(curr.model.lm)$coefficients) == reg.names[iVAR])
			pval.seq[iVAR] <- summary(curr.model.lm)$coefficients[iROW,4]
		}
		leave.idx <- which.max(pval.seq)
		if (pval.seq[leave.idx] >= alpha.out) {
			print(paste("Variable ", reg.names[leave.idx], " leaves the model (pval=", sprintf("%6.4f", pval.seq[leave.idx]), ")", sep=""))
		} else {
			print("No variables leave the model")
			leave.idx <- NA
		}
	} else {
		print("No variables available to leave the model")
	}
	return(leave.idx)
}

forward.selection <- function(alpha.in, resp.name, reg.names, data.name) {
	k <- length(reg.names)
	curr.var.in <- rep(x=FALSE, times=k)
	stop <- FALSE
	while(!stop) {
		enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
		if (is.na(enter.idx)) {
			stop <- TRUE
		} else {
			curr.var.in[enter.idx] <- TRUE
		}
	}
	curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
	print(paste("Final model: ", curr.model.str, sep=""))
	curr.model.lm <- eval.lm(curr.model.str, data.name)
	return(curr.model.lm)
}

backward.elimination <- function(alpha.out, resp.name, reg.names, data.name) {
	k <- length(reg.names)
	curr.var.in <- rep(x=TRUE, times=k)
	stop <- FALSE
	while(!stop) {
		leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
		if (is.na(leave.idx)) {
			stop <- TRUE
		} else {
			curr.var.in[leave.idx] <- FALSE
		}
	}
	curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
	print(paste("Final model: ", curr.model.str, sep=""))
	curr.model.lm <- eval.lm(curr.model.str, data.name)
	return(curr.model.lm)
}

stepwise.selection <- function(alpha.in, alpha.out, resp.name, reg.names, data.name) {
	k <- length(reg.names)
	curr.var.in <- rep(x=FALSE, times=k)
	stop <- FALSE
	while(!stop) {
		enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
		if (is.na(enter.idx)) {
			stop <- TRUE
		} else {
			curr.var.in[enter.idx] <- TRUE
			leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
			if (!is.na(leave.idx)) {
				curr.var.in[leave.idx] <- FALSE
				if (leave.idx == enter.idx) {
					stop <- TRUE
				}
			}
		}
	}
	curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
	print(paste("Final model: ", curr.model.str, sep=""))
	curr.model.lm <- eval.lm(curr.model.str, data.name)
	return(curr.model.lm)
}

# -------------------------------------------------------+
# R function for Gram-Schmidt orthonormalization         |
# -------------------------------------------------------+

orthnorm.GS <- function(X0.mat) {
	n <- dim(X0.mat)[[1]]
	k <- dim(X0.mat)[[2]]
	ones <- rep(x=1, times=n)
	X.mat <- as.matrix(cbind(ones, X0.mat))
	p <- dim(X.mat)[[2]]
	U.mat <- as.matrix(ones)
	for (j in 1:k) {
		y.vect <- X0.mat[,j]
		UpU.mat <- t(U.mat) %*% U.mat
		UpU.inv <- solve(UpU.mat)
		Upy.mat <- t(U.mat) %*% y.vect
		b.hat <- UpU.inv %*% Upy.mat
		res <- y.vect - U.mat %*% b.hat
		U.mat <- as.matrix(cbind(U.mat, res / sqrt(sum(res^2))))
	}
	U0.mat <- U.mat[,2:p]
	return(U0.mat)
}

# -------------------------------------------------------+
# R function for orthonormalization through an           |
# eigenvalue-eigenvector decomposition                   |
# -------------------------------------------------------+

orthnorm.eigen <- function(X0.mat) {
	n <- dim(X0.mat)[[1]]
	k <- dim(X0.mat)[[2]]
	# Unit-length scaling
	X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
	x.bar.list <- numeric(length=k)
	css.list <- numeric(length=k)
	for (j in 1:k) {
		x.bar.list[j] <- mean(X0.mat[,j])
		xj.cent <- X0.mat[,j] - x.bar.list[j]
		css.list[j] <- sum(xj.cent^2)
		X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
	}
	XpX.mat.scl <- t(X.mat.scl) %*% X.mat.scl
	# Orthonormalize
	eig.out <- eigen(XpX.mat.scl)
	Lambda.inv.sqrt <- diag(1/sqrt(eig.out$values))
	T.mat <- eig.out$vectors
	U0.mat <- X.mat.scl %*% T.mat %*% Lambda.inv.sqrt
	return(U0.mat)
}

# -------------------------------------------------------+
# R function for orthonormalization by Cholesky          |
# decomposition                                          |
# -------------------------------------------------------+

orthnorm.Chol <- function(X0.mat) {
	n <- dim(X0.mat)[[1]]
	k <- dim(X0.mat)[[2]]
	# Unit-length scaling
	X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
	x.bar.list <- numeric(length=k)
	css.list <- numeric(length=k)
	for (j in 1:k) {
		x.bar.list[j] <- mean(X0.mat[,j])
		xj.cent <- X0.mat[,j] - x.bar.list[j]
		css.list[j] <- sum(xj.cent^2)
		X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
	}
	XpX.mat.scl <- t(X.mat.scl) %*% X.mat.scl
	# Orthonormalize
	T.mat <- chol(XpX.mat.scl)
	T.inv <- solve(T.mat)
	U0.mat <- X.mat.scl %*% T.inv
	return(U0.mat)
}

# -------------------------------------------------------+
# R function for the DUPLEX algorithm                    |
# -------------------------------------------------------+

DUPLEX.alg <- function(U0.mat) {
	n <- dim(U0.mat)[[1]]
	#k <- dim(U0.mat)[[2]]
	mode <- "est"
	est.idx.list <- numeric(length=0)
	pred.idx.list <- numeric(length=0)
	rem.idx.list <- 1:n
	nREM <- length(rem.idx.list)
	while (nREM > 1) {
		max.dist <- -Inf
		for (i in 1:(nREM-1)) {
			for (j in (i+1):nREM) {
				diff <- U0.mat[rem.idx.list[i],] - U0.mat[rem.idx.list[j],]
				dist <- sqrt(sum(diff^2))
				if (dist > max.dist) {
					max.dist <- dist
					idx.pair <- rem.idx.list[c(i,j)]
				}
			}
		}
		if (mode == "est") {
			est.idx.list <- c(est.idx.list, idx.pair)
			mode <- "pred"
		} else if (mode == "pred") {
			pred.idx.list <- c(pred.idx.list, idx.pair)
			mode <- "est"
		}
		rem.idx.list <- setdiff(rem.idx.list, idx.pair)
		nREM <- length(rem.idx.list)
	}
	if (nREM == 1) {
		if (mode == "est") {
			est.idx.list <- c(est.idx.list, rem.idx.list)
		} else if (mode == "pred") {
			pred.idx.list <- c(pred.idx.list, rem.idx.list)
		}
	}
	out.list <- list(est=est.idx.list, pred=pred.idx.list)
	return(out.list)
}

