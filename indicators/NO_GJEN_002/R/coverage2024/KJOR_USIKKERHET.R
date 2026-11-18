args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Bruk: Rscript KJOR_USIKKERHET.R modellgrunnlag.csv [gruk_modellgrunnlag.csv]")
}
if (!requireNamespace("glmmTMB", quietly = TRUE)) stop("Installer glmmTMB i R.")
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
source(file.path(script_dir, "delta.R"))
read_data <- function(path) {
  d <- read.csv(path, fileEncoding = "UTF-8")
  stopifnot(all(c("id","program","flate","region","score") %in% names(d)),
            !anyNA(d[c("id","program","flate","region","score")]),
            !anyDuplicated(d$id), all(is.finite(d$score)),
            all(d$score >= 0 & d$score <= 1))
  d$program <- factor(d$program)
  d$region <- factor(d$region)
  d$gruppe <- interaction(d$program, d$flate, drop = TRUE)
  d
}

fit_one <- function(d, formula, scenario, regional) {
  notes <- character()
  model <- tryCatch(withCallingHandlers(
    glmmTMB::glmmTMB(formula, data = d, family = glmmTMB::ordbeta()),
    warning = function(w) {
      notes <<- c(notes, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = identity)
  diagnostic <- data.frame(modell = scenario, regional = regional,
                           formel = paste(deparse(formula), collapse = ""),
                           konvergens = NA_integer_, positiv_hessian = NA,
                           sd_flate = NA_real_, sd_program = NA_real_,
                           merknad = paste(notes, collapse = " | "))
  empty <- list(diag = diagnostic, result = NULL)
  if (inherits(model, "error")) {
    empty$diag$merknad <- paste(diagnostic$merknad, conditionMessage(model))
    return(empty)
  }
  vc <- glmmTMB::VarCorr(model)$cond
  sd_for <- function(name) if (is.null(vc[[name]])) 0 else unname(attr(vc[[name]], "stddev")[1])
  diagnostic$sd_flate <- sd_for("gruppe")
  diagnostic$sd_program <- sd_for("program")
  diagnostic$konvergens <- model$fit$convergence
  diagnostic$positiv_hessian <- isTRUE(model$sdr$pdHess)
  if (!isTRUE(diagnostic$konvergens == 0L && diagnostic$positiv_hessian)) {
    return(list(diag = diagnostic, result = NULL))
  }

  g <- expand.grid(program = levels(d$program),
                   region = if (regional) levels(d$region) else levels(d$region)[1],
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  g$program <- factor(g$program, levels = levels(d$program))
  g$region <- factor(g$region, levels = levels(d$region))
  g$gruppe <- factor(NA_character_, levels = levels(d$gruppe))
  fixed <- if (regional) "0 + region" else "1"
  if (scenario != "Joachims_forslag") fixed <- paste(fixed, "+ program")
  X <- model.matrix(as.formula(paste("~", fixed)), g)
  X <- X[, names(glmmTMB::fixef(model)$cond), drop = FALSE]
  p <- model$sdr$par.fixed
  beta <- which(names(p) == "beta")
  theta <- which(names(p) == "theta")
  psi <- which(names(p) == "psi")
  stopifnot(length(beta) == ncol(X), length(psi) == 2L,
            p[psi[1]] < p[psi[2]],
            isTRUE(all.equal(sum(exp(2*p[theta])), diagnostic$sd_flate^2 + diagnostic$sd_program^2)))
  eta_check <- predict(model, newdata = g, type = "link", re.form = NA, allow.new.levels = TRUE)
  stopifnot(isTRUE(all.equal(as.numeric(X %*% p[beta]), as.numeric(eta_check), tolerance = 1e-7)))
  marginal <- function(par, xmat) {
    cut <- par[psi]
    sd <- sqrt(sum(exp(2*par[theta])))
    vapply(drop(xmat %*% par[beta]), function(eta) {
      integrate(function(z) {
        x <- eta + sd*z
        p1 <- plogis(x - cut[2])
        dnorm(z)*(p1 + (plogis(x - cut[1]) - p1)*plogis(x))
      }, -8, 8, rel.tol = 1e-10, abs.tol = 1e-11)$value
    }, numeric(1))
  }
  g$omrade <- if (regional) as.character(g$region) else "Samlet"
  result <- do.call(rbind, lapply(unique(g$omrade), function(region) {
    ix <- which(g$omrade == region)
    use <- if (regional) d[d$region == region, ] else d
    counts <- as.numeric(table(use$program)[as.character(g$program[ix])])
    weights <- list(lik_programvekt = rep(1, length(ix)), observert_programmiks = counts)
    do.call(rbind, lapply(names(weights), function(w) {
      est <- function(par) weighted.mean(marginal(par, X[ix, , drop = FALSE]), weights[[w]])
      data.frame(modell = scenario, region = region, n = nrow(use), vekting = w,
                 as.list(delta(model, est)), row.names = NULL)
    }))
  }))
  list(diag = diagnostic, result = result)
}

d <- read_data(args[1])
stopifnot(setequal(levels(d$program), c("ANO","ASO")))
fits <- list()
for (regional in c(FALSE, TRUE)) {
  rhs <- if (regional) "0 + region" else "1"
  specs <- c(Joachims_forslag = " + (1|program) + (1|gruppe)",
             Fast_program_med_flate = " + program + (1|gruppe)",
             Fast_program_uten_flate = " + program")
  for (s in names(specs)) {
    fits[[length(fits)+1L]] <- fit_one(d, as.formula(paste0("score ~ ", rhs, specs[[s]])), s, regional)
  }
}
write.csv(do.call(rbind, lapply(fits, "[[", "diag")), "modelldiagnostikk_se.csv", row.names = FALSE)
result <- do.call(rbind, lapply(fits, "[[", "result"))
stopifnot(!is.null(result))
ref <- read.csv(file.path(script_dir, "referanse_punktestimater.csv"), fileEncoding = "UTF-8")
key <- paste(ref$modell, ref$region)
ii <- match(paste(result$modell, result$region), key)
expected <- ifelse(result$vekting == "lik_programvekt",
                   ref$lik_programvekt[ii], ref$observert_programmiks[ii])
stopifnot(!anyNA(expected), max(abs(result$estimat - expected)) < 1e-5)
if (length(args) == 2L) {
  g <- read_data(args[2])
  stopifnot(identical(levels(g$program), "GRUK"), nrow(g) == 263L,
            sum(g$score == 1) == 43L, all(g$score > 0))
  g$tap <- 1-g$score
  fit <- glmmTMB::glmmTMB(tap ~ 1, ziformula = ~1, family = glmmTMB::beta_family(), data = g)
  stopifnot(isTRUE(fit$fit$convergence == 0L && fit$sdr$pdHess))
  est <- function(par) {
    beta <- par[names(par) == "beta"]
    zi <- par[names(par) == "betazi"]
    stopifnot(length(beta) == 1L, length(zi) == 1L)
    as.numeric(1 - plogis(beta)*plogis(-zi))
  }
  gruk <- delta(fit, est)
  stopifnot(abs(gruk["estimat"] - .841092274672684) < 1e-5,
            abs(gruk["estimat"] - (1-mean(predict(fit, type = "response")))) < 1e-7)
  result <- rbind(result, data.frame(modell = "GRUK_separat", region = "GRUK-utvalget",
                                   n = nrow(g), vekting = "lik_polygonvekt",
                                   as.list(gruk), row.names = NULL))
}
write.csv(result, "modellresultater_se.csv", row.names = FALSE)
main <- subset(result, (modell == "Fast_program_med_flate" & vekting == "lik_programvekt") |
                       modell == "GRUK_separat")
write.csv(main, "leveransetabell_se.csv", row.names = FALSE)
capture.output(sessionInfo(), file = "R_versjoner_se.txt")
print(main)
