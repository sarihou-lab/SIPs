#MPR= 0.280I(2.0163) + 0.336K(2.0163) + 0.392Q(2.0163) - 1.0031,

# Computation of I(2.0163)
Dt <- 0.0001
x <- seq(2.0163, 4, by = Dt)

# integrand at each x point
y <- exp(-0.5 * x^2) * pnorm(5.0408 - 2*x)

# Compute the full expression: N(2.0163) + (1/sqrt(2pi)) * Riemann sum
I <- pnorm(2.0163) + sum(y) * Dt / sqrt(2 * pi)

# Print result
I

# Computation of K(2.0163)

J <- numeric(39675)

u <- 1:48337
u <- (u - 48337) * Dt

for (i in 1:39675){
  x_val <- (i - 29593) * Dt          # current x value
  y <- exp(-0.5*(x_val - u)^2) * pnorm(u + 0.83358)
  J[i] <- pnorm(x_val) + sum(y) * Dt / sqrt(2*pi)
}

x <- seq(2.0163, 4, by = Dt)

i_idx <- (5.0408 - 2*x) * 10000 + 29593
i_idx <- trunc(i_idx)

# Outer integrand
y_outer <- exp(-0.5 * x^2) * J[i_idx]

# Final result
K <- pnorm(2.0163) + sum(y_outer) * Dt / sqrt(2*pi)
K


#Computation of Q(2.0163)


# Grids
u_L <- seq(-4.8240, 0, by = Dt)
u_M <- seq(-4.8334, 0, by = Dt)
x_main <- seq(2.0163, 4, by = Dt)

# We need L evaluated at (u_M + 0.8334)
x_L_grid <- u_M + 0.8334

# Precompute inner term
phi_uL <- pnorm(u_L + 0.8240)

L_vals <- numeric(length(x_L_grid))

for (k in seq_along(x_L_grid)) {
  x <- x_L_grid[k]
  integrand <- exp(-0.5 * (x - u_L)^2) * phi_uL
  L_vals[k] <- pnorm(x) + sum(integrand) * Dt / sqrt(2*pi)
}

# Points where we need M
x_M_grid <- 5.0408 - 2 * x_main

M_vals <- numeric(length(x_M_grid))

for (k in seq_along(x_M_grid)) {
  x <- x_M_grid[k]
  integrand <- exp(-0.5 * (x - u_M)^2) * L_vals
  M_vals[k] <- pnorm(x) + sum(integrand) * Dt / sqrt(2*pi)
}

integrand <- exp(-0.5 * x_main^2) * M_vals
Q <- pnorm(2.0163) + sum(integrand) * Dt / sqrt(2*pi)

Q

MPR <- 0.280*I + 0.336*K + 0.392*Q - 1.0031
MPR
