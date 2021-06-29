
<!-- README.md is generated from README.Rmd. Please edit that file -->

# revpref

<!-- badges: start -->

<!-- badges: end -->

The goal of `revpref` is to provide a set of tools to test whether
consumer demand data satisfy a number of revealed preference axioms;
compute measures of goodness-of-fit when the data is inconsistent with
the axioms; and compute power against uniformly random behavior. Below
we provide a brief description of the functions provided in this
package.

  - Nonparametric tests to check consistency of the data with revealed
    preference axioms at any efficiency level
    
      - `warp` tests consistency with the Weak Axiom of Revealed
        Preference at efficiency level e and reports the number of WARP
        violations.
    
      - `sarp` tests consistency with the Strong Axiom of Revealed
        Preference at efficiency level e and reports the number of SARP
        violations.
    
      - `garp` tests consistency with the Generalized Axiom of Revealed
        Preference at efficiency level e and reports the number of GARP
        violations.
    
      - Note: for all the three functions described above (`warp`,
        `sarp` and `garp`), the user must provide a T X N price matrix
        and a T X N quantity matrix were T corresponds to the number of
        observations and N corresponds to the number of consumption
        categories. Optionally, the user can also provide an efficiency
        level (e) to check whether the observed data obeys a given axiom
        at efficiency level e. When e is not specified, it defaults to 1
        which checks consistency with the exact axiom.

  - Goodness-of-fit to measure the severity of violations
    
      - `ccei` computes the critical cost efficiency index, CCEI (also
        known as the Afriat efficiency index). CCEI is the maximal value
        of the efficiency level at which the data satisfies GARP.
        Intuitively, CCEI indicates how close the data is to satisfying
        the (strict) GARP conditions. The user needs to provide a T X N
        price matrix and a T X N quantity matrix were T corresponds to
        the number of observations and N corresponds to the number of
        consumption categories. Optionally, the user can specify the
        axiom (WARP, SARP or GARP) for which CCEI needs to be computed.
        When no axiom is specified, the function takes the default
        option as GARP.
    
      - `mpi` computes the minimum and maximum money pump index (MPI).
        Echenique et al. (2011) proposed the money pump index as a
        measure of severity of GARP violation. It is defined as the
        amount of money that an arbitrageur can “pump” from an
        irrational consumer. While the MPI concept is conceptually
        appealing, it may be computationally challenging to compute this
        index for data sets with large number of observations. In
        particular, Smeulders et al. (2013) showed that computing mean
        and median MPI is an NP-hard problem. As easy-to-apply
        alternatives, Smeulders et al. (2013) proposed the minimum and
        maximum MPIs which can be computed efficiency (in polynomial
        time). The function `mpi` implements the algorithm provided by
        these authors to compute the minimum and maximum MPI values for
        the given data.

  - Bronars’ power to capture the probability of rejecting a given
    revealed preference axiom
    
      - `bronars` computes the power index proposed by Bronars (1987).
        Bronars’ power is defined as the probability of rejecting a
        given rationality axiom given that the data is defined by an
        irrational behavior. Bronars suggests Becker’s (1962) approach
        for simulating an irrational behavior. More specifically, we
        simulate irrational demand by generating consumption choices
        that are uniformly distributed on the budget hyperplane. The
        function `power` computes the Bronars power index for any of
        three axiom (WARP, SARP, or GARP) at any efficiency level.

## Installation

You can install the released version of revpref from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("revpref")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ksurana21/revpref")
```

## Example

Below we provide some simple examples to illustrate the three types of
functionality available within the package. To begin with we define
price and quantity matrices. Both of these matrices (defined below) have
ten rows corresponding to the number of observations and three columns
corresponding to the number of consumption categories.

``` r

# Load the package
library(revpref)

# Define a price matrix 
p = matrix(c(4, 4, 4, 1, 9, 3, 2, 8, 3, 1, 
             8, 4, 3, 1, 9, 3, 2, 8, 8, 4,
             1, 4, 1, 8, 9, 3, 1, 8, 3, 2),
           nrow = 10, ncol = 3, byrow = TRUE)

# Define a quantity matrix
q = matrix(c( 01.81, 00.19, 10.51, 17.28, 02.26, 04.13, 12.33, 02.05, 02.99, 06.06,
              05.19, 00.62, 11.34, 10.33, 00.63, 04.33, 08.08, 02.61, 04.36, 01.34,
              09.76, 01.37, 36.35, 01.02, 03.21, 04.97, 06.20, 00.32, 08.53, 10.92),
           nrow = 10, ncol = 3, byrow = TRUE)
```

**Nonparametric tests**

First, we check whether or not our data set is consistent with the
rationality axioms. We begin with GARP.

``` r

# Test consistency with GARP and compute the number of GARP violations
result <- garp(p, q)
result
#> [1] 0 8
```

The first element is a binary indicator telling us whether or not the
data set passed the rationality test. Here, we see that the first
element is 0 which means that the data set failed to satisfy GARP
restrictions. The second element indicates that there are 8 GARP
violations. In this example, we did not specify the efficiency level, in
which case the default value of 1 was applied. In the next example, we
check GARP at 90% efficiency level.

``` r

# Test consistency with GARP and compute the number of GARP violations at 90% efficiency
result <- garp(p, q, efficiency = 0.90)
result
#> [1] 1 0
```

Here we see that the first element is 1 which means that the data set is
consistent with GARP at 90% efficiency level. As expected, the second
element is 0 indicating that there are no GARP violations at this
efficiency level. Next, we follow similar procedures to test consistency
with SARP and WARP at full efficiency.

``` r

# Test consistency with SARP and compute the number of SARP violations
result <- sarp(p, q)
result 
#> [1] 0 8

# Test consistency with WARP and compute the number of WARP violations
result <- warp(p, q)
result 
#> [1] 0 1
```

We see that the data set is inconsistent with both SARP and WARP
conditions. All the three tests revealed that our data set failed to
satify the rationality hypothesis. However, these tests did not indicate
how close our data set was to satisfying the axioms. In the next step,
we compute goodness-of-fit indices to measure the severity of
violations.

**Goodness-of-fit**

The package provides functionalities for two goodness-of-fit measures.
The first measure is the critical cost efficiency index (CCEI) which is
defined as maximal efficiency level such that the data is consistent
with rationality restrictions. From the previous exercises, we know that
our data set failed to satisfy GARP at full efficiency but is consistent
with GARP at 90% efficiency. As such we can expect the CCEI to be
greater than 0.9 but less than 1.

``` r

# Compute the Critical Cost Efficiency Index (CCEI)
result <- ccei(p, q)
result
#> [1] 0.9488409
```

Using the function `ccei`, we find that the highest efficiency level at
which the data set is consistent with GARP is equal to 0.9488. This
indicates that although our data set is inconsistent with strict GARP
restrictions, it is very close to satisfying the rationality
restriction. We can also compute CCEI for SARP and WARP by providing an
additional argument (model = “SARP” or model = “WARP”).

``` r

# Compute the Critical Cost Efficiency Index (CCEI) for SARP
result <- ccei(p, q, model = "SARP")
result
#> [1] 0.9488409
```

The second goodness-of-fit measure which can be computed through this
package is the money pump index (MPI). We can compute the minimum and
maximum MPI using the function `mpi` as follows,

``` r

# Compute the minimum and maximum Money Pump Index (MPI)
result <- mpi(p, q)
result
#> [1] 0.07242757 0.18732824
```

The output tells us that the minimum and maximum MPI for this data set
are 0.07243 and 0.1873, respectively. This means that the consumer is
losing 7.24% of money in the least severe violation and 18.73% of money
in the most severe violation. Implicitly, these values provide a bound
on the amounts of money that can be pumped from this irrational
consumer.

**Power**

In addition to goodness-of-fit, empirical studies usually also report
the power measure. As discussed above, the power of a revealed
preference test is a measure of the likelihood of detecting an
irrational behavior. We can use the function `bronars` to compute
Bronars’ power index for the given data set.

``` r

# Compute power 
result <- bronars(p, q)
result
#> [1] 0.866
```

We find that for the given budget conditions, there is about 86.60%
probability of detecting an irrational behavior. As discussed above,
here we have used Becker’s (1962) approach of using uniformly random
consumption choices to simulate irrational consumer. As a final
exercise, we analyze how the power of our GARP test changes with the
efficiency level.

``` r

# Compute power of the GARP test at efficiency levels between 0.4 to 1
power <- c()
for(i in seq(0.4, 1, by = 0.05)){
  power = rbind(power, c(i, bronars(p, q, model = "GARP", simulation = 1000, 
                                    efficiency = i)))
}

# Plot the power measures against efficiency levels
power <- as.data.frame(power)
names(power) <- c("efficiency", "power")
plot(power, type = "b",col = "black", lwd = 1, pch = 20, bty = "L")
```

<div class="figure" style="text-align: center">

<img src="man/figures/README-unnamed-chunk-8-1.png" alt="\label{fig} Power versus efficiency of GARP test" width="75%" />

<p class="caption">

Power versus efficiency of GARP test

</p>

</div>

As depicted in the Figure above, the power of the GARP test increases
with the efficiency level. This is expected as the lower is the
efficiency level, the weaker is the test. In other words, with lower
values of efficiency, a larger fraction of the simulated data will be
able to pass the test and hence the chances of rejecting GARP will be
smaller.

## References

  - Becker, Gary S. “Irrational behavior and economic theory.” Journal
    of political economy 70, no. 1 (1962): 1-13.

  - Bronars, Stephen G. “The power of nonparametric tests of preference
    maximization.” Econometrica: Journal of the Econometric Society
    (1987): 693-698.

  - Echenique, Federico, Sangmok Lee, and Matthew Shum. “The money pump
    as a measure of revealed preference violations.” Journal of
    Political Economy 119, no. 6 (2011): 1201-1223.

  - Smeulders, Bart, Laurens Cherchye, Frits CR Spieksma, and Bram De
    Rock. “The money pump as a measure of revealed preference
    violations: A comment.” Journal of Political Economy 121, no. 6
    (2013): 1248-1258.
