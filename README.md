# BIOS694

# Replication: BART for Individualized Treatment Rules

This project replicates key simulation results from the paper:

**Logan et al. (2019)**  
*Decision making and uncertainty quantification for individualized treatments using Bayesian Additive Regression Trees*

---

##  Objectives

- Evaluate BART’s performance in estimating individualized treatment effects.
- Compare the default BART model (`BARTd`) and a cross-validated version (`BARTcv`).
- Replicate Figures 2 and 3 from the original paper using simulation.

---

##  Components

### Figure 3a–c:  
- Uses a single **small training set** (`n = 500`).
- Compares **true outcome probabilities** vs. **posterior means** under treatment `A = 0` and `A = 1`.
- Also evaluates estimation of treatment effect differences.

### Figure 3g:  
- Replicates the treatment effect estimation using **400 small training sets**.
- Assesses bias and shrinkage in posterior estimates.

### Figure 3b–d–f:  
- Uses a **large training set** (`n = 5000`) and replicates the above evaluations.

### Figure 3h:  
- Runs the same evaluation as 3g but with **400 large training sets**.

### Figure 2: Performance Comparison Across Scenarios

This figure compares two BART-based individualized treatment rule (ITR) methods:

- **BARTd**: BART using default parameters (`ntree = 200`, `k = 2.0`).
- **BARTcv**: BART with hyperparameters (`ntree`, `k`) selected using 5-fold cross-validation to maximize value function performance.

#### Simulation Procedure

- Four representative scenarios were selected from the original paper’s 23:
  - **K1** and **K3**: both performed reasonably well.
  - **K4** and **K6**: BARTd performed suboptimal.
  
- For each scenario:
  - A fixed **test set** of 2000 samples is generated.
  - For **50 replicates**, training data of size `n = 500` are sampled.
  - Both BARTd and BARTcv models are trained on each training set.
  - Predictions for treatment `A=0` and `A=1` are generated on the test set.
  - For each patient, the model selects the treatment with the higher predicted outcome.
  - The **value function** is computed: the average predicted outcome for the recommended treatment.
  - Each method’s value is **normalized** by the true optimal value function:
  
    ```
    Value Ratio = Estimated Value / True Optimal Value
    ```

- Final values are stored in a table `Result_K`, which summarizes average performance across replications.

#### Output Plot

- A line plot (created using `ggplot2`) visualizes performance across scenarios.
- X-axis: scenario label (K1, K3, K4, K6).
- Y-axis: fraction of optimal value.
- Each line represents one method:
  - **Red** for `BARTd`
  - **Black** for `BARTcv`


---

## Dependencies

This project requires the following R packages:

```r
install.packages(c("BART", "ggplot2", "dplyr", "tidyr"))

# You may also need:
devtools::install_github("pachamaltese/speff2trial")
