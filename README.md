# forecasting-example

This module contains practical exercises for forecasting backlog completion periods.

At Armakuni, we often find ourselves having to explain why customers should point complexity rather than time and should forecast rather than commit to dates. There are well-defined techniques for doing so, such as PERT and Monte Carlo simulations. It is very useful to be able to walk a customer through the theory behind them, how they can use them with their existing story trackers (e.g. Jira), and what techniques they can use to improve estimate accuracy.

By completing these exercises, you should be in a position to confidently do this.

## Exercise 1: PERT

In this exercise you will learn about the Program Evaluation and Review Technique (PERT). This is a tool that can be used to estimate the elapsed time for a backlog, where the completion time for each task is uncertain.

PERT is a good tool, but has limitations if the task chain becomes complex - e.g. if the critical path is sensitive to stochastic completion times.

## Exercise 2: Monte Carlo simulation

In this exercise, you will learn how to perform a single-worker Monte Carlo simulation, where you repeatedly and randomly simulate the backlog work in order to build up an overall probability distribution for the entire backlog.

## Exercise 3: Monte Carlo simulation with a team

In this exercise, you will extend the single-worker Monte Carlo simulation to a more realistic team effort, where all team members work from the same backlog.

## Exercise 4: Monte Carlo simulation with assigned work

In this exercise, you will tackle a case where work is pre-assigned to specific workers, to observe the effect this has on forecast completion time and variability.

## Exercise 5: Improving confidence intervals

In this exercise, you will examine the effect that getting better at estimation has on the confidence interval for backlog completion.

## Exercise 6: Conclusion

Finally, we will have a discussion about what this means for the customer. We will describe aspects of story writing and ways of working that affect your confidence in your forecasts. We will talk about how the customer can use the language of uncertainty when interacting with their stakeholders.