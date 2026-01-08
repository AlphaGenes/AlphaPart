# Contributing to AlphaPart

We want to start by thanking you for taking the time to contribute!

The following is a set of guidelines for contributing to AlphaPart and
its packages hosted in the [AlphaGenes
Organization](https://github.com/AlphaGenes) on GitHub. We suggest using
your best judgment, and feel free to propose changes to this document in
a pull request.

------------------------------------------------------------------------

#### Table Of Contents

------------------------------------------------------------------------

[I have only a question!](#i-have-only-a-question)

[What should I know before I get
started?](#what-should-i-know-before-i-get-started)

- [AphaPart package](#alphapart-package)
- [AlphaPart Decisions](#design-decisions)

[How Can I Contribute?](#how-can-i-contribute)

- [Reporting Bugs](#reporting-bugs)
- [Pull Requests](#pull-requests)

------------------------------------------------------------------------

## I have only a question!

We encourage people from opening an issue to ask a question or report a
bug.

------------------------------------------------------------------------

## What should I know before I get started?

### AlphaPart package

AlphaPart is an open-source project. When you initially consider
contributing to AlphaPart, you might be sure about using the
AlphaGenes/AlphaPart repository to implement the functionality you want
to change or report a bug. This section should help you with that.

AlphaPart has intentionally coded using standard `R` to build the base
idea and `C++` using the `Rcpp` package to speed up the analysis. `R`
code can be founded in the usual `R` folder while `C++` is in the `scr`
folder.

### Design Decisions

When we make a significant decision in upgrading the package version, we
will document it in the [News
file](https://alphagenes.github.io/AlphaPart/NEWS.md). If you question
how we have been updating the package and fixed bugs, check to see if it
is reported there. If it is *not* documented there, please open a new
issue on
[AlphaGenes/AlphaPart](https://github.com/AlphaGenes/AlphaPart/issues)
and ask your question.

------------------------------------------------------------------------

## How Can I Contribute?

### Reporting Bugs

This section guides you through submitting a bug report for AlphaPart.
Following these guidelines helps maintainers and the community
understand your message, reproduce the reported behaviour, and find
related reports.

Before creating bug reports at
[AlphaGenes/AlphaPart](https://github.com/AlphaGenes/AlphaPart/issues),
please check if someone already did the same report. Thus you don’t need
to make another one. When you create a bug report, please include as
many details as possible to resolve issues faster.

**Note:** If you find a **Closed** issue that seems like it is the same
thing that you’re experiencing, open a new issue and include a link to
the original issue in the body of your new one.

#### How to submit a (good) bug report?

Bugs are tracked as [GitHub
issues](https://guides.github.com/features/issues/). We expected you to
explain the problem and include additional information to help
maintainers reproduce the problem.

- **Use a clear and informative title** for the issue to identify the
  problem you are experiencing.

- **Describe the exact steps which reproduce the problem** in as many
  details as possible. In this case, you can use a tiny dataset as an
  example to demonstrate the steps.

- If applicable, please, **explain which behaviour you expected to see
  instead and why you expected that.**

- **If the problem is related to package performance or computer
  memory**, include a CPU and Memory profile with your report. You may
  want to start by looking into `top` or `htop` to see the processes
  overview in the Unix operating system (Linux and MAC). On the other
  hand, you can open the Performance Analyzer log for the Windows system
  and try to understand why the issue is occurring.

### Pull Requests

What we are looking for when accepting a pull request:

- Maintain AlphaPart quality
- Fix problems that are important to end-users
- Engage the community in working toward the best possible AlphaPart

A [pull
request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests)
is a way to suggest changes in our repository.

When we merge those changes, they should be deployed to the CRAN
repository as soon as we have enough tests and changes in the
development version. To learn more about opening a pull request in this
repository, see [Opening a pull request](#opening-a-pull-request) below.

### Opening a pull request

You can use the GitHub user interface for minor changes, such as fixing
a typo or updating a readme or function. You can also fork the repo and
then clone it locally to view changes and run tests in your machine.

### Submit your pull request and get it reviewed by our team

- Once you submit your PR, others from the AlphaPart team will review
  it.
- After that, we may have questions about your PR, and we may ask you to
  solve them.
- Did you have an issue, such as a merge conflict? Check out our [git
  tutorial](https://lab.github.com/githubtraining/managing-merge-conflicts)
  on resolving merge conflicts and other issues.

Once your PR is successfully merged, you will be proudly listed as a
contributor in the [contributor
chart](https://github.com/AlphaGenes/AlphaPart/graphs/contributors).

------------------------------------------------------------------------
