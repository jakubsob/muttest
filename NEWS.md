# 0.1.0

- ✨ Create a testing plan with `plan`.
- ✨ Run mutation testing with `muttest`.
- ✨ Support mutating operators with `operator`.
- ✨ Control copying project to temporary directory with `CopyStrategy`:
  - `PackageCopyStrategy` implemented for copying package files.
- ✨ Control test execution for each mutant with `TestStrategy`:
  - `FullTestStrategy` for running all tests for each mutant.
  - `FileTestStrategy` for running only test files matching mutant files.
- ✨ See test results with `MutationReporter`.
  - `ProgressMutationReporter` for printing progress to the console.
