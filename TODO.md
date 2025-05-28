# PSINetR Development To-Do List

## Package Metadata
- [x] Fill in actual author information in DESCRIPTION file
- [x] Update GitHub username references in README.md

## Data Source Configuration
- [ ] Add specific repository URL information in download_from_repo() function
- [ ] Add specific Zenodo DOI information in download_from_zenodo() function
- [ ] Consider adding package options for default URLs/DOIs

## Testing & Validation
- [ ] Test data pulls from the actual repository
- [ ] Add test data to inst/extdata for unit testing
- [ ] Expand test coverage for error conditions and edge cases
- [ ] Test with both public and private repository access

## Documentation
- [ ] Add database schema information to package
- [ ] Build out vignettes with real data examples
- [ ] Create a vignette specifically on data structure and variables
- [ ] Add function to display schema information
- [ ] Include visualizations of example analyses in vignettes

## Additional Features
- [ ] Add data exploration helper functions
- [ ] Consider adding visualization functions for common use cases
- [ ] Add data validation functions to check downloaded data integrity

## CI/CD
- [x] Add renv.lock with formal package dependency locking
- [x] Configure test coverage tracking (local implementation instead of codecov)
- [ ] Consider implementing version-specific data access

## Community
- [ ] Add CONTRIBUTING.md with guidelines for contributors
- [ ] Create issue templates for GitHub
- [ ] Set up discussions on GitHub repository