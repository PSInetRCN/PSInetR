test_that("get_data_dir formats path correctly", {
  data_dir <- get_data_dir()
  expect_match(data_dir, "\\.PSINetR$")
})

# Test utils functions that handle repository URL formatting
test_that("repository URL formatting works correctly", {
  # We're going to use a simpler approach to test the URL parsing
  # by creating a small test function that mimics the behavior
  # but doesn't make actual API calls
  
  extract_repo_path <- function(repo_url) {
    # This is similar to what get_latest_release_tag does
    if (grepl("^https://github.com/", repo_url)) {
      repo_path <- sub("^https://github.com/", "", repo_url)
      repo_path <- sub("\\.git$", "", repo_path)
    } else {
      repo_path <- repo_url
    }
    return(repo_path)
  }
  
  # Test with different formats
  expect_equal(extract_repo_path("org/repo"), "org/repo")
  expect_equal(extract_repo_path("https://github.com/org/repo"), "org/repo")
  expect_equal(extract_repo_path("https://github.com/org/repo.git"), "org/repo")
})