#!/bin/bash

# Fetch the latest changes from the remote repository
git fetch origin

# Get the list of all branches excluding the main branch
branches=$(git branch -a | grep -v "main")

# Iterate over each branch and merge main into it
for branch in $branches; do
  # Checkout the branch
  git checkout $branch
  # Merge main into the branch
  git merge origin/main
  # Push the updated branch to the remote repository
  git push origin $branch
done

# Checkout the main branch again
git checkout main
