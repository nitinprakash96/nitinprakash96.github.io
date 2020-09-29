Follow the following commands to deploy a hakyll site with github pages.

```
# Temporarily store uncommited changes
git stash

# Verify correct branch
# I work on develop branch for most of my changes. Can specify according to need
git checkout develop

# Build new files
stack exec site clean
stack exec msite build

# Get previous files
git fetch --all
# Condidering master branch as the safe branch here
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a _site/. .

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin master:master

# Restoration
git checkout develop
git branch -D master
git stash pop
```
