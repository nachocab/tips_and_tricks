# reference
http://gitref.org/index.html

# undo git add - unadd, remove from index?
git reset

# create a new repository in github
touch README.md
git init
git add . && git commit -m "It begins."
git create -d "My new thing" # (using hub: creates a new project on GitHub with the name of current directory)
git push -u origin master

# git remote add origin https://github.com/nachocab/leanback.git

# push to created repository (origin)
git push -u origin master

# discard unsaved changes from file(s) and restore an old version: commit first!
git co 82f5 my_old_file my_other_old_file

# remove a file from git index and the filesystem (use --cached to not remove it)
git rm my_file
git rm -r my_dir/

# remove a file from git index: unstage it.(but don't remove it from filesystem) reset staged changes
git rm --cached my_file

# unstage a file (removes the modification from the index, no the entire file)
git reset my_file

# discard unstaged changes on a single file
git checkout path/to/file

# discard all unstaged changes,
git checkout -- .

# find search text string contents
git grep <regexp> $(git rev-list --all)

# revert reset reload, checkout file from previous commit and overwrite current version
git co HEAD main.html

# rename a file inside git
git mv my_file your_file

# only stage tracked (known) files
git add -u

# show contents of last commit as a diff
git show
git show --name-only

# show contents of a file not as a diff
git show HEAD:file

# show unstaged contents (what you could still add to the index) compared to the last commit
git diff
# show the names of the files with unstanged changes
git diff --name-only
# show unstanged contents compared to an older commit
git diff HEAD~ # the parent of HEAD
git diff "@{yesterday}"
git whatchanged --since="2 weeks ago"

# show difference between two commits
git diff 1b6d "master~2"

# show staged (indexed) contents
git diff --cached

# show tracked files
git ls-files

# create a new branch and move to it (one step)
git checkout -b my_new_branch
# create a new branch (name current commit) (two steps)
git branch my_new_branch
# switch to new branch
git co my_new_branch

# go back three commits (you're at the 4th commit, go back to 1)
# this puts you in an unnamed branch, so you can edit it and name it with git checkout -b dirty_branch
# use case: you need to fix a production bug.
git co HEAD~3
git co master~3

# go to commit by name (it will switch branches if necessary)
git co :/"fixed that thing"

# save current work and fix a bug
git commit -a # you're saving the current state fo fix an older commit, you could use a stash
git checkout -b my_fixes 1b6d # fix it
git commit -a -m "Bug fixed"
git checkout master # go back to what you were doing
git merge my_fixes # merge the temporary branch

# delete a branch
git branch -d my_fixes

# When you realize you should have created a branch 7 commits ago
git branch -m master part2  # Rename "master" branch to "part2". -m => move, rename
git branch master HEAD~7    # Create new "master", 7 commits upstream. We're still in part2 branch
git checkout HEAD~7 -b master  # Create a branch, and switch to it.

# refer to the second parent (the current branch at the time of the merge is the first parent)
git log HEAD^2
# refer to the first parent
git log HEAD^
# refer to the second parent of the first parent
git log HEAD^^2

# List all branches
git branch

# save current state in a temporary branch and go back to the previous commit
git stash
# go back to the stashed state, you many need to resolve some conflicts
git stash apply

# change the last commit message
git commit --amend
# add a file to the last commit
git commit --amend -a

# remove files from previous commit (made a mistake)
git reset --soft HEAD^
git rm --cached my_file
git commit -c ORIG_HEAD
git commit -C ORIG_HEAD # don't re-edit the commit message

# delete every commit up to 766f
git reset --hard 766f

# undo a previous commit without deleting it
git revert 766f

# web viewer
git instaweb
git instaweb --stop

# add interactively
git add -p
git add -i

# see deleted hashes
git reflog

# go back to older commit
git checkout "@{10 minutes ago}"
git checkout "@{5}"

# add a remote branch
git remote add origin git@github.com:username/Hello-World.git
git pull origin master
git push -u origin master