#repo_dir <- rstudioapi::selectDirectory(caption = "C:/Users/swaka/Documents/GitHub/grade-estimator")
#file.create("C:/Users/swaka/Documents/GitHub/grade-estimator/.nojekyll")

###

# update_site.R
# Rebuilds the Shinylive website from app/app.R into the docs/ folder.
# Keep this file in the top level of your grade-estimator repository.
# How to use: open this file in RStudio and click "Source".

repo_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
app_dir  <- file.path(repo_dir, "app")
site_dir <- file.path(repo_dir, "docs")

if (!file.exists(file.path(app_dir, "app.R")))
  stop("Can't find app/app.R next to this script. Is update_site.R in the repository folder?")

shinylive::export(app_dir, site_dir)
file.create(file.path(site_dir, ".nojekyll"))

message("Done! Now open GitHub Desktop, click 'Commit to main', then 'Push origin'.")

#app_dir  <- "C:/Users/YOUR-NAME/Documents/grade_estimator"
#repo_dir <- "C:/Users/YOUR-NAME/Documents/GitHub/grade-estimator"
