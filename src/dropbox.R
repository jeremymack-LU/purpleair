# Update data on Dropbox --------------------------------------------------
transfer_plots <- function(file1,file2,file3,file4,file5,file6,file7,file8,file9){
  # Authenticate dropbox with stored RDS token
  drop_auth(rdstoken='drop.RDS')
  
  # Write updated data to dropbox
  drop_upload(file1, path='/r/projects/purpleair/output')
  drop_upload(file2, path='/r/projects/purpleair/output')
  drop_upload(file3, path='/r/projects/purpleair/output')
  drop_upload(file4, path='/r/projects/purpleair/output')
  drop_upload(file5, path='/r/projects/purpleair/output')
  drop_upload(file6, path='/r/projects/purpleair/output')
  drop_upload(file7, path='/r/projects/purpleair/output')
  drop_upload(file8, path='/r/projects/purpleair/output')
  drop_upload(file9, path='/r/projects/purpleair/output')
}