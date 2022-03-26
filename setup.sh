# In linux, the .nvmrc file is automatically read, but not in the windows version of nvm
nvm install $(less .nvmrc) 
nvm use $(less .nvmrc) # Make sure you have necessary permissions on your shell
npm i