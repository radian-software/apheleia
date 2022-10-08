# Need ruby for gem, need gcc and ruby headers for native gem deps
apt-get install -y ruby ruby-dev gcc

# Install the plugin
npm install -g prettier @prettier/plugin-ruby

# Have to install from source because release not tagged yet
# https://github.com/ruby-syntax-tree/syntax_tree-rbs/pull/34
# https://stackoverflow.com/a/11767563
gem install specific_install
gem specific_install -l https://github.com/ruby-syntax-tree/syntax_tree-rbs.git

# These are required dependencies documented at
# https://www.npmjs.com/package/@prettier/plugin-ruby
gem install prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs
