# Need ruby for gem, need gcc and ruby headers for native gem deps
apt-get install -y ruby ruby-dev gcc

# Install the plugin
npm install -g prettier @prettier/plugin-ruby

# These are required dependencies documented at
# https://www.npmjs.com/package/@prettier/plugin-ruby
gem install prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs
