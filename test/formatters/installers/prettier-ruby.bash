# Need ruby for gem, need gcc and ruby headers for native gem deps
apt-get install -y ruby ruby-dev gcc

# Apparently rubygems does not know how to do dependency resolution.
# So we have to manually install an old version of this dependency to
# avoid the latest version getting installed and then failing to build
# because it does not support ruby 2.7 which is what we have.
gem install rbs -v 3.1.3

# These are required dependencies documented at
# https://www.npmjs.com/package/@prettier/plugin-ruby
gem install prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs

# Install the plugin
npm install -g prettier @prettier/plugin-ruby
