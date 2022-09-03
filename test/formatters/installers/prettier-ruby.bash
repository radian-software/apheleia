# Need ruby for gem, need gcc and ruby headers for native gem deps
apt-get install -y ruby ruby-dev gcc

# Despite the name this is actually installing the Prettier plugin for
# Ruby, not Prettier itself.
gem install prettier

# Then of course we need to install what sounds like the same thing
# again, but isn't...
npm install -g prettier @prettier/plugin-ruby

# These are required dependencies documented at
# https://www.npmjs.com/package/@prettier/plugin-ruby
gem install prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs
