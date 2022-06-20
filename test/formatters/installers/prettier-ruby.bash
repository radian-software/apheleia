# Need ruby for gem, need gcc for native gem deps
apt-get install -y ruby gcc

# Need to do this manually for some reason because gem does not
# apparently include a dependency solver that can recognize the older
# version must be installed to support the Ruby version currently in
# use.
gem install syntax_tree -v 2.3.1

# Despite the name this is actually installing the Prettier plugin for
# Ruby, not Prettier itself. Also, same issue as above where we have
# to specify the version manually.
gem install prettier -v 2.1.0
