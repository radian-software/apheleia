# Fucking Node devs removed NODE_PATH support so now you have to do
# this bullshit to get import() to work with globally installed
# packages.
ln -s /usr/lib/node_modules ./
