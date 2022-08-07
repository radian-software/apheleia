wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

sudo apt-get update
sudo apt-get install -y dotnet-sdk-6.0

dotnet tool install -g fantomas-tool

sudo mkdir /opt/.dotnet
sudo cp -R "$HOME/.dotnet/." /opt/.dotnet/.
sudo ln -s /opt/.dotnet/tools/fantomas /usr/local/bin/
