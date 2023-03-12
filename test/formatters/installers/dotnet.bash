deb_file=$(mktemp)
curl -L --output "$deb_file" https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb
sudo dpkg -i "$deb_file"
rm "$deb_file"

apt-get update && apt-get install -y dotnet-sdk-7.0
