apt-get install -y dotnet-sdk-8.0

dotnet tool install csharpier -g

ls "${HOME}/.dotnet/tools" | while read -r tool; do
    ln -s "${HOME}/.dotnet/tools/${tool}" /usr/local/bin/
done
