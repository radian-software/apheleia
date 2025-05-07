apt-get install -y dotnet-sdk-8.0

dotnet tool install csharpier -g

if ! [[ "$PATH" =~ $HOME/.dotnet/tools:$HOME/bin: ]]; then
    PATH="$HOME/.dotnet/tools:$HOME/bin:$PATH"
fi
export PATH
