# install recent julia through JuliaUp
curl -fsSL https://install.julialang.org | sh -s -- --yes
ln -s /root/.juliaup/bin/julia /usr/bin/julia
chmod +x /usr/bin/julia
# install Runic.jl in @runic shared project
julia --project=@runic -e 'using Pkg; Pkg.add("Runic")'