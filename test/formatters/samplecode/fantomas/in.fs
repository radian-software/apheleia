type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString () =
            match this with
            | TyLam (t1, t2) -> sprintf "(%O -> %O)" t1 t2
            | TyVar a -> a
            | TyCon (s, ts) -> s

let Multiple9x9 () =
    for i in 1 .. 9 do
        printf "\n";
        for j in 1 .. 9 do
            let k = i * j in
            printf "%d x %d = %2d " i j k;
        done;
    done;;
Multiple9x9 ();;
