{- More than one user?  Use a function! -}

let makeUser = \(user : Text) ->
      let home       = "/home/${user}"
      let privateKey = "${home}/.ssh/id_ed25519"
      let publicKey  = "${privateKey}.pub"
      in  { home, privateKey, publicKey }
    {- Add another user to this list -}
in  [ makeUser "bill"
    , makeUser "jane"
    ]
