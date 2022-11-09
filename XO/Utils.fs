module XO.Utils


type Result<'a, 'b> with
    member self.iter(f) : unit =
        match self with
        | Ok a -> f a
        | Error _ -> ()
