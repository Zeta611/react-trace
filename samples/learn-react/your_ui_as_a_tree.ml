let FancyText x = x
;;
let InspirationGenerator children =
  let (index, setIndex) = useState 0 in
  # need to have indexing
  children
;;
let Copyright year =
  year
;;
let App _ =
  [FancyText 42, InspirationGenerator (Copyright 2004)]
;;
App ()
