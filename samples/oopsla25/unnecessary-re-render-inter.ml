let Child setS =
  useEffect (setS (fun _ -> false));
  ();;
let Parent b =
  let (s, setS) = useState b in
  if s then Child setS else ();;
Parent true
