let Inf2 x =
  let (s, setS) = useState x in
  setS (fun s -> s);
  s;;
Inf2 0
