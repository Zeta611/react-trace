let Inf x =
  let (s, setS) = useState 0 in
  useEffect (setS (fun s -> s + 1));
  s;;
Inf 0
