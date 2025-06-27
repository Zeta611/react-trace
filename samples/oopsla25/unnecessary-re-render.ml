let Flicker x =
  let (s, setS) = useState x in
  useEffect (setS (fun _ -> 42));
  s;;
Flicker 0
