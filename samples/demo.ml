let Demo x =
  let (s, setS) = useState x in
  let f = fun s -> s + 1 in
  if s = 0 then setS f;
  useEffect (if s = 1 then setS f);
  if s <= 1 then () else
    button (fun _ -> setS f)
;;
Demo 0
