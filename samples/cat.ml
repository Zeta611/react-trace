let C x =
  let (s, setS) = useState x in
  if (s > 0) then setS (fun _ -> 0);
  useEffect (print s);
  [ fun _ -> setS (fun s -> s + 1),
    s ]
;;
C 0
