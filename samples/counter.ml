let C x =
  let (s, setS) = useState 1 in
  if (s > 3) then setS (fun _ -> 3);
  if (s < 1) then setS (fun _ -> 1);
  useEffect (print s);
  [ fun _ -> setS (fun s -> s + 1),
    fun _ -> setS (fun s -> s - 1),
    s ]
;;
C ()
