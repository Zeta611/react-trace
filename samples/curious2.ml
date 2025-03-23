let Counter x =
  print "Counter";
  let (s, setS) = useState x in
  useEffect (
    print "Effect";
    if s > 0 then
      setS (fun s -> print "Update"; 0));
  print "Return";
  [s, button (fun _ ->
    setS (fun s -> s+1);
    setS (fun s -> print "Update"; s+1))];;
Counter 0
