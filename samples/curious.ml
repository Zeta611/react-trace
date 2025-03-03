let Counter x =
  print "Counter";
  let (s, setS) = useState x in
  useEffect (
    print "Effect";
    if s mod 2 = 0 then
      setS (fun s -> print "Update"; s + 1)
  );
  print "Return";
  view [s]
;;
view [Counter 0]
