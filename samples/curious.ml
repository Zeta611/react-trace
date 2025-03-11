let Counter x =
  print "Counter";
  let (s, setS) = useState x in
  useEffect (
    print "Effect";
    setS (fun s -> if s mod 2 = 0 then (print "Update"; s + 1) else s)
    #if s mod 2 = 0 then
    #  setS (fun s -> print "Update"; s + 1)
  );
  print "Return";
  s
;;
Counter 0
