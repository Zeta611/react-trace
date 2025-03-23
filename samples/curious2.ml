let SelfCounter x =
  let (s, setS) = useState x in
  print s;
  useEffect (
    print "Effect";
    if s < 3 then
      setS (fun s -> s + 1));
  print "Return";
  [s];;
SelfCounter 0
