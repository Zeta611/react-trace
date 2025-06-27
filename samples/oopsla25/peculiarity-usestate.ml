let Counter x =
  print "Counter";
  let (s, setS) = useState x in
  print "Return";
  [s, button (fun _ ->
    setS (fun s -> s + 1);
    setS (fun s -> print "Update"; s + 1))];;
Counter 0
