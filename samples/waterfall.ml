let Waterfall x =
  let (x, setX) = useState x in
  let (y, setY) = useState 0 in
  let (z, setZ) = useState 0 in

  print ("x: " + x + ", y: " + y + ", z: " + z);

  [button (fun _ ->
    setX (fun x -> x + 1);
    setY (fun _ -> x);
    setZ (fun _ -> y)
  )]
;;
Waterfall 0
