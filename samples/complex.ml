let C x =
  let (s, setS) = useState x in
  if s = 42 then
    setS (fun s -> s + 1);
  s
;;
let D _ =
  let (s, setS) = useState true in
  useEffect (setS (fun _ -> false));
  C 42
;;
[D (), 0]
