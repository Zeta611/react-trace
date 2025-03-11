let C _ =
  let (setter, setSetter) = useState^setter () in
  let (render, setRender) = useState^render 0 in
  (if render < 3 then setRender(fun r -> r + 1));

  (if (setter <> ()) && (render < 3) then setter(fun _ -> 1));
  let (value, setValue) = useState^value 0 in
  (if (setter = ()) && (render < 3) then setSetter(fun _ -> setValue));
  print render;
  print value;
  value
;;
C ()
