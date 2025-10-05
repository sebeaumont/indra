open OcamlCanvas.V1

let () =
  Backend.init ();
  (* screen size calibration *)
  let pixels = (1512, 982) in
  let height = 982.0 in
  let width = 1512.0 in
  let yoff = 37.0 in
  let xoff = 0.0 in

  let c = Canvas.createOnscreen ~decorated:false ~size:pixels () in

  Canvas.setFillColor c Color.blue;
  Canvas.fillRect c ~pos:(xoff, 0.0 +. yoff) ~size:(10.0, 10.0);
  Canvas.fillRect c ~pos:(width -. 10.0, height -. 10.0) ~size:(10.0, 10.0);

  Canvas.show c;

  let e1 =
    React.E.map
      (fun { Event.canvas = _; timestamp = _; data = () } -> Backend.stop ())
      Event.close in

  let e2 =
    React.E.map
      (fun {
             Event.canvas = _;
             timestamp = _;
             data = { Event.key; char = _; flags = _ };
             _;
           } -> if key = KeyEscape then Backend.stop ())
      Event.key_down in

  let e3 =
    React.E.map
      (fun {
             Event.canvas = _;
             timestamp = _;
             data = { Event.position = x, y; button = _ };
           } ->
        Canvas.setFillColor c Color.red;
        Canvas.clearPath c;
        Canvas.arc c
          ~center:(float_of_int x, float_of_int y)
          ~radius:5.0 ~theta1:0.0 ~theta2:(2.0 *. Const.pi) ~ccw:false;
        Canvas.fill c ~nonzero:false;
        Format.printf "%d %d\n" x y)
      Event.button_down in

  let frames = ref 0L in

  let e4 =
    React.E.map
      (fun { Event.canvas = _; timestamp = _; data = _ } ->
        frames := Int64.add !frames Int64.one)
      Event.frame in

  Backend.run (fun () ->
      ignore e1;
      ignore e2;
      ignore e3;
      ignore e4;
      Printf.printf "Displayed %Ld frames. Goodbye !\n" !frames)
