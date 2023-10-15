let days_until_next_d_day () =
  let today = Sys.time () in
  let next_d_day =
    let rec find_next_d_day month =
      if month > 12 then
        find_next_d_day 1
      else
        let sunday = (month, 1, 1) |> Date.from_triple |> Date.next_sunday in
        if sunday.day <= today.tm_mday then
          find_next_d_day (month + 1)
        else
          sunday
    in
    find_next_d_day (today.tm_mon + 1)
  in
  (next_d_day - today).days

let main () =
  let days_until_next_d_day = days_until_next_d_day () in
  Printf.printf "Days until the next D day: %d\n" days_until_next_d_day;
  if days_until_next_d_day = 0 then
    Printf.printf "Today is a D day!\n"

let () = main ()
