let trojkaty ls =
    let l1 = List.sort compare ls in
    let l2 = l1 in
    let wyn = ref (0,0) in
    let akt = ref (0,0) in
    while List.length l1 > 1 do
        while List.length l2 > 0 && (fst (!akt) < 2 || hd l2 < hd l1 + hd (hd l1)) do
            akt := (fst (!akt) + 1, snd (!akt));
            if fst (!akt) > fst(!wyn) then wyn := !akt;
        done;
    done;
    (!wyn)|>fst;;