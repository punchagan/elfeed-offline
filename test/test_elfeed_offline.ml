module OS = Elfeed_offline_web.Offline_search
module P = OS.Parse

let parse_date_tests =
  let now =
    Ptime.of_date ~tz_offset_s:0 (2019, 6, 24)
    |> Option.map Ptime.to_float_s
    |> Option.get
  in
  let test_data =
    [ ("5-days-ago", (Some 1560902400, None))
    ; ("4-days-ago", (Some 1560988800, None))
    ; ("5-days-ago--3-days-ago", (Some 1560902400, Some 1561075200))
    ; ("3-days-ago--5-days-ago", (Some 1560902400, Some 1561075200))
    ; ("2019-06-20", (Some 1560988800, None))
    ; ("2019-06-19--2019-06-21", (Some 1560902400, Some 1561075200))
    ; ("2019-06-21--2019-06-19", (Some 1560902400, Some 1561075200)) ]
  in
  List.map
    (fun (input, expected) ->
      let test_name = Printf.sprintf "parse_date_to_ms \"%s\"" input in
      Alcotest.test_case test_name `Quick (fun () ->
          let result =
            P.parse_date_to_s ~now input
            |> fun (x, y) ->
            (Option.map int_of_float x, Option.map int_of_float y)
          in
          Alcotest.(check (pair (option int) (option int)))
            "parsed date in ms" expected result ) )
    test_data

let entry_testable = Alcotest.testable (OS.Filter.pp "%s") ( = )

let parse_query_tests =
  let test_data =
    [ ( "+unread -starred #10"
      , { OS.Filter.empty with
          must_have= ["unread"]
        ; must_not_have= ["starred"]
        ; limit= Some 10 } )
    ; ( "+tag1 +tag2 -tag3 #5"
      , { OS.Filter.empty with
          must_have= ["tag1"; "tag2"]
        ; must_not_have= ["tag3"]
        ; limit= Some 5 } )
    ; ("-tagA -tagB", {OS.Filter.empty with must_not_have= ["tagA"; "tagB"]})
    ; ("#20", {OS.Filter.empty with limit= Some 20})
    ; ( "=http://example.com/feed"
      , {OS.Filter.empty with feeds= ["http://example.com/feed"]} )
    ; ( "~http://example.com/feed"
      , {OS.Filter.empty with not_feeds= ["http://example.com/feed"]} )
    ; ( "@2019-06-20--2019-06-22"
      , { OS.Filter.empty with
          after_ms= Some 1560988800000.
        ; before_ms= Some 1561161600000. } )
    ; ("", OS.Filter.empty) ]
  in
  List.map
    (fun (input, expected) ->
      let test_name = Printf.sprintf "parse_query \"%s\"" input in
      Alcotest.test_case test_name `Quick (fun () ->
          let actual = P.parse_query input in
          Alcotest.(check' ~expected ~actual ~msg:test_name entry_testable) ) )
    test_data

let () =
  Alcotest.run "Elfeed Offline Search Tests"
    [("Parse Date", parse_date_tests); ("Parse Query", parse_query_tests)]
