open Brr

let star_button_el (data : State.entry) =
  let svg =
    {|<svg xmlns="http://www.w3.org/2000/svg" width="24"
              height="24"
              viewBox="0 0 24 24"
              fill="currentColor"
              stroke="currentColor"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="lucide lucide-star-icon lucide-star"
            >
              <title>Star</title>
              <path
                d="M11.525 2.295a.53.53 0 0 1 .95 0l2.31 4.679a2.123 2.123 0 0 0 1.595 1.16l5.166.756a.53.53 0 0 1 .294.904l-3.736 3.638a2.123 2.123 0 0 0-.611 1.878l.882 5.14a.53.53 0 0 1-.771.56l-4.618-2.428a2.122 2.122 0 0 0-1.973 0L6.396 21.01a.53.53 0 0 1-.77-.56l.881-5.139a2.122 2.122 0 0 0-.611-1.879L2.16 9.795a.53.53 0 0 1 .294-.906l5.165-.755a2.122 2.122 0 0 0 1.597-1.16z"
              />
            </svg>|}
  in
  let star_btn =
    El.v (Jstr.v "button")
      ~at:
        [ At.class'
            (Jstr.v
               (if data.is_starred then "star starred" else "star unstarred") )
        ]
      []
  in
  let _ = Jv.set (Brr.El.to_jv star_btn) "innerHTML" (Jv.of_string svg) in
  star_btn

let read_unread_el (data : State.entry) =
  let svg_read =
    {|<svg
              xmlns="http://www.w3.org/2000/svg"
              width="24"
              height="24"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="lucide lucide-mail-open-icon lucide-mail-open"
            >
              <title>Mark as Read</title>
              <path
                d="M21.2 8.4c.5.38.8.97.8 1.6v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V10a2 2 0 0 1 .8-1.6l8-6a2 2 0 0 1 2.4 0l8 6Z"
              />
              <path d="m22 10-8.97 5.7a1.94 1.94 0 0 1-2.06 0L2 10" />
            </svg>|}
  in
  let svg_unread =
    {|<svg
              xmlns="http://www.w3.org/2000/svg"
              width="24"
              height="24"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="lucide lucide-mail-icon lucide-mail"
            >
              <title>Mark as Unread</title>
              <path d="m22 7-8.991 5.727a2 2 0 0 1-2.009 0L2 7" />
              <rect x="2" y="4" width="20" height="16" rx="2" />
            </svg>|}
  in
  let btn =
    El.v (Jstr.v "button")
      ~at:
        [ At.class'
            (Jstr.v (if data.is_unread then "read-btn" else "unread-btn")) ]
      []
  in
  let svg = if data.is_unread then svg_read else svg_unread in
  let _ = Jv.set (Brr.El.to_jv btn) "innerHTML" (Jv.of_string svg) in
  btn
