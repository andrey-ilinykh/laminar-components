package lc.view
//import com.raquo.laminar.api.L.svg._

import com.raquo.laminar.api.L._


object Utils {

  def dash = svg.svg(
    svg.height := "1.5em",
    svg.width := "1.5em",
    svg.viewBox := "0 0 16 16",
    svg.path(
      svg.d :="M2 8a.5.5 0 0 1 .5-.5h11a.5.5 0 0 1 0 1h-11A.5.5 0 0 1 2 8Z"
    )
  )
  def sortUp = svg.svg(
    svg.height := "1.5em",
    svg.width := "1.5em",
    svg.viewBox := "0 0 16 16",
    svg.path(
      svg.d :="M3.5 12.5a.5.5 0 0 1-1 0V3.707L1.354 4.854a.5.5 0 1 1-.708-.708l2-1.999.007-.007a.498.498 0 0 1 .7.006l2 2a.5.5 0 1 1-.707.708L3.5 3.707V12.5zm3.5-9a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5zM7.5 6a.5.5 0 0 0 0 1h5a.5.5 0 0 0 0-1h-5zm0 3a.5.5 0 0 0 0 1h3a.5.5 0 0 0 0-1h-3zm0 3a.5.5 0 0 0 0 1h1a.5.5 0 0 0 0-1h-1z"
    )
  )
  def sortDown = svg.svg(
    svg.height := "1.5em",
    svg.width := "1.5em",
    svg.viewBox := "0 0 16 16",
    svg.path(
      svg.d :="M3.5 2.5a.5.5 0 0 0-1 0v8.793l-1.146-1.147a.5.5 0 0 0-.708.708l2 1.999.007.007a.497.497 0 0 0 .7-.006l2-2a.5.5 0 0 0-.707-.708L3.5 11.293V2.5zm3.5 1a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5zM7.5 6a.5.5 0 0 0 0 1h5a.5.5 0 0 0 0-1h-5zm0 3a.5.5 0 0 0 0 1h3a.5.5 0 0 0 0-1h-3zm0 3a.5.5 0 0 0 0 1h1a.5.5 0 0 0 0-1h-1z"
    )
  )
  def caretDown = svg.svg(
    svg.height := "1.5em",
    svg.width := "1.5em",
    svg.viewBox := "0 0 16 16",
    svg.path(
      svg.d :=
        "M7.247 11.14 2.451 5.658C1.885 5.013 2.345 4 3.204 4h9.592a1 1 0 0 1 .753 1.659l-4.796 5.48a1 1 0 0 1-1.506 0z")

  )

  def caretEmpty = svg.svg(
    svg.height := "1.5em",
    svg.width := "1.5em",
    svg.viewBox := "0 0 16 16",
    svg.path(
      svg.d :=""
    )

  )

  def caretRight = svg.svg(
    svg.height := "1.5em",
    svg.width := "1.5em",
    svg.viewBox := "0 0 16 16",
    svg.path(
      svg.d := "m12.14 8.753-5.482 4.796c-.646.566-1.658.106-1.658-.753V3.204a1 1 0 0 1 1.659-.753l5.48 4.796a1 1 0 0 1 0 1.506z"
    )
  )
  //  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-caret-down-fill" viewBox="0 0 16 16">
  //    <path d="M7.247 11.14 2.451 5.658C1.885 5.013 2.345 4 3.204 4h9.592a1 1 0 0 1 .753 1.659l-4.796 5.48a1 1 0 0 1-1.506 0z"/>
  //  </svg>
}
