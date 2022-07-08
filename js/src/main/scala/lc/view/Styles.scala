package lc.view

import scalacss.DevDefaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val btnDanger =style(
    color(Color("#fff")),
    backgroundColor(Color("#dc3545")),
    borderColor(Color("#dc3545")),
    &.hover(
      color(Color("#fff")),
      backgroundColor(Color("#c82333")),
      borderColor(Color("#d2130")),
      cursor.pointer
    )
  )
  val button = style(
    display.inlineBlock,
    fontWeight._400,
    textAlign.center,
    whiteSpace.nowrap,
    verticalAlign.middle,
    userSelect.none,
    border(1.px, solid, transparent),
    padding(0.375.rem, 0.75.rem),
    fontSize(1.rem),
    lineHeight(1.5),
    borderRadius(0.25.rem)
  )

  val thtd = style(
    padding(0.75.rem ),
    verticalAlign.top,
    borderTop(1.px, solid, Color("#dee2e6") )
  )
  val table = style(
    width(100.%%),
    marginBottom(1.rem),
    backgroundColor(Color("transparent")),
    unsafeChild("th")(
      thtd
    ),
    unsafeChild("td")(
      thtd
    )
  )
  val selectedRow = style(
    backgroundColor(Color("#17a2b8"))
  )


  val tableBordered = style(
    border(1.px, solid, Color("#dee2e6")),
    unsafeChild("th")(
      border(1.px, solid, Color("#dee2e6")),
    ),
    unsafeChild("td")(
      border(1.px, solid, Color("#dee2e6")),
    ),
    unsafeChild("thead")(
      unsafeChild("th")(
        borderBottomWidth(2.px)
      )
    ),
    unsafeChild("thead")(
      unsafeChild("td")(
        borderBottomWidth(2.px)
      )
    )
  )
}
