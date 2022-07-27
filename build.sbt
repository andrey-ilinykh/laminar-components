import sbt.Keys.organization
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    name := "laminar-components",
    normalizedName := "laminar-components",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.13.8",
    organization := "org.ailinykh",
    crossScalaVersions := Seq("2.13.8"),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "andreyilinykh",
        "Andrey Ilinykh",
        "ailinykh@gmail.com",
        url("https://github.com/andrey-ilinykh")
      )
    )
  )
)


val baseScalacSettings =
  "-encoding" :: "UTF-8" ::
    "-unchecked" ::
    "-deprecation" ::
    "-explaintypes" ::
    "-feature" ::
    "-language:_" ::
    "-Xfuture" ::
    "-Xlint" ::
    "-Ymacro-annotations" ::
    "-Yno-adapted-args" ::
    "-Ywarn-value-discard" ::
    "-Ywarn-unused" ::
    "-Xlog-implicits" ::
    Nil

lazy val scalacSettings = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) =>
        baseScalacSettings.diff(
          "-Xfuture" ::
            "-Yno-adapted-args" ::
            "-Ywarn-infer-any" ::
            "-Ywarn-nullary-override" ::
            "-Ywarn-nullary-unit" ::
            Nil
        )
      case _ => baseScalacSettings
    }
  }
)
lazy val commonSettings = scalacSettings

lazy val root = project
  .in(file("."))
  .aggregate(lcJS, lcJVM)
  .settings(commonSettings,
    skip / publish := true
  )

lazy val lc = (crossProject(JSPlatform, JVMPlatform) in file("."))
  .settings(
    commonSettings,
    name := "laminar-components",
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "0.14.2",
      "com.github.japgolly.scalacss" %%% "core" % "1.0.0"
    )
  ).jsSettings(
  scalaJSLinkerConfig ~= {
    _.withSourceMap(true)
  },
)
  .enablePlugins(ScalaJSPlugin)

lazy val example = project
  .in(file("example"))
  .dependsOn(lcJS)
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    skip / publish := true
  )
  .enablePlugins(ScalaJSPlugin)

lazy val lcJS = lc.js

lazy val lcJVM = lc.jvm