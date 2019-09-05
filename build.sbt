enablePlugins(ScalaJSPlugin)

name := "Diffusion experiment"
scalaVersion := "2.12.8" // or any other Scala version >= 2.10.2

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

resolvers += "jitpack" at "https://jitpack.io"

updateOptions := updateOptions.value.withLatestSnapshots(false)

libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.7",
    "com.github.wbillingsley.veautiful" %%% "veautiful" % "v0.1-SNAPSHOT",
    "com.github.wbillingsley.veautiful" %%% "veautiful-templates" % "v0.1-SNAPSHOT"
)