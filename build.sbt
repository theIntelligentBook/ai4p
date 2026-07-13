name := "ai4p"

val deployFast = taskKey[Unit]("Copies the fastLinkJS script to deployscripts/")
val deployFull = taskKey[Unit]("Copies the fullLinkJS script to deployscripts/")

import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / scalaVersion := "3.5.2"

lazy val ai4p = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .settings(
    resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies ++= Seq(
      "com.wbillingsley" %%% "doctacular" % "0.3.0",
    ),

    // For java.security.SecureRandom which is used in UUID generation
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),

    libraryDependencies ++= Seq(
      "com.github.wbillingsley" % "lavamaze" % "master-SNAPSHOT", // Need to single-% as it's a top-level jitpack project
      "com.github.theintelligentbook" % "circuitsup" % "master-SNAPSHOT"
    ),

    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    
    // For vite bundler
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("ai4p"))) 
    },

    // To use ScalablyTypedConverterExternalNpmPlugin
    externalNpm := {
      baseDirectory.value
    },

    // Used by GitHub Actions to get the script out from the .gitignored target directory
    deployFast := {
      val opt = (Compile / fastOptJS).value
      IO.copyFile(opt.data, new java.io.File("client/target/compiled.js"))
    },

    deployFull := {
      val opt = (Compile / fullOptJS).value
      IO.copyFile(opt.data, new java.io.File("client/target/compiled.js"))
    }
  )




