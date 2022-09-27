name := "railway-dispatcher"

version := "0.1"

ThisBuild / scalaVersion := "2.13.6"

lazy val http4sVersion   = "1.0.0-M4"
lazy val circeVersion    = "0.14.0"
lazy val zioCatsVersion  = "2.4.1.0"
lazy val zioVersion      = "1.0.11"
lazy val zioMagicVersion = "0.3.8"

scalacOptions ++= Seq("-Xsource:3")

libraryDependencies ++= Seq(
  "io.github.kitlangton" %% "zio-magic"           % zioMagicVersion,
  "dev.zio"              %% "zio"                 % zioVersion,
  "dev.zio"              %% "zio-interop-cats"    % zioCatsVersion,
  "org.http4s"           %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"           %% "http4s-dsl"          % http4sVersion,
  "org.http4s"           %% "http4s-circe"        % http4sVersion,
  "io.circe"             %% "circe-generic"       % circeVersion,
  "dev.zio"              %% "zio-test"            % zioVersion % "test",
  "dev.zio"              %% "zio-test-sbt"        % zioVersion % "test"
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
