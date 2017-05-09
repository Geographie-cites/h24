
organization := "eighties"

name := "h24"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.1"

val monocleVersion = "1.4.0"

val geotoolsVersion = "14.3"

val jtsVersion = "1.13"

val breezeVersion = "latest.integration"

resolvers ++= Seq(
  "osgeo" at "http://download.osgeo.org/webdav/geotools/",
  "geosolutions" at "http://maven.geo-solutions.it/",
  "geotoolkit" at "http://maven.geotoolkit.org/"
)



libraryDependencies ++= Seq (
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  "org.geotools" % "gt-referencing" % geotoolsVersion,
  "org.geotools" % "gt-shapefile" % geotoolsVersion,
  "org.geotools" % "gt-epsg-wkt" % geotoolsVersion,
  "org.geotools" % "gt-cql" % geotoolsVersion,
  "org.geotools" % "gt-geotiff" % geotoolsVersion,
  "org.geotools" % "gt-image" % geotoolsVersion,
  "org.geotools" % "gt-coverage" % geotoolsVersion,
  "com.vividsolutions" % "jts" % jtsVersion,
  "com.github.tototoshi" %% "scala-csv" % "1.3.4",
  "org.apache.commons" % "commons-compress" % "1.11",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.tukaani" % "xz" % "1.6",
  "com.github.pathikrit" %% "better-files" % "2.17.1",
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  "org.scalanlp" %% "breeze-viz" % breezeVersion,
  "org.typelevel"  %% "squants"  % "1.1.0",
  "joda-time" % "joda-time" % "2.9.7",
  //"com.thoughtworks.xstream" % "xstream" % "1.4.9",
  "io.suzaku" %% "boopickle" % "1.2.6"
)
 
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

enablePlugins(SbtOsgi)

osgiSettings

OsgiKeys.exportPackage := Seq("eighties.h24.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("!scala.*","**")

OsgiKeys.embeddedJars := (Keys.externalDependencyClasspath in Compile).value map (_.data) filter (_.name.startsWith("gt-"))

OsgiKeys.requireCapability := ""


