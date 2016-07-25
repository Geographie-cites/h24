
name := "h24"

scalaVersion := "2.11.8"

val monocleVersion = "1.2.0"

val geotoolsVersion = "14.3"

val jtsVersion = "1.13"

val breezeVersion = "0.12"

resolvers ++= Seq(
  "osgeo" at "http://download.osgeo.org/webdav/geotools/",
  "geosolutions" at "http://maven.geo-solutions.it/"
)

libraryDependencies ++= Seq (
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  "javax.media" % "jai_core" % "1.1.3" from "http://download.osgeo.org/webdav/geotools/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar",
  "org.geotools" % "gt-referencing" % geotoolsVersion,
  "org.geotools" % "gt-shapefile" % geotoolsVersion,
  "org.geotools" % "gt-epsg-wkt" % geotoolsVersion,
  "org.geotools" % "gt-cql" % geotoolsVersion,
  "org.geotools" % "gt-geotiff" % geotoolsVersion,
  "org.geotools" % "gt-image" % geotoolsVersion,
  "org.geotools" % "gt-coverage" % geotoolsVersion,
  "com.vividsolutions" % "jts" % jtsVersion,
  "com.github.tototoshi" %% "scala-csv" % "1.3.1",
  "org.apache.commons" % "commons-compress" % "1.11",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.tukaani" % "xz" % "1.5",
  "com.github.pathikrit" %% "better-files" % "2.15.0",
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  "org.scalanlp" %% "breeze-viz" % breezeVersion
)
 
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
