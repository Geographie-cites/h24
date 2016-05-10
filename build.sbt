
scalaVersion := "2.11.8"

val monocleVersion = "1.2.0"

val geotoolsVersion = "14.3"

val jtsVersion = "1.13"

resolvers ++= Seq(
  "osgeo" at "http://download.osgeo.org/webdav/geotools/"
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
  "com.vividsolutions" % "jts" % jtsVersion
)
 
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)



