package org.kangchun.maven2sbt

import scala.xml._


/**
  * Created by seokangchun on 2017. 6. 24..
  */
object Maven2Sbt extends App {
  val scalaVersion = "2.11.7"
  val pomXml = getClass.getResourceAsStream("/pom.xml")

  var pom = XML.load(pomXml)

  val groupId = pom \ "groupId" text
  val artifactId = pom \ "artifactId" text
  val version = pom \ "version" text

  val dotSeparatedPattern = """\$\{(.+)\}""".r

  def findPropertyName(name: String): Option[String] = name match {
    case dotSeparatedPattern(value) => Some(value.trim)
    case _ => None
  }

  def dotSeparatedToCamelCase(dotSeparated: String): String = {
    dotSeparated
  }

  // elements to array
  var properties2: Seq[String] = pom \ "properties" flatMap { properties =>
    properties.child.map {
      property => property.text
    }
  }
  //println(properties2)

  // elements to array
  var properties3: Seq[String] = for {
    properties <- pom \ "properties"
    property <- properties.child
  } yield property.text

  // println(properties3)


  val properties: Map[String, String] = (for {
    properties <- pom \ "properties"
    property <- properties.child
    label = property.label
    if !label.startsWith("#PCDATA")
  } yield property.label -> property.text).toMap

  //println(properties)

  case class Dependency(groupId: String,
                        artifactId: String,
                        version: String,
                        scope: Option[String]) {
    def toDependencyString: String =
      s""""$groupId" % "$artifactId" % ${findPropertyName(version).fold("\"" + version + "\"")(dotSeparatedToCamelCase)}${scope.fold("")(x => s""" % "$x"""")}"""
  }

  val dependencies: Seq[Dependency] =
    pom \ "dependencies" \ "dependency" map { dependency =>
      val groupId = dependency \ "groupId" text
      val artifactId = dependency \ "artifactId" text
      val version = dependency \ "version" text
      val scope = dependency \ "scope" text

      Dependency(groupId,
        artifactId,
        version,
        Option(scope).filter(_.nonEmpty))
    }

  //println(dependencies.mkString("\n"))

  val libraryDependencies = dependencies match {
    case Nil =>
      ""
    case x :: Nil =>
      s"""libraryDependencies += "${x.toDependencyString}"""
    case x :: xs =>
      s"""libraryDependencies ++= Seq(
         |  ${x.toDependencyString},
         |  ${xs.map(_.toDependencyString).mkString(",\n  ")}
         |)
         |""".stripMargin
  }

  case class Repository(id: String, name: String, url: String)

  val repositories: Seq[Repository] = for {
    repositories <- pom \ "repositories"
    repository <- repositories.child
    url = (repository \ "url").text
    if url.nonEmpty
    id = (repository \ "id").text
    name = (repository \ "name").text
  } yield Repository(id, name, url)

  val resolvers = repositories match {
    case Nil =>
      ""
    case x :: Nil =>
      s"""resolvers += "${x.name}" at "${x.url}""""
    case x :: xs =>
      s"""resolvers ++= Seq(
         |  "${x.name}" at "${x.url}",
         |  ${xs.map(x => s""""${x.name}" at "${x.url}"""").mkString(",\n  ")}
         |)
         |""".stripMargin
  }

  val buildSbt =
    s"""
       |organization := "$groupId"
       |
       |name := "$artifactId"
       |
       |version := "$version"
       |
       |scalaVersion := "$scalaVersion"
       |
       |${properties map { case (k, v) => s"""val ${dotSeparatedToCamelCase(k)} = "$v"""" } mkString "\n"}
       |
       |$resolvers
       |
       |$libraryDependencies
       |
     """.stripMargin

  println(buildSbt)
}
