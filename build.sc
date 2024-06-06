import mill._
import mill.scalalib.publish.{Developer, License, PomSettings, VersionControl}
import mill.scalalib._
import mill.scalanativelib._
import scalalib._
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import $ivy.`com.github.lolgab::mill-mima::0.0.23`

import de.tobiasroeser.mill.vcs.version.VcsVersion
import com.github.lolgab.mill.mima._

val dottyVersion = sys.props.get("dottyVersion")

val scalaVersions = List("2.12.17", "2.13.11", "2.11.12", "3.3.1") ++ dottyVersion

val scalaNative = "0.5.1"

object requests extends Module {
  object jvm extends Cross[RequestsJvmModule](scalaVersions)
  trait RequestsJvmModule extends RequestsModule {
    object test extends ScalaTests with RequestsTestModule
  }

  object native extends Cross[RequestsNativeModule](scalaVersions) 
  trait RequestsNativeModule extends RequestsModule with ScalaNativeModule {
    override def scalaNativeVersion = scalaNative
    object test extends ScalaNativeTests with RequestsTestModule  {
      def nativeLinkingOptions = super.nativeLinkingOptions() ++ Seq("-L/usr/local/opt/openssl@3/lib")   
    }
    // TODO not sure what the implications of adding this are exactly
    def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.github.lolgab::scala-native-crypto::0.1.0") 

    override def nativeEmbedResources = true

  }

}

trait RequestsModule extends ScalaModule with PlatformScalaModule with CrossScalaModule with PublishModule with Mima {

  def publishVersion = VcsVersion.vcsState().format()

  def mimaPreviousVersions =
    (
      Seq("0.7.0", "0.7.1", "0.8.2") ++
      Option.when(VcsVersion.vcsState().commitsSinceLastTag != 0)(VcsVersion.vcsState().lastTag).flatten
    ).distinct
  override def mimaBinaryIssueFilters = Seq(
    ProblemFilter.exclude[ReversedMissingMethodProblem]("requests.BaseSession.send"),
    ProblemFilter.exclude[DirectMissingMethodProblem]("requests.Response.string")
  )

  def pomSettings = PomSettings(
    description = "Scala port of the popular Python Requests HTTP client",
    organization = "com.lihaoyi",
    url = "https://github.com/com-lihaoyi/requests-scala",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("com-lihaoyi", "requests-scala"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def ivyDeps = Agg(ivy"com.lihaoyi::geny::1.1.0")

  trait RequestsTestModule extends TestModule.Utest {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.8.3",
      ivy"com.lihaoyi::ujson::3.3.0"
    )
  }

}
