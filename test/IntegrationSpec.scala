import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

class IntegrationSpec extends SpecificationWithJUnit {

  "Application" should {

    "work from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port)

      browser.pageSource must contain("Anagram")
    }
  }
}
