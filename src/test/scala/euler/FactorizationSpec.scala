package euler

import org.scalatest.Matchers
import org.scalatest.WordSpec

class FactorizationSpec extends WordSpec with Matchers {

  "Factorization" when {
    "the max value is 100" should {
      val fs = Factorization(100)

      "produce (3,1) :: (2,1) for 6" in {
        fs.factorize(6) should be((3, 1) :: (2, 1) :: Nil)
      }

      "produce (5,1) :: (2,2) for 20" in {
        fs.factorize(20) should be((5, 1) :: (2, 2) :: Nil)
      }
    }
  }

}