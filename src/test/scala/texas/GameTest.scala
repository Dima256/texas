package texas

class GameTest extends org.scalatest.funspec.AnyFunSpec {

  it ("should be 2c3c KcKh=KdKs | TwoPair > OnePair") {
    Game("texas-holdem 2h3h4h5d8d KdKs KcKh 2c3c").exists(_ == "2c3c KcKh=KdKs")
  }

  it ("should be isLeft | Bad Input") {
    Game("texas-holdem 2h3h4h5d8d KdKs KcKh Kc3c").isLeft
  }

  it ("TcJh 4d5s | T-A-Straight > A-5-Straight") {
    Game("texas-holdem AhKhQh2d3d 4d5s TcJh").exists(_ == "TcJh 4d5s")
  }

  it ("should be AcKs=AdKh Kh8c | Kicker A > Kicker K when all have Four") {
    Game("texas-holdem 2h2c2d2s8d AcKs AdKh Kh8c").exists(_ == "AcKs=AdKh Kh8c")
  }

  it ("should be 2d4h 2c3s | TwoPair(higher) > TwoPair") {
    Game("texas-holdem 2h3c4dTs9d 2c3s 2d4h").exists(_ == "2d4h 2c3s")
  }

  it ("should be Ac5s 7h8h | Flush > Straight") {
    Game("texas-holdem 2h3h4hTs9d Ac5s 7h8h").exists(_ == "7h8h Ac5s")
  }

  it ("should be 6h5h Ah8h | StraightFlush > Flush") {
    Game("texas-holdem 2h3h4hTs9d 6h5h Ah8h").exists(_ == "6h5h Ah8h")
  }

  it ("should be Ah8h Jh5h | Q-Flush > J-Flush with A and K on the board") {
    Game("texas-holdem Ah3hKhTs9d Jh5h Qh8h").exists(_ == "Qh8h Jh5h")
  }
}
