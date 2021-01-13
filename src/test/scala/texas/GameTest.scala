package texas

class GameTest extends org.scalatest.funspec.AnyFunSpec {

  it ("should be 2hJh Ks4s=Kc4h Kh7s KdJc 6s7d 2sAs") {
    assert(
    Game("texas-holdem 5h6dAhAcQc Kc4h KdJc 2sAs Ks4s Kh7s 6s7d 2hJh")
      .exists(_ == "2hJh Ks4s=Kc4h Kh7s KdJc 6s7d 2sAs")
    )
  }

  it ("Bad Input") {
    assert(Game("texas-holdem 2h3h4h5d8d KdKs KcKh Kc3c").isLeft)
  }

  it ("T-A-Straight > A-5-Straight") {
    assert(Game("texas-holdem AhKhQh2d3d 4d5s TcJh").exists(_ == "4d5s TcJh"))
  }

  it ("Kicker A > Kicker K when all have Four") {
    assert(Game("texas-holdem 2h2c2d2s8d AcKs AdKd Kh8c").exists(_ == "Kh8c AdKd=AcKs"))
  }

  it ("TwoPair(higher) > TwoPair") {
    assert(Game("texas-holdem 2h3c4dTs9d 2c3s 2d4h").exists(_ == "2c3s 2d4h"))
  }

  it ("Flush > Straight") {
    assert(Game("texas-holdem 2h3h4hTs9d Ac5s 7h8h").exists(_ == "Ac5s 7h8h"))
  }

  it ("StraightFlush > Flush") {
    assert(Game("texas-holdem 2h3h4hTs9d 6h5h Ah8h").exists(_ == "Ah8h 6h5h"))
  }

  it ("Q-Flush > J-Flush with A and K on the board") {
    assert(Game("texas-holdem Ah3hKhTs9d Jh5h Qh8h").exists(_ == "Jh5h Qh8h"))
  }

}
