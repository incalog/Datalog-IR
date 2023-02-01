package inca.measurements.util

object Units {
  sealed trait MeasurementUnit {
    def isConvertible(other: MeasurementUnit): Boolean
    def convert(v: Double, trg: MeasurementUnit): Double
  }
  sealed trait Time extends MeasurementUnit {
    def isConvertible(other: MeasurementUnit): Boolean = other match {
      case _: Time => true
      case _: Space => false
    }
    def convert(v: Double, trg: MeasurementUnit): Double = trg match {
      case Seconds => v * this.toSeconds
      case Milliseconds => v * this.toMilliseconds
      case Nanoseconds => v * this.toNanoSeconds
      case _: Space =>
        throw new IllegalArgumentException("Cannot convert time value to space value")
    }
    def toSeconds: Double
    def toMilliseconds: Double
    def toNanoSeconds: Double
  }
  case object Seconds extends Time {
    override def toString: String = "time (s)"
    override def toSeconds: Double = 1
    override def toMilliseconds: Double = 1000
    override def toNanoSeconds: Double = toMilliseconds * 1000
  }
  case object Milliseconds extends Time {
    override def toString: String = "time (ms)"
    override def toSeconds: Double = 1 / 1000
    override def toMilliseconds: Double = 1
    override def toNanoSeconds: Double = 1000
  }
  case object Nanoseconds extends Time {
    override def toString: String = "time (ns)"
    override def toSeconds: Double = toMilliseconds * 1000
    override def toMilliseconds: Double = 1000
    override def toNanoSeconds: Double = 1
  }

  trait Space extends MeasurementUnit {
    def isConvertible(other: MeasurementUnit): Boolean = other match {
      case _: Time => false
      case _: Space => true
    }
    def convert(v: Double, trg: MeasurementUnit): Double = trg match {
      case GigaBytes => v * this.toGigaBytes
      case MegaBytes => v * this.toMegaBytes
      case KiloBytes => v * this.toKiloBytes
      case _: Time => throw new IllegalArgumentException("Cannot convert space value to time value")
    }
    def toGigaBytes: Double
    def toMegaBytes: Double
    def toKiloBytes: Double
  }
  case object GigaBytes extends Space {
    override def toString: String = "memory (GB)"
    override def toGigaBytes: Double = 1
    override def toMegaBytes: Double = 1024
    override def toKiloBytes: Double = toMegaBytes * 1024
  }
  case object MegaBytes extends Space {
    override def toString: String = "memory (MB)"
    override def toGigaBytes: Double = 1 / 1024
    override def toMegaBytes: Double = 1
    override def toKiloBytes: Double = 1024
  }
  case object KiloBytes extends Space {
    override def toString: String = "memory (KB)"
    override def toGigaBytes: Double = toMegaBytes * 1024
    override def toMegaBytes: Double = 1024
    override def toKiloBytes: Double = 1
  }
}
