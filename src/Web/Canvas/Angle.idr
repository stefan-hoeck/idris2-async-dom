module Web.Canvas.Angle

%default total

--------------------------------------------------------------------------------
--          Angle
--------------------------------------------------------------------------------

public export
record Angle where
  constructor A
  radians : Double

export %inline
rad : Double -> Angle
rad = A

export %inline
deg : Double -> Angle
deg x = rad (x / 180 * pi)

export
(.degree) : Angle -> Double
(.degree) (A x) = (x / pi) * 180

export
Show Angle where
  show x = show x.degree 
