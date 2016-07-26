
#Properties of the opinion model

## Sigma adoption

```scala
  def sigmaAdoption(current: Double, all: Vector[Double], sigma: Double, random: Random): Double = {
    val dist = new Gaussian(0.0, sigma)
    val d = all.map(x => dist.value(x - current))
    val v = new RasterVariate(d, Array(d.size))
    val index = (v.compute(random)(0) * d.size).toInt
    all(index)
  }
```

Assymptotic behaviour with 2 opinions:
 - The majority opinion gets a 100% adoption
 - 50-50 random 100% adoption of 1 side

Assymptotic behaviour with 3 opinions:
 - The central opinion gets a 100% adoption
 - Extrem opinions get a part of the opinion (after that, it should converge to the same as the 2 opinion dynamic but very slowly due to thediference between the value sigma and the opinions delta)



