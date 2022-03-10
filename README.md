# A Haskell Raytracer

## Example
![Preview of rendered image](haskell-rt-preview.png)

![Preview of rendered image](haskell-rt-reflective-preview.png)

## How to use (only tested on Linux)
1. Install GHC
2. Download source code
3. Go open a terminal next to the source code
4. Build the source and run it like so:
```
ghc -O2 -dynamic -main-is Raytrace Raytrace.hs && time ./Raytrace > image.ppm
```

(You can remove ``-O2`` if it is taking too long to compile, but it will result in longer render times)