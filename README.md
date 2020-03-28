# raytrace
Generates raytraced PNG images. Supports spheres and triangles because they were the easiest shapes to implement.

## Compiling
```shell
stack build
```
This will produce an executable appropriate for your system somewhere in `stack-work/dist`.

## Running
```shell
raytrace-exe <config path> <output path>
```
For example:
```shell
raytrace-exe examples/infinity-mirror.yaml ~/Desktop/infinity-mirror.png
```
On my machine, `inifinity-mirror.yaml` takes about 20 seconds to finish running. Reducing the image dimensions is the biggest influence on runtime.
