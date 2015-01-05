ImageCompress
=============

Image compression with Discrete Cosine Transform in Scala. Supports
setting a target file size for the compressor to reach. The intention
is for the results to be somewhat simple to decode on low-power
processors such as the z80. That may not have been achieved yet, as
no decoder has been written yet in z80 assembly.

`test.sh` relies on a jar called imgcompress.jar existing in the current
directory, and scala on the PATH. Compile code to imgcompress.jar to use
it
