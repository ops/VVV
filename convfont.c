/* ppm2bin.c by Marko Mäkelä */

/* This simple converter creates a 8x16 font of a 8x8 font by doubling each
 * line. */

#include<stdio.h>
#include<stdlib.h>

int main (int argc, char **argv);

int main (int argc, char **argv) {
  FILE *input;
  FILE *output;
  int i, j;
  unsigned char buf[2048];

  if (argc != 3) {
    fprintf (stderr, "Usage: %s infile outfile\n", *argv);
    return 1;
  }

  if (!(input = fopen (argv[1], "rb"))) {
    fprintf (stderr, "Unable to open the input file\n");
    return 2;
  }

  if (!(output = fopen (argv[2], "wb"))) {
    fprintf (stderr, "Unable to open the output file\n");
    return 2;
  }

  while ((i = fread (buf, 1, sizeof(buf), input)))
    for (j = 0; j < i; j++) {
      fputc (buf[j], output);
      fputc (buf[j], output);
    }

  fclose (input);
  fclose (output);

  return 0;
}
