/* ppm2bin.c by Marko Mäkelä */

/* This simple converter converts a 2-color PPM file to a binary file that
 * can be included with the VIC-20 demo.
 * The PPM format stores all bits horizontally.  Each line starts at a byte
 * boundary, and the extra bits at end of a line are zero.
 * The binary format outputted by this program stores groups of 8 bits
 * vertically.
 */

/* The PPM format seems to be as follows:
 * 1. Header bytes: 0x50, 0x34, 0x0a ("P4\n")
 * 2. Picture width and height in pixels and a newline ("%u %u\n")
 * 3. Raw binary pixel data
 */

#include<stdio.h>
#include<stdlib.h>

int main (int argc, char **argv);

int main (int argc, char **argv) {
  FILE *input;
  FILE *output;
  unsigned width, height, start, x, y;
  unsigned char *buf;

  if (argc != 3) {
    fprintf (stderr, "Usage: %s infile outfile\n", *argv);
    return 1;
  }

  if (!(input = fopen (argv[1], "rb"))) {
    fprintf (stderr, "Unable to open the input file\n");
    return 2;
  }

  if (2 > fscanf (input, "P4\n%u %u\n", &width, &height)) {
    fprintf (stderr, "%s is not in supported PPM format!\n", argv[1]);
    return 3;
  }

  fprintf (stderr, "%s: file %s: width %u, height %u\n", *argv, argv[1],
	   width, height);

  if (!(buf = malloc (height))) {
    fprintf (stderr, "Unable to allocate a data buffer\n");
    return 4;
  }

  if (!(output = fopen (argv[2], "wb"))) {
    fprintf (stderr, "Unable to open the output file\n");
    return 2;
  }

  start = ftell (input);
  width >>= 3;

  for (x = 0; x < width; x++) {
    unsigned c;

    for (y = height; y--;) {
      fseek (input, start + width * y + x, SEEK_SET);
      buf[y] = getc (input);
    }

    for (y = 0; y < height;) {
      /* search for blocks of same byte */

      for (c = y; c < height && buf[y] == buf[c]; c++);
      c -= y;

      if (c)  /* found a block */
	do {
	  putc ((c - 1) & 127, output);
	  putc (buf[y], output);

	  if (c > 128) { /* create multiple records if necessary */
	    c -= 128;
	    continue;
	  }
	} while (0);

      for (c = y += c; c < height - 1 && buf[c + 1] != buf[c]; c++);
      c -= y;

      if (c)  /* found a block */
	do {
	  putc ((c - 1) | 128, output);
	  fwrite (&buf[y], 1, c & 127, output);

	  if (c > 128) {
	    c -= 128;
	    continue;
	  }
	} while (0);

      y += c;
    }
  }

  fclose (input);
  fclose (output);

  return 0;
}
