/* list multicolour font */

#include <stdio.h>

void
main (void)
{
  unsigned char c[8], d[8];
  unsigned short int m;
  int i, j;

  while (!feof (stdin)) {
    fread (c, 8, 1, stdin);
    fread (d, 8, 1, stdin);

    if (feof (stdin)) break;

    for (j = 0; j < 8; j++) {
      m = (((unsigned short)c[j]) << 8) | d[j];

      for (i = 7; i >= 0; i--)
	putc (" -+*" [(m >> (i << 1)) & 3],
	      stdout);

      putchar ('\n');
    }

    putchar ('\n');
  }
}
